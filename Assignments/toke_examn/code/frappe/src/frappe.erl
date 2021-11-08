-module(frappe).
-behaviour(gen_server).
-import(item_transformer, [start_item/2, stop_item/1, transform/4, read_sync/1, read_async/2, update/3]).
-import(queue, []).
-export([init/1, handle_call/3, handle_cast/2, clean_items/2]).
% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1,
         set/4,
         read/2,
         insert/4,
         update/4,
         upsert/3,
         stable/3,
         all_items/1,
         stop/1
        ]).

-type key() :: term().
-type value() :: term().
-type cost() :: pos_integer().
-type transformation() :: fun(({existing, value()} | new) ->
  {new_value, value(), cost()} | any()).

%%% -------------------------------------------------------
%%% API
%%% -------------------------------------------------------

% Jeg tænker en Frappe gen_server, og en gen_statem til hvert item. Så kan man holde track på om et item er ved at blive "write"d i. Frappe skal så have holde styr på sin capacity, en queue for LRU, og et dict der mapper keys til processes
% Starts a new Frappe server with capacity Cap > 0
-spec fresh(pos_integer()) -> term().
fresh(Cap) ->
  gen_server:start(?MODULE, Cap, []).

% Replaces an Item process associated 
%   with the Key, with a new Item process with initial Value and Cost
% Any ongoing work in the Item process is stopped and the process is killed
-spec set(pid(), key(), value(), cost()) -> term().
set(FS, Key, Value, Cost) ->
  gen_server:call(FS, {set, Key, Value, Cost}).

% Reads value from an Item process
-spec read(pid(), key()) -> term().
read(FS, Key) ->
  gen_server:call(FS, {read, Key}).

% Starts an Item process with initial Value and Cost.
% If Key is already associated with an Item, returns an error
-spec insert(pid(), key(), value(), cost()) -> term().
insert(FS, Key, Value, Cost) ->
  gen_server:call(FS, {insert, Key, Value, Cost}).

% Updates the value of an Item process 
% If Key isn't associated with an Item, returns an error
-spec update(pid(), key(), value(), cost()) -> term().
update(FS, Key, Value, Cost) ->
  gen_server:call(FS, {update, Key, Value, Cost}).

% Starts an Item process associated with a Key 
% where Value and Cost is calculated in the Transformation.
% If an Item process is already associated with a key, the transformation i queued up
-spec upsert(pid(), key(), transformation()) -> any().
upsert(FS, Key, Fun) ->
  gen_server:call(FS, {upsert, Key, Fun}).

-spec stable(pid(), key(), any()) -> term().
stable(FS, Key, Ref) ->
  gen_server:cast(FS, {stable, Key, Ref}).

-spec all_items(pid()) -> term().
all_items(FS) ->
  gen_server:call(FS, all_items).

-spec stop(pid()) -> term().
stop(FS) ->
  % Does this just kill it or call the server to manually cleanup?
  gen_server:stop(FS).



%%% -------------------------------------------------------
%%% Callback Functions
%%% -------------------------------------------------------

init(Cap) ->
  if Cap < 1 ->
    {error, "Cap is not positive"};
    true -> 
      State = #{
        cap => Cap,
        lru => queue:new(), 
        items => #{}
      },
      {ok, State}
  end.

%%% -------------------- Read Item -------------------------
handle_call({read, Key}, _From, State) -> 
  #{items := Items, lru := LRU} = State,
  NewLRU = update_lru(Key, LRU),
  case maps:get(Key, Items, false) of 
    false -> {reply, nothing, State};
    {none, _C, _T} -> {reply, nothing, State#{lru := NewLRU }};
    {Value, _C, _T} -> {reply, {ok, Value}, State#{lru := NewLRU }}
  end;

%%% -------------------- All Items -------------------------
handle_call(all_items, _From, State) -> 
  #{items := Items} = State,
  AllItems = maps:fold(fun(Key, {Value, Cost, _T}, List) -> 
    case Value of
      nothing -> List;
      _ -> List ++ [{Key, Value, Cost}]
    end
  end, [], Items),
  {reply, AllItems, State};

%%% -------------------- Set Item -------------------------
handle_call({set, Key, Value, Cost}, _From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items } = State,
  if Cost > Cap ->
    {reply, {error, "Cap exceeded"}, State};
    true -> 
      {NLRU, NItems} = prepare_LRU(LRU, Items, Key, Cost, Cap),
      case maps:get(Key, NItems, false) of 
          false -> 
            {NewLRU, NewItems, _} = add_item(Key, Value, Cost, NItems, NLRU),
            {reply, ok, State#{lru := NewLRU, items := NewItems}};
          {_, _, Transformer} -> 
            NewItems = update_item(Key, Value, Cost, Transformer, NItems),
            {reply, ok, State#{lru := NLRU, items := NewItems}}
        end
  end;

%%% -------------------- Insert Item -------------------------
handle_call({insert, Key, Value, Cost}, _From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items } = State,
  if Cost > Cap ->
      {reply, {error, "Cap exceeded"}, State};
    true -> 
      case maps:get(Key, Items, false) of 
        false -> 
          {NLRU, NItems} = prepare_LRU(LRU, Items, Key, Cost, Cap),
          {NewLRU, NewItems, _} = add_item(Key, Value, Cost, NItems, NLRU),
          {reply, ok, State#{lru := NewLRU, items := NewItems}};
        {Value, Cost, _Transformer} -> 
          {reply, {error, "Duplicate item"}, State}
      end
  end;

%%% -------------------- Update Item -------------------------
handle_call({update, Key, Value, Cost}, _From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items } = State,
  if Cost > Cap ->
      {reply, {error, "Cap exceeded"}, State};
    true -> 
      case maps:get(Key, Items, false) of 
        false -> {reply, {error, "Item not found"}, State};
        {_, _, Transformer} -> 
          {NLRU, NItems} = prepare_LRU(LRU, Items, Key, Cost, Cap),
          NewItems = update_item(Key, Value, Cost, Transformer, NItems),
          {reply, ok, State#{lru := NLRU, items := NewItems}}
      end
  end;

%%% -------------------- Upsert Item -------------------------
handle_call({upsert, Key, Fun}, From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items} = State,
  case maps:get(Key, Items, false) of 
      false ->   
        % Cost (0) and Value (none) is not yet known
        {_, NItems} = prepare_LRU(LRU, Items, Key, 0, Cap),
        {NQueue, NItems, Transformer} = add_item(Key, none, 0, NItems, LRU),
        item_transformer:transform(Transformer, From, Fun, new),
        {noreply, State#{items => NItems, lru => NQueue}};
      {Value, _, Transformer} ->        
        item_transformer:transform(Transformer, From, Fun, {existing, Value}),
        {noreply, State}
  end;

%%% -------------------- Stable -------------------------
handle_call({stable, Key, Ref}, From, State) -> 
  #{ items := Items } = State,
  case maps:get(Key, Items, false) of 
      false ->                
        {reply, nothing, State};
      {Value, Cost, Transformer} ->
        item_transformer:stable(read) ,
        gen_statem:cast(Transformer),
        {noreply, State}
  end.

%%% -------------------------------------------------------
%%% Auxiliary Functions
%%% -------------------------------------------------------

prepare_LRU(LRU, Items, Key, Cost, Cap) -> 
  {NLRU, NItems} = make_room(LRU, Items, Key, Cost, Cap),
  ULRU = update_lru(Key, NLRU),
  {ULRU, NItems}.

add_item(Key, Value, Cost, Items, Queue) ->
  {ok, Transformer} = item_transformer:start_item(self(), Key),
  NQueue = queue:in(Key, Queue),
  {NQueue, Items#{Key => {Value, Cost, Transformer}}, Transformer}.

update_item(Key, Value, Cost, Transformer, Items) ->
  Items#{Key => {Value, Cost, Transformer}}.

update_lru(Key, Queue) ->
  case queue:member(Key, Queue) of
    true -> UQueue = queue:delete(Key, Queue),
            queue:in(Key, UQueue);
    false -> Queue
  end.

get_load(Items) ->
  maps:fold(fun(_K, {_, Cost, _}, Sum) -> 
    Cost + Sum 
  end, 0, Items).

make_room(Queue, Items, Key, Cost, Cap) -> 
  % Prevent removal of the Key itself if Cap is exceeded and Key is LRU
  NQueue = queue:delete(Key, Queue),
  % Avoid duplicates by removing it
  Load = get_load(Items),
  % Make room in the queue for the new Cost
  {NLRU, RKeys} = pop_to_cap(NQueue, Items, Cost, Cap, Load, []),
  NItems = clean_items(RKeys, Items),
  {NLRU, NItems}.

% Keep popping until we have Capacity for the new Cost
pop_to_cap(Queue, Items, Cost, Cap, Load, RKeys) ->
  if Load + Cost =< Cap ->   
      {Queue, RKeys};
    true ->
      case queue:out(Queue) of
        {{value, Key}, NewQueue} -> 
          % Key will always be in dictionary here, no need for a "case of"
          {IValue, ICost, _} = maps:get(Key, Items),
          pop_to_cap(NewQueue, Items, Cost, Cap, Load - ICost, RKeys ++ [Key]);
        {empty, NewQueue} -> 
          {NewQueue, RKeys}
      end
  end.

% Remove popped items in our Items map to keep them synchronized
clean_items(RKeys, Items) ->
  lists:foreach(fun (Key) ->
    case maps:get(Key, Items, false) of 
        false -> 
          ok;
        {_, _, Transformer} -> 
          item_transformer:stop_item(Transformer)
    end
  end, RKeys),
  maps:without(RKeys, Items).