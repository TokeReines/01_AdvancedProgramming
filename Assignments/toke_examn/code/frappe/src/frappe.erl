-module(frappe).
-behaviour(gen_server).
-import(item, []).
-import(queue, []).
-export([init/1, handle_call/3, handle_cast/2]).
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

-spec set(pid(), key(), value(), cost()) -> term().
set(FS, Key, Value, Cost) ->
  gen_server:call(FS, {set, Key, Value, Cost}).

-spec read(pid(), key()) -> term().
read(FS, Key) ->
  gen_server:call(FS, {read, Key}).

-spec insert(pid(), key(), value(), cost()) -> term().
insert(FS, Key, Value, Cost) ->
  gen_server:call(FS, {insert, Key, Value, Cost}).

-spec update(pid(), key(), value(), cost()) -> term().
update(FS, Key, Value, Cost) ->
  gen_server:call(FS, {update, Key, Value, Cost}).

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
        items => #{},
        transformers => #{}
      },
      {ok, State}
  end.

%%% -------------------- Read Item -------------------------
handle_call({read, Key}, _From, State) -> 
  #{items := Items, lru := LRU} = State,
  case maps:get(Key, Items, false) of 
    {Value, _Cost} -> 
      NewLRU = update_lru(Key, LRU),
      {reply, {ok, Value}, State#{lru := NewLRU }};
    false -> {reply, nothing, State}
  end;

%%% -------------------- All Items -------------------------
handle_call(all_items, _From, State) -> 
  #{items := Items} = State,
  AllItems = maps:fold(fun(Key, {Value, Cost}, List) -> List ++ [{Key, Value, Cost}] end, [], Items),
  {reply, AllItems, State};

%%% -------------------- Set Item -------------------------
handle_call({set, Key, Value, Cost}, _From, State) -> 
  #{cap := Cap, 
    lru := LRU,
    items := Items } = State,
  if Cost > Cap ->
    {reply, {error, "Cap exceeded"}, State};
    true -> 
      {NLRU, NItems} = set_item(LRU, Items, Key, Value, Cost, Cap),
      {reply, ok, State#{lru := NLRU, items := NItems}}
  end;

%%% -------------------- Insert Item -------------------------
handle_call({insert, Key, Value, Cost}, _From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items } = State,
  if Cost > Cap ->
      {reply, {error, "Cap exceeded"}, State};
    true -> 
      case maps:get(Key, Items, false) of 
        {Value, Cost} -> 
          {reply, {error, "Duplicate item"}, State};
        false -> 
          {NLRU, NItems} = set_item(LRU, Items, Key, Value, Cost, Cap),
          {reply, ok, State#{lru := NLRU, items := NItems}}     
      end
  end;

%%% -------------------- Update Item -------------------------
handle_call({update, Key, Value, Cost}, _From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items } = State,
  if Cost > Cap ->
      {reply, {error, "Cap exceeded"}, State};
    true -> 
      case maps:get(Key, Items, false) of 
        {Value, Cost} -> 
          {NLRU, NItems} = set_item(LRU, Items, Key, Value, Cost, Cap),
          {reply, ok, State#{lru := NLRU, items := NItems}};
        false ->                
          {reply, {error, "Item not found"}, State}
      end
  end;

%%% -------------------- Upsert Item -------------------------
handle_call({upsert, Key, Fun}, From, State) -> 
  #{ cap := Cap, lru := LRU, items := Items, transformers := Transformers} = State,
  {Transformer, NewTransformers} = get_transformer(Key, Transformers),
  NewState = State#{transformers => NewTransformers},
  case maps:get(Key, Items, false) of 
      {Value, Cost} -> 
        item_transformer:transform(Transformer, From, Fun, {existing, Value}),
        {noreply, NewState};
      false ->                
        item_transformer:transform(Transformer, From, Fun, new),
        {noreply, NewState}
  end.

%%% -------------------- Stable -------------------------
handle_cast({stable, Key, Ref}, State) -> 
  #{ items := Items } = State,
  case maps:get(Key, Items, false) of 
      {Value, Cost, Transformer} ->
        item_transformer:transform() ,
        gen_statem:cast(Transformer),
        {noreply, State};
      false ->                
        {noreply, State}
  end.

%%% -------------------------------------------------------
%%% Auxiliary Functions
%%% -------------------------------------------------------

get_transformer(Key, Transformers) ->
  case maps:get(Key, Transformers, false) of 
    Transformer -> 
      {Transformer, Transformers};
    false -> 
      NewTransformer = item_transformer:start(self()),
      {NewTransformer, Transformers#{Key => NewTransformer}}
  end.

set_item(LRU, Items, Key, Value, Cost, Cap) -> 
  {NLRU, RItems} = make_room(LRU, Items, Key, Cost, Cap),
  CItems = clean_items(Items, RItems),
  ULRU = update_lru(Key, NLRU),
  NewItems = CItems#{Key => {Value, Cost}},
  {ULRU, NewItems}.

update_lru(Key, Queue) ->
  UQueue = queue:delete(Key, Queue),
  queue:in(Key, UQueue).

get_load(Items) ->
  maps:fold(fun(_K, {_V, Cost}, Sum) -> Cost + Sum end, 0, Items).

% Remove popped items in our Items map to keep them synchronized
clean_items(Items, RItems) ->
  maps:without(RItems, Items).

make_room(Queue, Items, Key, Cost, Cap) -> 
  NQueue = queue:delete(Key, Queue),
  % Avoid duplicates by removing it
  Load = get_load(Items),
  % Make room in the queue for the new Cost
  pop_to_cap(NQueue, Items, Cost, Cap, Load, []).

% Keep popping until we have Capacity for the new Cost
pop_to_cap(Queue, Items, Cost, Cap, Load, RItems) ->
  if Load + Cost =< Cap ->    
      {Queue, RItems};
    true ->
      case queue:out(Queue) of
        {{value, Key}, NewQueue} -> 
          {_Value, ICost} = maps:get(Key, Items),
          pop_to_cap(NewQueue, Items, Cost, Cap, Load - ICost, RItems ++ [Key]);
        {empty, NewQueue} -> {NewQueue, RItems}
      end
  end.
