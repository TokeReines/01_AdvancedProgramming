-module(frappe).
-behaviour(gen_server).
-import(item_transformer, [start_item/2, stop_item/1, transform/3, internal_read/1, read_item/2, update/3, insert/3]).
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

-type queue() :: any().
-type key() :: term().
-type value() :: term().
-type cost() :: pos_integer().
-type transformation() :: fun(({existing, value()} | new) ->
  {new_value, value(), cost()} | any()).

%%% -------------------------------------------------------
%%% API
%%% -------------------------------------------------------

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
  gen_server:call(FS, {stable, Key, Ref}).

-spec all_items(pid()) -> term().
all_items(FS) ->
  gen_server:call(FS, all_items).

-spec stop(pid()) -> term().
stop(FS) ->
  gen_server:stop(FS).



%%% -------------------------------------------------------
%%% Callback Functions
%%% -------------------------------------------------------
-spec init(cost()) -> any().
init(Cap) ->
  if Cap < 1 ->
    {error, "Cap is not positive"};
    true -> 
      State = #{
        cap => Cap,
        lru => queue:new(), 
        items => #{},
        stable => #{}
      },
      {ok, State}
  end.

%%% --------------------- Not used -------------------
handle_cast (_, _) ->
  ok.

%%% -------------------- Read Item -------------------------
handle_call({read, Key}, From, State) -> 
  #{items := Items} = State,
  case maps:get(Key, Items, false) of 
    false -> {reply, nothing, State};
    Transformer -> 
      item_transformer:read_item(Transformer, From),
      {noreply, State}
  end;

%%% -------------------- All Items -------------------------
handle_call(all_items, _From, State) -> 
  #{items := Items} = State,
  AllItems = maps:fold(fun(Key, Transformer, List) -> 
    {Value, Cost} = item_transformer:internal_read(Transformer),
    case Value of
      nothing -> List;
      _ -> List ++ [{Key, Value, Cost}]
    end
  end, [], Items),
  {reply, AllItems, State};

%%% -------------------- Set Item -------------------------
handle_call({set, Key, Value, Cost}, From, State) -> 
  #{ items := Items } = State,
  case maps:get(Key, Items, false) of 
    false -> 
      {ok, Transformer} = item_transformer:start_item(self(), Key),
      item_transformer:set(Transformer, Value, Cost, From),
      NItems = add_ghost_item(Key, Transformer, Items),
      {noreply, State#{items := NItems}};
    Transformer -> 
      item_transformer:set(Transformer, Value, Cost, From),
      {noreply, State}
  end;

%%% -------------------- Insert Item -------------------------
handle_call({insert, Key, Value, Cost}, From, State) -> 
  #{ items := Items } = State,
  case maps:get(Key, Items, false) of 
    false -> 
      {ok, Transformer} = item_transformer:start_item(self(), Key),
      item_transformer:insert(Transformer, Value, Cost, From),
      NItems = add_ghost_item(Key, Transformer, Items),
      {noreply, State#{items := NItems}};
    Transformer -> 
      item_transformer:insert(Transformer, Value, Cost, From),
      {noreply, State}
  end;

%%% -------------------- Update Item -------------------------
handle_call({update, Key, Value, Cost}, From, State) -> 
  #{ items := Items } = State,
  case maps:get(Key, Items, false) of 
    false -> {reply, {error, "Item not found"}, State};
    Transformer -> 
      item_transformer:update(Transformer, Value, Cost, From),
      {noreply, State}
  end;

%%% -------------------- Upsert Item -------------------------
handle_call({upsert, Key, Fun}, From, State) -> 
  #{ items := Items} = State,
  case maps:get(Key, Items, false) of 
      false ->
        {ok, Transformer} = item_transformer:start_item(self(), Key),
        NItems = add_ghost_item(Key, Transformer, Items),
        item_transformer:upsert(Transformer, Fun, From),
        {noreply, State#{items := NItems}};
      Transformer ->    
        item_transformer:upsert(Transformer, Fun, From),
        {noreply, State}
  end;

%%% -------------------- Stable -------------------------
handle_call({stable, Key, Ref}, From, State) -> 
  #{ items := Items, stable := Stable } = State,
  NStable = update_stable(Key, Ref, From, Stable),
  case maps:get(Key, Items, false) of
      false -> 
        {reply, ok, State#{stable := NStable}};
      Transformer ->
        item_transformer:stable(Transformer, From, Ref),
        {reply, ok, State#{stable := NStable}}
  end;

%%% -------------------- READ/WRITE (INTERNAL API) -------------------------
handle_call({write, Key, Cost, Value, ReadWrite}, _, State) -> 
  #{ cap := Cap, lru := LRU, items := Items, stable := Stable} = State,
  if Cost > Cap ->
      {reply, {error, "Cap exceeded"}, State};
    Cost =< 0 ->      
      {reply, {error, "Cap is non-positive"}, State};
    true ->  
      case ReadWrite of
        read -> 
          NLRU = update_lru(Key, LRU),
          {reply, ok, State#{lru := NLRU}};
        write ->
          {NLRU, NItems} = write(LRU, Items, Key, Cost, Cap),
          NStable = handle_stable(Key, Value, Stable),
          {reply, ok, State#{lru := NLRU, items := NItems, stable := NStable}}
      end
  end.



%%% -------------------------------------------------------
%%% Auxiliary Functions
%%% -------------------------------------------------------

% Reply to processes awaiting a stable response (listeners)
-spec handle_stable(key(), value(), map()) -> map().
handle_stable(Key, Value, Stable) ->
  Stabled = maps:get(Key, Stable, []),
  lists:foreach(fun({From, Ref}) ->
    gen_server:reply(From, {Ref, Value})
  end, Stabled),
  maps:without([Key], Stable).

% Add process to map of listeners
-spec update_stable(key(), term(), term(), map()) -> map().
update_stable(Key, Ref, From, Stable) ->
  case maps:get(Key, Stable, false) of
    false -> 
      Stable#{Key => [{From, Ref}]};
    IStable -> 
      Stable#{Key := IStable ++ [{From, Ref}]}
  end.

% Update LRU Cache
-spec write(queue(), map(), key(), cost(), cost()) -> {queue(), map()}.
write(LRU, Items, Key, Cost, Cap) -> 
  % Prevent removal of the Key itself if Cap is exceeded and Key is LRU
  NLRU = queue:delete(Key, LRU),
  Transformer = maps:get(Key, Items),
  NItems = maps:remove(Key, Items),
  {ULRU, UItems} = make_room(NLRU, NItems, Cost, Cap),
  add_item(Key, Cost, Transformer, ULRU, UItems).

% A ghost item only exists in the cache (map), not in the LRU. 
% A ghost item will never be popped to make space, and can live in an eternity
-spec add_ghost_item(key(), pid(), map()) -> map().
add_ghost_item(Key, Transformer, Items) ->
  Items#{Key => Transformer}.

% In contrast to a ghost item, this function adds the item to the LRU queue.
% When space is needed, and this item is LRU, it will be popped and stopped
-spec add_item(key(), cost(), pid(), queue(), map()) -> {queue(), map()}.
add_item(Key, _Cost, Transformer, LRU, Items) ->
  {queue:in(Key, LRU), Items#{Key => Transformer}}.

% Moves a key to the front of the LRU queue
-spec update_lru(key(), queue()) -> queue().
update_lru(Key, Queue) ->
  case queue:member(Key, Queue) of
    true -> UQueue = queue:delete(Key, Queue),
            queue:in(Key, UQueue);
    false -> Queue
  end.

-spec get_load(map()) -> cost().
get_load(Items) ->
  maps:fold(fun(_K, T, Sum) -> 
    {_, Cost} = item_transformer:internal_read(T),
    Cost + Sum     
  end, 0, Items).

% LRU queue is possibly full - capacity might have been reached
% Pop items from the LRU queue until we have room for and item with given Cost
-spec make_room(queue(), map(), cost(), cost()) -> {queue(), map()}.
make_room(Queue, Items, Cost, Cap) ->
  Load = get_load(Items),
  % Make room in the queue for the new Cost
  {NLRU, RKeys} = pop_to_cap(Queue, Items, Cost, Cap, Load, []),
  NItems = clean_items(RKeys, Items),
  {NLRU, NItems}.

% Recursively keep popping until we have Capacity for the new Cost
% Returns a list of keys that have been popped
-spec pop_to_cap(queue(), map(), cost(), cost(), cost(), list()) -> {queue(), list(key())}.
pop_to_cap(Queue, Items, Cost, Cap, Load, RKeys) ->
  if Load + Cost =< Cap ->   
      {Queue, RKeys};
    true ->
      case queue:out(Queue) of
        {{value, Key}, NewQueue} -> 
          % Key will always be in dictionary here, no need for a "case of"
          Transformer = maps:get(Key, Items),
          {_, ICost} = item_transformer:internal_read(Transformer),
          pop_to_cap(NewQueue, Items, Cost, Cap, Load - ICost, RKeys ++ [Key]);
        {empty, NewQueue} -> 
          {NewQueue, RKeys}
      end
  end.

% Remove popped items in our Items map to keep them synchronized with the LRU
-spec clean_items(list(key()), map()) -> map().
clean_items(RKeys, Items) ->
  lists:foreach(fun (Key) ->
    case maps:get(Key, Items, false) of 
        false -> 
          ok;
        Transformer -> 
          item_transformer:stop_item(Transformer)
    end
  end, RKeys),
  maps:without(RKeys, Items).