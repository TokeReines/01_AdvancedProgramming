-module(apqc_frappe_lru).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, precondition/2, postcondition/3, next_state/3]).
-export([prop_cache_lru_cap_invariant/0]).
-export([frappe_fresh/1]).

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


prop_cache_lru_cap_invariant() -> 
  ?FORALL(Cmds, commands(?MODULE),
  begin
      %eqc:format("~p~n", [Cmds]),
      {_,S,_} = Result = run_commands(?MODULE, Cmds),
      cleanup(S),
      check_commands(Cmds, Result)
  end).

check_commands(Cmds, {_,_,Res} = HSRes) ->
  pretty_commands(?MODULE, Cmds, HSRes,
                aggregate(command_names(Cmds),
                          equals(Res, ok))).

cleanup(#{ f := none }) -> ok;
cleanup(#{ f := F, items := Items , lru := LRU}) -> frappe:stop(F).

%%% -----------------------------------------------
%%% Model
%%% -----------------------------------------------

-type frappe_model() :: #{ f := none | pid(), items := any(), cost := any()}.

-spec initial_state() -> frappe_model().
initial_state() ->   
  #{ f => none, items => #{}, cap => 0, lru => queue:new()}.


model_insert(#{items := Items, cap := Cap, lru := LRU } = S, Item) -> 
  {Key, Value, Cost} = Item,
  case (Cost =< 0) or (Cost > Cap) of
    true -> S;
    false ->
      case maps:get(Key, Items, false) of
        false ->           
          DLRU = queue:delete(Key, LRU),
          Load = get_load(Items),
          Goal = Cap - Cost,
          {NLRU, NItems} = pop_lru(DLRU, Items, Load, Goal),
          S#{ items => NItems# {Key => {Value, Cost}}, lru => queue:in(Key, NLRU) };
        _ -> S
      end
  end.

model_set(#{items := Items, cap := Cap, lru := LRU } = S, Item) -> 
  {Key, Value, Cost} = Item,
  case (Cost =< 0) or (Cost > Cap) of
    true -> S;
    false ->
        DLRU = queue:delete(Key, LRU),
        DItems = maps:remove(Key, Items),
        Load = get_load(DItems),
        Goal = Cap - Cost,
        {NLRU, NItems} = pop_lru(DLRU, DItems, Load, Goal),
        S#{ items => NItems# {Key => {Value, Cost}}, lru => queue:in(Key, NLRU) }
  end.

model_update(#{items := Items, cap := Cap, lru := LRU } = S, Item) -> 
  {Key, Value, Cost} = Item,
  case (Cost =< 0) or (Cost > Cap) of
    true -> S;
    false ->
      case maps:get(Key, Items, false) of
        false -> S;
        _ ->
          DLRU = queue:delete(Key, LRU),
          DItems = maps:remove(Key, Items),
          Load = get_load(DItems),
          Goal = Cap - Cost,
          {NLRU, NItems} = pop_lru(DLRU, DItems, Load, Goal),
          S#{ items => NItems# {Key => {Value, Cost}}, lru => queue:in(Key, NLRU) }
      end
  end.

%%% ----------------- Model Helpers -------------------

pop_lru(LRU, Items, Load, Goal) ->
  if Load =< Goal ->   
      {LRU, Items};
    true ->
      case queue:out(LRU) of
        {{value, Key}, NLRU} -> 
          {_, Cost} = maps:get(Key, Items),
          pop_lru(NLRU, maps:without([Key], Items), Load - Cost, Goal);
        {empty, NLRU} -> 
          {NLRU, Items}
      end
  end.

get_load(Items) ->
  maps:fold(fun(_K, {_V, Cost}, Load) -> 
    Cost + Load     
  end, 0, Items).

%%% ----------------------------------------------
%%% Wrapper functions
%%% ---------------------------------------------

frappe_fresh(Cap) -> 
  Res = frappe:fresh(Cap),
  {ok, FS} = Res,
  FS.

%%% ----------------------------------------------
%%% Generators
%%% ---------------------------------------------

key() -> elements([a,b,c,d,e,f,g,h]).
key_from(Items) -> elements([Key || Key <- maps:keys(Items)] ++ [snowflake]).
key_maybe_from(Items) -> oneof([key_from(Items), key()]).

value() -> nat().
cost() -> choose(0,5).
capacity() -> choose(9999, 99999).


%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ f := none }) ->
  return({call, ?MODULE, frappe_fresh, [5] });

command( #{ f := F, items := Items }) -> 
  frequency([  { 2, {call, frappe, insert, [F, key_maybe_from(Items), value(), cost()]}}
             , { 1, {call, frappe, read, [F, key_from(Items)]}}
             , { 1, {call, frappe, all_items, [F]}}
             , { 2, {call, frappe, set, [F, key_maybe_from(Items), value(), cost()]}}
             , { 2, {call, frappe, update, [F, key_maybe_from(Items), value(), cost()]}}
  ]).          

next_state(S, V, {call, _, frappe_fresh, [Capacity]}) ->
  S#{ f := V, cap := Capacity };

next_state(#{cap := Cap} = S, _V, {call, _, insert, [_, Key, Value, Cost]}) ->
  model_insert(S, {Key, Value, Cost});

next_state(S, _V, {call, _, set, [_, Key, Value, Cost]}) ->
  model_set(S, {Key, Value, Cost});

next_state(S, _V, {call, _, update, [_, Key, Value, Cost]}) ->
  model_update(S, {Key, Value, Cost});

next_state(S, _, _) ->
  S.

precondition(_, _) -> 
  true.

postcondition(#{cap := Cap, items := Items}, _, _) -> 
  get_load(Items) =< Cap.
