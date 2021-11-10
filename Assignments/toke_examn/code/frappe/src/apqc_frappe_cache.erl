-module(apqc_frappe_cache).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, precondition/2, postcondition/3, next_state/3]).
-export([prop_cache_under_capacity/0]).
-export([frappe_fresh/1]).

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


prop_cache_under_capacity() -> 
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
cleanup(#{ f := F, items := Items }) -> frappe:stop(F).

%%% -----------------------------------------------
%%% Model
%%% -----------------------------------------------

-type frappe_model() :: #{ f := none | pid(), items := any(), cost := any()}.

-spec initial_state() -> frappe_model().
initial_state() ->   
  #{ f => none, items => #{}, cap => 0 }.


model_insert(#{items := Items } = S, Item) -> 
  {Key, Value, Cost} = Item,
  case Cost =< 0 of
    true -> S;
    false ->
      case maps:get(Key, Items, false) of
        false -> S#{ items => Items# {Key => {Value, Cost}}};
        _ -> S
      end
  end.

model_set(#{items := Items, cap := Cap} = S, Item) -> 
  {Key, Value, Cost} = Item,
  case Cost =< 0 of
    false -> S#{items => Items# {Key => {Value, Cost}}};
    true -> S
  end.

model_update(#{items := Items, cap := Cap} = S, Item) -> 
  {Key, Value, Cost} = Item,
  case Cost =< 0 of
    true -> S;
    false ->
      case model_is_key(S, Key) of
        false -> S;
        _ -> 
          S#{ items => Items#{Key => {Value, Cost}}}
      end
  end.

%%% ----------------- Model Helpers -------------------
get_load(Items) ->
  maps:fold(fun(_K, {_V, Cost}, Load) -> 
    Cost + Load     
  end, 0, Items).

model_is_set_cost_valid(#{cap := Cap, items := Items}, Item) ->
  {Key, _V, Cost} = Item,
  NItems = maps:without([Key], Items),
  Load = get_load(NItems),
  if 
    Load + Cost > Cap -> false;
    true -> true  
  end.

model_is_insert_cost_valid(#{cap := Cap, items := Items}, Item) ->
  {_K, _V, Cost} = Item,
  Load = get_load(Items),
  if 
    Load + Cost > Cap -> false;
    true -> true  
  end.

model_is_key_value(#{ items := Items} = _S, Item) ->
  {Key, Value, Cost} = Item,
  case maps:get(Key, Items, false) of
    false -> false;
    {IValue, ICost} -> (IValue =:= Value) and (ICost =:= Cost)
  end.

model_is_key(#{ items := Items} = _S, Key) ->
  case maps:get(Key, Items, false) of
    false -> false;
    _ -> true
  end.

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
cost() -> choose(0,1).
capacity() -> choose(9999, 99999).


%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ f := none }) ->
  return({call, ?MODULE, frappe_fresh, [capacity()] });

command( #{ f := F, items := Items }) -> 
  frequency([  { 1, {call, frappe, insert, [F, key_maybe_from(Items), value(), cost()]}}
             , { 1, {call, frappe, read, [F, key_from(Items)]}}
             , { 1, {call, frappe, all_items, [F]}}
             , { 1, {call, frappe, set, [F, key_maybe_from(Items), value(), cost()]}}
             , { 1, {call, frappe, update, [F, key_maybe_from(Items), value(), cost()]}}
             % , { 1, {call, frappe, update, [F, key_maybe_from(Items), eqc_frappe:sym_mktrans(eqc_frappe:opr_gen())]}}
  ]).          

next_state(S, V, {call, _, frappe_fresh, [Capacity]}) ->
  S#{ f := V, cap := Capacity };

next_state(S, _V, {call, _, insert, [_, Key, Value, Cost]}) ->
  model_insert(S, {Key, Value, Cost});

next_state(S, _V, {call, _, set, [_, Key, Value, Cost]}) ->
  model_set(S, {Key, Value, Cost});

next_state(S, _V, {call, _, update, [_, Key, Value, Cost]}) ->
  model_update(S, {Key, Value, Cost});

next_state(S, _, _) ->
  S.

precondition(S, {call, _, set,[_, Key, Value, Cost]}) ->
  model_is_set_cost_valid(S, {Key, Value, Cost});

precondition(S, {call, _, insert,[_, Key, Value, Cost]}) ->
  model_is_insert_cost_valid(S, {Key, Value, Cost});

precondition(_, _) -> 
  true.

postcondition(_S, {call, _, set, [_, Key, Value, Cost]}, Res) -> 
  case Cost > 0 of
    true -> Res =:= ok;
    false -> 
      case Res of
        ok -> false;
        {error, _} -> true
      end
  end;

postcondition(S, {call, _, insert, [_, Key, _, Cost]}, Res) -> 
  case model_is_key(S, Key) of
    true -> 
      case Res of
        {error, _} -> true;
        ok -> false
       end;
    false ->
      case Res of
        {error, _} -> Cost =< 0;
        ok -> Cost > 0
      end
  end;

postcondition(S, {call, _, update, [_, Key, _, Cost]}, Res) -> 
  case model_is_key(S, Key) of
    true ->       
      case Res of
        {error, _} -> Cost =< 0;
        ok -> Cost > 0
      end;
    false ->
      case Res of
        {error, _} -> true;
        ok -> false
       end
  end;

postcondition(#{items := Items} = S, {call, _, read, [_, Key]}, Res) -> 
  case maps:get(Key, Items, false) of
    false -> Res =:= nothing;
    {Value, _C} -> 
      case Res of
        {ok, RValue} -> Value =:= RValue;
        _ -> false
      end
  end;

postcondition(#{items := Items} = S, {call, _, all_items, _}, Res) -> 
  lists:all(fun({K, {Value, Cost}}) ->
      lists:member({K, Value, Cost}, Res)
  end, maps:to_list(Items));

postcondition(#{items := Items} = _S, {call, _, frappe_fresh, [_]}, Res) -> 
  true;

postcondition(_, _, _) -> 
  true.
