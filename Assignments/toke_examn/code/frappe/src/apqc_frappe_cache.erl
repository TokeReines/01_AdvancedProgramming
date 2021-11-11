-module(apqc_frappe_cache).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, precondition/2, postcondition/3, next_state/3, command/1]).
-export([prop_cache_under_capacity_impl/0]).
-export([frappe_fresh/1]).

-import(eqc_frappe, [mktrans_impl/2, sym_mktrans_impl/1, terminating_transformation_impl/1]).


prop_cache_under_capacity_impl() -> 
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
cleanup(#{ f := F }) -> frappe:stop(F).

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

model_set(#{items := Items} = S, Item) -> 
  {Key, Value, Cost} = Item,
  case Cost =< 0 of
    false -> S#{items => Items# {Key => {Value, Cost}}};
    true -> S
  end.

model_update(#{items := Items} = S, Item) -> 
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

model_upsert(#{items := Items, cap := Cap} = S, {Key, Fun}) -> 
  case run_transform_fun(Fun, Key, Items) of
    {new_value, NValue, NCost} -> 
      case (NCost > 0) and (NCost =< Cap) of
        false -> S;
        true ->  S#{items => Items# {Key => {NValue, NCost}}}
      end;
    _ -> 
      S
  end.

%%% ----------------- Model Helpers -------------------
run_transform_fun(Fun, Key, Items) ->
  try
    case maps:get(Key, Items, false) of
      false -> 
        Fun(new);
      {Value, _} -> 
        Fun({existing, Value})
    end
  catch
      throw : Throw -> 
        case Throw of
          {new_value, Val, C} -> {new_value, Val, C};
          _ -> nothing
        end;
      _ : _ -> nothing 
  end.

get_load(Items) ->
  maps:fold(fun(_K, {_V, Cost}, Load) -> 
    Cost + Load     
  end, 0, Items).

model_is_update_cost_valid(#{cap := Cap, items := Items}, Item) ->
 {Key, _V, Cost} = Item,
  NItems = maps:without([Key], Items),
  Load = get_load(NItems),
  if 
    Load + Cost > Cap -> false;
    true -> true  
  end.

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

frappe_stable(FS, Key) -> 
  spawn(fun() ->
    Stable = frappe:stable(FS, Key),
    receive
      Res -> Res
    end
  end).

res_is_error(Res) ->
  case Res of
    {error, _} -> true;
    _ -> false
  end.

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
             , { 1, ?LET({Key, Fun}, 
                         terminating_transformation_impl(key_maybe_from(Items)), 
                         {call, frappe, upsert, [F, Key, Fun]})}
  ]).          

next_state(S, V, {call, _, frappe_fresh, [Capacity]}) ->
  S#{ f := V, cap := Capacity };

next_state(S, _V, {call, _, insert, [_, Key, Value, Cost]}) ->
  model_insert(S, {Key, Value, Cost});

next_state(S, _V, {call, _, set, [_, Key, Value, Cost]}) ->
  model_set(S, {Key, Value, Cost});

next_state(S, _V, {call, _, update, [_, Key, Value, Cost]}) ->
  model_update(S, {Key, Value, Cost});

next_state(S, _V, {call, _, upsert, [_, Key, Fun]}) ->
  model_upsert(S, {Key, Fun});

next_state(S, _, _) ->
  S.

% precondition(S, {call, _, set,[_, Key, Value, Cost]}) ->
%   model_is_set_cost_valid(S, {Key, Value, Cost});

% precondition(S, {call, _, insert,[_, Key, Value, Cost]}) ->
%   model_is_insert_cost_valid(S, {Key, Value, Cost});

% precondition(S, {call, _, insert,[_, Key, Value, Cost]}) ->
%   model_is_update_cost_valid(S, {Key, Value, Cost});

precondition(_, _) -> 
  true.

postcondition(_S, {call, _, set, [_, _, _, Cost]}, Res) -> 
  case Cost > 0 of
    true -> Res =:= ok;
    false -> Res =/= ok
  end;

postcondition(#{ items := Items, cap := Cap }, {call, _, upsert, [_, Key, Fun]}, Res) -> 
  case run_transform_fun(Fun, Key, Items) of
    {new_value, _, NCost} ->       
      case (NCost > 0) and (NCost =< Cap) of
        true -> 
          Res =:= ok;
        false -> 
          case Res of
            ok -> false;
            _ -> true
          end
      end;
    error ->     
      Res =/= ok
  end;

postcondition(S, {call, _, insert, [_, Key, _, Cost]}, Res) -> 
  case model_is_key(S, Key) of
    true -> res_is_error(Res);
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
    false -> res_is_error(Res)
  end;

postcondition(#{items := Items}, {call, _, read, [_, Key]}, Res) -> 
  case maps:get(Key, Items, false) of
    false -> Res =:= nothing;
    {Value, _C} -> 
      case Res of
        {ok, RValue} -> Value =:= RValue;
        _ -> false
      end
  end;

postcondition(#{items := Items}, {call, _, all_items, _}, Res) -> 
  lists:all(fun({K, {Value, Cost}}) ->
      lists:member({K, Value, Cost}, Res)
  end, maps:to_list(Items));

postcondition(_, _, _) -> 
  true.
