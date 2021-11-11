-module(apqc_frappe_lru).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, precondition/2, postcondition/3, next_state/3, command/1]).
-export([prop_capacity_invariant_impl/0]).
-export([frappe_fresh/1]).

-import(eqc_frappe, [mktrans_impl/2, sym_mktrans_impl/1, terminating_transformation_impl/1]).

prop_capacity_invariant_impl() -> 
  ?FORALL(Cmds, commands(?MODULE),
  begin
      %eqc:format("~p~n", [Cmds]),
      {_,S,_} = Result = run_commands(?MODULE, Cmds),
      cleanup(S),
      collect(length(Cmds), check_commands(Cmds, Result))
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

model_upsert(#{items := Items, cap := Cap, lru := LRU} = S, {Key, Fun}) -> 
  case run_transform_fun(Fun, Key, Items) of
    {new_value, NValue, NCost} -> 
      case (NCost =< 0) or (NCost > Cap) of
        true -> S;
        false ->
          DLRU = queue:delete(Key, LRU),
          DItems = maps:remove(Key, Items),
          Load = get_load(DItems),
          Goal = Cap - NCost,
          {NLRU, NItems} = pop_lru(DLRU, DItems, Load, Goal),
          S#{ items => NItems# {Key => {NValue, NCost}}, lru => queue:in(Key, NLRU) }
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
% capacity() -> choose(9999, 99999).


%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ f := none }) ->
  return({call, ?MODULE, frappe_fresh, [5] });

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

precondition(_, _) -> 
  true.

postcondition(#{cap := Cap, items := Items}=S, {call, _, insert, [_, Key, Value, Cost]}, Res) -> 
  case Cost =< Cap of
    false ->
      get_load(Items) =< Cap;

    true -> 
      case maps:get(Key, Items, false) of        
        false -> 
          #{items := NItems} = model_insert(S, {Key, Value, Cost}),
          get_load(NItems) =< Cap;

        _ -> 
          get_load(Items) =< Cap
      end
  end;

postcondition(#{cap := Cap, items := Items}=S, {call, _, update, [_, Key, Value, Cost]}, Res) -> 
  case Cost =< Cap of
    false ->
      get_load(Items) =< Cap;
    
    true -> 
      case maps:get(Key, Items, false) of
        false -> 
          get_load(Items) =< Cap;

        _ -> 
          TItems = maps:without([Key], Items),
          #{items := NItems} = model_update(S#{items := TItems}, {Key, Value, Cost}),
          get_load(NItems) =< Cap
      end
  end;

postcondition(#{cap := Cap, items := Items}=S, {call, _, set, [_, Key, Value, Cost]}, Res) -> 
  case Cost =< Cap of
    false ->
      get_load(Items) =< Cap;
    
    true -> 
      TItems = maps:without([Key], Items),
      #{items := NItems} = model_update(S#{items := TItems}, {Key, Value, Cost}),
      get_load(NItems) =< Cap
  end;

postcondition(#{ items := Items, cap := Cap }=S, {call, _, upsert, [_, Key, Fun]}, Res) -> 
  case run_transform_fun(Fun, Key, Items) of
    {new_value, NValue, NCost} ->       
      case (NCost > 0) and (NCost =< Cap) of
        true -> 
          TItems = maps:without([Key], Items),
          #{items := NItems} = model_set(S#{items := TItems}, {Key, NValue, NCost}),
          get_load(NItems) =< Cap;
        false -> 
          get_load(Items) =< Cap
      end;
    error ->     
      get_load(Items) =< Cap
  end;

% postcondition(#{ items := Items, cap := Cap }=S, {call, _, upsert, [_, Key, Fun]}, Res) -> 
%   case run_transform_fun(Fun, Key, Items) of
%     {new_value, NValue, NCost} ->       
%       case (NCost =< 0) or (NCost > Cap) of
%         true ->
%           get_load(Items) =< Cap;
%         false ->
%           TItems = maps:without([Key], Items),
%           #{items := NItems} = model_set(S#{items := TItems}, {Key, NValue, NCost}),
%           get_load(NItems) =< Cap
%       end;
%     error ->      
%       get_load(Items) =< Cap
%   end;


postcondition(#{cap := Cap, items := Items}, _, _) ->
  get_load(Items) =< Cap.
