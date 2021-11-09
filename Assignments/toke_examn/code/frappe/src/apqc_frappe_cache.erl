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
      {_,S,_} = Result = run_commands(?MODULE, Cmds),
      cleanup(S),
      check_commands(Cmds, Result)
  end).

check_commands(Cmds, {_,_,Res} = HSRes) ->
  pretty_commands(?MODULE, Cmds, HSRes,
                aggregate(command_names(Cmds),
                          equals(Res, ok))).

cleanup(#{ f := none, items := [] }) ->
  ok;

cleanup(#{ f := F, items := Items }) ->
  % lists:foreach(fun(Item) -> item_transformer:stop_item(Item) end, Items),
  frappe:stop(F).

%%% -----------------------------------------------
%%% Model
%%% -----------------------------------------------

-type frappe_model() :: #{ f := none | pid(), items := list(pid())}.

-spec initial_state() -> frappe_model().
initial_state() ->   
  #{ f => none, items => [] }.

model_insert() -> not_implemented.
model_read() -> not_implemented.
model_set() -> not_implemented.
model_update() -> not_implemented.
model_upsert() -> not_implemented.

%%% ----------------------------------------------
%%% Wrapper functions
%%% ---------------------------------------------

frappe_fresh(Cap) -> 
  {ok, FS} = frappe:fresh(Cap),
  FS.

frappe_read(FS, Key) -> 
  frappe:read(FS, Key).

frappe_insert(FS, Key, Value, Cost) -> 
  frappe:insert(FS, Key, Value, Cost).

frappe_set(FS, Key, Value, Cost) -> 
  frappe:set(FS, Key, Value, Cost).

frappe_all_items(FS) -> 
  {ok, Value} = frappe:all_items(FS),
  Value.

frappe_upsert(FS, Key, Fun) ->
  spawn(fun() -> frappe:upsert(FS, Key, Fun) end).

%%% ----------------------------------------------
%%% Generators
%%% ---------------------------------------------

key() -> nat(). %elements([a,b,c,d,e,f,g,h]).
key_from(Items) -> elements([Key || {Key, _} <- Items] ++ [snowflake]).
value() -> nat().
cost() -> nat().

gen_transform(Value, Cost) -> 
   fun (new) -> 
        {new_value, Value, Cost};
      ({existing, _OldValue}) ->
        {new_value, Value, Cost}
  end.


%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ f := none }) ->
  return({call, ?MODULE, frappe_fresh, [5] });

command( #{ f := F, items := Items }) -> 
  frequency([ { 1, {call, ?MODULE, frappe_set, [F, key(), value(), cost()]}}
            , { 4, {call, ?MODULE, frappe_read, [F, key_from(Items)]}}
  ]).
  % frequency([{1, {call, ?MODULE, frappe_read, [Frappe]}}]).
    % {5, oneof([{call, ?MODULE, rps_queue_up, [FS, name(), rounds(Queue)]} || maps:keys(Queue) /= []]
    %           ++ [{call, ?MODULE, spawn_queue_up, [FS, name(), rounds()]}])},
    % {5, ?LET({Coordinator, Player}, move_gen(Coordinators, Players), 
    %          {call, rps, move, [Coordinator, Player, move()] }}

next_state(S, V, {call, _, frappe_fresh, _}) ->
  S#{ f := V };

next_state(S, _V, {call, _, read, _}) ->
  S;

next_state(S, _, _) ->
  S.



precondition(_, _) -> true.

postcondition(_, _, _) -> true.

% c(qc_rps).  
% eqc:quickcheck(qc_rps:prop_rps()).