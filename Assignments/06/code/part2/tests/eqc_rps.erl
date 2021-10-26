-module(eqc_rps).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-export([prop_rps/0]).


prop_rps() -> 
  ?FORALL(Cmds, commands(?MODULE), 
  begin
    {_, S, _} = Result = run_commands(?MODULE, Cmds),
    cleanup(S),
    check_commands(Cmds, Result)   
  end).

check_commands(Cmds, {_, _, Res} = HSRes) ->
  pretty_commands(?MODULE, Cmds, HSRes,
                  aggregate(command_names(Cmds),
                            equals(Res, ok))).
  

cleanup(_S) -> 
  ok.

initial_state() ->
  #{}.

command(_S) -> 
  oneof([]).

next_state(S, _, _) ->
  S.

precondition(_, _) -> true.

postcondition(_, _, _) -> true.