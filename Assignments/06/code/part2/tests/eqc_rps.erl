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

%%% -----------------------------------------------
%%% Model
%%% -----------------------------------------------

-type rps_model() :: #{ serv := none | pid()
                      , coords := list(pid()) 
                      , queue := #{	integer() => pid()}
                      }.
cleanup(#{ serv := none, coords := [] }) ->
  ok;
cleanup(#{ serv := RpsServ, coords := Coords }) ->
  lists:foreach(fun(Coord) -> coordinator:stop(Coord) end, Coords),
  rps:stop(RpsServ).

-spec initial_state() -> rps_model().
initial_state() ->
  #{ serv => none, coords => [], queue => #{} }.

%%% ----------------------------------------------
%%% Generators
%%% ---------------------------------------------

name() -> elements([$a, $m]).
rounds() -> frequency([{1, return(0)},
                       {5, choose(1,5)}]).

%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

-spec command(_) -> any().
command( #{ serv := none }) ->
  return({call, rps, start, [] });
command( #{ serv := BrokerRef }) -> 
  oneof([ {call, rps, queue_up, [BrokerRef, name(), rounds()]}
    
        ]).

next_state(S, {ok, BrokerRef}, {call, rps, start, _}) ->
  S#{ serv := BrokerRef };

next_state(S, V, {call, rps, queue_up, []}) ->
  S#{ serv := V };

next_state(S, _, _) ->
  S.

precondition(_, _) -> true.

postcondition(_, _, _) -> true.