-module(qc_rps).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-export([prop_rps/0]).

-export([rps_start/0, spawn_queue_up/3]).

-export([name/0, rounds/0]).


prop_rps() -> 
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


%%% -----------------------------------------------
%%% Model
%%% -----------------------------------------------

-type rps_model() :: #{ serv := none | pid()
                      , coords := list(pid()) 
                      , queue := #{	integer() => string()}
                      }.
cleanup(#{ serv := none, coords := [] }) ->
  ok;
cleanup(#{ serv := RpsServ, coords := Coords }) ->
  lists:foreach(fun(Coord) -> coordinator:stop(Coord) end, Coords),
  rps:stop(RpsServ).

-spec initial_state() -> rps_model().
initial_state() ->
  #{ serv => none, coords => [], queue => #{} }.


rps_start() -> 
  {ok, BrokerRef} = rps:start(),
  BrokerRef.

spawn_queue_up(BrokerRef, Name, Rounds) ->
  spawn(fun() -> rps:queue_up(BrokerRef, Name, Rounds) end).

model_queue_up(Queue, Name, Rounds) -> 
    case maps:get(Rounds, Queue, false) of
        false ->  
            Queue#{Rounds => Name};
        _ ->  
            maps:remove(Rounds, Queue)
    end.

model_is_queued(Queue, Rounds) ->
    case maps:get(Rounds, Queue, false) of
        false -> false;
        _ -> true
    end.

%%% ----------------------------------------------
%%% Generators
%%% ---------------------------------------------

name() -> [choose($a, $m)].
rounds() -> frequency([{1, return(0)},
                       {5, choose(1,5)}]).

rounds(Queue) -> elements(maps:keys(Queue)).

%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ serv := none }) ->
  return({call, ?MODULE, rps_start, [] });
command( #{ serv := BrokerRef, queue := Queue }) -> 
  oneof(
    [{call, rps, queue_up, [BrokerRef, name(), rounds(Queue)]} || maps:keys(Queue) /= []]
    ++ 
    [ {call, ?MODULE, spawn_queue_up, [BrokerRef, name(), rounds()]} ]).

next_state(S, V, {call, _, rps_start, _}) ->
  S#{ serv := V };

next_state(S, _V, {call, _, statistics, _}) ->
  S;

next_state(#{queue :=  Queue} = S, _V, {call, _, spawn_queue_up, [_, Name, Round]}) ->
  S#{queue := model_queue_up(Queue, Name, Round)}; % ! If a match is started, match sould be added to the ongoing

next_state(#{queue :=  Queue} = S, _V, {call, rps, queue_up, [_, Name, Round]}) ->
  S#{queue := model_queue_up(Queue, Name, Round)}; % ! If a match is started, match sould be added to the ongoing


% next_state(S, V, {call, rps, queue_up, []}) ->
%   S#{ serv := V };

next_state(S, _, _) ->
  S.

precondition(_, _) -> true.

postcondition(#{queue := Queue} = _S, {call, rps, queue_up, [_, Name, Rounds]}, Res) -> 
    case model_is_queued(Queue, Name) of
        true -> case Res of {error, _} -> true; _ -> false end;
        false -> case Res of
            {ok, _, _} -> true; 
            {error, _} -> Rounds =:= 0
        end
    end;
  
postcondition(_, _, _) -> true.


% c(qc_rps).  
% eqc:quickcheck(qc_rps:prop_rps()).