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
                      , queue := #{	integer() := string()}
                      , ongoing := integer()
                      }.
cleanup(#{ serv := none, coords := [] }) ->
  ok;
cleanup(#{ serv := RpsServ, coords := Coords }) ->
  lists:foreach(fun(Coord) -> coordinator:stop(Coord) end, Coords),
  rps:stop(RpsServ).

-spec initial_state() -> rps_model().
initial_state() ->
  #{ serv => none, coords => [], queue => #{}, ongoing => 0 }.


rps_start() -> 
  {ok, BrokerRef} = rps:start(),
  BrokerRef.

spawn_queue_up(BrokerRef, Name, Rounds) ->
  spawn(fun() -> rps:queue_up(BrokerRef, Name, Rounds) end).

model_queue_up(Queue, Name, Rounds) -> 
  if 
    Rounds > 0 ->
      case maps:get(Rounds, Queue, false) of
        false -> Queue#{Rounds => Name};
        _     -> maps:remove(Rounds, Queue)
      end;
    true -> Queue
  end.

model_match_started(Queue, Rounds, Ongoing) ->
  if
    Rounds > 0 ->
      case maps:get(Rounds, Queue, false) of
        false -> Ongoing;
        _     -> Ongoing + 1
      end;
    true -> Ongoing
  end.

model_is_queued(Queue, Rounds) ->
  case maps:get(Rounds, Queue, false) of
    false -> false;
    _     -> true
  end.

%%% ----------------------------------------------
%%% Generators
%%% ---------------------------------------------

name() -> [choose($a, $m)].
rounds() -> frequency([{1, return(0)},
                       {5, choose(1,5)}]).

rounds(Queue) -> elements(maps:keys(Queue)).

% moves() -> elements([rock, paper, scissor]).

% queue_up_and_play(BrokerRef, Name, Rounds) ->
%   {ok, _Other, Coor} = rps:queue_up(BrokerRef, Name, Rounds),
%   random_rps_to_game_over(Coor).

% random_rps_to_game_over(Coor) ->
%   case rps:move(Coor, moves()) of
%       {game_over, Me, SomeLoser} ->
%           {ok, Me, SomeLoser};
%       server_stopping ->
%           server_stopping;
%       _ -> random_rps_to_game_over(Coor)
%   end.

%%% ---------------------------------------------
%%% Statem callbacks
%%% ---------------------------------------------

command( #{ serv := none }) ->
  return({call, ?MODULE, rps_start, [] });
command( #{ serv := BrokerRef, queue := Queue }) -> 
  oneof(
    [{call, rps, queue_up, [BrokerRef, name(), rounds(Queue)]} || maps:keys(Queue) /= []]
    ++ 
    [ {call, ?MODULE, spawn_queue_up, [BrokerRef, name(), rounds()]} 
    , {call, rps, statistics, [BrokerRef]} 
    ]).

next_state(S, V, {call, _, rps_start, _}) ->
  S#{ serv := V };

next_state(S, _V, {call, _, statistics, _}) ->
  S;

next_state(#{queue :=  Queue, ongoing := Ongoing} = S, _V, {call, _, spawn_queue_up, [_, Name, Rounds]}) ->
  S#{ queue := model_queue_up(Queue, Name, Rounds)
    , ongoing := model_match_started(Queue, Rounds, Ongoing)
    };

next_state(#{queue :=  Queue, ongoing := Ongoing} = S, V, {call, rps, queue_up, [_, Name, Rounds]}) ->
  io:format("~w~n", [eval(V)]),
  S#{ queue := model_queue_up(Queue, Name, Rounds)
    , ongoing := model_match_started(Queue, Rounds, Ongoing)
    };

next_state(S, _, _) ->
  S.

precondition(_, _) -> true.

% postcondition(#{queue := Queue, ongoing := Ongoing} = _S, {call, rps, statistics, _}, Res) -> 
%   {ok, _, ResInQueue, ResOngoing} = Res,
%   % io:format("~w,~w,~w,~w,~w~n", [Res,ResInQueue, ResOngoing, maps:size(Queue), Ongoing]),
%   % ! There is racecondition where quede up players isn't qeuede up before statistics is called. 
%   % (ResInQueue =< maps:size(Queue)) and (ResOngoing =< Ongoing);
%   {ResInQueue, ResOngoing} =:= {maps:size(Queue), Ongoing};

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