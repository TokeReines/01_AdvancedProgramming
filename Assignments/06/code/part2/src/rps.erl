-module(rps).

-behaviour(gen_server).

-import(coordinator, [move/1, start/1]).

-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).

%%% -------------------------------------------------------
%%% API
%%% -------------------------------------------------------

% Return {ok,Pid} or {error,{already_started,Pid}} on succesfull init
% On failed init {error,Reason}
% 
%  State = {LongestGame, InQueue[{PlayerName, BestOf}], Ongoing[CoordinatorIDs]}
start() -> gen_server:start_link({local, ?MODULE}, [], []).

queue_up(BrokerRef, Name, Rounds) -> coordinator:start(Name, Name, Rounds).

move(Coordinator, Choice) -> coordinator:move(Coordinator, Choice).

statistics(_) -> nope.

drain(_, _, _) ->  nope.

init(_Args) -> {ok, {0, [], []}}.