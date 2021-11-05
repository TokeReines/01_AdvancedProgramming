-module(rps).

-behaviour(gen_server).

-import(coordinator, [move/1, start/1, stop_coordinator/1]).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2]).

%%% -------------------------------------------------------
%%% API
%%% -------------------------------------------------------

start() ->
    gen_server:start_link(?MODULE, [], []).

queue_up(BrokerRef, Name, Rounds) ->
    gen_server:call(BrokerRef, {queue_up, Name, Rounds}, infinity).

-spec move(pid(), term()) -> term().
move(Coordinator, Choice) ->
    try
      coordinator:move(Coordinator, Choice)
    catch
      _ : _ ->  error
    end.

statistics(BrokerRef) ->
    gen_server:call(BrokerRef, statistics).

-spec drain(pid(), pid() | 'none', string()) -> term().
drain(BrokerRef, Pid, Msg) ->
    gen_server:cast(BrokerRef, {drain, Pid, Msg}).

stop(BrokerRef) -> {gen_server:stop(BrokerRef)}.

%%% -------------------------------------------------------
%%% Callback Functions
%%% -------------------------------------------------------

init(_Args) ->
    {ok, #{longestGame => 0, 
           inQueue => #{},  % Consider using a map or similar
           ongoing => [],
           isDraining => false,
           drainMessage => {}}}.


%%% -------------------- Queue Up -------------------------
handle_call({queue_up, Player1Name, Rounds}, Player1Ref, State) ->
    #{inQueue := Queue, 
      ongoing := Ongoing,
      isDraining := IsDraining} = State,
    if
      IsDraining -> 
        {reply, server_stopping, State};
      not is_integer(Rounds) ->
        {reply, {error, "Number of rounds has to be an integer"}, State};
      Rounds < 1 -> 
        {reply, {error, "Number of rounds has to be greater than 1"}, State};
      true ->   
        case maps:get(Rounds, Queue, false)  of
            false ->  
                NewQueue = Queue#{Rounds => {Player1Name, Player1Ref}}, 
                NewState = State#{inQueue := NewQueue},
                {noreply, NewState};
            {Player2Name, Player2Ref} ->  
                {ok, Cid} = coordinator:start(Player1Ref, Player2Ref, Rounds, self()),
                NewOngoing = [Cid | Ongoing],
                NewQueue = maps:remove(Rounds, Queue),
                NewState = State#{inQueue := NewQueue, ongoing := NewOngoing},
                gen_server:reply(Player2Ref, {ok, Player1Name, Cid}),
                {reply, {ok, Player2Name, Cid}, NewState}
        end
    end;

%%% -------------------- Statistics -----------------------
handle_call(statistics, _From, State) ->
    #{longestGame := LongestGame,
      inQueue     := InQueue, 
      ongoing     := Ongoing} = State,
    Reply = {ok, LongestGame, maps:size(InQueue), length(Ongoing)},
    {reply, Reply, State};
  

%%% ---------------- Catch all call -----------------------
handle_call(Request, From, State) ->
    {reply, clientReply, State}.


%%% ---------------- Drain -----------------------
handle_cast({drain, Pid, Msg}, State) ->
    #{inQueue     := InQueue, 
      ongoing     := Ongoing} = State,
    maps:foreach(fun(_Key, Value) -> 
            {_, QPid} = Value,
            gen_server:reply(QPid, server_stopping)
        end, InQueue),
    lists:foreach(fun(Cid) -> 
            coordinator:drain_coordinator(Cid)
        end, Ongoing),
    case Ongoing =:= [] of
      true -> Pid ! Msg,
              {stop, normal, State};        
      false ->
            NewState = State#{inQueue := #{}, isDraining := true, drainMessage := {Pid, Msg}},
            {noreply, NewState}
    end;
   
handle_cast({coordinator_drained, Cid}, State) -> 
    #{ongoing := Ongoing, drainMessage := DrainMessage} = State, 
    {Pid, Msg} = DrainMessage,
    NewOngoing = lists:delete(Cid, Ongoing),
    case NewOngoing =:= [] of
      true ->
          Pid ! Msg, 
          {stop, normal, State};
      false -> 
        {noreply, State#{ongoing := NewOngoing}}
    end;

%%% ---------------- Stop Coordinator ---------------------
handle_cast({game_over, Rounds, Cid}, State) ->
  #{longestGame := LongestGame, 
    ongoing     := Ongoing,
    isDraining  := IsDraining,
    drainMessage:= DrainMessage} = State,
  io:format("~w, ~n", [IsDraining]),
  NewOngoing = lists:delete(Cid, Ongoing),
  case IsDraining of
    true -> 
      {Pid, Msg} = DrainMessage,
      case NewOngoing =:= [] of
        true ->
            Pid ! Msg, 
            {stop, normal, State};
        false -> 
          {noreply, State#{ongoing := NewOngoing}}
      end;
    false ->
      case LongestGame < Rounds of
          true ->  {noreply, State#{longestGame := Rounds, ongoing := NewOngoing}};
          false -> {noreply, State#{ongoing := NewOngoing}}
      end
  end;

%%% ---------------- Catch all cast -----------------------
handle_cast(Request, State) ->
    {noreply, State}.
