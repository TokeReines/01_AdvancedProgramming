-module(rps).

-behaviour(gen_server).

-import(coordinator, [move/1, start/1, stop_coordinator/1]).
-export([test/0, test_setup/0, stop/1, test_setup2/0]).
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
    % gen_server:call(move, {Coordinator, Choice}).
    try
      coordinator:move(Coordinator, Choice)
    catch
      _ : _ ->  io:format("ERROR"), error
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
    io:format("CATCH ALL ~w, ~w, ~w  ~n", [Request, From, State]),
    {reply, clientReply, State}.


%%% ---------------- Drain -----------------------
handle_cast({drain, Pid, Msg}, State) ->
    #{inQueue     := InQueue, 
      ongoing     := Ongoing} = State,
    io:format("Draining~n"),
    maps:foreach(fun(_Key, Value) -> 
            {_, QPid} = Value,
            gen_server:reply(QPid, server_stopping)
        end, InQueue),
    lists:foreach(fun(Cid) -> 
            coordinator:drain_coordinator(Cid)
        end, Ongoing),
    io:format("Ongoing lenght: ~w, ~n", [Ongoing]),
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
    io:format("coordinator_drained~n"),
    NewOngoing = lists:delete(Cid, Ongoing),
    case NewOngoing =:= [] of
      true ->
          io:format("On going is empty ~w ~w~n", [Pid, Msg]),
          Pid ! Msg, 
          {stop, normal, State};
      false -> 
      io:format("On going is NOT empty ~w ~w~n", [Pid, Msg]),
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
      io:format("Isdraining ~n"),
      {Pid, Msg} = DrainMessage,
      case NewOngoing =:= [] of
        true ->
            io:format("Empty ongoing! ~n"),
            Pid ! Msg, 
            {stop, normal, State};
        false -> 
          io:format("Not empty ongoing ~n"),
          {noreply, State#{ongoing := NewOngoing}}
      end;
    false ->
      io:format("NOT DRAINING ~n"),
      case LongestGame < Rounds of
          true ->  {noreply, State#{longestGame := Rounds, ongoing := NewOngoing}};
          false -> {noreply, State#{ongoing := NewOngoing}}
      end
  end;

%%% ---------------- Catch all cast -----------------------
handle_cast(Request, State) ->
    io:format("CATCH ALL CAST~w, ~w~n", [Request, State]),
    {noreply, State}.



test() -> 
    {ok, BrokerRef} = start(),
    io:format("Server id: ~w ~n", [BrokerRef]),
    spawn(fun() -> 
            {ok, Res, CPid} = queue_up(BrokerRef, "P1", 3),
            io:format("P1: ~w ~n", [Res]),     
            Res1 = move(CPid, scissor),
            io:format("P1: ~w ~n", [Res1]), 
            Res2 = move(CPid, scissor),
            io:format("P1: ~w ~n", [Res2]),    
            Res3 = move(CPid, scissor),
            io:format("P1: ~w ~n", [Res3])
          end),
      spawn_link(fun() -> rock_bot:queue_up_and_play(BrokerRef) end).
    % spawn(fun() -> 
    %         {ok, Res, CPid} = queue_up(BrokerRef, "P2", 2),
    %         io:format("P2: ~w ~n", [Res]),
    %         Res1 = move(CPid, paper),
    %         io:format("P2: ~w ~n", [Res1]),  
    %         Res2 = move(CPid, paper),
    %         io:format("P2: ~w ~n", [Res2]),    
    %         Res3 = move(CPid, paper),
    %         io:format("P2: ~w ~n", [Res3])
    %       end),       
    % spawn(fun() -> 
    %         {ok, Res, CPid} = queue_up(BrokerRef, "P3", 3),
    %         io:format("P3: ~w ~n", [Res]),
    %         Res1 = move(CPid, scissor),
    %         io:format("P3: ~w ~n", [Res1]),    
    %         Res2 = move(CPid, scissor),
    %         io:format("P3: ~w ~n", [Res2]),    
    %         Res3 = move(CPid, scissor),
    %         io:format("P3: ~w ~n", [Res3])
    %       end).
    % spawn(fun() -> 
    %     Res = rps:statistics(BrokerRef),
    %     io:format("Statistics: ~w ~n", [Res])
    %   end).       
    % 
    % 
% Spawn bot:
% {ok, A} = rps:start().
% spawn_link(fun() -> rock_bot:queue_up_and_play(A) end).
% {ok, _, C} = rps:queue_up(A, "Julian", 3).
% rps:move(C, scissor).
% rps:drain(A, test, test).
% rps:statistics(A).
% 
test_setup() -> 
    {ok, A} = rps:start(),
    spawn_link(fun() -> rock_bot:queue_up_and_play(A) end),
    {ok, _, C} = rps:queue_up(A, "Julian", 3),
    rps:move(C, paper),
    rps:drain(A, self(), "Stahpping"),
    io:format("Playing paper"),
    rps:move(C, paper),
    receive
        Msg -> io:format("Receiving Drain ~w, ~n", [Msg])
    end,
    {A, C}.

test_setup2() -> 
    {ok, A} = rps:start(),
    spawn_link(fun() -> rock_bot:queue_up_and_play(A) end),
    {ok, _, C} = rps:queue_up(A, "Julian", 3),
    rps:move(C, paper),
    rps:drain(A, self(), "Stahpping"),
    {A, C}.