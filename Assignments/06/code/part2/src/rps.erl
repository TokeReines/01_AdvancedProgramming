-module(rps).

-behaviour(gen_server).

% -import(coordinator, [move/1, start/1]).
-export([test/0]).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2]).

%%% -------------------------------------------------------
%%% API
%%% -------------------------------------------------------

start() ->
    gen_server:start_link(?MODULE, [], []).

queue_up(BrokerRef, Name, Rounds) ->
    gen_server:call(BrokerRef, {queue_up, Name, Rounds}, infinity).

    % coordinator:start(Name, Name, Rounds).

move(Coordinator, Choice) ->
    coordinator:move(Coordinator, Choice).

statistics(BrokerRef) ->
    gen_server:call(BrokerRef, statistics).

-spec drain(pid(), pid() | 'none', string()) -> term().
drain(BrokerRef, Pid, Msg) ->
    gen_server:cast(BrokerRef, {drain, Pid, Msg}).

%%% -------------------------------------------------------
%%% Callback Functions
%%% -------------------------------------------------------

init(_Args) ->
    {ok, #{longestGame => 0, 
           inQueue => [],  % Consider using a map or similar
           ongoing => [],
           isDraining => false}}.

%%% -------------------- Queue Up -------------------------
handle_call({queue_up, Name, Rounds}, From, State) ->
    % TODO: Handle errors to check that rounds are > 0 and names are terms.
    #{inQueue := Queue, 
      ongoing := Ongoing,
      isDraining := IsDraining} = State,
    if
      IsDraining -> 
        {reply, server_stopping, State};
      not is_integer(Rounds) ->
        {reply, {error, "Number of rounds has to be an integer"}, State};
      Rounds < 1 -> 
        {reply, {error, "Number of rounds has to be greater thant 1"}, State};
      true ->   
        case lists:keyfind(Rounds, 2, Queue)  of
            false ->  
                NewQueue = [{Name, Rounds, From} | Queue], 
                NewState = State#{inQueue := NewQueue},
                io:format("State: ~w ~n", [NewState]),
                {noreply, NewState};
            {QueuedName, _, QPid} ->  
                {ok, Cid} = coordinator:start(Name, QueuedName, Rounds),
                NewOngoing = [Cid | Ongoing],
                NewQueue = lists:keydelete(Rounds, 2, Queue),
                NewState = State#{inQueue := NewQueue, ongoing := NewOngoing},
                io:format("State: ~w ~n", [NewState]),
                gen_server:reply(QPid, {ok, QueuedName, Cid}),
                {reply, {ok, QueuedName, Cid}, NewState}
        end
    end;

%%% -------------------- Statistics -----------------------
handle_call(statistics, _From, State) ->
    #{longestGame := LongestGame,
      inQueue     := InQueue, 
      ongoing     := Ongoing} = State,
    Reply = {ok, LongestGame, length(InQueue), length(Ongoing)},
    {reply, Reply, State};

%%% -------------- Coordinator drained --------------------


%%% ---------------- Catch all call -----------------------
handle_call(Request, From, State) ->
    io:format("~w, ~w, ~w  ~n", [Request, From, State]),
    {reply, clientReply, State}.


%%% ---------------- Drain -----------------------
handle_cast(drain, State) ->
    #{inQueue     := InQueue, 
      ongoing     := Ongoing} = State,
    lists:foreach(fun(Elem) -> 
            {_, _, QPid} = Elem,
            gen_server:reply(QPid, server_stopping)
        end, InQueue),
    lists:foreach(fun(Cid) -> 
            coordinator:drain_coordinator(Cid)
        end, Ongoing),
    NewState = State#{inQueue := [], isDraining := true},
    {noreply, NewState};

%%% ---------------- Catch all cast -----------------------
handle_cast(Request, State) ->
    io:format("~w, ~w~n", [Request, State]),
    {noreply, State}.



test() -> 
    {ok, BrokerRef} = start(),
    spawn(fun() -> 
            Res = queue_up(BrokerRef, "P1", 3),
            io:format("P1: ~w ~n", [Res])
          end),       
    spawn(fun() -> 
            Res = queue_up(BrokerRef, "P2", 2),
            io:format("P2: ~w ~n", [Res])
          end),       
    spawn(fun() -> 
            Res = queue_up(BrokerRef, "P3", 3),
            io:format("P3: ~w ~n", [Res])
          end),
    spawn(fun() -> 
        Res = rps:statistics(BrokerRef),
        io:format("Statistics: ~w ~n", [Res])
      end).       