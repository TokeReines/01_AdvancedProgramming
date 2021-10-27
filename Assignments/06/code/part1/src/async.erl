-module(server).
-behaviour(gen_statem).

-export([callback_mode/0, init/1]).
-export([new/2, stop/1, poll/1, wait/1]).
-export([processing/3, finished/3, failed/3]).
-export([test_wait/0, test_error/0, test_timed_func/0]).

%%% -------------------------------------------------------
%%% Public client API
%%% -------------------------------------------------------

new(Fun, Arg) -> 
    % {ok, Aid} = gen_statem:start({local, ?MODULE}, ?MODULE, [], []),
    {ok, Aid} = gen_statem:start(?MODULE, [], []),
    work(Aid, Fun, Arg) ,
    Aid.

wait(Aid) -> 
    A = gen_statem:call(Aid, wait),
    case A of
        {ok, Value} -> Value;
        {throw, ErrorMessage} -> throw(ErrorMessage);
        {exit, ErrorMessage} -> exit(ErrorMessage);
        {error, ErrorMessage} -> error(ErrorMessage)
    end.

wait_catch(Aid) -> 
    A = gen_statem:call(Aid, wait),
    case A of
        {ok, Value} -> {ok, Value};
        {throw, ErrorMessage} -> {exception, ErrorMessage};
        {exit, ErrorMessage} -> {exception, ErrorMessage};
        {error, ErrorMessage} -> {exception, ErrorMessage}
    end.

%     Work:
%     Spawn a process calling wait(Aid) for all Aids
%     receive (block) and return the received value 


wait_any(Aids) -> 
    Work = fun(A) ->    
        Self = self(),
        [ spawn_link(fun() -> Self ! {Aid, wait(Aid)} end) || Aid <- Aids],
        io:format("Inside Work! ~n"),
        receive
            {Aid, Data} -> 
                io:format("Received Data! ~w ~w ~n", [Aid, Data]),
                {Aid, Data};
            _ -> 
                io:format("Received Stuff! ~w ~n", [ok]), 
                ok
        end
    end,     
    Async = new(Work, 1),
    {ok, {Aid, Data}} = wait(Async),
    {Aid, Data}.


poll(Aid) -> gen_statem:call(Aid, poll).
stop(Aid) -> gen_statem:stop(Aid).

%%% -------------------------------------------------------
%%% Mandatory callback functions
%%% -------------------------------------------------------

init(_) -> {ok, processing, []}.
callback_mode() -> state_functions.

%%% -------------------------------------------------------
%%% State callbacks [processing, failed, completed]
%%% -------------------------------------------------------

%%% -------------------- Failed ---------------------------

processing({call, From}, wait, Data) -> 
    io:format("Waiting ~n"),
    {keep_state, Data, [{postpone, true}]};

processing({call, From}, poll, Data) -> 
    io:format("Still processing ~n"),
    {keep_state, Data, [{reply, From, nothing}]};

%% Used by the worker to signal when function has finished
processing(cast, {finished, Result}, Data) -> 
    io:format("Worker Finished! ~n"),
    {next_state, finished, {ok, Result}};

%% Used by the worker to signal if function has failed
processing(cast, {failed, {FailedWith, Message}}, Data) -> 
    io:format("Worker Failed! ~n"),
    {next_state, failed, {FailedWith, Message}}.

%%% -------------------- Failed ---------------------------

failed({call, From}, wait, Data) -> 
    io:format("Wait Failed: Here's the data ~w  ~n", [Data]),
    {keep_state, Data, [{reply, From, Data}]};

failed({call, From}, poll, Data) -> 
    io:format("Poll Failed: Here's the data ~w ~n", [Data]),
    {keep_state, Data, [{reply, From, Data}]}.

%%% -------------------- Finished ---------------------------
finished({call, From}, wait, Data) -> 
    io:format("Wait Finished: Here's the data ~w ~n", [Data]),
    {keep_state, Data, [{reply, From, Data}]};

finished({call, From}, poll, Data) -> 
    io:format("Poll: Here's the data ~w ~n", [Data]),
    {keep_state, Data, [{reply, From, Data}]}.

  
%%% -------------------- Worker ---------------------------

work(Aid, Fun, Arg) -> 
    spawn(fun() -> 
        try 
            timer:sleep(2000),
            Res = Fun(Arg),
            gen_statem:cast(Aid, {finished, Res})
        catch
            throw : Throw -> gen_statem:cast(Aid, {failed, {throw, Throw}});
            exit : Exit -> gen_statem:cast(Aid, {failed, {exit, Exit}});
            error : Error -> gen_statem:cast(Aid, {failed, {exit, Error}})
        end
    end).
    
test_error() -> fun(S) -> throw(S) end.
test_timed_func() -> fun(X) -> timer:sleep(2000), X + 1 end.
test_fun() -> fun(X) -> X + 1 end. 

test_wait() ->   
    io:format("Starting ServerA ~n"),
    ServerA = new(test_fun(), 1),
    io:format("Starting ServerB ~n"),
    ServerB = new(test_error(), 2),
    A = wait_any([ServerB]),
    try
        stop(ServerA),
        stop(ServerB)
    catch
        _ -> A
    end,
    A.