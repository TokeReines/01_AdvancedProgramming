-module(server).
-behaviour(gen_statem).

-export([callback_mode/0, init/1]).
-export([new/1, stop/0, poll/1, wait/1, handle/3, test_error/1, test_timed_func/1, processing/3, finished/3]).

%%% Public client API

new({Fun, Arg}) -> 
    {ok, Aid} = gen_statem:start({local, ?MODULE}, ?MODULE, [], []),
    work(Aid, Fun, Arg),
    Aid.

wait(Aid) -> gen_statem:call(Aid, wait).
poll(Aid) -> gen_statem:call(Aid, poll).
stop() -> gen_statem:stop(?MODULE).

%%% Mandatory callback functions

init(_) -> {ok, processing, []}.
callback_mode() -> state_functions.

%%% State callbacks [processing, failed, completed]

processing({call, From}, wait, Data) -> 
    io:format("Waiting ~n"),
    {keep_state, Data};

processing(cast, poll, Data) -> 
    io:format("Still processing ~n"),
    {keep_state, Data};

processing(cast, {finished, Result}, Data) -> 
    io:format("Worker Finished! ~n"),
    {next_state, finished, {ok, Result}};

processing(cast, {failed, Result}, Data) -> 
    io:format("Worker Failed! ~n"),
    {next_state, finished, {error, Result}}.

finished({call, From}, wait, Data) -> 
    io:format("Wait: Here's the data ~n"),
    gen_statem:reply(From, Data),
    {keep_state, Data};

finished({call, From}, poll, Data) -> 
    io:format("Poll: Here's the data ~n"),
    gen_statem:reply(From, Data),
    {keep_state, Data}.

handle(EventType, EventContent, Data) ->
  io:format("handle does not understand (~w, ~w, ~w)~n",
  [EventType, EventContent, Data]),
  {keep_state, Data}.
  

% Server Loop:
    % New: Supervisor()  -> Worker
    % Wait: Supervisor() -> await New Supervisor
    % Poll: poll New Supervisor

%                                Server: gen_statem()
%                               Wait()  | Poll()
%                                     Worker: ?
%
%

%%% Worker stuff

work(Aid, Fun, Arg) -> 
    spawn(fun() -> 
        try 
            Res = Fun(Arg),
            timer:sleep(5000),
            gen_statem:cast(Aid, {finished, Res})
        catch
            throw : Throw -> gen_statem:cast(Aid, {failed, Throw});
            exit : Exit -> gen_statem:cast(Aid, {failed, Exit});
            error : Error -> gen_statem:cast(Aid, {failed, Error})
        end
    end).
    
  % fun(S) -> throw(S) end.
test_error(S) -> fun() -> throw(S) end.
test_timed_func(X) -> timer:sleep(10000), X + 1.
% fun (X) -> timer:sleep(10000), X + 1 end