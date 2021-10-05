-module(async).

-export([loop/1 ,new/2, wait/1, poll/1, setup/0]).


loop(State) ->
    {Worker, Result} = State,
    receive 
        {new, {Fun, Arg}} -> 
            io:fwrite("New~n"),
            Me = self(),
            process_flag(trap_exit, true),
            spawn_link (fun() ->
                Res = Fun(Arg),
                Me ! {finished, Res},
                receive
                    From -> From ! Res
                end
            end),
            loop(State);
        {finished, Result} -> 
            io:fwrite("Fun finished~n"),
            loop({Worker, Result});
        {wait, From} -> 
            io:fwrite("Waiting~n"),
            case Result of
                {exception, Reason} ->  From ! {exception, Reason};
                _ -> Worker ! From
            end,            
            loop(State);
        {poll, From} -> 
            io:fwrite("Polling~n"),
            case Result of
                nothing -> io:fwrite("nothing~n"), From ! nothing;
                {exception, Reason} -> io:fwrite("exception~n"), From ! {exception, Reason};
                true -> io:fwrite("basecase~n"), From ! {ok, Result}
            end,
            loop(State);
        {'EXIT', Worker, Reason} -> 
            io:fwrite("Process died~n"),
            loop({Worker, {exception, Reason}})
    end.
    

%new(Fun, Arg) that starts a concurrent computation that computes Fun(Arg). It returns an action ID
%X = async:new(fun({}) -> timer:sleep(15000) end, {}).
new(Fun, Arg) -> 
    Worker = spawn(fun() -> loop({nothing, nothing}) end),
    Worker ! {new, {Fun, Arg}},
    Worker.

% Waits for an asyncronous action to complete, and return the values of the computation. If the asynchronous
% action threw an exception, then the exceptions is rethrown by wait.
wait(Aid) -> 
    Aid ! {wait, self()},
    receive
        Res -> Res
    end.

% that check wether an asynchronoys action has completed yet. If it has not completed yet
% then the result is 'nothing'; otherwise the result is '{exceptions, Ex}' if the 
% the asynchronous action raised the exceptio 'Ex', or '{ok, Res}' if it returned the value 'Res'.
poll(Aid) -> 
    Aid ! {poll, self()},
    receive
        Res -> Res
    end.

setup() ->
    E4 = async:new(fun(N) ->N+1 end, 1),
    async:poll(E4).
