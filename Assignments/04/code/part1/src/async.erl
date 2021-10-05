-module(async).

-export([loop/1 ,new/2, wait/1, poll/1, setup/0]).

loop(State) ->
    {Worker, Result} = State,
    receive 
        {new, {Fun, Arg}} -> 
            Me = self(),
            process_flag(trap_exit, true),
            Work = spawn_link (fun() ->
                Res = Fun(Arg),
                Me ! {finished, Res},
                receive
                    From -> From ! Res
                end
            end),
            {Result, Exception} = receive
                {Worker, NewVal} -> 
                io:fwrite("Analytic function success~n"),
                {NewVal, Ex};
                {'EXIT', Worker, Reason} -> 
                io:fwrite("Analytic function throw~n"),
                Reason
            end,
            loop({Work, Result, Exception});
        {wait, From} -> 
            case Result of
                {exceptions} ->  From ! {exceptions, Exception};
                _ -> Worker ! From
            end,            
            loop(State);
        {poll, From} -> 
            case Result of
                {nothing, nothing} -> From ! nothing;
                {exception, Reason} ->  From ! {exception, Reason};
                _ -> From ! {ok, Result}
            end,
            loop(State);
        {'EXIT', Worker, Reason} -> 
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
    E4 = async:new(fun(N) -> 1+1, throw(true) end, 1). 