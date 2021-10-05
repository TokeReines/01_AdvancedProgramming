-module(async).

-export([loop/1 ,new/2, wait/1, poll/1]).


loop(State) ->
    {Worker, Result, Exception} = State,
    receive 
        {new, {Fun, Arg}} -> 
            process_flag(trap_exit, true),
            Me = self(),
            Work = spawn_link (fun() ->
                Res = Fun(Arg),
                Me ! {finished, Res},
                receive
                    From -> From ! 5
                end
            end),
            loop({Work, Result, Exception});
        {finished, Res} -> 
            loop({Worker, Res, nothing});
        {wait, From} -> 
            case Result of
                exceptions ->  From ! {exceptions, Exception};
                _ -> Worker ! From
            end,            
            loop(State);
        {poll, From} -> 
            case Result of
                nothing -> From ! nothing;
                exceptions ->  From ! {exceptions, Exception};
                _ -> From ! {ok, Result}
            end,
            loop(State);
        {'EXIT', Worker, Reason} -> 
            loop({Worker, exceptions, Reason})
    end.
    

%new(Fun, Arg) that starts a concurrent computation that computes Fun(Arg). It returns an action ID
%X = async:new(fun({}) -> timer:sleep(15000) end, {}).
new(Fun, Arg) -> 
    Worker = spawn(fun() -> loop({nothing, nothing, nothing}) end),
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
