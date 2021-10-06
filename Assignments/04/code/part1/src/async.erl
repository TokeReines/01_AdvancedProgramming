-module(async).

-export([loop/1 ,new/2, wait/1, poll/1, funfun/0]).


loop(State) ->
    {Worker, Result} = State,
    receive 
        {new, {Fun, Arg}} -> 
            io:fwrite("New~n"),
            Me = self(),
            process_flag(trap_exit, true),
            NewWorker = spawn_link (fun() ->
                Res = Fun(Arg),
                Me ! {finished, Res},
                receive
                    From -> From ! Res
                end
            end),
            loop({NewWorker, Result});
        {finished, Res} -> 
            io:fwrite("Finished~n"),
            loop({Worker, Res});
        {wait, From} -> 
            io:fwrite("Wait~n"),
            case Result of
                nothing ->  Worker ! From;
                {exception, Reason} ->  From ! {exception, Reason};
                _ -> Worker ! From
            end,            
            loop(State);
        {poll, From} -> 
            io:fwrite("Poll~n"),
            case Result of
                nothing -> From ! nothing;
                {exception, Reason} -> From ! {exception, Reason};
                _ -> From ! {ok, Result}
            end,
            loop(State);
        {'EXIT', Worker, Reason} -> 
            io:fwrite("Exit~n"),
            loop({Worker, {exception, Reason}})
    end.

new(Fun, Arg) -> 
    Worker = spawn(fun() -> loop({nothing, nothing}) end),
    Worker ! {new, {Fun, Arg}},
    Worker.

wait(Aid) -> 
    Aid ! {wait, self()},
    receive
        Res -> Res
    end.

poll(Aid) -> 
    Aid ! {poll, self()},
    receive
        Res -> Res
    end.