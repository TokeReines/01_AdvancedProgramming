-module(worker).
-behaviour(gen_server).

-export([start_link/1, init/1]).

% ServerName, Module, Args, Options
start_link({Fun, Arg}) -> gen_server:start_link({local, ?MODULE}, ?MODULE, {Fun, Arg}, []).

init({Fun, Arg}) -> 
    try Res = Fun(Arg),
      {ok, Res}
    catch
      error : _ -> {err, error}
    end.
    
work(Aid, Fun, Arg) -> 
    spawn(fun() -> 
        try 
            timer:sleep(20000),
            Res = Fun(Arg),
            gen_statem:cast(Aid, {finished, Res})
        catch
            throw : Throw -> gen_statem:cast(Aid, {failed, Throw});
            exit : Exit -> gen_statem:cast(Aid, {failed, Exit});
            error : Error -> gen_statem:cast(Aid, {failed, Error})
        end
    end).