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
    
