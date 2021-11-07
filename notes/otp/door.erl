-module(door).

-behaviour(gen_statem).

-export([start_link/0]).

-export([locked/3, open/3]).

-export([init/1, callback_mode/0]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

open({call, From}, close, Data) ->
  io:format("Closing the door!~n", []),
  {next_state, locked, Data, [{reply, From, ok}]};

open(state_timeout, [], Data) ->
  io:format("Closing the door!~n", []),
  {next_state, locked, Data};

open(EventType, EventContent, Data) ->
  io:format("open state does not understand (~w, ~w, ~w)~n",
    [EventType, EventContent, Data]),
  {keep_state, Data}.

locked(cast, open, Data) ->
  io:format("cast, open, ~w~n", [Data]),
  {next_state, open, Data};

locked({call, From}, open, Data) ->
  io:format("call from ~w, open, ~w~n", [From, Data]),
  {next_state, open, Data, [
    {reply, From, ok},
    {state_timeout, 5000, []}
  ]};

locked(EventType, EventContent, Data) ->
  io:format("locked state does not understand (~w, ~w, ~w)~n",
    [EventType, EventContent, Data]),
  {keep_state, Data}.

init(_) -> {ok, locked, []}.

callback_mode() -> state_functions.
