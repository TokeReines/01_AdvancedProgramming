-module(coordinator).

-behaviour(gen_statem).

-export([start/3, move/2]).
-export([callback_mode/0, init/1]).
-export([idle/3, rock/3, paper/3, scissor/3]).
-export([test/0]).

%%% -------------------------------------------------------
%%% Coordinator API - Only know to rps
%%% -------------------------------------------------------

start(Player1, Player2, N) ->
    gen_statem:start(?MODULE, {Player1, Player2, N}, []).

move(Coordinator, Choice) ->
    gen_statem:call(Coordinator, Choice).

%%% -------------------------------------w------------------
%%% Mandatory callback functions
%%% -------------------------------------------------------

% Broker data: [#{playerN => CoordinatorId}]

%[#{player1 => {PlayerName, Pid, [RoundsWon]},
%   player2 => {PlayerName, Pid, [RoundWon]},
%   bestOf => NRounds,
%   wins => N
%   ties => NRounds
%   }]

% State = PlayerNames and list of round won, Number of best-of-Rounds,
init({Player1, Player2, N}) ->
    State = idle,
    Data = 
        #{player1 => {Player1, []},
          player2 => {Player2, []},
          bestOf => N,
          nonties => 0,
          ties => 0},
    {ok, State, Data}.

callback_mode() ->
    state_functions.

%%% -------------------------------------------------------
%%% State callbacks
%%% -------------------------------------------------------

idle({call, From}, Choice, Data) ->
    io:format("You chose ~w! Waiting for opponent?. ~n", [Choice]),
    {next_state, Choice, {From, Data}}.

rock({call, Player2}, Choice, {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> tie(Data, Player1, Player2);
        paper -> nontie(Data, Player2, Player1, paper);
        scissor -> nontie(Data, Player1, Player2, rock)
    end.

paper({call, Player2}, Choice, {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> nontie(Data, Player1, Player2, paper);
        paper -> tie(Data, Player1, Player2);
        scissor -> nontie(Data, Player2, Player1, scissor)
    end.

scissor({call, Player2}, Choice,  {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> nontie(Data, Player2, Player1, rock);
        paper -> nontie(Data, Player1, Player2, scissor);
        scissor -> tie(Data, Player1, Player2)
    end.

%%% -------------------------------------------------------
%%% State helpers
%%% -------------------------------------------------------

tie(Data, Player1, Player2) ->
    #{ties := Ties} = Data,
    NewData = Data#{ties := Ties + 1},
    {next_state, idle, NewData, [{reply, Player2, tie}, {reply, Player1, tie}]}.

nontie(Data, Winner, Loser, WinningMove) ->
    #{nonties := NonTies} = Data,
    NewData = Data#{nonties := NonTies + 1},
    {next_state, idle, NewData, [{reply, Winner, win}, {reply, Loser, {loss, WinningMove}}]}.

%%% -------------------------------------------------------
%%% Appendix
%%% -------------------------------------------------------

test() ->
    {ok, Coordinator} = start("P1", "P2", 3),
    spawn(fun() ->
             Res = move(Coordinator, scissor),
             io:format("P1: ~w ~n", [Res])
          end),
    spawn(fun() ->
             Res = move(Coordinator, paper),
             io:format("P2: ~w ~n", [Res])
          end).
