-module(coordinator).

-behaviour(gen_statem).

-export([start/3, move/2]).
-export([callback_mode/0, init/1]).
-export([idle/3, rock/3, paper/3, scissor/3, invalid_choice/3]).
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
init({{Player1Name, Player1Pid}, {Player2Name, Player2Pid}, N}) ->
    State = idle,
    Data = 
        #{
            Player2Pid => 0,
            Player1Pid => 0,
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
    case lists:member(Choice, [rock, paper, scissor]) of
        true -> {next_state, Choice, {From, Data}};
        false -> {next_state, invalid_choice, {From, Data}}
    end.

rock({call, Player2}, Choice, {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> tie(Data, Player1, Player2);
        paper -> nontie(Data, Player2, Player1, paper);
        scissor -> nontie(Data, Player1, Player2, rock);
        _ -> nontie(Data, Player1, Player2, scissor)
    end.

paper({call, Player2}, Choice, {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> nontie(Data, Player1, Player2, paper);
        paper -> tie(Data, Player1, Player2);
        scissor -> nontie(Data, Player2, Player1, scissor);
        _ -> nontie(Data, Player1, Player2, paper)
    end.

scissor({call, Player2}, Choice,  {Player1, Data}) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> nontie(Data, Player2, Player1, rock);
        paper -> nontie(Data, Player1, Player2, scissor);
        scissor -> tie(Data, Player1, Player2);
        _ -> nontie(Data, Player1, Player2, scissor)
    end.

invalid_choice({call, Player2}, Choice, {Player1, Data}) ->
    io:format("Invalid choice: You made a ~w move!  ~n", [Choice]),
    case Choice of
        rock -> nontie(Data, Player2, Player1, Choice);
        paper -> nontie(Data, Player2, Player1, Choice);
        scissor -> nontie(Data, Player2, Player1, Choice);
        _ -> tie(Data, Player1, Player2) % Both made invalid moves
    end.

%%% -------------------------------------------------------
%%% State helpers
%%% -------------------------------------------------------

tie(Data, Player1, Player2) ->
    io:format("Tie ~n"),
    #{ties := Ties} = Data,
    NewData = Data#{ ties := Ties + 1 },
    {next_state, idle, NewData, [
        {reply, Player2, tie}, 
        {reply, Player1, tie}
    ]}.

nontie(Data, Winner, Loser, WinningMove) ->
    io:format("Nontie ~n"),
    #{ nonties := NonTies, bestOf := BestOf, Winner := WinnerWins, Loser := LoserWins} = Data, 
    NewWinnerWins = WinnerWins + 1,
    NewData = Data#{ nonties := NonTies + 1, Winner := NewWinnerWins}, 
    case NonTies + 1 == BestOf of
        true -> 
            {next_state, idle, NewData, [
                {reply, Winner, {game_over, NewWinnerWins, NonTies - NewWinnerWins}}, 
                {reply, Loser, {game_over, LoserWins, NonTies - LoserWins}}
        ]}; 
        false -> 
            {next_state, idle, NewData, [
                {reply, Winner, win}, 
                {reply, Loser, {loss, WinningMove}}
        ]}
    end.

%%% -------------------------------------------------------
%%% Appendix
%%% -------------------------------------------------------

test() ->
    {ok, Coordinator} = start("P1", "P2", 3),
    spawn(fun() ->
             Res1 = move(Coordinator, paper),
             io:format("P1: ~w ~n", [Res1]),
             Res2 = move(Coordinator, paper),
             io:format("P1: ~w ~n", [Res2]),
             Res3 = move(Coordinator, paper),
             io:format("P1: ~w ~n", [Res3])
          end),
    spawn(fun() ->
             Res1 = move(Coordinator, rock),
             io:format("P2: ~w ~n", [Res1]),
             Res2 = move(Coordinator, scissor),
             io:format("P2: ~w ~n", [Res2]),
             Res3 = move(Coordinator, rock),
             io:format("P2: ~w ~n", [Res3])
          end).
