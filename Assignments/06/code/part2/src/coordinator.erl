-module(coordinator).
-behaviour(gen_statem).

-export([start/3, move/2]).
-export([callback_mode/0, init/1]).
-export([idle/3, rock/3, paper/3, scissor/3]).
-export([test/0]).
 
%%% -------------------------------------------------------
%%% Coordinator API - Only know to rps
%%% -------------------------------------------------------
 
start(Player1, Player2, N) -> gen_statem:start(?MODULE, {Player1, Player2, N}, []).

move(Coordinator, Choice) -> gen_statem:call(Coordinator, Choice).

%%% -------------------------------------w------------------
%%% Mandatory callback functions
%%% -------------------------------------------------------

% Broker data: [#{playerN => CoordinatorId}]

%[#{player1 => {PlayerName, Pid, [RoundsWon]},
%   player2 => {PlayerName, Pid, [RoundWon]},
%   bestOf => NRounds,
%   current => N
%   ties => NRounds
%   }]

% State = PlayerNames and list of round won, Number of best-of-Rounds, 
init({Player1, Player2, N}) -> 
    State = idle,
    Data = #{
        player1 => {Player1, []},
        player2 => {Player2, []},
        bestOf => N },
    {ok, State, Data}.

callback_mode() -> state_functions.

%%% -------------------------------------------------------
%%% State callbacks
%%% -------------------------------------------------------
% case lists:member(Choice, [rock, paper, scissor]) of
%     true -> ok;
%     false -> throw("Invalid move")
% end,

idle({call, From}, Choice, Data) -> 
    io:format("You chose ~w! Waiting for opponent?. ~n", [Choice]),
    {next_state, Choice, {From, Data}}.

rock({call, Player2}, Choice, {Player1, Data}) ->    
    io:format("You made a ~w move!  ~n", [Choice]),
    case Choice of 
        rock -> {next_state, idle, Data, [{reply, Player2, tie}, {reply, Player1, tie}]};
        paper -> 
            % Do some winning logic here
            {next_state, idle, Data, [{reply, Player2, win}, {reply, Player1, {loss, paper}}]};
        scissor -> 
            % Do some loosing logic here
            {next_state, idle, Data, [{reply, Player2, {loss, rock}}, {reply, Player1, win}]}
    end.

paper({call, From}, Choice, Data) ->
    case Choice of 
        rock -> ok;
        paper -> ok;
        scissor -> ok
    end,
    {next_state, finished, {ok}}.

scissor({call, From}, Choice, Data) ->
    case Choice of 
        rock -> ok;
        paper -> ok;
        scissor -> ok
    end,
    {next_state, finished, {ok}}.

test() ->
    {ok, Coordinator} = start("P1", "P2", 3),
    spawn(fun() ->
        Res = move(Coordinator, rock),
        io:format("P1: ~w ~n", [Res])
    end),
    spawn(fun() ->
        Res = move(Coordinator, scissor),
        io:format("P2: ~w ~n", [Res])
    end).