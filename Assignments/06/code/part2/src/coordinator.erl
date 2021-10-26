-module(coordinator).

-behaviour(gen_statem).

-export([start/4, move/2, drain_coordinator/1, stop_coordinator/1]).
-export([callback_mode/0, init/1]).
-export([idle/3, rock/3, paper/3, scissor/3, invalid_choice/3, draining/3]).

%%% -------------------------------------------------------
%%% Coordinator API - Only know to rps
%%% -------------------------------------------------------

start(Player1Ref, Player2Ref, NRounds, BrokerRef) ->
    gen_statem:start(?MODULE, {Player1Ref, Player2Ref, NRounds, BrokerRef}, []).

move(Coordinator, Choice) ->
    gen_statem:call(Coordinator, Choice).

drain_coordinator(Coordinator) -> 
    io:format("Drain Coordinator ~n"),
    gen_statem:cast(Coordinator, drain).

stop_coordinator(Coordinator) ->
    gen_statem:stop(Coordinator).

%%% -------------------------------------w------------------
%%% Mandatory callback functions
%%% -------------------------------------------------------

% Broker data: [#{playerN => CoordinatorId}]

%[#{player1Pid => {RoundsWon},
%   player2Pid => {RoundWon},
%   bestOf => NRounds,
%   wins => N,
%   ties => NRounds,
%   brokerRef => brokerRef
%   }]

% State = PlayerNames and list of round won, Number of best-of-Rounds,
init({Player1Ref, Player2Ref, NRounds, BrokerRef}) ->
    State = idle, 
    {Player1Pid, _} = Player1Ref,
    {Player2Pid, _} = Player2Ref,
    Data = 
        #{
            firstMover => {},
            Player1Pid => 0,
            Player2Pid => 0,
            broker => BrokerRef,
            bestOf  => NRounds,
            nonties => 0,
            ties    => 0},
    {ok, State, Data}.

callback_mode() -> state_functions.

%%% -------------------------------------------------------
%%% State callbacks
%%% -------------------------------------------------------

idle({call, From}, Choice, Data) ->
    io:format("Idle State, played: ~w ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if
      IsAllowedToPlay -> 
        case lists:member(Choice, [rock, paper, scissor]) of
            true -> {next_state, Choice, Data#{firstMover := From}};
            false -> 
                io:format("You made an invalid move ~w?. ~n", [Choice]),
                {next_state, invalid_choice, Data#{firstMover := From}}
        end;
      true -> {keep_state, Data}
    end;

idle(cast, drain, Data) -> 
    io:format("Idle State, draining ~n"),
    {next_state, draining, Data}.

rock({call, From}, Choice, Data) ->
    io:format("Rock State, played: ~w ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay ->
          OtherPlayerRef = maps:get(firstMover, Data),
          case Choice of
              rock -> tie(Data, From);
              paper -> nontie(Data, From, OtherPlayerRef, paper);
              scissor -> nontie(Data, OtherPlayerRef, From, rock);
              _ -> nontie(Data, OtherPlayerRef, From, rock)
          end;
      true -> {keep_state, Data}
    end;

rock(cast, drain, Data) -> 
    io:format("Rock State, draining ~n"),
    {next_state, draining, Data}.

paper({call, From}, Choice, Data) ->
    io:format("Paper State, played: ~w ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if
      IsAllowedToPlay ->
        OtherPlayerRef = maps:get(firstMover, Data),
        case Choice of
            rock -> nontie(Data, OtherPlayerRef, From, paper);
            paper -> tie(Data, From);
            scissor -> nontie(Data, From, OtherPlayerRef, scissor);
            _ -> nontie(Data, OtherPlayerRef, From, paper)
        end;
      true -> {keep_state, Data}
    end;

paper(cast, drain, Data) -> 
    io:format("Paper State, draining ~n"),
    {next_state, draining, Data}.

scissor({call, From}, Choice,  Data) ->
    io:format("Scissor State, played: ~w ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay -> 
        OtherPlayerRef = maps:get(firstMover, Data),
        case Choice of
            rock -> nontie(Data, From, OtherPlayerRef, rock);
            paper -> nontie(Data, OtherPlayerRef, From, scissor);
            scissor -> tie(Data, From);
            _ -> nontie(Data, OtherPlayerRef, From, scissor)
        end;
      true -> {keep_state, Data}
    end;

scissor(cast, drain, Data) -> 
    io:format("Scissor State, draining ~n"),
    {next_state, draining, Data}.

invalid_choice({call, From}, Choice, Data) ->
    io:format("Invalid Choice State, played: ~w ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay -> 
        OtherPlayerRef = maps:get(firstMover, Data),
        case Choice of
            rock -> nontie(Data, From, OtherPlayerRef, Choice);
            paper -> nontie(Data, From, OtherPlayerRef, Choice);
            scissor -> nontie(Data, From, OtherPlayerRef, Choice);
            _ -> tie(Data, From) % Both made invalid moves
        end;
      true -> {keep_state, Data}
    end;

invalid_choice(cast, drain, Data) -> 
    io:format("Invalid choice state, draining ~n"),
    {next_state, draining, Data}.

% game_over(cast, drain, Data) -> {next_state, draining, Data};
% game_over({call, _From}, _, Data) -> {keep_state, Data}.

draining({call, From}, _, Data) ->
    io:format("Draining State"),
    IsAllowedToPlay = is_allowed_player(From, Data),
    #{firstMover := FirstMover, broker := BrokerRef} = Data,
    if 
      IsAllowedToPlay ->
          {Pid, _} = From,
          NewData = maps:remove(Pid, Data),
          case FirstMover =:= {} of
            true -> 
                io:format("No FirstMover~n"),
                {keep_state, NewData#{firstMover := From}};
            false -> 
                io:format("Casting draing to brokerref~n"),
                gen_statem:cast(BrokerRef, {coordinator_drained, self()}),
                {stop_and_reply, normal, [{reply, From, server_stopping}, 
                                        {reply, FirstMover, server_stopping}]}
          end;
      true -> {keep_state, Data}
    end.


%%% -------------------------------------------------------
%%% State helpers
%%% -------------------------------------------------------
 
is_allowed_player(PlayerRef, Data) ->
    {PlayerPid, _} = PlayerRef,
    maps:is_key(PlayerPid, Data).

tie(Data, From) ->
    io:format("Tie ~n"),
    #{ties := Ties, firstMover := FirstMover} = Data,
    NewData = Data#{ ties := Ties + 1, firstMover := {}},
    {next_state, idle, NewData, [
        {reply, From, tie}, 
        {reply, FirstMover, tie}
    ]}.

nontie(Data, Winner, Loser, WinningMove) ->
    {WinnerPid, _} = Winner,
    {LoserPid, _} = Loser,
    #{ nonties := NonTies,
       ties := Ties,
       bestOf := BestOf,
       broker := BrokerRef,
       WinnerPid := WinnerWins, 
       LoserPid := LoserWins} = Data, 
    NewWinnerWins = WinnerWins + 1, 
    NewNonTies = NonTies + 1,
    NewData = Data#{ nonties := NewNonTies, WinnerPid := NewWinnerWins, firstMover := {}}, 
    case NewNonTies > BestOf / 2 of
        true -> 
            io:format("Nontie, game over - cast to rbroker ~n"),
            gen_statem:cast(BrokerRef, {game_over, Ties + NewNonTies, self()}),
            % ! Use stop_and_reply
            % {next_state, game_over, NewData, [
            %     {reply, Winner, {game_over, NewWinnerWins, NewNonTies - NewWinnerWins}}, 
            %     {reply, Loser, {game_over, LoserWins, NewNonTies - LoserWins}}
            % ]};
            io:format("Nontie, game over, stop and reply ~n"),
            {stop_and_reply, normal, [{reply, Winner, {game_over, NewWinnerWins, NewNonTies - NewWinnerWins}}, 
                 {reply, Loser, {game_over, LoserWins, NewNonTies - LoserWins}}]}; 
        false ->  
            io:format("Nontie, more games to play ~n"),
            {next_state, idle, NewData, [
                {reply, Winner, win}, 
                {reply, Loser, {loss, WinningMove}}
        ]}
    end.
