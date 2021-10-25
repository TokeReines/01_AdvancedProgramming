-module(coordinator).

-behaviour(gen_statem).

-export([start/4, move/2, drain_coordinator/1, stop_coordinator/1]).
-export([callback_mode/0, init/1]).
-export([idle/3, rock/3, paper/3, scissor/3, invalid_choice/3, draining/3, game_over/3]).

%%% -------------------------------------------------------
%%% Coordinator API - Only know to rps
%%% -------------------------------------------------------

start(Player1Ref, Player2Ref, NRounds, BrookerRef) ->
    gen_statem:start(?MODULE, {Player1Ref, Player2Ref, NRounds, BrookerRef}, []).

move(Coordinator, Choice) ->
    gen_statem:call(Coordinator, Choice).

drain_coordinator(Coordinator) -> 
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
init({Player1Ref, Player2Ref, NRounds, BrookerRef}) ->
    State = idle, 
    {Player1Pid, _} = Player1Ref,
    {Player2Pid, _} = Player2Ref,
    Data = 
        #{
            hasChoosen => {},
            Player1Pid => 0,
            Player2Pid => 0,
            brooker => BrookerRef,
            bestOf  => NRounds,
            nonties => 0,
            ties    => 0},
    {ok, State, Data}.

callback_mode() -> state_functions.

%%% -------------------------------------------------------
%%% State callbacks
%%% -------------------------------------------------------

idle({call, From}, Choice, Data) ->
    io:format("You chose ~w! Waiting for opponent?. ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if
      IsAllowedToPlay -> 
        case lists:member(Choice, [rock, paper, scissor]) of
            true -> {next_state, Choice, Data#{hasChoosen := From}};
            false -> {next_state, invalid_choice, Data#{hasChoosen := From}}
        end;
      true -> {keep_state, Data}
    end;

idle(cast, drain, Data) -> {next_state, draining, Data}.

rock({call, From}, Choice, Data) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay ->
          OtherPlayerRef = maps:get(hasChoosen, Data),
          case Choice of
              rock -> tie(Data, From);
              paper -> nontie(Data, From, OtherPlayerRef, paper);
              scissor -> nontie(Data, OtherPlayerRef, From, rock);
              _ -> nontie(Data, OtherPlayerRef, From, rock)
          end;
      true -> {keep_state, Data}
    end;

rock(cast, drain, Data) -> {next_state, draining, Data}.

paper({call, From}, Choice, Data) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if
      IsAllowedToPlay ->
        OtherPlayerRef = maps:get(hasChoosen, Data),
        case Choice of
            rock -> nontie(Data, OtherPlayerRef, From, paper);
            paper -> tie(Data, From);
            scissor -> nontie(Data, From, OtherPlayerRef, scissor);
            _ -> nontie(Data, OtherPlayerRef, From, paper)
        end;
      true -> {keep_state, Data}
    end;

paper(cast, drain, Data) -> {next_state, draining, Data}.

scissor({call, From}, Choice,  Data) ->
    io:format("You made a ~w move!  ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay -> 
        OtherPlayerRef = maps:get(hasChoosen, Data),
        case Choice of
            rock -> nontie(Data, From, OtherPlayerRef, rock);
            paper -> nontie(Data, OtherPlayerRef, From, scissor);
            scissor -> tie(Data, From);
            _ -> nontie(Data, OtherPlayerRef, From, scissor)
        end;
      true -> {keep_state, Data}
    end;

scissor(cast, drain, Data) -> {next_state, draining, Data}.

invalid_choice({call, From}, Choice, Data) ->
    io:format("Invalid choice: You made a ~w move!  ~n", [Choice]),
    IsAllowedToPlay = is_allowed_player(From, Data),
    if 
      IsAllowedToPlay -> 
        OtherPlayerRef = maps:get(hasChoosen, Data),
        case Choice of
            rock -> nontie(Data, From, OtherPlayerRef, Choice);
            paper -> nontie(Data, From, OtherPlayerRef, Choice);
            scissor -> nontie(Data, From, OtherPlayerRef, Choice);
            _ -> tie(Data, From) % Both made invalid moves
        end;
      true -> {keep_state, Data}
    end;

invalid_choice(cast, drain, Data) -> {next_state, draining, Data}.

game_over(cast, _, Data) -> {keep_state, Data};
game_over({call, _From}, _, Data) -> {keep_state, Data}.

draining({call, From}, _, Data) ->
    IsAllowedToPlay = is_allowed_player(From, Data),
    #{hasChoosen := HasChoosen, brooker := BrookerRef} = Data,
    if 
      IsAllowedToPlay ->
          {Pid, _} = From,
          NewData = maps:remove(Pid, Data),
          case HasChoosen =:= {} of
            true -> 
              {keep_state, NewData#{hasChoosen := From}, [{reply, From, server_stopping}]};
            false -> 
              gen_statem:cast(BrookerRef, {coordinator_drained, self()}),
              {keep_state, Data, [{reply, From, server_stopping}]}
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
    #{ties := Ties, hasChoosen := HasChoosen} = Data,
    NewData = Data#{ ties := Ties + 1, hasChoosen := {}},
    {next_state, idle, NewData, [
        {reply, From, tie}, 
        {reply, HasChoosen, tie}
    ]}.

nontie(Data, Winner, Loser, WinningMove) ->
    {WinnerPid, _} = Winner,
    {LoserPid, _} = Loser,
    #{ nonties := NonTies,
       ties := Ties,
       bestOf := BestOf,
       brooker := BrookerRef,
       WinnerPid := WinnerWins, 
       LoserPid := LoserWins} = Data, 
    NewWinnerWins = WinnerWins + 1, 
    NewNonTies = NonTies + 1,
    NewData = Data#{ nonties := NewNonTies, WinnerPid := NewWinnerWins, hasChoosen := {}}, 
    case NewNonTies > BestOf / 2 of
        true -> 
            gen_statem:call(BrookerRef, {game_over, Ties + NewNonTies}),
            % ! Use stop_and_reply
            {next_state, game_over, NewData, [
                {reply, Winner, {game_over, NewWinnerWins, NewNonTies - NewWinnerWins}}, 
                {reply, Loser, {game_over, LoserWins, NewNonTies - LoserWins}}
            ]}; 
        false ->  
            {next_state, idle, NewData, [
                {reply, Winner, win}, 
                {reply, Loser, {loss, WinningMove}}
        ]}
    end.
