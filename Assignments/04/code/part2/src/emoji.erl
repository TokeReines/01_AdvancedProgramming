-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type label() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).
-type emojiProcessMap() :: [{shortcode(), pid()}].
-type analyticsMap() :: [{label(), fun(), any()}].
-type emojiMap() :: [{shortcode(), emoji()}].


-spec start(emojiMap()) -> {any(), any()}.

start(Initial) ->
  case isuniqueemojimap(Initial) of
    true ->
      EPMap = spawnEmojiServers(Initial),
      E = spawn(fun () -> loopServer(EPMap) end),
      {ok, E};
    false -> 
      {error, "Initial emoji list contains duplicate of the same shortcode"}
  end.

-spec spawnEmojiServers(emojiMap()) -> emojiProcessMap().
spawnEmojiServers(EMap) -> 
  lists:map(fun(Elem) -> 
        {Short, Emo} = Elem,
        Pid = spawnEmojiServer(Emo),
        {Short, Pid}
    end, EMap).

-spec spawnEmojiServer(emoji()) -> pid().
spawnEmojiServer(Emo) -> 
    spawn(fun () -> loopEmoji({Emo, []}) end).

% Main emojo server
-spec loopServer(emojiProcessMap()) -> any().
loopServer(State) -> % ! Make seperation of concerns into auxilary functions
  receive
    {remove_analytics, Short, Label} ->
      case lists:keysearch(Short, 1, State) of
        {value, {_Shortcode, Pid}} -> Pid ! {remove_analytics, Label}
      end,
      loopServer(State);
    % * Stop server
    {From, Ref, stop} ->
      UPids = lists:ukeysort(2, State),
      lists:foreach(fun (Elem) ->
         {_Shortcode, Pid} = Elem,
         request_reply(Pid, stop)
        end, UPids),
      From ! {Ref, ok};
    % * Get analytics
    {From, Ref, {get_analytics, Short}} ->
      case lists:keysearch(Short, 1, State) of
        false -> From ! {Ref, {error, "Shortcode is not registered"}};
        {value, {_Shortcode, Pid}} -> Pid ! {From, Ref, get_analytics}
      end,
      loopServer(State);
    % * Add analytics function to emoji
    {From, Ref, {analytics, Short, Fun, Label, Init}} ->
      Res = lists:keysearch(Short, 1, State),
      case Res of
        % Emoji isn't registered 
        false -> 
          From ! {Ref, {error, "No shortcode"}};
        % Attach analytic function to emoji process
        {value, {_Shortcode, Pid}} ->
          Pid ! {From, Ref, {analytics, Short, Fun, Label, Init}}
      end,
      loopServer(State);
    % * Get emoji
    {From, Ref, {lookup, Short}} ->
      Res = lists:keysearch(Short, 1, State),
      case Res of
        % Emoji isn't registered 
        false -> 
          From ! {Ref, no_emoji};
        % Ask emoji process to send emoji
        {value, {_Shortcode, Pid}} ->
          Pid ! {From, Ref, {get_emoji, Short}}
      end,
      loopServer(State);
    % * Register new short cpde
    {From, Ref, {new_shortcode, Short, Emo}} ->
      case isnewshortcode(Short, State) of
        % Create a new emoji process and update the state of the main server
        true ->
          Pid = spawnEmojiServer(Emo),
          {NewState, Res} = {State ++ [{Short, Pid}], ok}, % ! List can also be made with [ {Short, Pid} | State]
          From ! {Ref, Res},
          loopServer(NewState);
        % Shortcode is already registered
        false ->
          From ! {Ref, {error, "Shortcode already exists"}},
          loopServer(State)
      end;
    % * Delete Emoji
    {delete, Short} ->
      Emoji = lists:keysearch(Short, 1, State),
      case Emoji of
        % Emoji doesn't exist
        false -> loopServer(State);
        % Emoji exists, send stop to emoji process and remove it from main emoji server
        {value, {_Shortcode, Pid}} ->
          Pid ! delete,
          NewState = lists:filter(fun(Elem) -> 
            {_, Eid} = Elem,
            Eid /= Pid
          end, State),
          % NewState = lists:keydelete(Short, 1, State),
          loopServer(NewState)
      end;
    % * Register a new alias
    {From, Ref, {alias, Short1, Short2}} ->
      Emoji1 = lists:keysearch(Short1, 1, State),
      Emoji2 = lists:keysearch(Short2, 1, State),
      if 
        Emoji1 == false -> 
          From ! {Ref, {error, "The shortcode for the alias does not exist"}},
          loopServer(State);
        Emoji2 /= false  -> 
          From ! {Ref, {error, "The alias already exist"}},
          loopServer(State);
        true -> 
          {value, {_Shortcode, Pid}} = Emoji1,
          {NewState, Res} = {State ++ [{Short2, Pid}], ok}, % ! List can also be made with [ {Short, Pid} | State]
          From ! {Ref, Res},
          loopServer(NewState)
      end
  end.

-spec runAnalyticsFun(analytic_fun(any()), shortcode(), any()) -> any().
runAnalyticsFun(Fun, Short, Value) ->
  Me = self(),
  process_flag(trap_exit, true),
  Worker = spawn_link(fun()->
    NewVal = Fun(Short, Value),
    Me ! {self(), NewVal}
  end),
  NewValue = receive
    {Worker, NewVal} -> 
      NewVal;
    {'EXIT', Worker, _Reason} -> 
      Value % Still outputs "Error in process <X.XX.X> with exit value:..."
  end,
  NewValue.

% Micro server for a single shortcode and its registered aliases
-spec loopEmoji({emoji(), analyticsMap()}) -> any().
loopEmoji(State) -> 
  {Emoji, AnalMap} = State,
  receive
    {_From, delete} -> delete;
    {remove_analytics, Label} ->
      NewAnalMap = lists:keydelete(Label, 1, AnalMap),
      loopEmoji({Emoji, NewAnalMap});
    {From, Ref, stop} ->
      From ! {Ref, ok};
    % * Lookup emoji
    {From, Ref, {get_emoji, Short}} -> 
      NewAnalMap = lists:map(fun(Elem) -> 
        {ALabel, Fun, AnalValue} = Elem,
        {ALabel, Fun, runAnalyticsFun(Fun, Short, AnalValue)}
      end, AnalMap),
      Res = {ok, Emoji},
      From ! {Ref, Res},
      loopEmoji({Emoji, NewAnalMap});
    {From, Ref, get_analytics} ->
      % ! Should we return an error if there is no functions registered
      Stats = lists:map(fun(Elem) -> 
          {ALabel, _AFun, AState} = Elem,
          {ALabel, AState}
        end, AnalMap),
      From ! {Ref, {ok, Stats}},
      loopEmoji(State);
    % * Registers a new analytics function
    {From, Ref, {analytics, _, Fun, Label, Init}} ->
      % Check for duplicate Labels
      Res = lists:keysearch(Label, 1, AnalMap),
      case Res of
        {value, {_, _, _}} ->
          From ! {Ref, {error, "This analytics label already exists: " ++ Label}},
          loopEmoji(State);
        false ->
          NewState = {Emoji, [{Label, Fun, Init} | AnalMap]},
          From ! {Ref, ok},
          loopEmoji(NewState)
      end
  end.

-spec request_reply(pid(), any()) -> any().
request_reply(Pid, Request) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Request},
  receive
    {Ref, Response} -> Response
  end.

-spec non_blocking(pid(), any()) -> any().
non_blocking(Pid, Msg) -> Pid ! Msg.

-spec new_shortcode(pid(), shortcode(), emoji()) -> any().
new_shortcode(E, Short, Emo) -> request_reply(E, {new_shortcode, Short, Emo}).

-spec alias(pid(), shortcode(), shortcode()) -> any().
alias(E, Short1, Short2) -> request_reply(E, {alias, Short1, Short2}).

-spec delete(pid(), shortcode()) -> any(). 
delete(E, Short) -> non_blocking(E, {delete, Short}).

-spec lookup(pid(), shortcode()) -> any().
lookup(E, Short) -> request_reply(E, {lookup, Short}).

-spec analytics(pid(), shortcode(), analytic_fun(any()), label(), any()) -> any().
analytics(E, Short, Fun, Label, Init) -> request_reply(E, {analytics, Short, Fun, Label, Init}).

-spec get_analytics(pid(), shortcode()) -> any().
get_analytics(E, Short) -> request_reply(E, {get_analytics, Short}).

-spec remove_analytics(pid(), shortcode(), label()) -> any().
remove_analytics(E, Short, Label) -> non_blocking(E, {remove_analytics, Short, Label}).

-spec stop(pid()) -> any().
stop(E) -> request_reply(E, stop).

% Helper Functions
-spec isuniqueemojimap(emojiMap()) -> boolean().
isuniqueemojimap(EmojiMap) -> 
  if 
    EmojiMap == [] -> true;
    true -> UniqueEmojiMap = lists:ukeysort(1, EmojiMap),
            length(EmojiMap) == length(UniqueEmojiMap)
  end.

-spec isnewshortcode(shortcode(), emojiProcessMap()) -> boolean().
isnewshortcode(Short, EmojiList) ->
  not lists:keymember(Short, 1, EmojiList).