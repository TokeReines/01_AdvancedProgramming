-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, isuniqueemojimap/1, setup/0]).

-type shortcode() :: string().
-type label() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).
-type emojiProcessMap() :: [{shortcode(), pid()}].
-type analyticsProcessMap() :: [{label(), pid(), any()}].
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

-spec spawnAnalyticsServer(analytic_fun(_)) -> pid().
spawnAnalyticsServer({Fun, State}) -> 
    spawn(fun () -> loopAnalytics({Fun, State}) end).

% Main emojo server
-spec loopServer(emojiProcessMap()) -> any().
loopServer(State) -> % ! Make seperation of concerns into auxilary functions
  receive
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
          % io:format("LoopServer lookup 2 ~lp ~n", [Pid]),
          Pid ! {From, Ref, get_emoji}
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
          From ! {Ref, {error, "Shortcode already exists"}}
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

% Micro server for a single shortcode and its registered aliases
-spec loopEmoji({emoji(), analyticsProcessMap()}) -> any().
loopEmoji(State) -> 
  {Emoji, AnalMap} = State,
  receive
    % {From, {analytic_completed, Fun, Label, Value}} ->
    %   % Update most recent state of analytic function
    %   lists:keyreplace(Label, 1, AnalMap, {Label, From, Value}), % ! Could use key map and only update Value instead
    %   NewState = {Emoji, AnalMap},
    %   loopEmoji(NewState);
    % * Lookup emoji
    {From, Ref, get_emoji} -> 
      Res = {ok, Emoji},
      From ! {Ref, Res},
      % Run/Map on AnalMap [{label, func, state}]
      % for Label, {Func, State} in AnalMap -> 
      %   spawn_link(Func(State)), 
      %   receive
      %     NewState -> AnalMap[label] = {Func, NewState}
      %     'EXIT' -> Something went wrong
      %   end.
      loopEmoji(State);
    {From, Ref, get_analytics} ->
      Stats = lists:map(fun(Elem) -> 
          {ALabel, _APid, AState} = Elem,
          {ALabel, AState}
        end, AnalMap),
      From ! {Ref, {ok, Stats}},
      loopEmoji(State);
    % * Registers a new analytics function
    {From, Ref, {analytics, Short, Fun, Label, Init}} ->
      % Check for duplicate Labels
      Res = lists:keysearch(Label, 1, AnalMap),
      case Res of
        {value, {_Label, _Pid, _State}} ->
          From ! {Ref, {error, "This analytics label already exists: " ++ Label}};
        false ->
          Anal = spawnAnalyticsServer({Fun, Init}),
          NewState = {Emoji, [{Label, Anal, Init} | AnalMap]},
          From ! {Ref, ok},
          loopEmoji(NewState)
      end
    % delete -> none
  end.

% Micro server for a single analytics function for a specic shortcode and its registered aliases
-spec loopAnalytics(analytic_fun(_)) -> any().
loopAnalytics(State) ->
  {Fun, Value} = State,
  receive
    {From, Ref, get_analytics} ->
      From ! {ok, Value},
      loopAnalytics({Fun, Value});
    {From, Ref, stop} -> 
      not_implemented;
    {From, Ref, {run, Short}} -> 
      NewValue = Fun(Short, Value),
      loopAnalytics({Fun, NewValue});
    {'EXIT', Pid, Reason} -> 
      not_implemented
  end.

-spec request_reply(pid(), any()) -> any().
request_reply(Pid, Request) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Request},
  receive
    {Ref, Response} -> Response
  end.

non_blocking(Pid, Msg) -> Pid ! Msg.

-spec new_shortcode(pid(), shortcode(), emoji()) -> any().
new_shortcode(E, Short, Emo) -> request_reply(E, {new_shortcode, Short, Emo}).

alias(E, Short1, Short2) -> request_reply(E, {alias, Short1, Short2}).

delete(E, Short) -> non_blocking(E, {delete, Short}).

-spec lookup(pid(), shortcode()) -> any().
lookup(E, Short) -> request_reply(E, {lookup, Short}).

analytics(E, Short, Fun, Label, Init) -> request_reply(E, {analytics, Short, Fun, Label, Init}).
% try take a look in the accessed analytics function defined. the SC parameter is the shortcode

get_analytics(E, Short) -> request_reply(E, {get_analytics, Short}).

remove_analytics(_, _, _) -> not_implemented.

-spec stop(pid()) -> any().
stop(_) -> not_implemented. %! Use ukeysort on Pid to get a list of all process that need to be send stop

% Helper Functions
-spec isuniqueemojimap(emojiMap()) -> boolean().
isuniqueemojimap(EmojiMap) -> 
  if 
    EmojiMap == [] -> true;
    true -> UniqueEmojiMap = lists:ukeysort(1, EmojiMap),
            length(EmojiMap) == length(UniqueEmojiMap)
  end.

isnewshortcode(Short, EmojiList) ->
  not lists:keymember(Short, 1, EmojiList).

setup() ->
    {ok, E} = emoji:start([]),
    ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
    ok = emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>),
    ok = emoji:alias(E, "poop", "hankey"),
    ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
    E.

% Shortcuts for the terminal
% c("emoji.erl").
% Emojis = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}].
% Emojis2 = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}, {"algeria", <<"🇩‍🇿"/utf8>>}].
% {ok, E} = emoji:start(Emojis).
% emoji:lookup(E, "algeria").
% emoji:new_shortcode(E, "bat", <<"🦇"/utf8>>).