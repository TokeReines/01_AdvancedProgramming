-module(emoji2).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, isuniqueemojimap/1]).

-type shortcode() :: string().
-type emoji() :: binary().
% -type analytic_fun(State) :: fun((shortcode(), State) -> State).
-type emojiProcessMap() :: [{shortcode(), pid()}].
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
  map(fun(Elem) -> 
        {Short, Emo} = Elem,
        Pid = spawn(fun () -> loopEmoji(Emo) end ),
        {Short, Pid}
    end, EMap).

% -spec spawnEmojiServer({shortcode(), emoji()}) -> {shortcode(), pid()}.
% spawnEmojiServer({Short, Emo}) -> 
%   Pid = spawn(fun () -> loopEmoji(Emo) end ),
%   {Short, Pid}.
% state = Emo, analytics

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].

-spec loopServer(emojiProcessMap()) -> any().
loopServer(State) ->
  receive
    {From, {lookup, Short}} ->
      Res = lists:keysearch(Short, 1, State), % ! Can also return false
      case Res of 
        false -> 
          From ! {self(), no_emoji};
        {value, {_Shortcode, Pid}} ->
          % io:format("LoopServer lookup 2 ~lp ~n", [Pid]),
          Pid ! {From, get_emoji}
      end,
      loopServer(State)
  end.

-spec loopEmoji(emoji()) -> any().
loopEmoji(State) -> 
  receive
    {From, get_emoji} -> 
      From ! {ok, State},
      loopEmoji(State)
  end.

-spec request_reply(pid(), any()) -> any().
request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {ok, Response} -> Response
  end.



-spec new_shortcode(pid(), shortcode(), emoji()) -> any().
new_shortcode(E, Short, Emo) -> not_implemented.

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

-spec lookup(pid(), shortcode()) -> any().
lookup(E, Short) -> request_reply(E, {lookup, Short}).

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

-spec stop(pid()) -> any().
stop(_) -> not_implemented.

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

% Shortcuts for the terminal
% c("emoji.erl").
% Emojis = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}].
% Emojis2 = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}, {"algeria", <<"🇩‍🇿"/utf8>>}].
% {ok, E} = emoji2:start(Emojis).
% emoji2:lookup(E, "algeria").
% emoji2:new_shortcode(E, "bat", <<"🦇"/utf8>>).