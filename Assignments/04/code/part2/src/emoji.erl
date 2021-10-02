-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, isuniqueemojimap/1]).

-type shortcode() :: string().
-type emoji() :: binary().
% -type analytic_fun(State) :: fun((shortcode(), State) -> State).
-type emojimap() :: [{shortcode(), emoji()}].

-spec start(emojimap()) -> {any(), any()}.
start(Initial) -> 
  case isuniqueemojimap(Initial) of
    true ->
      E = spawn(fun () -> loop(Initial) end),
      {ok, E};
    false -> 
      {error, "Initial emoji list contains duplicate of the same shortcode"}
  end.

-spec loop(emojimap()) -> any().
loop(State) ->
  receive
    {From, {new_shortcode, Short, Emo}} -> 
      {NewState, Res} = {{Short, Emo} ++ State, ok},
      From ! {self(), Res},
      loop(NewState)
  end.

request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

new_shortcode(_, _, _) -> not_implemented.

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

lookup(_, _) -> not_implemented.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

% Helper Functions
-spec isuniqueemojimap(emojimap()) -> boolean().
isuniqueemojimap(EmojiMap) -> 
  if 
    EmojiMap == [] -> true;
    true -> UniqueEmojiMap = lists:ukeysort(1, EmojiMap),
            length(EmojiMap) == length(UniqueEmojiMap)
  end.

% Shortcuts for the terminal
% c("emoji.erl").
% Emojis = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}].
% Emojis2 = [{"algeria", <<"🇩‍🇿"/utf8>>},{"afghanistan", <<"🇦‍🇫"/utf8>>},{"abcd", <<"🔡"/utf8>>}, {"algeria", <<"🇩‍🇿"/utf8>>}].