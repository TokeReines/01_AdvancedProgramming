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
    % * New Shortcode
    {From, {new_shortcode, Short, Emo}} ->
      case isnewshortcode(Short, State) of % ! should maybe be performed in its own process
        true -> 
          {NewState, Res} = {State ++ [{Short, Emo}], ok},
          From ! {self(), Res},
          loop(NewState);
        false ->
          From ! {self(), {error, "Shortcode already exists"}}
      end;
    {delete, Short} -> 
      {NewState} = lists:keydelete(Short, 1, State),
      loop(NewState);
    % ! Stop server - Should also stop all other process'.
    {From, stop} -> From ! {self(), ok};
    % ! Look up - Should evaluate analytics functions when looked up
    {From, {lookup, Short}} ->
      Retval = lists:keysearch(Short, 1, State), % ! should maybe be performed in its own process
      case Retval of
        {value, {_Short, Emoji}} -> 
          Res = {ok, Emoji},
          From ! {self(), Res},
          loop(State);
        false -> 
          From ! {self(), no_emoji},
          loop(State)
      end
  end.

-spec request_reply(pid(), any()) -> any().
request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.



-spec new_shortcode(pid(), shortcode(), emoji()) -> any().
new_shortcode(E, Short, Emo) -> request_reply(E, {new_shortcode, Short, Emo}).

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

-spec lookup(pid(), shortcode()) -> any().
lookup(E, Short) -> request_reply(E, {lookup, Short}).

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

-spec stop(pid()) -> any().
stop(E) -> request_reply(E, stop).

% Helper Functions
-spec isuniqueemojimap(emojimap()) -> boolean().
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
% {ok, E} = emoji:start(Emojis).
% emoji:lookup(E, "algeria").
% emoji:new_shortcode(E, "bat", <<"🦇"/utf8>>).