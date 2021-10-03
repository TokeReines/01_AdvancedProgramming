-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_start_server_with_emoji()
       , test_start_server_with_duplicate_shortcode()
       , test_shortcode_smiley()
       , test_duplicate_shortcode_smiley()
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_start_server_with_emoji() -> 
    {"We can call start/1 with some emojis and it does not crash",
      fun () ->
        ?assertMatch({ok, _}, emoji:start(someemoji:small()))
      end }.

test_start_server_with_duplicate_shortcode() -> 
    {"Calling start/1 with emojis with non-unique shortcodes returns an error",
      fun () ->
        ?assertMatch({error, _}, emoji:start([{"+1", <<"👍️"/utf8>>}, {"+1", <<"👍️"/utf8>>}]))
      end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.

test_duplicate_shortcode_smiley() ->
    {"Registering existing shortcode returns error",
     fun () ->
       {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
       ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<240,159,152,131>>))
     end }.


% c("../src/emoji.erl").
% c(test_emoji).
% c(someemoji).
% test_emoji:test_all().