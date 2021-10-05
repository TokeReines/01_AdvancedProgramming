-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_new_shortcode()
       , test_alias()
       , test_delete()
       , test_lookup()
       , test_analytics()
       , test_lookup_alias_get_analytics()
       , test_lookup_get_analytics()
       , test_remove_analytics()
       , test_stop()
       ]
      },
      {"Basic emoji and alias functionality", spawn,
        [ test_register_multiple_emojis()
        , test_register_lookup()
        , test_register_lookup_delete_lookup()
        , test_register_sc_alias()
        , test_alias_of_alias()
        , test_alias_delete()
        , test_alias_delete_orginal()
        , test_alias_delete_alias()
        , test_alias_of_alias_delete_alias()
        ]
      }
      % {"Start/stop behaviour", spawn,
      %  [ test_start_server_with_emoji()
      %  , test_start_stop_server()
      %  , test_start_server_with_duplicate_shortcode()
      %  , test_duplicate_shortcode_smiley()
      %  ]
      % }
    ].

% * Basic behaviour
test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.  

test_new_shortcode() ->
    {"Register new shortcode",
     fun () ->
       {ok, E} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(E, "smiley",
                                            <<240,159,152,131>>))
     end }.

test_alias() -> 
    {"We can make an alias",
     fun () -> 
     {ok, E} = emoji:start([]),
     ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
     ?assertEqual(ok, emoji:alias(E, "smiley", "a"))
    end }.

test_delete() ->
    {"We can delete a shortcode",
     fun () -> 
     {ok, E} = emoji:start([]),
     ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
     emoji:delete(E, "smiley"),
     ?assertMatch(no_emoji, emoji:lookup(E, "smiley"))
    end }.

test_lookup() ->
  {"Doing a lookup returns the correct Emoji",
     fun () ->
       {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
       {ok, Res} = emoji:lookup(E, "smiley"),
       ?assertMatch(Res, <<240,159,152,131>>)
     end }.

test_analytics() ->
  {"Registering an analytics function, stores it correctly",
     fun () ->
       {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      %  Res = emoji:analytics(E, "Counter", fun(_, N) -> N+1 end, "smiley", 0),
       ?assertEqual(ok, emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0))
     end }.

test_lookup_get_analytics() ->
  {"Looking up an emoji, runs the analytics functions",
     fun () ->
       {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
       ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
       {ok, _} = emoji:lookup(E, "smiley"),
       {ok, _} = emoji:lookup(E, "smiley"),
       {ok, _} = emoji:lookup(E, "smiley"),
      %  {ok, Stat} = emoji:get_analytics(E, "Counter"),
       ?assertEqual({ok,[{"Counter",3}]}, emoji:get_analytics(E, "smiley"))
     end }.

test_lookup_alias_get_analytics() ->
  {"Looking up an emoji, runs the analytics functions",
     fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),       
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:analytics(E, "smiley1", fun(_, N) -> N+1 end, "Counter", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      %  {ok, Stat} = emoji:get_analytics(E, "Counter"),
       ?assertEqual({ok,[{"Counter",1}]}, emoji:get_analytics(E, "smiley"))
     end }.

test_remove_analytics() -> 
    { "We can remove an analytics function", 
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
        {ok, _} = emoji:lookup(E, "smiley"),
        {ok, _} = emoji:get_analytics(E, "smiley"),
        emoji:remove_analytics(E, "smiley", "Counter"),
        ?assertMatch({error, _}, emoji:get_analytics(E, "Counter"))
    end }.

test_stop() -> 
  {"Start and stop a server",
    fun () ->
      {ok, E} = emoji:start([]),
      ?assertEqual(ok, emoji:stop(E))
  end }.

% * Basic emoji and alias functionality

test_register_multiple_emojis() ->
  { "Register emoji 3 time with 2 SC's",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      {error, _} = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:new_shortcode(E, "smiley1", <<240,159,152,131>>))
  end}.

test_register_lookup() ->
  { "Register emoji and lookup",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley"))
  end}.
test_register_lookup_delete_lookup() ->
  { "Register emoji, lookup, delete, and check that its deleted",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      {ok, _} = emoji:lookup(E, "smiley"),
      emoji:delete(E, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley"))
  end}.

test_register_sc_alias() ->
  { "Register emoji, create alias, and lookup the alias",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley1"))
  end}.

test_alias_of_alias() ->
  { "Register emoji, create alias, create alias of alias, and lookup",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:alias(E, "smiley1", "smiley2"),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley2"))
  end}.

test_alias_delete() ->
  { "Register emoji, create alias, delete alias, check that alias is gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley1"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1"))
  end}.

test_alias_delete_orginal() ->
  { "Register emoji, create alias, delete original, check that alias is gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1"))
  end}.

test_alias_delete_alias() ->
  { "Register emoji, create alias, delete alias, check that both are gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley1"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1")),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley"))
  end}.

test_alias_of_alias_delete_alias() ->
  { "Register emoji, create alias, delete alias, check that both are gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:alias(E, "smiley1", "smiley2"),
      ok = emoji:alias(E, "smiley2", "smiley3"),
      emoji:delete(E, "smiley3"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1")),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley2")),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley3"))
  end}.

% * "Start/stop behaviour"
 
% test_start_server_with_emoji() -> 
%     {"We can call start/1 with some emojis and it does not crash",
%       fun () ->
%         ?assertMatch({ok, _}, emoji:start(someemoji:small()))
%       end }.

% test_start_stop_server() -> 
%     {"Start and stop a server",
%       fun () ->
%         {ok, E} = emoji:start([]),
%         ?assertEqual(ok, emoji:stop(E))
%       end }.
      
% test_start_server_with_duplicate_shortcode() -> 
%     {"Calling start/1 with emojis with non-unique shortcodes returns an error",
%       fun () ->
%         ?assertMatch({error, _}, emoji:start([{"+1", <<"👍️"/utf8>>}, {"+1", <<"👍️"/utf8>>}]))
%       end }.



% test_start_stop_with_emojis() -> 
%     {"Start server. Populate with emojis. Stop server",
%       fun () ->
%         {ok, E} = emoji:start([]),
%         ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
%         ok = emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>),
%         ?assertEqual(ok, emoji:stop(E)) 
%     end }.

% test_stop_stop_with_emojis_and_analytics() -> 
%     {"Start server. Populate with emojis and analytics. Stop server",
%       fun () ->
%         {ok, E} = emoji:start([]),
%         ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
%         ok = emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>),
%         ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
%         ?assertEqual(ok, emoji:stop(E)) 
%     end }.

% test_duplicate_shortcode_smiley() ->
%   {"Registering existing shortcode returns error",
%     fun () ->
%       {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}, {"smiley", <<240,159,152,131>>}]),
%       ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<240,159,152,131>>))
%     end }.

% c("../src/emoji.erl").
% c(test_emoji).
% c(someemoji).
% test_emoji:test_all().
% 
% c("src/emoji.erl").
% c("tests/someemoji.erl").
% c("tests/test_emoji.erl").