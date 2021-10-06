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
      {"Basic emoji and alias functionality, with start stop", spawn,
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
      },
      {"Anlytics: `Hit = fun (_, N) -> end` and  `Last = fun (S, _) -> S end`", spawn,
        [ test_register_last_x2()
        , test_register_hit_last()
        , test_register_hit_works()
        , test_register_hit_last_works()
        , test_register_hit_last_works_alias()
        , test_register_hit_last_works_aliasx3()
        , test_register_hit_last_remove()
        , test_register_hit_last_works_remove()
        ]
      },
      {"Misc", spawn,
        [ test_start_server_small()
        , test_start_server_medium()
        , test_start_server_with_duplicate_shortcode()
        , test_duplicate_shortcode_smiley()
        , test_register_throw()
        ]
      }
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
       ?assertEqual({ok,[{"Counter",3}]}, emoji:get_analytics(E, "smiley"))
     end }.

test_lookup_alias_get_analytics() ->
  {"Looking up an emoji, runs the analytics functions",
     fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),       
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:analytics(E, "smiley1", fun(_, N) -> N+1 end, "Counter", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
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

% * Basic emoji and alias functionality, with start/stop

test_register_multiple_emojis() ->
  { "Register emoji 3 time with 2 SC's",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      {error, _} = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:new_shortcode(E, "smiley1", <<240,159,152,131>>)),
      ok = emoji:stop(E)
  end}.

test_register_lookup() ->
  { "Register emoji and lookup",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley")),
      ok = emoji:stop(E)
  end}.
test_register_lookup_delete_lookup() ->
  { "Register emoji, lookup, delete, and check that its deleted",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      {ok, _} = emoji:lookup(E, "smiley"),
      emoji:delete(E, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley")),
      ok = emoji:stop(E)
  end}.

test_register_sc_alias() ->
  { "Register emoji, create alias, and lookup the alias",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley1")),
      ok = emoji:stop(E)
  end}.

test_alias_of_alias() ->
  { "Register emoji, create alias, create alias of alias, and lookup",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:alias(E, "smiley1", "smiley2"),
      ?assertMatch({ok, _}, emoji:lookup(E, "smiley2")),
      ok = emoji:stop(E)
  end}.

test_alias_delete() ->
  { "Register emoji, create alias, delete alias, check that alias is gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley1"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1")),
      ok = emoji:stop(E)
  end}.

test_alias_delete_orginal() ->
  { "Register emoji, create alias, delete original, check that alias is gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1")),
      ok = emoji:stop(E)
  end}.

test_alias_delete_alias() ->
  { "Register emoji, create alias, delete alias, check that both are gone",
    fun () -> 
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      emoji:delete(E, "smiley1"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley1")),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley")),
      ok = emoji:stop(E)
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
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley3")),
      ok = emoji:stop(E)
  end}.

% * Analytics functionality

hit(_, N) -> N+1.
last(S, _) -> S.

test_register_last_x2() ->
  {"Rigster last two times for shame shortcode/alias with same label",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun last/2, "last", 0),
      ?assertMatch({error, _}, emoji:analytics(E, "smiley", fun last/2, "last", 0))
    end}.

test_register_hit_works() -> 
  {"Rigster hit and check that get_analytics works for 5 lookups",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      ?assertEqual({ok,[{"Counter",5}]}, emoji:get_analytics(E, "smiley"))
  end}.

test_register_hit_last() -> 
  {"Rigster last and hit with different labels",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun last/2, "last", 0),
      ?assertEqual(ok, emoji:analytics(E, "smiley", fun hit/2, "Counter", 0))
  end}.

test_register_hit_last_works() -> 
  {"Rigster last and hit, and check that they work",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley", fun last/2, "Last", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      ?assertEqual({ok,[{"Last","smiley"},{"Counter",3}]}, emoji:get_analytics(E, "smiley"))
  end}.

test_register_hit_last_works_alias() -> 
  {"Rigster last and hit, and check that they work with alias'",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley1", fun last/2, "Last", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley1"),
      ?assertEqual({ok,[{"Last","smiley1"},{"Counter",3}]}, emoji:get_analytics(E, "smiley"))
  end}.

test_register_hit_last_works_aliasx3() -> 
  {"Rigster last and hit, and check that they work with alias'",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:alias(E, "smiley", "smiley1"),
      ok = emoji:alias(E, "smiley1", "smiley2"),
      ok = emoji:analytics(E, "smiley1", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley2", fun last/2, "Last", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley1"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley2"),
      {ok, _} = emoji:lookup(E, "smiley1"),
      ?assertEqual({ok,[{"Last","smiley1"},{"Counter",5}]}, emoji:get_analytics(E, "smiley1"))
  end}.

test_register_hit_last_remove() -> 
  {"Rigster last and hit, and remove",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley", fun last/2, "Last", 0),
      emoji:remove_analytics(E, "smiley", "Counter"),
      emoji:remove_analytics(E, "smiley", "Last"),
      ?assertMatch({ok, []}, emoji:get_analytics(E, "smiley"))
  end}.
test_register_hit_last_works_remove() -> 
  {"Rigster last and hit, they work, and remove",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley", fun last/2, "Last", 0),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      emoji:remove_analytics(E, "smiley", "Counter"),
      emoji:remove_analytics(E, "smiley", "Last"),
      ?assertMatch({ok, []}, emoji:get_analytics(E, "smiley"))
  end}.

% * "Misc"
 
test_start_server_small() -> 
  {"Start server with someemoji:small",
    fun () ->
      ?assertMatch({ok, _}, emoji:start(someemoji:small()))
  end }.

test_start_server_medium() -> 
  {"Start server with someemoji:medium",
    fun () ->
      ?assertMatch({ok, _}, emoji:start(someemoji:medium()))
  end }.

      
test_start_server_with_duplicate_shortcode() -> 
  {"Start server with non-unique shortcodes",
    fun () ->
      ?assertMatch({error, _}, emoji:start([{"-1", <<240,159,145,142,240,159,143,189>>}, {"-1", <<240,159,145,142,240,159,143,189>>}]))
  end }.

test_duplicate_shortcode_smiley() ->
  {"Registering existing shortcode returns error",
    fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertMatch({error, _}, emoji:new_shortcode(E, "smiley", <<240,159,152,131>>))
  end }.

test_register_throw() -> 
  {"Register hit and throw (fun(S, _) -> throw(S) end), and still works, and removable",
    fun () ->
      {ok, E} = emoji:start([]),
      ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ok = emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(E, "smiley", fun(S, _) -> throw(S) end, "Throw", []),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      {ok, _} = emoji:lookup(E, "smiley"),
      ?assertMatch({ok, [{"Throw", []},{"Counter", 3}]}, emoji:get_analytics(E, "smiley"))
  end }.