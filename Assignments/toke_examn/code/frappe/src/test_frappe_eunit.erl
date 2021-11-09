-module(test_frappe_eunit).
-include_lib("eunit/include/eunit.hrl").
-export([eunit_tests/0]).

eunit_tests() -> 
      {"Basic behavior",  spawn,
          [ test_fresh(),
            test_set(),
            test_read(),
            test_read_lru(),
            test_set_break_cap(),
            test_insert(),
            test_insert_break_cap(),
            test_insert_duplicate(),
            test_insert_duplicate_break_cap(),
            test_all_items(),
            test_all_items_break_cap(),
            test_upsert_as_update(),
            test_upsert_as_new(),
            test_upsert_long_running_break_by_set(),
            test_stable()
          ]
        }.


test_fresh() ->
    {"Start a frappe, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, frappe:fresh(5))
     end}.

test_set() -> 
    {"Test set, legal",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:set(FS, "a", "a", 3)),
      frappe:stop(FS)
    end
    }.

test_read() -> 
    {"Test read, simple",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:set(FS, "a", "value", 3)),
      ?assertMatch({ok, "value"}, frappe:read(FS, "a")),
      frappe:stop(FS)
    end
    }.

test_read_lru() -> 
    {"Test read, lru update after",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      ?assertMatch(ok, frappe:insert(FS, "b", "b", 2)),
      ?assertMatch({ok, "a"}, frappe:read(FS, "a")),
      ?assertMatch(ok, frappe:insert(FS, "c", "c", 2)),
      ?assertMatch({ok, "a"}, frappe:read(FS, "a")),
      ?assertMatch(nothing, frappe:read(FS, "b")),
      frappe:stop(FS)
    end
    }.

test_set_break_cap() -> 
    {"Test Set breaking the Cap",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch({error, _}, frappe:set(FS, "a", "a", 6)),
      frappe:stop(FS)
    end
    }.

test_insert() -> 
    {"Test insert",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      frappe:stop(FS)
    end
    }.

test_insert_break_cap() -> 
    {"Test insert breaking cap",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch({error, _}, frappe:insert(FS, "a", "a", 6)),
      frappe:stop(FS)
    end
    }.

test_insert_duplicate() -> 
    {"Test insert duplicate error",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      ?assertMatch({error, _}, frappe:insert(FS, "a", "a", 3)),
      frappe:stop(FS)
    end
    }.

test_insert_duplicate_break_cap() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      ?assertMatch(ok, frappe:insert(FS, "b", "a", 3)),
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      frappe:stop(FS)
    end
    }.

test_all_items() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 1)),
      ?assertMatch(ok, frappe:insert(FS, "b", "a", 2)),
      ?assertMatch([{"a", "a", 1}, {"b", "a", 2}], frappe:all_items(FS)),
      frappe:stop(FS)
    end
    }.

test_all_items_break_cap() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 1)),
      ?assertMatch(ok, frappe:insert(FS, "b", "a", 2)),
      ?assertMatch(ok, frappe:insert(FS, "bigone", "a", 5)),
      ?assertMatch([{"bigone", "a", 5}], frappe:all_items(FS)),
      frappe:stop(FS)
    end
    }.

test_upsert_as_update() -> 
    {"Test upsert as update (on existing Key)",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(FS, "a", [a, b], 1)),
      ?assertMatch(ok, frappe:upsert(FS, "a", upsert_list([c, d], 2))),
      ?assertMatch(ok, frappe:upsert(FS, "a", upsert_bad_format())),
      ?assertMatch({ok, [a,b,c,d]}, frappe:read(FS, "a")),
      ?assertMatch({error, _Error}, frappe:upsert(FS, "a", upsert_list([c, d], 7))),
      ?assertMatch({ok, [a,b,c,d]}, frappe:read(FS, "a")),
      frappe:stop(FS)
    end
    }.

test_upsert_as_new() -> 
    {"Test upsert as new",
     fun() ->
      {ok, FS} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:upsert(FS, "a", upsert_list([a, b], 2))),
      % Insert is queued and run after upsert, which then is on an existing key
      ?assertMatch({error, _Error}, frappe:insert(FS, "a", [x, y], 2)),
      ?assertMatch(ok, frappe:upsert(FS, "a", upsert_bad_format())),
      ?assertMatch({ok, [a,b]}, frappe:read(FS, "a")),
      ?assertMatch({error, _Error}, frappe:upsert(FS, "b", upsert_list([c, d], 7))),
      ?assertMatch(nothing, frappe:read(FS, "b")),
      frappe:stop(FS)
    end
    }.

test_upsert_long_running_break_by_set() -> 
  {"Test slow upsert interrupted by set",
     fun() ->
      {ok, FS} = frappe:fresh(5),    
      long_running_upsert_worker(FS, "a"),
      % Give process 20 ms to call the upsert
      timer:sleep(20),
      ?assertMatch(ok, frappe:set(FS, "a", crazy, 3)),
      ?assertMatch({ok, crazy}, frappe:read(FS, "a")),
      frappe:stop(FS)
    end
  }.

test_stable() -> 
  {"Test stable",
     fun() ->
      {ok, FS} = frappe:fresh(5),
      stable_worker(FS, "a", flemish),
      stable_worker(FS, "a", monchino),
      timer:sleep(50),
      ?assertMatch(ok, frappe:insert(FS, "a", "a", 3)),
      frappe:stop(FS)
      end
    }.

upsert_list(Value, Cost) ->
  fun (new) -> 
        {new_value, Value, Cost};
      ({existing, OldValue}) ->
        NewValue = OldValue ++ Value,
        {new_value, NewValue, Cost}
  end.

upsert_bad_format() ->
  fun (new) -> 
        superman;
      ({existing, _}) ->
        caleidoscope
  end.

upsert_long_running(Value, Cost) ->
  fun (new) -> 
        timer:sleep(1000000),
        {new_value, Value, Cost};
      ({existing, OldValue}) ->
        timer:sleep(1000000),
        NewValue = OldValue ++ Value,
        {new_value, NewValue, Cost}
  end.

stable_worker(FS, Key, Ref) ->
  spawn(fun() ->
    Stable = frappe:stable(FS, Key, Ref),
    ?assertMatch({Ref, _}, Stable)
  end).

long_running_upsert_worker(FS, Key) ->
  spawn(fun() ->
    frappe:upsert(FS, Key, upsert_long_running([a, b], 2))
  end).