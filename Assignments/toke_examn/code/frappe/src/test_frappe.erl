-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2

%% Maybe you want to use eunit
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.

test_all() -> eunit:test(testsuite(), [verbose]).


testsuite() -> 
      [ {"Basic behavior",  spawn,
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
            test_all_items_break_cap()
          ]
        }
      ].

test_fresh() ->
    {"Start a frappe, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, frappe:fresh(5))
     end}.

test_set() -> 
    {"Test set, legal",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:set(A, "a", "a", 3)),
      frappe:stop(A)
    end
    }.

test_read() -> 
    {"Test read, simple",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:set(A, "a", "value", 3)),
      ?assertMatch({ok, "value"}, frappe:read(A, "a")),
      frappe:stop(A)
    end
    }.

test_read_lru() -> 
    {"Test read, lru update after",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 3)),
      ?assertMatch(ok, frappe:insert(A, "b", "b", 2)),
      ?assertMatch({ok, "a"}, frappe:read(A, "a")),
      ?assertMatch(ok, frappe:insert(A, "c", "c", 2)),
      ?assertMatch({ok, "a"}, frappe:read(A, "a")),
      ?assertMatch(nothing, frappe:read(A, "b")),
      frappe:stop(A)
    end
    }.

test_set_break_cap() -> 
    {"Test Set breaking the Cap",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch({error, _}, frappe:set(A, "a", "a", 6)),
      frappe:stop(A)
    end
    }.

test_insert() -> 
    {"Test insert",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 3)),
      frappe:stop(A)
    end
    }.

test_insert_break_cap() -> 
    {"Test insert breaking cap",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch({error, _}, frappe:insert(A, "a", "a", 6)),
      frappe:stop(A)
    end
    }.

test_insert_duplicate() -> 
    {"Test insert duplicate error",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 3)),
      ?assertMatch({error, _}, frappe:insert(A, "a", "a", 3)),
      frappe:stop(A)
    end
    }.

test_insert_duplicate_break_cap() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 3)),
      ?assertMatch(ok, frappe:insert(A, "b", "a", 3)),
      ?assertMatch(ok, frappe:insert(A, "a", "a", 3)),
      frappe:stop(A)
    end
    }.

test_all_items() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 1)),
      ?assertMatch(ok, frappe:insert(A, "b", "a", 2)),
      ?assertMatch([{"a", "a", 1}, {"b", "a", 2}], frappe:all_items(A)),
      frappe:stop(A)
    end
    }.

test_all_items_break_cap() -> 
    {"Test insert duplicate after cap break",
     fun() ->
      {ok, A} = frappe:fresh(5),      
      ?assertMatch(ok, frappe:insert(A, "a", "a", 1)),
      ?assertMatch(ok, frappe:insert(A, "b", "a", 2)),
      ?assertMatch(ok, frappe:insert(A, "bigone", "a", 5)),
      ?assertMatch([{"bigone", "a", 5}], frappe:all_items(A)),
      frappe:stop(A)
    end
    }.

test_everything() ->
  test_all().
