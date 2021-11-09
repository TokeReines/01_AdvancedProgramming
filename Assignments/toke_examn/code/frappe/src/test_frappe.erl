-module(test_frappe).

-export([]). % Remember to export the other functions from Q2.2
-import(test_frappe_eunit, [eunit_tests/0]).
-import(apqc_frappe, [prop_cache_under_capacity/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([test_all/0, test_everything/0]).

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.

prop_frappe_test() ->
    {"prop_sample", {timeout, 10000, ?_assert(eqc:quickcheck(eqc_frappe:prop_cache_under_capacity()))}}.

test_all() ->
  [ eunit:test([eunit_tests()], [verbose]),
    prop_frappe_test()
  ].

test_everything() ->
  test_all().



      