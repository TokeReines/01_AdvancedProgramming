-module(test_frappe).

-export([]). % Remember to export the other functions from Q2.2
-import(test_frappe_eunit, [eunit_tests/0]).
-import(apqc_frappe_cache, [prop_cache_under_capacity_impl/0]).
-import(apqc_frappe_lru, [prop_capacity_invariant_impl/0]).
-import(eqc_frappe, [mktrans_impl/2, sym_mktrans_impl/1, terminating_transformation_impl/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([test_all/0, test_everything/0, mktrans/2, terminating_transformation/1, 
  prop_cache_under_capacity/0, prop_capacity_invariant/0, sym_mktrans/1]).

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.

mktrans(Opr, Arg) ->
  mktrans_impl(Opr, Arg).

sym_mktrans(Opr) ->
  sym_mktrans_impl(Opr).

terminating_transformation(KeyGen) ->
  terminating_transformation_impl(KeyGen).

prop_cache_under_capacity() ->
    eqc:quickcheck(apqc_frappe_cache:prop_cache_under_capacity_impl()).

prop_capacity_invariant() ->
    eqc:quickcheck(apqc_frappe_lru:prop_capacity_invariant_impl()).

test_all() ->
  [ eunit:test([eunit_tests()], [verbose]),
    prop_cache_under_capacity(),
    prop_capacity_invariant()
  ].

test_everything() ->
  test_all().


% %%% --------------------------------------      
% %%% ---------- MKTRANS -------------------
% %%% --------------------------------------
% mktrans(add, Value) -> 
%    fun (new) -> 
%         {new_value, Value, Value};
%       ({existing, OldValue}) ->
%         NewValue = OldValue + Value,
%       {new_value, NewValue, NewValue}
%   end;

% mktrans(update, Value) -> 
%    fun (new) -> {new_value, Value, 1};
%       ({existing, _}) -> {new_value, Value, 1}
%   end;

% mktrans(set_cost, Cost) -> 
%   fun (new) -> {new_value, Cost, Cost};
%       ({existing, OldValue}) -> {new_value, OldValue, Cost}
%   end;

% mktrans(to_list, Cost) -> 
%   fun (new) -> {new_value, [], Cost};
%       ({existing, OldValue}) -> {new_value, [OldValue], Cost}
%   end.

% sym_mktrans(Opr) -> {call, ?MODULE, mktrans, [Opr, args_gen(Opr)]}.

% % atom_gen() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
% cost_gen() -> choose(1,5).
% % int_gen() -> choose(1,5).
% opr_gen() -> elements([add, update, set_cost, to_list]).

% args_gen(Opr) -> 
%   case Opr of
%     add -> cost_gen();
%     update -> cost_gen() ;
%     set_cost -> cost_gen();
%     to_list -> cost_gen()
%   end.

% terminating_transformation(KeyGen) ->
%   ?LET(
%     {Key, Opr}, 
%     {KeyGen, opr_gen()}, 
%     {Key, sym_mktrans(Opr)}
%   ).

% eval() ->
%   A = {call,eqc_frappe,mktrans,[add,2]},
%   eval(A).