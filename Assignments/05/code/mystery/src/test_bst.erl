-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
bst(Key, Value) ->
    ?LET(KVS, eqc_gen:list({Key, Value}),
         lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
                     empty(),
                     KVS)).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
key_from(T)-> eqc_gen:elements(bst:keys(eval(T))++[snowflake]).
key_maybe_from(T, F) -> eqc_gen:frequency([
    {1, atom_key()}, 
    {F, key_from(T)}]
).
int_value() -> eqc_gen:int().


%%% ! -- invariant properties

% all generated bst are valid, meaning that 
% All leafs are valid as they don't contain Key value pairs
% All keys in the left sub tree is less than the key in the current tree
% All key in the right sub tree is greater then the key in the current tree
prop_arbitrary_valid() ->
    ?FORALL(T, 
            bst(atom_key(), int_value()),
            valid(T)).

% if we insert into a valid tree it stays valid, meaning that
% keys is still less/greater then the current tree with respect to them being left/right sub trees 
prop_insert_valid() ->
    ?FORALL({K, V, T},
            {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, T))).

prop_empty_valid() -> 
    ?LET(T, empty(), valid(T)).

prop_delete_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
        ?FORALL(K, key_from(T), valid (delete(K,T)))).

prop_union_valid() ->
    ?FORALL({T1, T2}, 
            {bst(atom_key(), int_value()), bst(atom_key(), int_value())}, 
            valid (union(T1, T2))).

%%% ! -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, T)),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, T)
                       end)).


prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, 
            {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, insert(K, V, T)),
                       {found, V})).

prop_find_post_absent() -> 
     % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K, T}, 
            {atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K, delete(K, T)),
                       nothing)).  

% TODO: Add frequency - Important for int keys
prop_delete_post() ->
    ?FORALL({K1, K2, T},
            {atom_key(), atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, delete(K1, T)),
                      case K1 =:= K2 of
                          true -> nothing;
                          false -> find(K2, T)
                      end)).

% TODO: Add frequency - Important for int keys
prop_union_post() ->
    ?FORALL({K, T1, T2},
            {atom_key(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            eqc:equals(find(K, union(T1, T2)),
                      case find(K, T1) of
                          {found, _V} -> {found, _V};
                          nothing -> find(K, T2)
                      end)).

% TODO: Test for size of T != size of T' after insert/delete in same manner as above, taking into account whether or not the key was present or not.  

%%% ! -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, T)) >= bst:size(T)).

% Inserting an existing key results in a tree identical to the original tree, with an updated value
prop_insert_existing() ->
    ?FORALL({T, V}, 
            {bst(atom_key(), int_value()), int_value()}, 
            ?FORALL(K, 
                    key_from(T), 
                    eqc:equal(keys(T), keys(insert(K, V, T))))).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

% Inserting 2 different keys should be commutative, and the order of inserts shouldn't matter. 
% Inserting 2 identical keys, should result in the same tree as only inserting it once.
prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(), bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, insert(K2, V2, T)),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, T);
                           false -> insert(K2, V2, insert(K1, V1, T))
                       end)).

prop_size_delete() ->
  % ! ∀ k t. size (delete k t) =< size t
  ?FORALL({K,T}, {atom_key(), bst(atom_key(), int_value())},
          bst:size(delete(K, T)) =< bst:size(T)).

% Deleting 2 different keys should be commutative, and the order of deletions shouldn't matter. 
% Deleting 2 identical keys, should result in the same tree as only deleting it once.
prop_delete_delete() ->
    ?FORALL({K1, K2, T},
            {atom_key(), atom_key(), bst(atom_key(), int_value())},
            obs_equals(delete(K1, delete(K2, T)),
                       case K1 =:= K2 of
                           true ->  delete(K1, T);
                           false -> delete(K2, delete(K1, T))
                       end)).

% The union of two trees should result in a size equal to or greater than the largest of the two.
prop_size_union() ->
    ?FORALL({T1, T2},
            {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            bst:size(bst:union(T1, T2)) >= max(bst:size(T1), bst:size(T2))).

% Union is commutative
prop_union_union() ->
    ?FORALL({T1, T2},
            {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            obs_equals(bst:union(T1, T2), bst:union(T2, T1))).

% TODO: Is obs_equals(T1, bst:union(T1, T1)) relevant?
% TODO: Is bst:size(T1) == bst:size(bst:union(T1, T1)) relevant?

%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(insert(K, V, T)),
                   sorted_insert(K, V, delete_key(K, model(T))))).


-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].



%% -- Test all properties in the module: eqc:module(test_bst)
