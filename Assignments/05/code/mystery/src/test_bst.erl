-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).

%%% ===========================================================================
%%% Generators
%%% ===========================================================================

%%% A non-symbolic generator for bst, parameterised by key and value generators
bst(Key, Value) ->
    ?LET(KVS, eqc_gen:list({Key, Value}),
         lists:foldl(fun({K, V}, T) -> insert(K, V, T) end,
                     empty(),
                     KVS)).

%%% A symbolic generator for bst, parameterised by key and value generators
bst_sym(Key, Value) -> 
    ?LAZY(
        frequency([{1, {call, bst, empty, []}},
            {8, ?LETSHRINK([T], [bst_sym(Key, Value)], 
                {call, bst, insert, [Key, Value, T]})},
            {4, ?LETSHRINK([T], [bst_sym(Key, Value)], 
                {call, bst, delete, [Key, T]})}
        ])
    ).

insert_sym(K, V, T) -> {call, bst, insert, [K, V, T]}.
find_sym(K, T) -> {call, bst, find, [K, T]}.

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
atom_key_2() -> eqc_gen:elements([i,j,k,l,m,n,o,p]).
key_from(T)-> eqc_gen:elements(bst:keys(eval(T))++[snowflake]).
int_value() -> eqc_gen:int().

%%% ===========================================================================
%%% Invariant properties
%%% =========================================================================== 

% all generated bst are valid, meaning that 
% All leafs are valid as they don't contain Key value pairs
% All keys in the left sub tree is less than the key in the current tree
% All key in the right sub tree is greater then the key in the current tree
prop_arbitrary_valid() ->
    ?FORALL(T, 
            bst_sym(atom_key(), int_value()),
            valid(eval(T))
    ).

% if we insert into a valid tree it stays valid, meaning that
% keys is still less/greater then the current tree with respect to them being left/right sub trees 
prop_insert_valid() ->
    ?FORALL({K, V, T},
        {atom_key(), int_value(), bst_sym(atom_key(), int_value())},
        valid(insert(K, V, eval(T)))
    ).

prop_empty_valid() -> 
    ?LET(T, empty(), valid(T)).

prop_delete_valid() ->
    ?FORALL(Bst, 
        bst_sym(atom_key(), int_value()),
        begin
            T = eval(Bst),
            ?FORALL(K, key_from(T), valid (delete(K, T)))
        end
    ).

prop_union_valid() ->
    ?FORALL({Bst1, Bst2}, {bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},         
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            valid (union(eval(T1), eval(T2)))
        end
    ).


%%% ===========================================================================
%%% Postcondition properties
%%% =========================================================================== 
 
%% the size is larger after an insert   
prop_insert_size() ->
  % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, Bst}, 
        {atom_key(), int_value(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            bst:size(insert(K, V, T)) >= bst:size(T)
        end
    ).

prop_find_insert_post() ->
    ?FORALL({K1, K2, V, Bst},
        {atom_key(), atom_key(), int_value(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            eqc:equals(find(K2, insert(K1, V, T)),
                case K1 =:= K2 of
                    true ->  {found, V};
                    false -> find(K2, T)
                end)
        end
    ).


prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, Bst},
        {atom_key(), int_value(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            eqc:equals(find(K, insert(K, V, T)),
                        {found, V})
        end
    ).

prop_find_post_absent() -> 
     % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K, Bst}, 
        {atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            eqc:equals(find(K, delete(K, T)), nothing)
        end
    ).  

prop_find_delete_post() ->
    ?FORALL({K1, K2, Bst},
        {atom_key(), atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            eqc:equals(find(K2, delete(K1, T)),
                case K1 =:= K2 of
                    true -> nothing;
                    false -> find(K2, T)
                end)
        end
    ).

prop_union_post() ->
    ?FORALL({K, Bst1, Bst2},
        {atom_key(), bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            eqc:equals(find(K, union(T1, T2)),
                case find(K, T1) of
                    {found, _V} -> {found, _V};
                    nothing -> find(K, T2)
                end)
        end
    ).

%%% ===========================================================================
%%% Metamorphic properties
%%% =========================================================================== 

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).
    
%%% ? -- Metamorphic Insert 
prop_insert_insert_weak() ->
  ?FORALL({K1, K2, V1, V2, Bst}, 
        {atom_key(), atom_key_2(), int_value(), int_value(), bst_sym(atom_key(), int_value())}, 
        begin
            T = eval(Bst),
            obs_equals(insert(K1, V1, (insert( K2, V2, T))), insert(K2, V2, (insert( K1, V1, T))))
        end
    ).

% Inserting 2 different keys should be commutative, and the order of inserts shouldn't matter. 
% Inserting 2 identical keys, should result in the same tree as only inserting it once.
prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, Bst},
        {atom_key(), atom_key(), int_value(), int_value(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            obs_equals(insert(K1, V1, insert(K2, V2, T)),
                case K1 =:= K2 of
                    true ->  insert(K1, V1, T);
                    false -> insert(K2, V2, insert(K1, V1, T))
                end)
        end
    ).
prop_insert_delete_weak() -> 
    ?FORALL({K1, V, K2, Bst}, 
        {atom_key(), int_value(), atom_key_2(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            obs_equals(insert(K1, V, delete(K2, T)), delete(K2, insert(K1, V, T)))
        end
    ).

prop_insert_delete() ->
    ?FORALL({K1, V, K2, Bst},
        {atom_key(), int_value(), atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            obs_equals(insert(K1, V, delete(K2, T)),
                case K1 =:= K2 of
                    true ->  insert(K1, V, T);
                    false -> delete(K2, insert(K1, V, T))
                end)
        end
    ).
                      
prop_insert_union() ->
    ?FORALL({K, V, Bst1, Bst2},
        {atom_key(), int_value(), bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            obs_equals(insert(K, V, union(T1, T2)), union(insert(K, V, T1), T2))
        end
    ).

%%% ? Metamorphic Delete

prop_delete_insert_weak() -> 
    ?FORALL({K1, K2, V, Bst}, 
        {atom_key(), atom_key_2(), int_value(), bst_sym(atom_key(), int_value())}, 
        begin
            T = eval(Bst),
            obs_equals(delete(K1, insert(K2, V, T)), insert(K2, V, delete(K1, T)))
        end
    ).

prop_delete_empty() ->
    ?FORALL(K,
        atom_key(),
        equals(delete(K, empty()), empty())).

% This is an almost duplicate of insert_delete, nonetheless it is nice to have, for completeness
prop_delete_insert() ->
    ?FORALL({K1, V, K2, Bst},
        {atom_key(), int_value(), atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            obs_equals(delete(K1, insert(K2, V, T)), 
                case K1 =:= K2 of
                    true ->  delete(K1, T);
                    false -> insert(K2, V, delete(K1, T))
                end)
        end
    ).

% Deleting 2 different keys should be commutative, and the order of deletions shouldn't matter. 
% Deleting 2 identical keys, should result in the same tree as only deleting it once.
prop_delete_delete() ->
    ?FORALL({K1, K2, Bst},
        {atom_key(), atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            obs_equals(delete(K1, delete(K2, T)),
                    case K1 =:= K2 of
                        true ->  delete(K1, T);
                        false -> delete(K2, delete(K1, T))
                    end)
        end
    ).

prop_delete_union() ->
  ?FORALL({K, Bst1, Bst2},
        {atom_key(), bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            obs_equals(delete(K, union(T1, T2)), union(delete(K, T1), delete(K, T2)))
        end
    ).

prop_delete_size() ->
  % ∀ k t. size (delete k t) =< size t
    ?FORALL({K,Bst}, {atom_key(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            bst:size(delete(K, T)) =< bst:size(T)
        end
    ).


%%% ? Metamorphic Union
% The union of two trees should result in a size equal to or greater than the largest of the two.
prop_size_union() ->
    ?FORALL({Bst1, Bst2},
        {bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            bst:size(union(T1, T2)) >= max(bst:size(T1), bst:size(T2))
        end
    ).

prop_union_empty1() ->
    ?FORALL(Bst, 
        bst_sym(atom_key(), int_value()),
        begin
            T = eval(Bst),
            union(empty(), T) =:= T
        end
    ).

prop_union_empty2() ->
    ?FORALL(Bst, 
        bst_sym(atom_key(), int_value()),
        begin
            T = eval(Bst),
            union(T, empty()) =:= T
        end
    ).


prop_union_delete_insert() ->
    ?FORALL({Bst1, Bst2, K, V}, 
        {bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value()), atom_key(), int_value()},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            obs_equals(union(delete(K, T1), insert(K, V, T2)), insert(K, V, union(T1, T2)))
        end
    ).

prop_union_union_idem() ->
    ?FORALL(Bst,
        bst_sym(atom_key(), int_value()),
        begin
            T = eval(Bst),
            obs_equals(union(T, T), T)
        end
    ).
    
% Tests both structure and key/values
prop_union_union_assoc() ->
    ?FORALL({Bst1, Bst2, Bst3},
        {bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            T3 = eval(Bst3),
            equals(union(union(T1, T2), T3), union(T1, union(T2, T3)))
        end
    ).

prop_find_empty() ->
    ?FORALL(K,
        atom_key(),
        equals(find(K, empty()), nothing)).


%%% ===========================================================================
%%% Model based properties
%%% =========================================================================== 

model(T) -> to_sorted_list(T).
-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

union_model(T1, T2) -> lists:ukeymerge(1, T1, T2).

%%% ? - Empty Model
prop_empty_model() ->
  ?LET(T, empty(), equals(model(T), [])).

%%% ? -- Insert 
prop_insert_model() ->
    ?FORALL({K, V, Bst}, 
        {atom_key(), int_value(), bst_sym(atom_key(), int_value())},
        begin
            T = eval(Bst),
            equals(model(insert(K, V, T)), sorted_insert(K, V, delete_key(K, model(T))))
        end
    ).
                  
%%% ? -- delete
prop_delete_model() ->
    ?FORALL(Bst, 
        bst_sym(atom_key(), int_value()),
        begin
            T = eval(Bst),
            ?FORALL(K, key_from(T),
                    equals(model(delete(K, T)), delete_key(K, model(T)))
            )
        end
    ).

%%% ? -- union
prop_union_model() ->
    ?FORALL({Bst1, Bst2}, 
        {bst_sym(atom_key(), int_value()), bst_sym(atom_key(), int_value())},
        begin
            T1 = eval(Bst1),
            T2 = eval(Bst2),
            equals(model(union(T1, T2)), union_model(model(T1), model(T2)))
        end
    ).

%%% ===========================================================================
%%% Generator measures
%%% =========================================================================== 

prop_measure() ->
  ?FORALL(T, 
      bst_sym(atom_key(), int_value()),
      collect(bst:size(eval(T)), true)
  ).

prop_measure1() ->
?FORALL({T, K}, 
    {bst_sym(atom_key(), int_value()), atom_key()},
    collect(measure1(K, eval(T)), true)
).

prop_measure2() ->
?FORALL({T, K}, 
    {bst_sym(atom_key(), int_value()), atom_key()},
    collect(measure2(K, eval(T)), true)
).

measure1(Key, Tree) ->
  IsPresent = lists:any(fun(K) -> K == Key end, keys(Tree)),
  if
    IsPresent -> "Present";
    true -> "absent"
  end.

measure2(Key, Tree) ->
  T = to_sorted_list(eval(Tree)),
  
  Empty = T == [],
  JustK = keys(Tree) == [Key],
  AtStart = lists:all(fun(K) -> K >=  Key end, keys(Tree)),
  AtEnd = lists:all(fun(K) -> K =<  Key end, keys(Tree)),
  if 
    Empty -> "empty";
    JustK -> "just k";
    AtStart -> "at start";
    AtEnd -> "at end";
    true -> "middle"
  end.

prop_aggregate() ->
  ?FORALL(T, 
      bst_sym(atom_key(), int_value()),
      aggregate(call_names(T), true)
  ).

%% -- Test all properties in the module: eqc:module(test_bst)
