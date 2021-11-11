-module(eqc_frappe).

-include_lib("eqc/include/eqc.hrl").

-export([mktrans_impl/2, sym_mktrans_impl/1, terminating_transformation_impl/1]).
-import(frappe, [fresh/1, set/4, insert/4, read/2]).

mktrans_impl(add, Value) -> 
   fun (new) -> 
        {new_value, Value, Value};
      ({existing, OldValue}) ->
        NewValue = OldValue + Value,
      {new_value, NewValue, NewValue}
  end;

mktrans_impl(update, Value) -> 
   fun (new) -> {new_value, Value, 1};
      ({existing, _}) -> {new_value, Value, 1}
  end;

mktrans_impl(set_cost, Cost) -> 
  fun (new) -> {new_value, Cost, Cost};
      ({existing, OldValue}) -> {new_value, OldValue, Cost}
  end;

mktrans_impl(to_list, Cost) -> 
  fun (new) -> {new_value, [], Cost};
      ({existing, OldValue}) -> {new_value, [OldValue], Cost}
  end.

sym_mktrans_impl(Opr) -> {call, ?MODULE, mktrans_impl, [Opr, args_gen(Opr)]}.

atom_gen() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
cost_gen() -> choose(1,5).
int_gen() -> choose(1,5).
opr_gen() -> elements([to_list]). % add, update, set_cost, 

args_gen(Opr) -> 
  case Opr of
    add -> cost_gen();
    update -> cost_gen() ;
    set_cost -> cost_gen();
    to_list -> cost_gen()
  end.

terminating_transformation_impl(KeyGen) ->
  ?LET(
    {Key, Opr}, 
    {KeyGen, opr_gen()}, 
    {Key, sym_mktrans_impl(Opr)}
  ).


%%% ----------------------------------------
%%% ----------- Extra props! ---------------
%%% ----------------------------------------
value() -> nat().
valid_cost() -> choose(1,2).
cost() -> choose(0,2).
key() -> elements([a,b,c,d,e,f,g,h]).

% frappe_sym(Key, Value, Cost) -> 
%     ?LAZY(
%         frequency([{1, {call, frappe, fresh, [9999]}},
%             {8, ?LETSHRINK([FS], [frappe_sym(Key, Value, Cost)], 
%                 {call, frappe, insert, [FS, Key, Value, Cost]})},
%             {4, ?LETSHRINK([FS], [frappe_sym(Key, Value, Cost)], 
%                 {call, frappe, set, [FS, Key, Value, Cost]})}
%         ])
%     ).
% prop_insert_read() ->
%     ?FORALL({FS, Key, Value, Cost},
%         {frappe_sym(key(), value(), cost()), key(), value(), valid_cost()},
%         begin
%           %frappe:insert(FS, Key, Value, Cost),
%           FS1 = eval(FS),
%           true
%           % case frappe:read(FS1, Key) of
%           %   {ok, Value} -> true;
%           %   _ -> false
%           % end
%         end
%     ).
