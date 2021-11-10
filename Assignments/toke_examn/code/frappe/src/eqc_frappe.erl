-module(eqc_frappe).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).

gen_opr() ->
  elements([append]).

mktrans(add, Value) -> 
   fun (new) -> 
        {new_value, Value, Value};
      ({existing, OldValue}) ->
        NewValue = OldValue + Value,
      {new_value, NewValue, NewValue}
  end;

mktrans(update, Value) -> 
   fun (new) -> {new_value, Value, 1};
      ({existing, _}) -> {new_value, Value, 1}
  end;

mktrans(set_cost, Cost) -> 
  fun (new) -> {new_value, Cost, Cost};
      ({existing, OldValue}) -> {new_value, OldValue, Cost}
  end;

mktrans(to_list, Cost) -> 
  fun (new) -> {new_value, [], Cost};
      ({existing, OldValue}) -> {new_value, [OldValue], Cost}
  end.

sym_mktrans(Opr) -> {call, ?MODULE, mktrans, [Opr, args_gen(Opr)]}.

atom_gen() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
cost_gen() -> choose(1,5).
int_gen() -> choose(1,5).
opr_gen() -> elements([add, update, set_cost, to_list]).

args_gen(Opr) -> 
  case Opr of
    add -> cost_gen();
    update -> cost_gen() ;
    set_cost -> cost_gen();
    to_list -> cost_gen()
  end.

terminating_transformation(KeyGen) ->
  ?LET(
    {Key, Opr}, 
    {KeyGen, opr_gen()}, 
    {Key, sym_mktrans(Opr)}
  ).

eval() ->
  A = {call,eqc_frappe,mktrans,[add,2]},
  eval(A).