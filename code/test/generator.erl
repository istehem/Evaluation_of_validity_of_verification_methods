-module(generator).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_set1() ->
  ?FORALL(S, sets_gen(),
	  ?IMPLIES( sets:size(eval(S)) > 1,
		    eqc:collect(sets:size(eval(S)), eqc:equals(eval(S),sets:union(eval(S),eval(S)))))).

sets_gen() ->
  ?SIZED(Size, sets_gen(Size)).

sets_gen(0) ->
  {call, sets, new, []};
sets_gen(Size) ->
  Smaller = sets_gen(Size div 2),
  frequency([{1, sets_gen(0)},
	     {4, {call, sets, union, [Smaller, Smaller]}},
	     {6, {call, sets, add_element, [int(), Smaller]}}]).
