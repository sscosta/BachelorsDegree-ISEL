/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Traveling Salesman Problem
 * Author    : Mats Carlsson
 */

:- module(tsp, [tsp/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [
	append/3
			      ]).
:- use_module(library(atts)).
:- attribute costs/1.

verify_attributes(_, _, []).

tsp(chip, Succ, Cost) :-
	problem([[0,205,677,581,461,878,345],
	         [205,0,882,427,390,1105,540],
		 [677,882,0,619,316,201,470],
		 [581,427,619,0,412,592,570],
		 [461,390,316,412,0,517,190],
		 [878,1105,201,592,517,0,691],
		 [345,540,470,570,190,691,0]],
		Succ, Pred, Cost),
	append(Succ, Pred, All),
	labeling([minimize(Cost),variable(selector),value(enumerator)], All).
tsp(ilog, Succ, Cost) :-
	problem([[2,4,4,1,9,2,4,4,1,9],
	         [2,9,5,5,5,2,9,5,5,5],
		 [1,5,2,3,3,1,5,2,3,3],
		 [2,6,8,9,5,2,6,8,9,5],
		 [3,7,1,6,4,3,7,1,6,4],
		 [1,2,4,1,7,1,2,4,1,7],
		 [3,5,2,7,6,3,5,2,7,6],
		 [2,7,9,5,5,2,7,9,5,5],
		 [3,9,7,3,4,3,9,7,3,4],
		 [4,1,5,9,2,4,1,5,9,2]],
		Succ, Pred, Cost),
	append(Succ, Pred, All),
	labeling([minimize(Cost),variable(selector),value(enumerator)], All).

problem(Matrix, Succ, Pred, Cost) :-
	length(Matrix, N),
	transpose(0, N, Matrix, Transpose),
	length(Costs1, N),
	length(Costs2, N),
	length(Succ, N),
	length(Pred, N),
	cost_sum(Matrix, 0, Succ, Costs1),
	sum(Costs1, #=, Cost),
	cost_sum(Transpose, 0, Pred, Costs2),
	sum(Costs2, #=, Cost),
	circuit(Succ, Pred).

transpose(N, N, _, []) :- !.
transpose(I, N, Mat0, [Row|Transpose]) :-
	extract_column(Mat0, Row, Mat),
	J is I+1,
	transpose(J, N, Mat, Transpose).

extract_column([], [], []).
extract_column([[X|Rest]|Mat0], [X|Row], [Rest|Mat]) :-
	extract_column(Mat0, Row, Mat).

cost_sum([], _, [], []).
cost_sum([Row|Mat], I, [X|Xs], [C|Cs]) :-
	element(X, Row, C),
	costs_and_values(Row, 0, I, List),
	keysort(List, Costs),
	put_atts(X, costs(Costs)),
	J is I+1,
	cost_sum(Mat, J, Xs, Cs).

costs_and_values([], _, _, []).
costs_and_values([_|Row], I, I, List) :- !,
	J is I+1,
	costs_and_values(Row, J, I, List).
costs_and_values([Cost|Row], I, K, [Cost-J|List]) :-
	J is I+1,
	costs_and_values(Row, J, K, List).

%% ff,max_regret variable choice
selector([V|Vars], X, Rest) :-
	fd_size(V, S),
	var_regret(V, R),
	selector(Vars, V, S, R, X, Rest).

selector([], X, _, _, X, []).
selector([V|Vars], V0, S0, R0, X, Rest) :-
	integer(V), !,
	selector(Vars, V0, S0, R0, X, Rest).
selector([V|Vars], V0, S0, R0, X, [Y|Rest]) :-
	fd_size(V, S),
	var_regret(V, R),
	(   S<S0 -> Y=V0, selector(Vars, V, S, R, X, Rest)
	;   S=S0, R>=R0 -> Y=V0, selector(Vars, V, S, R, X, Rest)
	;   Y=V, selector(Vars, V0, S0, R0, X, Rest)
        ).

%% min cost value choice
enumerator(Var, _Rest, BB, BB1) :-
	fd_set(Var, Set),
	get_atts(Var, costs(CostMap0)),
	var_value_cost(CostMap0, Set, Value, _, CostMap),
	(   put_atts(Var, -costs(_)),
	    first_bound(BB, BB1),
	    Var #= Value
        ;   later_bound(BB, BB1),
	    Var #\= Value,
	    (integer(Var) -> true; put_atts(Var, costs(CostMap)))
        ).

var_regret(Var, Regret) :-
	fd_set(Var, Set),
	get_atts(Var, costs(CostMap0)),
	var_value_cost(CostMap0, Set, _, Cost1, CostMap1),
	var_value_cost(CostMap1, Set, _, Cost2, _),
	Regret is Cost2-Cost1.

var_value_cost([Cost-Value|Map], Set, Value, Cost, Map) :-
	fdset_member(Value, Set), !.
var_value_cost([_|Map0], Set, Value, Cost, Map) :-
	var_value_cost(Map0, Set, Value, Cost, Map).

