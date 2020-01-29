/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Professor Smart's safe combination
 * Author    : Mats Carlsson
 */

:- use_module(library(clpfd)).

safe(Lab, L) :-
	L = [A,B,C,D,_,F,G,H,I],
	domain(L, 1, 9),
	all_distinct(L),
	self_distinct(L, 0),
	D-F #= G,
	A*B*C #= H+I,
	B+C+F #< H,
	I #< H,
	labeling(Lab, L).

self_distinct([], _).
self_distinct([X|Xs], I) :- J is I+1, X #\= J, self_distinct(Xs, J).

