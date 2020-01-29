/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : queens.pl                                              */
/* Title          : N-queens problem                                       */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz - INRIA France                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* Put N queens on an NxN chessboard so that there is no couple of queens  */
/* threatening each other.                                                 */
/*                                                                         */
/* Solution:                                                               */
/* N=4  [2,4,1,3]                                                          */
/* N=8  [1,5,8,6,3,7,2,4]                                                  */
/* N=16 [1,3,5,2,13,9,14,12,15,6,16,7,4,11,8,10]                           */
/*-------------------------------------------------------------------------*/

:- module(queens,[queens/3,
		  heur_queens/3,
		  alt_queens/2
		 ]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

queens(Lab, L, N) :-
	length(L,N),
	domain(L, 1, N),
	constrain_all(L),
	labeling(Lab, L).

heur_queens(Lab, L, N) :-
	length(L,N),
	domain(L, 1, N),
	constrain_all(L),
	order_di(N, L, L1),
	labeling(Lab, L1).

% worse than heur_queens/3
alt_queens(L, N) :-
	length(L,N),
	domain(L, 1, N),
	constrain_all(L),
	middle_out(N, L, L1),
	from(0, N, Val),
	middle_out(N, Val, Val1),
	labeling([ff,value(enumerator(Val1))], L1).

% enumerator(Vals, Var, _Rest, BB, BB) :-
% 	member(Var, Vals).	
enumerator(Vals, Var, _Rest, BB, BB) :-
	fd_set(Var, Set),
	filter_vals(Vals, Set, Cands),
	member(Var, Cands).	

filter_vals([], _, []).
filter_vals([X|Xs], Set, [X|Cands]) :-
	clpfd:'$fd_dom_contains'(Set, X), !,
	filter_vals(Xs, Set, Cands).
filter_vals([_|Xs], Set, Cands) :-
	filter_vals(Xs, Set, Cands).

from(N, N, []) :- !.
from(I, N, [J|L]) :-
	J is I+1,
	from(J, N, L).

middle_out(N, L, Out) :-
	reverse(L, R),
	shuffle_rev(N, L, R, [], Out).

shuffle_rev(0, _, _, Rs, Rs) :- !.
shuffle_rev(1, [X|_], _, Rs, [X|Rs]) :- !.
shuffle_rev(N, [X|Xs], [Y|Ys], Rs0, Rs) :-
	M is N-2,
	shuffle_rev(M, Xs, Ys, [X,Y|Rs0], Rs).

qlabeling([], _).
qlabeling([X|Xs], Vs) :-
	member(X, Vs),
	qlabeling(Xs, Vs).

constrain_all([]).
constrain_all([X|Xs]):-
	constrain_between(X,Xs,1),
	constrain_all(Xs).

constrain_between(_X,[],_N).
constrain_between(X,[Y|Ys],N) :-
	no_threat(X,Y,N),
	N1 is N+1,
	constrain_between(X,Ys,N1).

no_threat(X,Y,I) +:
	X in \({Y} \/ {Y+I} \/ {Y-I}),
	Y in \({X} \/ {X+I} \/ {X-I}).

% divide & conquer interleaved
% ?- order_di(7,[a,b,c,d,e,f,g], [d,f,b,g,c,e,a]).

order_di(N, L1, L2) :-
	order_di(N, L2, L1, []).

order_di(0, []) --> !.
order_di(N, L) -->
	{A is (N-1)>>1,
	 B is N-A-1},
	order_di(A, L1),
	[X],
	order_di(B, L2),
	{shuffle([X|L1], L2, L)}.
	
shuffle([], L, L).
shuffle([X|L1], L2, [X|L3]) :- shuffle(L2, L1, L3).
