/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Car Sequencing Problem
 * Author    : Mats Carlsson
 * Adapted from code by Bjorn Carlson.
 * 
 * The problem is to fill partially filled 9x9 squares of 81 squares such that
 * each row and column are permutations of [1,...,9], and each 3x3 square, 
 * where the leftmost column modulo 3 is 0, is a permutation of [1,...,9].  
 */ 
:- module(suudoku,[suudoku/3]).  
:- use_module(library(clpfd)).

suudoku(Label, P, Problem) :-
	problem(P, Problem),
	domain_problem(Problem),
	row_constraint(Problem),
	column_constraint(Problem),
	block_constraint(Problem),
	enum_problem(Problem, Label).

domain_problem([]).
domain_problem([P|R]) :-
	domain(P,1,9),
	domain_problem(R).

row_constraint([R|Rt]) :-
	
	all_different(R),
	row_constraint(Rt).
row_constraint([]).

column_constraint([C1,C2,C3,C4,C5,C6,C7,C8,C9]) :-
	column_constraint(C1,C2,C3,C4,C5,C6,C7,C8,C9).

column_constraint([C1|C1t],[C2|C2t],[C3|C3t],[C4|C4t],[C5|C5t],[C6|C6t],[C7|C7t],[C8|C8t],[C9|C9t]) :-
	all_different([C1,C2,C3,C4,C5,C6,C7,C8,C9]),
	column_constraint(C1t,C2t,C3t,C4t,C5t,C6t,C7t,C8t,C9t).
column_constraint([],[],[],[],[],[],[],[],[]).

block_constraint([C1,C2,C3,C4,C5,C6,C7,C8,C9]) :-
    block_constraint(C1,C2,C3),
    block_constraint(C4,C5,C6),
    block_constraint(C7,C8,C9).

block_constraint([C1,C2,C3|C1t],[C4,C5,C6|C2t],[C7,C8,C9|C3t]) :-
	all_different([C1,C2,C3,C4,C5,C6,C7,C8,C9]),
	block_constraint(C1t,C2t,C3t).
block_constraint([],[],[]).

enum_problem(P, L) :-
	append_all(P, Pf),
	labeling(L, Pf).

append_all([], []) :- !.
append_all([P|R], X) :-!,
	lists:append(P, Y, X),
	append_all(R, Y).

problem(1, P) :- % shokyuu
	P=[[1,_,_,8,_,4,_,_,_],
	   [_,2,_,_,_,_,4,5,6],
	   [_,_,3,2,_,5,_,_,_],
	   [_,_,_,4,_,_,8,_,5],
	   [7,8,9,_,5,_,_,_,_],
	   [_,_,_,_,_,6,2,_,3],
	   [8,_,1,_,_,_,7,_,_],
	   [_,_,_,1,2,3,_,8,_],
	   [2,_,5,_,_,_,_,_,9]].

problem(2, P) :-  % shokyuu
	P=[[_,_,2,_,3,_,1,_,_],
	   [_,4,_,_,_,_,_,3,_],
	   [1,_,5,_,_,_,_,8,2],
	   [_,_,_,2,_,_,6,5,_],
	   [9,_,_,_,8,7,_,_,3],
	   [_,_,_,_,4,_,_,_,_],
	   [8,_,_,_,7,_,_,_,4],
	   [_,9,3,1,_,_,_,6,_],
	   [_,_,7,_,6,_,5,_,_]].

problem(3, P) :-  % chuukyuu
	P=[[_,_,_,_,_,_,3,_,_],
	   [_,_,_,8,5,_,_,1,_],
	   [_,_,2,_,_,4,_,_,9],
	   [_,3,_,_,_,2,_,_,4],
	   [8,_,_,_,6,_,_,_,1],
	   [7,_,_,9,_,_,_,5,_],
	   [1,_,_,6,_,_,7,_,_],
	   [_,9,_,_,2,3,_,_,_],
	   [_,_,4,_,_,_,_,_,_]].

problem(4, P) :-  % joukyuu
	P=[[_,7,9,_,_,_,_,_,1],
	   [6,_,_,_,_,_,3,8,_],
	   [_,_,_,_,4,2,_,_,_],
	   [_,_,3,9,_,_,_,_,_],
	   [7,8,_,_,_,_,_,2,5],
	   [_,_,_,_,_,4,8,_,_],
	   [_,_,_,3,1,_,_,_,_],
	   [_,5,6,_,_,_,_,_,7],
	   [2,_,_,_,_,_,4,3,_]].

problem(5, P) :-  % shokyuu; from Mr. Horai
	P=[[_,5,_,7,_,1,_,4,_],
	   [7,_,3,_,_,_,1,_,2],
	   [_,8,_,4,_,6,_,9,_],
	   [9,_,4,_,6,_,8,_,3],
	   [_,_,_,8,_,7,_,_,_],
	   [1,_,8,_,5,_,6,_,9],
	   [_,1,_,6,_,3,_,8,_],
	   [5,_,6,_,_,_,7,_,1],
	   [_,3,_,5,_,9,_,2,_]].

problem(6, P) :- % Hard: suudoku2 99 (1989)
	P=[
	   [8,_,_,_,_,5,_,_,_],
	   [_,1,2,3,_,_,6,_,_],
	   [_,4,5,6,_,_,_,2,_],
	   [_,7,8,_,_,_,_,_,1],
	   [_,_,_,_,9,_,_,_,_],
	   [9,_,_,_,_,_,8,7,_],
	   [_,2,_,_,_,6,5,4,_],
	   [_,_,4,_,_,3,2,1,_],
	   [_,_,_,1,_,_,_,_,9]].
