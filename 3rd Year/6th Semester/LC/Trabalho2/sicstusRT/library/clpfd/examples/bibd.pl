/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Balanced Incomplete Block Design
 * Author    : Mats Carlsson
 *
 * The goal is to find an 8x14 binary matrix with
 * 7 ones in each row, 4 ones in each column,
 * the scalar product of any two rows being 3.
 */

:- module(bibd, [bibd/7]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% main :-
% 	bibd([], 8, 14, 7, 4, 3, _).

bibd(Opt, NR, NC, R, K, Lambda, Cells) :-
	bibd(NR, NC, R, K, Lambda, Cells),
	labeling(Opt, Cells),
	draw(Cells, 0, NC).

draw([], _, _).
draw([C|Cells], I, NC) :-
	write(C),
	J is I+1,
	(   J mod NC =:= 0 -> nl
	;   true
	),
	draw(Cells, J, NC).


bibd(NR, NC, R, K, Lambda, Cells) :-
	NRC is NR*NC,
	length(Cells, NRC),
	domain(Cells, 0, 1),
	rows(Cells, NC, Rows),
	columns(0, NC, Rows, Columns),
	each_sum(Rows, R),
	each_sum(Columns, K),
	each_product(Rows, Lambda),
	% symmetries
	order(Rows),
	order(Columns).

rows([], _, []) :- !.
rows(Cells, NC, [Row|Rows]) :-
	row(0, NC, Row, Cells, Cells1),
	rows(Cells1, NC, Rows).

row(NC, NC, []) --> !.
row(I, NC, [X|Xs]) --> [X],
	{J is I+1},
	row(J, NC, Xs).

columns(NC, NC, _, []) :- !.
columns(I, NC, Rows, [Col|Cols]) :-
	J is I+1,
	column(Rows, J, Col),
	columns(J, NC, Rows, Cols).

column([], _, []).
column([Row|Rows], J, [C|Col]) :-
	nth(J, Row, C),
	column(Rows, J, Col).

each_sum([], _).
each_sum([Row|Rows], R) :-
	sum(Row, #=, R),
	each_sum(Rows, R).

each_product([], _).
each_product([Row|Rows], Lambda) :-
	each_product(Rows, Row, Lambda),
	each_product(Rows, Lambda).

each_product([], _, _).
each_product([Row|Rows], Row0, Lambda) :-
	product(Row, Row0, S),
	sum(S, #=, Lambda),
	each_product(Rows, Row0, Lambda).

product([], [], []).
product([X|Xs], [Y|Ys], [XY|XYs]) :-
	X #/\ Y #<=> XY,
	product(Xs, Ys, XYs).
	
order([_]) :- !.
order([R1,R2|Rows]) :-
	order(R1, R2, 1),
	order([R2|Rows]).

order([], [], 1).
order([X|Xs], [Y|Ys], B) :-
	(#\ (X #=> Y) #\/ ((X#<=>Y) #/\ R)) #<=> B,
	order(Xs, Ys, R).
