/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Conway's game of life
 *             The goal is to find a 12x12 still-life pattern with 76 
 *             filled squares.
 * Author    : Mats Carlsson
 */


:- module(life, [still_life/5]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

% main :-
% 	still_life(plain_mirror, 12, 12, 76, _Cs).

still_life(plain, NR, NC, Sum, Cells) :-
	still(NR, NC, Sum, Cells),
	labeling([down], Cells),
	draw(Cells, 0, NC).
still_life(plain_rot, NR, NC, Sum, Cells) :-
	still(NR, NC, Sum, Cells),
	reverse(Cells, Rev),
	equate(Cells, Rev),
	labeling([down], Cells),
	draw(Cells, 0, NC).
still_life(plain_mirror, NR, NC, Sum, Cells) :-
	still(NR, NC, Sum, Cells),
	rows(Cells, NC, Rows),
	reverse(Rows, RRows),
	equate_rows(Rows, RRows),
	labeling([down], Cells),
	draw(Cells, 0, NC).
still_life(pos, NR, NC, Sum, Cells) :-
	length(Live, Sum),
	NRC is NR*NC-1,
	domain(Live, 0, NRC),
	ascending(Live, -1),
	still(NR, NC, Sum, Cells),
	tag_by_pos(Cells, 0, KL),
	global_cardinality(Live, KL),
	labeling([step], Live),
	draw(Cells, 0, NC).
still_life(pos_rot, NR, NC, Sum, Cells) :-
	length(Live, Sum),
	NRC is NR*NC-1,
	domain(Live, 0, NRC),
	ascending(Live, -1),
	still(NR, NC, Sum, Cells),
	tag_by_pos(Cells, 0, KL),
	global_cardinality(Live, KL),
	reverse(Live, LiveRev),
	label(Live, LiveRev),
	draw(Cells, 0, NC).

label([], []).
label([F|Fs], [B|Bs]) :-
	labeling([step,up], [F]),
	labeling([step,down], [B]),
	label(Fs, Bs).

rows([], _, []) :- !.
rows(Cells, NC, [Row|Rows]) :-
	row(0, NC, Row, Cells, Cells1),
	rows(Cells1, NC, Rows).

row(NC, NC, []) --> !.
row(I, NC, [X|Xs]) --> [X],
	{J is I+1},
	row(J, NC, Xs).

equate_rows([], []).
equate_rows([R|Rs], [S|Ss]) :-
	equate(R, S),
	equate_rows(Rs, Ss).

equate([], []).
equate([X|Xs], [Y|Ys]) :-
	X#=Y,
	equate(Xs, Ys).

tag_by_pos([], _, []).
tag_by_pos([C|Cells], I, [I-C|KL]) :-
	J is I+1,
	tag_by_pos(Cells, J, KL).

ascending([], _).
ascending([X|Xs], Y) :- X#>Y, ascending(Xs, X).

cell_positions([], _, _).
cell_positions([C|Cs], I, Live) :-
	eqs(Live, I, Eqs),
	sum(Eqs,#=,S),
	S#>=1 #<=> C,
	J is I+1,
	cell_positions(Cs, J, Live).

eqs([], _, []).
eqs([X|Xs], I, [E|Es]) :-
	X#=I #<=> E,
	eqs(Xs, I, Es).

draw([], _, _).
draw([C|Cells], I, NC) :-
	(   C=:=0 -> write(' ')
	;   write('*')
	),
	J is I+1,
	(   J mod NC =:= 0 -> nl
	;   true
	),
	draw(Cells, J, NC).


still(NR, NC, Sum, Cells) :-
	NRC is NR*NC,
	length(Cells, NRC),
	domain(Cells, 0, 1),
	sum(Cells, #=, Sum),
	tag_by_coords(Cells, 0, NC, KL),
	list_to_assoc(KL, Assoc),
	still_rows(-1, NR, NC, Assoc),
	% symmetries
	NR1 is NR>>1,
	NC1 is NC>>1,
	NR2 is NR-NR1,
	NC2 is NC-NC1,
	part(KL, 0, NR1, 0, NC,  Upper),
	part(KL, NR2, NR, 0, NC, Lower),
	part(KL, 0, NR, 0, NC1, Left),
	part(KL, 0, NR, NC2, NC, Right),
	sum(Upper, #=, USum),
	sum(Lower, #=<, USum),
	sum(Left, #=, LSum),
	sum(Right, #=<, LSum).

part([], _, _, _, _, []).
part([(R,C)-X|KL], Rmin, Rmax, Cmin, Cmax, L1) :-
	(   R>=Rmin, R<Rmax, C>=Cmin, C<Cmax -> L1=[X|L1b]
	;   L1=L1b
	),
	part(KL, Rmin, Rmax, Cmin, Cmax, L1b).

tag_by_coords([], _, _, []).
tag_by_coords([C|Cells], I, NC, [(Row,Col)-C|KL]) :-
	Row is I//NC,
	Col is I mod NC,
	J is I+1,
	tag_by_coords(Cells, J, NC, KL).

still_rows(I, NR, _, _) :-
	I>NR, !.
still_rows(I, NR, NC, Assoc) :-
	still_cells(-1, NC, I, Assoc),
	J is I+1,
	still_rows(J, NR, NC, Assoc).

still_cells(Col, NC, _, _) :-
	Col>NC, !.
still_cells(Col, NC, Row, Assoc) :-
	Up  is Row-1,
	Down is Row+1,
	Left is Col-1,
	Right is Col+1,
	getarr((Up,Left), Assoc, X1),
	getarr((Up,Col), Assoc, X2),
	getarr((Up,Right), Assoc, X3),
	getarr((Row,Left), Assoc, X4),
	getarr((Row,Col), Assoc, X5),
	getarr((Row,Right), Assoc, X6),
	getarr((Down,Left), Assoc, X7),
	getarr((Down,Col), Assoc, X8),
	getarr((Down,Right), Assoc, X9),
	S in (0..2)\/(4..6)\/(12..13),
	10*X5 + X1+X2+X3+X4+X6+X7+X8+X9 #= S,	
	Col1 is Col+1,
	still_cells(Col1, NC, Row, Assoc).

getarr(Key, Assoc, Val) :-
	get_assoc(Key, Assoc, Val), !.
getarr(_, _, 0).

