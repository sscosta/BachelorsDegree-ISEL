% Document type:  -*- Prolog -*-
% Filename:	  /amd/home/jojo/sicstus/sicstus3/library/jasper/examples/jqueens.pl
% Author:         Jesper Eskilson <jojo@sics.se>
% Last-Update:    Time-stamp: <1998-02-27 1452 jojo>

:- module(jqueens, [jqueens/2]).

:- use_module(library('clpfd/examples/queens')).

jqueens(Size,L) :-
	queens([ff], L, Size).	