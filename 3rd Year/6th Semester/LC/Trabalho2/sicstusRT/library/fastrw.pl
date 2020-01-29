/* Copyright (C) 1993 Swedish Institute of Computer Science */

%   File       : fastrw.pl
%   Author     : Mats Carlsson
%   Updated    : 3 September 1999
%   Purpose    : Fast term I/O

:- module(fastrw, [
	fast_read/1, fast_read/2,
	fast_write/1, fast_write/2,
	fast_buf_read/2,
	fast_buf_write/3
   ]).

fast_write(Term) :-
	current_output(Stream),
	stream_code(Stream, Code),
	do_fast_write(Term, Code).

fast_write(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_write(Term, Code).

fast_buf_write(Term, Size, Addr) :-
	open_buf_write(Code),
	do_fast_write(Term, Code),
	c_buffer_data(Code, Size, Addr).
	% stream_code(Stream, Code),
	% close(Stream).

do_fast_write(Term, Code) :- c_fast_write(Term, Code), fail.
do_fast_write(_, _).

fast_read(Term) :-
	current_input(Stream),
	stream_code(Stream, Code),
	do_fast_read(Term0, Code),
	Term = Term0.

fast_read(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_read(Term0, Code),
	Term = Term0.

fast_buf_read(Term, Addr) :-
	open_buf_read(Code, Addr),
	do_fast_read(Term0, Code),
	% stream_code(Stream, Code),
	% close(Stream),
	Term = Term0.

do_fast_read(Term, Code) :-
	kludge([Map0]),
	c_fast_read(Term, Map0, Code),
	keysort(Map0, Map),
	keyfuse(Map).

kludge(_).

keyfuse([]).
keyfuse([X|L]) :- L = [X|_], !, keyfuse(L).
keyfuse([_|L]) :- keyfuse(L).

%----------------------------------------------------------------

:- dynamic foreign/2, foreign_resource/2.
foreign(plc_fast_write, c_fast_write(+term, +address)).
foreign(plc_fast_read, c_fast_read(+term, +term, +address)).
foreign(plc_open_buf_write, open_buf_write([-address])).
foreign(plc_open_buf_read, open_buf_read([-address], +integer/*+address*/)).
foreign(plc_buffer_data, c_buffer_data(+address, -integer, -integer/*-address*/)).

% qpc needs an explicit function list
foreign_resource(fastrw,
		 [init(frw_init),deinit(frw_deinit),
		  plc_fast_write,plc_fast_read,
		  plc_open_buf_write,plc_open_buf_read,
		  plc_buffer_data]).

:- load_foreign_resource(library(system(fastrw))).
