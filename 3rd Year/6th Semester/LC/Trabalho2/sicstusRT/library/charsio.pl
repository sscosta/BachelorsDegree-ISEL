/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : charsio.pl
%   Author     : Stefan Andersson
%   Updated    : 3 September 1999
%   Purpose    : Defines predicates that read from, or write to, 
%              : a list of character codes.


:- module(charsio, [
	format_to_chars/3,
	format_to_chars/4,
	write_to_chars/2,
	write_to_chars/3,
	write_term_to_chars/3,
	write_term_to_chars/4,
	atom_to_chars/2,
	atom_to_chars/3,
	number_to_chars/2,
	number_to_chars/3,
	read_from_chars/2,
	read_term_from_chars/3,
	open_chars_stream/2,
	with_output_to_chars/2,
	with_output_to_chars/3,
	with_output_to_chars/4
		   ]).

:- meta_predicate	
	format_to_chars(?,:,?),
	format_to_chars(?,:,?,?),
	with_output_to_chars(:,?),
	with_output_to_chars(:,?,?),
	with_output_to_chars(:,?,?,?).


%% format_to_chars(+Fmt, +Args, -Chars)
%% format_to_chars(+Fmt, +Args, -S0, +S)

format_to_chars(Fmt, Args, Chars) :-
	format_to_chars(Fmt, Args, Chars, []).

format_to_chars(Fmt, Args, S0, S) :-
   %% [PM] 3.9b4 use of $init_charsio_stream is not re-entrant.
   %%            This is a problem with format specifiers such as
   %%            ~@ and ~p (via portray/1).
   %% '$init_charsio_stream'(StreamCode),
   %% StreamCode \== 0,
   %% stream_code(Stream, StreamCode),
   %% format(Stream, Fmt, Args),
   %% put(Stream, 0),
   %% '$stream_to_chars'(StreamCode, S0, S).
   with_output_to_chars(format(Stream, Fmt, Args), Stream, S0, S).


%% write_to_chars(+Term, -Chars)
%% write_to_chars(+Term, -S0, +S)

write_to_chars(Term, Chars) :-
	write_to_chars(Term, Chars, []).

write_to_chars(Term, S0, S) :-
        %% [PM] 3.9b4 get rid of init_charsio_stream to ease MULTI_SP support
	%% '$init_charsio_stream'(StreamCode),
	%% StreamCode \== 0,
	%% stream_code(Stream, StreamCode),
	%% write(Stream, Term),
	%% put(Stream, 0),
	%% '$stream_to_chars'(StreamCode, S0, S).
        with_output_to_chars(write(Stream, Term), Stream, S0, S).


%% write_term_to_chars(+Term, -Chars, +Options)
%% write_term_to_chars(+Term, -S0, +S, +Options)

write_term_to_chars(Term, Chars, Options) :-
	write_term_to_chars(Term, Chars, [], Options).

write_term_to_chars(Term, S0, S, Options) :-
        with_output_to_chars(write_term(Stream,Term,Options), Stream, S0, S).


%% atom_to_chars(+Atom, -Chars)
%% atom_to_chars(+Atom, -S0, +S) 

atom_to_chars(Atom, Chars) :-   % a.k.a. atom_codes/2?
	atom_to_chars(Atom, Chars, []).

atom_to_chars(Atom, S0, S) :-
	atom(Atom), !,
	'$atom_to_chars'(Atom, S0, S).
	% format_to_chars("~a", [Atom], S0, S).
atom_to_chars(Atom, S0, S) :-
	prolog:illarg(type(atom), atom_to_chars(Atom,S0,S), 1).


%% number_to_chars(+Number, -Chars)
%% number_to_chars(+Number, -S0, +S)

number_to_chars(Number, Chars) :- % a.k.a. number_codes/2?
	number_to_chars(Number, Chars, []).

number_to_chars(Number, S0, S) :-
	number(Number), !,
	'$number_to_chars'(Number, S0, S).		% in C
	% format_to_chars("~w", [Number], S0, S).
number_to_chars(Number, S0, S) :-
	prolog:illarg(type(number), number_to_chars(Number,S0,S), 1).


%% read_from_chars(+Chars, -Term)

read_from_chars(Chars, Term) :-
   %% [PM] 3.9b4 get rid of static charsio stream
   %% '$init_charsio_stream'(Chars, StreamCode),
   %% StreamCode \== 0,
   %% stream_code(Stream, StreamCode),
   %% read(Stream, Term).
   
   open_chars_stream(Chars, Stream),
   %% There is a small race condition here that could leave Stream open.
   call_cleanup(read(Stream, Term0),
                close(Stream)),
   Term = Term0.

%% read_term_from_chars(+Chars, -Term, +Options)
%% Suggested by Gertjan Van Noord

read_term_from_chars(Chars, Term, Options) :-
    open_chars_stream(Chars, Stream),
    call_cleanup(read_term(Stream, Term0, Options),
		 close(Stream)),
    Term = Term0.

%% open_chars_stream(?Chars, -Stream)

open_chars_stream(Chars, Stream) :- !,
	'$chars_to_stream'(Chars, StreamCode),
	stream_code(Stream, StreamCode).


%% with_output_to_chars(+Goal, -Chars)
%% with_output_to_chars(+Goal, -S0, -S)
%% with_output_to_chars(+Goal, -Stream, -S0, -S)

with_output_to_chars(Goal, Chars) :-
	with_output_to_chars(Goal, Chars, []).

with_output_to_chars(Goal, S0, S) :-
	with_output_to_chars(Goal, _, S0, S).

with_output_to_chars(Goal, Stream, S0, S) :-
	'$open_buf_stream'(StreamCode),
	StreamCode \== 0,
	stream_code(Stream, StreamCode),
	current_output(CurrOut),
	set_output(Stream),
	call_cleanup(call_to_chars(Goal,Stream,StreamCode,S0,S),
		     reset_stream(Stream,CurrOut)).

call_to_chars(Goal, Stream, StreamCode, S0, S) :-
	Goal, !,
	put(Stream, 0),
	'$stream_to_chars'(StreamCode, S0, S).

reset_stream(Stream, CurrOut) :-
	set_output(CurrOut),
	close(Stream).


:- dynamic foreign/2, foreign_resource/2.

foreign(xatom_to_chars, '$atom_to_chars'(+string,-term,-term)).
foreign(xnumber_to_chars, '$number_to_chars'(+term,-term,-term)).
%% [PM] 3.9b4 static charsio stream is gone
%% foreign(init_charsio_stream_w, '$init_charsio_stream'(-address('SP_stream'))).
%% foreign(init_charsio_stream_r, '$init_charsio_stream'(+chars,-address('SP_stream'))).
foreign(chars_to_stream, '$chars_to_stream'(+chars,-address('SP_stream'))).
foreign(open_buf_stream, '$open_buf_stream'(-address('SP_stream'))).
foreign(stream_to_chars, '$stream_to_chars'(+address('SP_stream'),-term,-term)).

foreign_resource(charsio, [
	xatom_to_chars,
	xnumber_to_chars,
	%% init_charsio_stream_w,
	%% init_charsio_stream_r,
	chars_to_stream,
	open_buf_stream,
	stream_to_chars
	%% [PM] 3.9b4 deinit is no longer used
        %% ,deinit(deinit_charsio)
			  ]).

:- load_foreign_resource(library(system(charsio))).

