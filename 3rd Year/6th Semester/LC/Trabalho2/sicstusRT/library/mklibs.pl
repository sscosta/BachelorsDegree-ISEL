% Build all the library object modules as hidden modules

:- multifile user:term_expansion/2.
:- dynamic user:term_expansion/2.
user:term_expansion((:- module(M,E)), (:- module(M,E,[hidden(true)]))).

:- dynamic use_error_message_hook/1.

:- multifile user:portray_message/2.
:- dynamic user:portray_message/2.
user:portray_message(error, Message) :-
	use_error_message_hook(Options), !,
	handle_error_message(Message, Options).

make(Module, Rest) :-
	make(Module, Rest, []).

make(Module, Rest, Options) :-
	prolog_flag(redefine_warnings, _, off),
        asserta(use_error_message_hook(Options)),
	absolute_file_name(., A),
	asserta(library_directory(A)),
	compile(library(Module)),
	wrap_files(Rest, Tail),
	save_files([library(Module)|Tail], library(Module)).

%% [PM] 3.8.5
%% Options is a list of
%% on_foreign_resource_error(Opt) --
%%         existence error when loading foreign resource (e.g.,
%%         run-time linker error)
%% on_error(Opt) -- any error
%% Where Opt is
%%  warn -- this is the SICStus standard behaviour, print the error
%%          and go on.
%%  halt -- (the default) call halt(1) to exit with an error
%%          code. Useful to tell make that something went wrong. This
%%          is used for most libraries.
handle_error_message(existence_error(load_foreign_resource(_),_,_,_,_), Options) :-
	prolog:member(on_foreign_resource_error(warn), Options), !,
	fail.
handle_error_message(_Message, Options) :-
	prolog:member(on_error(warn), Options), !,
	fail.
handle_error_message(Message, _Options) :- 
	%% This is the default
	%% prolog:member(on_error(halt), _Options), 
	!,
	abolish(user:portray_message/2),
	print_message(error, Message),
	halt(1).
handle_error_message(_Message, _Options) :-
	fail.

wrap_files([], []).
wrap_files([X|Xs], [library(X)|Ys]) :- wrap_files(Xs, Ys).

make_basic :-
	prolog_flag(redefine_warnings, _, off),
	make(arrays, []),
	make(assoc, []),
	make(atts, []),
	make(charsio, []),
	make(clpb, []),
	make(context, []),
	% make(db, []),
	make(fastrw, []),
	make(fdbg, []),
	make(flinkage, []),
	make(gauge, [context]),
	make(heaps, []),
	make('linda/server', []),
	make('linda/client', []),
	make(lists, []),
	make(mkindex, []),
	make(ordsets, []),
	make(queues, []),
	make(random, []),
	make(sockets, []),
	make(system, []),
	make(tcltk, []),
	make(terms, []),
	make(timeout, []),
	make(tkconsol, []),
	make(trees, []),
	make(ugraphs, []),
	make('vbsp/vbsp', []),
	make(wgraphs, []),
	make(expansion, []),
	make(nondetdecl, []),
	make(determinacy, []),
	make(detcheck, []),
	make(det, []),
	make(xref, []),
	make(pillow, []).

%% chr, clpq, clpr, clpfd, objects:

make_chr :-
	make(chr, ['chr/matching','chr/chrcmp','chr/getval','chr/sbag','chr/operator','chr/concat','chr/trace']).

make_clpq :-
	make(clpq, ['clpq/arith',
		    'clpq/arith_q',
		    'clpq/itf3',
		    'clpq/store',
		    'clpq/geler',
		    'clpq/nf',
		    'clpq/nfq',
		    'clpq/ordering',
		    'clpq/class',
		    'clpq/project',
		    'clpq/bv',
		    'clpq/ineq',
		    'clpq/redund',
		    'clpq/fourmotz',
		    'clpq/bb',
		    'clpq/dump']).
 
make_clpr :-
	make(clpr, ['clpr/arith',
		    'clpr/arith_r',
		    'clpr/itf3',
		    'clpr/store',
		    'clpr/geler',
		    'clpr/nf',
		    'clpr/nfr',
		    'clpr/ordering',
		    'clpr/class',
		    'clpr/project',
		    'clpr/bv',
		    'clpr/ineq',
		    'clpr/redund',
		    'clpr/fourmotz',
		    'clpr/bb',
		    'clpr/dump']).
 
make_clpfd :-
	make(clpfd, ['clpfd/fdsets',
		     'clpfd/ixq',
		     'clpfd/enum',
		     'clpfd/compiler',
		     'clpfd/lib']).


make_objects :-
	make(objects, ['objects/expand',
		       'objects/runlib',
		       'objects/object']).

