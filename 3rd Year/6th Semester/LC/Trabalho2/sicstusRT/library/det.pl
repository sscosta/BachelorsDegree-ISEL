/* ----------------------------------------------------------------------
    File:	det.pl
    Authors:	Dave Bowen and Peter Schachte
    SCCS:	@(#)det.pl	66.1 10/14/92
    Purpose:	Determinacy checker

    Copyright (C) 1989 Quintus Computer Systems, Inc.  Permission is hereby
    granted for this program to be used or modified for any purpose whatsoever
    provided only that this copyright notice is retained in the source.

    This program is intended to be used to look for unintended non-determinacy
    in programs which are intended to be mostly determinate.  Unintended
    non-determinacy should be eradicated because

	(1) it may give you wrong answers on backtracking
	(2) it may cause a lot of memory to be wasted

    The simple way to avoid non-determinacy is by appropriate use of cuts.
    However, it is not good practice to sprinkle cuts all over your program
    "just in case".  Cuts should be used sparingly because they damage the
    declarative reading of your code:  if a procedure contains a cut you cannot
    read each clause in it as an independent logical implication - you have to
    take into account the preceding clauses with cuts.

    The purpose of this program is to help you spot the places where cuts are
    really necessary.  It won't find them all, and it will point to some
    clauses where you don't want a cut because you really intended
    non-determinacy.  However, it is hoped that it will help.

    For advice on the correct placement of cuts, see the chapter "Writing
    Efficient Programs" in the Quintus Prolog Reference Manual.
    An important point to understand is how first-argument indexing can
    often be used to avoid the need for cuts.

    The way to use this program is, from the shell prompt:

	spdet [switches] file1 file2 ...

    where file1, file2, ... are files that you want checked.  A ".pl"
    extension may be omitted.

    The tool supports the following switches:

	-r	process files recursively, fully checking the specified
		files and all the files they load
	-d	print out declarations that should be added
	-D	print out all needed declarations

    Messages of the following form will be printed to standard error:

	* Non-determinate: foo/3 (clause 2)
	*    This clause contains a disjunction not forced to be deterministic.

    This message indicates that if clause 2 of foo/3 succeeds it will leave
    behind a choicepoint, because the clause contains a disjunction.  You
    should look at it carefully and decide whether you really want the system
    backtrack into that clause and look for another solution.  If not, put in
    a cut as early as possible in the clause.  That is, put the cut
    immediately after the goal which determines that the subsequent clauses
    should not be used on backtracking.

    Clauses are reported as being non-determinate if

	(1) the clause does not contain a cut, and
	(2) the clause does not have a first argument with a different 
	    principal functor than the first argument of any subsequent clause
	    for the same predicate, and
	(3) the clause does not end with 'fail' or 'raise_exception'

    Clauses are also reported as being non-determinate if

	(1) the clause contains a disjunction for which some disjunct other
	    than the last does not contain a cut and does not end in fail or
	    raise_exception; or
	(2) the clause invokes a goal known to be nondeterministic, unless
	    that goal is followed by a cut, fail, or raise_exception, or
	    appears in the condition of an if->then;else.  Goals known to be
	    nondetermistic include calls to predicates declared to be
	    nondeterministic or dynamic, calls to certain builtins, and
	    disjunctions that don't force each disjunct but the last to be
	    deterministic.

    Since not all the clauses of a multifile predicate may be available for
    analysis, det must not assume that a clause is determinate just because
    the principle functor of its first argument differs from those of
    subsequent clauses.

    You may declare a predicate to be [non]deterministic with a
    declaration of the form

	:- nondet prespec.
	:- det prespec.

    where predspce is name/arity or module:name/arity.  Such predicates are
    not warned about by the determinacy checker.  If you put [non]det
    declarations in your program, you must put

	:- load_files(library(nondetdecl), [when(compile_time), if(changed)]).

    near the top of your file so the Prolog compiler can ignore the [non]det
    declarations.  Alternatively, you may wish to put

	:- load_files(library(nondetdecl), [when(compile_time), if(changed)]).

    near the top of your file.  If you do this, the the Prolog compiler will
    perform determinacy checking every time you compile the file.

    Disclaimer:  This program does not catch all possible sources of
    non-determinacy.  In particular, note that it assumes determinacy when
    any two clauses have first arguments with different principal functors.
    Of course, this only implies determinacy if the predicate is always called
    with its first argument instantiated.  In general, this code assumes if a
    goal would be deterministic with certain arguments ground, then those
    arguments will be ground when the goal is invoked.

   ---------------------------------------------------------------------- */
    
:- module(det, []).
:- use_module(library(determinacy)).

:- op(1150, fx, nondet).
:- op(1150, fx, det).

:- dynamic processed/1.

det(Files, Options) :-
    initialize,
    retractall(processed(_)),
    set_options(Options),
    process_files(Files),
    (	get_option(recursive, true) ->
	    bottom_up_detcheck
    ;	true
    ),
    (	get_option(print_decls, true) ->
	    get_option(print_all_decls, All),
	    print_decls(All)
    ;	true
    ).

set_options([]).
set_options([Name=Value|Opts]) :-
    set_option(Name, Value),
    set_options(Opts).


/* ----------------------------------------------------------------------
   Determinacy check files
   ---------------------------------------------------------------------- */

process_files([]) :- !.
process_files([File|Files]) :- !,
    process_files(File),
    process_files(Files).
process_files(File) :-
    absolute_file_name(File, Path),
    (	processed(Path) ->
	    true
    ;	assert(processed(Path)),
	initialize_file(Path),
	open(Path, read, S),
	process_stream(S, Path-0, user, _)
    ).
    
process_stream(S, Path-Level, Mod, Filemod) :-
    prolog_flag(syntax_errors, Old, dec10),
    print_message(help, checking(Level,Path)), % NOT informational
    (	read_clauses(S, Pred, Mod, Filemod, Path, Clauses),
	process(Pred, Filemod, Path-Level, Clauses),
	fail
    ;	true
    ),
    close(S),
    prolog_flag(syntax_errors, _, Old).


/* ----------------------------------------------------------------------
    Returns a predicate in the form Name/Arity and a list of all its clauses.
    Backtracks over all the predicates defined in the file.  Mostly this
    collects clauses as they are found, but clauses for predicates that are
    declared discontiguous are stored until the end of the file is reached,
    and returned then.  For multifile predicates, only the clauses in the
    current file are returned.
   ---------------------------------------------------------------------- */

read_clauses(S, Pred, Defaultmod, Modname, File, Clauses) :-
    read_clause(S, Clause0),
    (   Clause0 = :-(module(Modname,Exports)) ->
	    assert(determinacy:module_exports(Modname, Exports)), % SP
	    assert(determinacy:file_processed(File, Modname)), % SP
	    read_clauses1(S, Modname, Pred, File, Clauses)
    ;   Clause0 = :-(module(Modname,Exports,_)) -> % SP
	    assert(determinacy:module_exports(Modname, Exports)), % SP
	    assert(determinacy:file_processed(File, Modname)), % SP
	    read_clauses1(S, Modname, Pred, File, Clauses)
    ;	Clause0 \== end_of_file ->
	    Modname = Defaultmod,
	    assert(determinacy:file_processed(File, Modname)), % SP
	    pred_for_clause(Clause0, Modname, InitPred),
	    read_clauses2(S, InitPred, Pred, Modname, File,
			  CList, [Clause0|CList], Clauses)
    ;       assert(determinacy:file_processed(File, Defaultmod)), % SP
	    fail
    ).

read_clauses1(S, Modname, Pred, File, Clauses) :-
    read_clause(S, Clause0),
    (   Clause0 == end_of_file ->
	    %  hang onto saved clauses when recursively processing
	    get_option(recursive, false),
	    forget_discontiguity,
	    saved_clauses(File, Pred, Clauses)
    ;	pred_for_clause(Clause0, Modname, InitPred),
	read_clauses2(S, InitPred, Pred, Modname, File,
		      CList, [Clause0|CList], Clauses)
    ).

read_clauses2(S, InitPred, Pred, Modname, File, CListtail, CList, Clauses) :-
    (	directive_pred(InitPred) ->
	    (	Pred = InitPred,
		CListtail = [],
		Clauses = CList
	    ;	read_clauses1(S, Modname, Pred, File, Clauses)
	    )
    ;	read_clause(S, Clause),
	pred_for_clause(Clause, Modname, ThisPred),
	(   ThisPred == InitPred ->
		CListtail = [Clause|CListtail2],
		read_clauses2(S, InitPred, Pred, Modname, File,
			      CListtail2, CList, Clauses)
	;   Pred = InitPred,			% nondeterministic!!!
	    CListtail = [],
	    Clauses = CList
	;   Clause == end_of_file ->
		%  hang onto saved clauses when recursively processing
		get_option(recursive, false),
		forget_discontiguity,
		saved_clauses(File, Pred, Clauses)
	;   read_clauses2(S, ThisPred, Pred, Modname, File,
			  Clist2, [Clause|Clist2], Clauses)
	)
    ).


/* ----------------------------------------------------------------------
    Messages
   ---------------------------------------------------------------------- */

:- multifile user:generate_message_hook/3.
:- dynamic   user:generate_message_hook/3.

user:generate_message_hook(checking(Level,AbsFile)) --> !,
	['~*c~a'-[Level,32,AbsFile],nl].




/* ----------------------------------------------------------------------
    Starting up det
   ---------------------------------------------------------------------- */

usage :- write(user_error,
'usage: spdet [-r] [-d] [-D] [-i ifile] fspec ...

  -r ......... process files recursively, fully checking the specified
               files and all the files they load
  -d ......... print out declarations that should be added
  -D ......... print out all needed declarations
  -i ifile ... an initialization file. Read in prior to checking.
  fspec ...... one or more filenames, ".pl" extensions optional

Check each specified file for predicates which might not be deterministic
even when their first arguments are bound, printing a diagnostic for each
such predicate.
').

% :- use_module(library(charsio)).
% :- use_module(messages(language('QU_messages'))).

user:runtime_entry(start) :-
	prolog_flag(argv, Args),
	parse(Args, Files, Options),
	functor(Files, '.', 2), !,
	det(Files, Options).
user:runtime_entry(start) :-
	usage,
	prolog_flag(system_type, runtime),
	halt(1).

parse([], [], []).
parse([H,A|R], F, L0) :- prs(H, A, L0, L),
    !, parse(R, F, L).
parse([H|T], F, L0) :- atom_chars(H, [0'-|V]),
    !, prsc(V, L0, L), parse(T, F, L).
parse([F|T], [F|R], L) :- parse(T, R, L).

prs('-i', A,                    L, L) :- compile(A).

prsc([], L, L).
prsc([H|T], L0, L) :- prsc1(H, L0, L1), prsc(T, L1, L).

prsc1(0'r, [recursive=true,process_dependency=process_stream|L], L).
prsc1(0'd, [print_decls=true|L], L).
prsc1(0'D, [print_decls=true,print_all_decls=true|L], L).

end_of_file.

/* ----------------------------------------------------------------------
    Error recovery.  When a recognized error occurs in the processing
    of a file specified in the command line, processing is resumed at the
    next file in the command line.
   ---------------------------------------------------------------------- */

go(Files, Options) :-
    on_exception(E, 
		 det(Files, Options), 
		 (   print_error_message(E),
		     try_to_recover(E,L))
    ).


try_to_recover(E, L) :-
    error_in_open(E, File),
    !,
    skip_past_culprit(L, File).
try_to_recover(_, L) :-
    current_stream(File, _, Stream),
    !,
    close(Stream),
    skip_past_culprit(L, File).
try_to_recover(_, _) :-
    halt(1).

error_in_open(E, File) :-
    nonvar(E),
    functor(E, _, A),
    A >= 1,
    arg(1, E, open(File,_,_)),
    nonvar(File).

skip_past_culprit([File|Rest], Culprit) :-
    absolute_file_name(File, Culprit),
    !,
    go(Rest).
skip_past_culprit([_File|Rest], Culprit) :-
    skip_past_culprit(Rest, Culprit).


handle_switches([], []).
handle_switches([S|Ss], Fs) :-
    (	switch(S, Options, _) ->
	    set_options(Options),
	    handle_switches(Ss, Fs)
    ;	looks_like_a_switch(S) ->
	    format('Unknown switch ~w~n', [S]),
	    usage,
	    halt(1)
    ;	Fs = [S|Ss]
    ).

switch('-r', [recursive=true,process_dependency=process_stream],
		'Check specified files and all files they load').
switch('-d', [print_decls=true],
		'Print out declarations that should be added').
switch('-D', [print_decls=true, print_all_decls=true],
		'Print out all needed declarations').

looks_like_a_switch(S) :-
    atom_codes(S, [0'-|_]).	% SP


usage :-
    format('usage: det [options] file ...~nOptions:~n', []),
    (   switch(S, _, Desc),
	format('    ~w  ~w~n', [S,Desc]),
	fail
    ;	true
    ),
    write('Check each specified file for predicates which might not be deterministic
even when their first arguments are bound, printing a diagnostic for each
such predicate.
').
