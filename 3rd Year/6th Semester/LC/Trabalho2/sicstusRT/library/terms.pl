/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : terms.pl
%   Author     : Mats Carlsson
%   Updated    : 25 July 2000
%   Purpose    : Miscellaneous operations on terms

:- module(terms, [
	subsumes_chk/2, 
	subsumes/2, 
	variant/2, 
	term_subsumer/3, 
	term_hash/2,
	term_hash/4,
	term_variables/2,
	term_variables_bag/2,
%	unify_with_occurs_check/2,
	acyclic_term/1,
	cyclic_term/1
	]).

:- use_module(library(assoc), [
	empty_assoc/1,
	get_assoc/3,
	put_assoc/4
	]).

%   subsumes_chk(General, Specific)
%   is true when Specific is an instance of General.  It
%   does not bind any variables in General or Specific.

subsumes_chk(General, Specific) :-
	var(General),
	var(Specific), !.
subsumes_chk(General, Specific) :-
	nonvar(Specific),
	\+ \+ prolog:'$subsumes'(General, Specific).


%   subsumes(General, Specific)
%   is true when Specific is an instance of General.  It will bind
%   variables in General (but not those in Specific) so that General
%   becomes identical to Specific.

subsumes(General, Specific) :-
        subsumes_chk(General, Specific),
        General = Specific.


%   variant(Term, Variant)
%   is true when Term and Variant are identical module renaming of variables,
%   provided Term and Variant have no variables in common.


variant(Term, Variant) :-
        subsumes_chk(Term, Variant),
        subsumes_chk(Variant, Term).



%   term_subsumer(+Term1, +Term2, -Term)
%   binds Term to a most specific generalisation of Term1 and Term2.
%   Using Plotkin's algorithm [Machine Intelligence 5, 1970], extended
%   by Dan Sahlin to handle cyclic structures.

term_subsumer(Term1, Term2, Subsumer) :-
	cyclic_term(Term1),
	cyclic_term(Term2), !,
	empty_assoc(S),
	cyclic_subsumer(Term1, Term2, S, _, S, Subsumer).
term_subsumer(Term1, Term2, Subsumer) :-
	empty_assoc(S),
	subsumer(Term1, Term2, S, _, Subsumer).

subsumer(Term1, Term2, S0, S, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N)
	->  functor(Term, F, N),
	    subsumer(N, Term1, Term2, S0, S, Term)
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   get_assoc(Term1-Term2, S0, V) -> S = S0, Term = V
	;   put_assoc(Term1-Term2, S0, Term, S)
	).

subsumer(0, _, _, S, S, _) :- !.
subsumer(N, T1, T2, S0, S, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	subsumer(T1x, T2x, S0, S1, T3x),
	M is N-1,
	subsumer(M, T1, T2, S1, S, T3).


cyclic_subsumer(Term1, Term2, S0, S, U, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N) ->
	    (   get_assoc(Term1-Term2, U, V) -> S = S0, Term = V
	    ;   functor(Term, F, N),
		put_assoc(Term1-Term2, U, Term, U1),
	        cyclic_subsumer(N, Term1, Term2, S0, S, U1, Term)
	    )
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   get_assoc(Term1-Term2, S0, V) -> S = S0, Term = V
	;   put_assoc(Term1-Term2, S0, Term, S)
	).

cyclic_subsumer(0, _, _, S, S, _, _) :- !.
cyclic_subsumer(N, T1, T2, S0, S, U, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	cyclic_subsumer(T1x, T2x, S0, S1, U, T3x),
	M is N-1,
	cyclic_subsumer(M, T1, T2, S1, S, U, T3).



%   term_hash(+Term, ?Hash)
%   If Term is ground, an integer hash value corresponding to Term 
%   is unified with Hash.  Otherwise, the goal just succeeds.

term_hash(Term, Value) :-
	(   Modulus = 16'2000000		% 1<<25 for 32-bit arch
    /** ;   Modulus = 16'10000000000000000000	% 1<<56 for 64-bit arch **/
	),
    /** prolog:'$large_data'(0, Modulus, _), !, **/
	prolog:'$term_hash'(Term, -1, Modulus, Value).

%   term_hash(+Term, +Depth, +Range, ?Hash)
%   If Term is instantiated to the given Depth, an integer hash value in
%   the range [0,Range) corresponding to Term is unified with Hash.
%   Otherwise, the goal just succeeds.

term_hash(Term, Depth, Range, Value) :-
	(   integer(Depth), Depth >= -1 -> true
	;   prolog:illarg(domain(integer,>=(-1)),
	                  term_hash(Term,Depth,Range,Value), 2)
	),
	(   integer(Range), Range >= 1 -> true
	;   prolog:illarg(domain(integer,>=(1)),
	                  term_hash(Term,Depth,Range,Value), 3)
	),
	prolog:'$term_hash'(Term, Depth, Range, Value).



%   term_variables(Term, Variables)
%   True if Variables is the set of variables occurring in Term.

term_variables(Term, Variables) :-
	prolog:term_variables(Term, Variables).

%   term_variables_bag(Term, Variables)
%   True if Variables is the list of variables occurring in Term,
%   in first occurrence order.

term_variables_bag(Term, Variables) :-
	prolog:'$term_variables'(Term, Variables).

% Became a BIP, because of ISO
%   unify_with_occurs_check(X, Y)
%   True if X and Y unify to a finite (acyclic) term.
%
% unify_with_occurs_check(X, Y) :-
% 	prolog:'$unify_with_occurs_check'(X, Y).



%   acyclic_term(X)
%   True if X is finite (acyclic).  Runs in linear time.

acyclic_term(X) :-
	prolog:'$acyclic'(X).



%   cyclic_term(X)
%   True if X is infinite (cyclic).  Runs in linear time.

cyclic_term(X) :-
	\+prolog:'$acyclic'(X).
