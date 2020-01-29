%   Module : expansion
%   SCCS: @(#)expansion.pl	19.1 30 Dec 1993
%   Author : Jonas Almgren
%   Defines: extensions to term_expansion/2

:- module(expansion, [
	clause_expansion_loop/3,	% to be called for recursive exp.
	goal_expansion_loop/3,		% to be called for recursive exp.
	xtrace/0,			% switch on tracing in expansion
	xdebug/0			% switch on debugging in expansion
		     ]).

/* The standard term_expansion/2 is often not enough to efficiently
   and correctly apply multiple, "simultaneously" active, or recursive,
   program transforms. Below, a straight forward, and at least partial,
   solution to this dilemma is implemented. This library "takes over"
   term_expansion/2, so you shouldn't use your own definitions for
   term_expansion/2 in conjunction with this library.

   The basic idea behind the hooks defined below is that they, in addition
   to the unexpanded input, and the expanded output argument, takes an
   input argument that can be used to look up what expansions have already
   been applied to the clause or goal. The argument is supposed to be a list
   of tokens, each token uniquely identifying an expansion. The tokens
   are defined by the user, and should simply be added to the input list,
   before expansions recursively are applied. This token list can for
   instance be used to avoid cyclic expansions (using term_expansion/2, you
   either had to look for fix points, or assert some information in the
   database).

   In replacement of term_expansion/2, we define two new hooks:
	user:clause_expansion(+Clause, +IdList, -ExpandedClause)
	user:meta_goal_expansion(+Goal, +IdList, -ExpandedGoal)

   The first of these hooks are called for each clause that is
   compiled, the second is called for each goal in the body of
   clauses not expanded by clause_expand/3. The second hook is
   also called for each meta goal that is an argument to a goal
   in the body of a clause. Note that in this context, a meta
   argument is defined as an argument that takes a goal to be
   called, not for instance a clause to be asserted. Also note
   that user defined meta-predicates are not included, and the
   meta arguments might be unbound at compilation time, in which
   case they are not expanded.

   This module exports the two predicates:
	clause_expansion_loop(+Clause, +IdList, -ExpandedClause)
	goal_expansion_loop(+Goal, +IdList, -ExpandedGoal)

    These are the predicates that should be called from within an
    expansion hook, if additional expansion should be applied to
    the expanded clause or goal.

    In addition to the virtual clause 'end_of_file', the virtual clause
    'beginning_of_file' has been added. The 'beginning_of_file' clause is
    generated before the first clause in a file is compiled. The presence
    of 'beginning_of_file' clause(s) in the expanded clauses, does not affect
    the compilation, they are simply ignored. If 'beginning_of_file' is
    expanded into a set of user defined clauses, those will be compiled
    before any clauses in the file.

    Example of clause expansion, renaming all clauses for the
    predicate abc/1 to bca/1 (slightly simplified, since normally you
    would have to match also against clauses with module prefixes):

    user:clause_expansion(abc(X), Ids, Exp) :-
	nonmember(abc_clause, Ids), !,
	clause_expansion_loop(cba(X), [abc_clause|Ids], Exp).
    user:clause_expansion((abc(X):-Body), Ids, Exp) :-
	nonmember(abc_clause, Ids), !,
	clause_expansion_loop((cba(X):-Body), [abc_clause|Ids], Exp).

    Example of a goal expansion, translating all calls to abc/1, with
    calls to cba/1. Note that (as said above) meta goal expansion always
    can be fooled, by creating the goal at runtime.

    user:meta_goal_expansion(abc(X), Ids, Exp) :-
	nonmember(abc_goal, Ids), !,
	goal_expansion_loop(cba(X), [abc_goal|Ids], Exp).

     Sometime it might seem unnecessary to call the expansion loop
     recursively, but it's never the less usually a good idea. This
     assures your expansion to be compatible with other expansions
     that possibly also have to transform some parts of the same
     clause or a goal.
*/

:- use_module(library(lists), [memberchk/2, member/2]).

:- multifile user:clause_expansion/3.
:- multifile user:meta_goal_expansion/3.
:- dynamic   user:clause_expansion/3.
:- dynamic   user:meta_goal_expansion/3.

clause_expansion_loop([], _, []) :-
	!.
clause_expansion_loop([Clause0|M0], Ids, [Clause|M]) :-
	!,
	clause_expansion_loop(Clause0, Ids, Clause),
	clause_expansion_loop(M0, Ids, M).
clause_expansion_loop(Clause0, Ids, Clause) :-
	( user:clause_expansion(Clause0, Ids, Clause) ->
	  true
	; dcg_expand(Clause0, Ids, Clause) ->
	  true
	; goal_expand_clause(Clause0, Ids, Clause)
	).

:- dynamic running_dcgs/0.

% An unfortunate necessity: the assert is used to avoid an obsolete
% term expansion pass.
%
dcg_expand(Clause0, Ids, Clause) :-
	\+member(dcg, Ids),
	assert(running_dcgs),
	expand_term(Clause0, Clause1),
	retractall(running_dcgs),
	Clause0 \== Clause1, % some expansion did happen
	clause_expansion_loop(Clause1, [dcg|Ids], Clause).

% We want to give expand_goal/3 an opportunity to expand goals both at
% the meta level, and at meta argument level. Note that meta arguments
% might be unbound (we do not allow the user to expand those).
%
goal_expand_clause((Head:-Body), Ids, (Head:-NewBody)) :-
	!,
	goal_expansion_loop(Body, Ids, NewBody).
goal_expand_clause(Fact, _, Fact).

goal_expansion_loop(Goal0, Ids, Goal) :-
	goal_expansion_loop(Goal0, Ids, Goal, _).

goal_expansion_loop(Goal, _, Goal, _) :-
	var(Goal), !.
goal_expansion_loop(Goal0, Ids, Goal, Flag) :-
	user:meta_goal_expansion(Goal0, Ids, Goal),
	!,
	Flag = expanded.
goal_expansion_loop(Goal0, Ids, Goal, Flag) :-
	split_meta(Goal0, LeftGoal0, RightGoal0),
	!,
	goal_expansion_loop(LeftGoal0, Ids, LeftGoal, Flag0),
	goal_expansion_loop(RightGoal0, Ids, RightGoal, Flag0),
	( nonvar(Flag0) -> % avoid copying unless necessary
	  Flag = Flag0,
	  build_meta(Goal0, LeftGoal, RightGoal, Goal)
	; Goal = Goal0
	).
goal_expansion_loop(Goal0, Ids, Goal, Flag) :-
	split_meta(Goal0, MetaGoal0),
	!,
	goal_expansion_loop(MetaGoal0, Ids, MetaGoal, Flag0),
	( nonvar(Flag0) -> % avoid copying unless necessary
	  Flag = Flag0,
	  build_meta(Goal0, MetaGoal, Goal)
	; Goal = Goal0
	).
goal_expansion_loop(Goal, _, Goal, _).

:- discontiguous split_meta/3.
:- discontiguous build_meta/4.

split_meta((A,B), A, B).
build_meta((_,_), A, B, (A,B)).
split_meta((A->B), A, B).
build_meta((_->_), A, B, (A->B)).
split_meta((A;B), A, B).
build_meta((_;_), A, B, (A;B)).

:- discontiguous split_meta/2.
:- discontiguous build_meta/3.

% Only meta arguments which are CALLED are considered here, so
% for instance the meta predicates predicate_property/2, assert/1
% or retract/1 are not included. If you want to expand arguments
% to such predicates, you have to catch them at the predicate
% level, not the argument level.
%
split_meta((\+ A), A).
build_meta((\+ _), A, (\+ A)).
split_meta(setof(_,B,_), B).
build_meta(setof(A,_,C), B, setof(A,B,C)).
split_meta(save_program(_,Meta), Meta).
build_meta(save_program(A,_), Meta, save_program(A,Meta)).
split_meta(setof(_,Meta,_), Meta).
build_meta(setof(B,_,A), Meta, setof(B,Meta,A)).
split_meta(^(_,Meta), Meta).
build_meta(^(A,_), Meta, ^(A,Meta)).
split_meta(bagof(_,Meta,_), Meta).
build_meta(bagof(B,_,A), Meta, bagof(B,Meta,A)).
split_meta(call(Meta), Meta).
build_meta(call(_), Meta, call(Meta)).
split_meta(findall(_,Meta,_), Meta).
build_meta(findall(B,_,A), Meta, findall(B,Meta,A)).

% Debugging help
%
:- dynamic xdebugging/0.
:- dynamic xtracing/0.

xdebug :-
	noxtrace,
	noxdebug,
	format('% The debugger will leap in expansion -- showing spypoints (debug)~n', []),	assert(xdebugging).

xtrace :-
	noxtrace,
	noxdebug,
	format('% The debugger will creep in expansion -- showing everything (trace)~n', []),
	assert(xtracing).

noxdebug :-
	retractall(xdebugging).

noxtrace :-
	retractall(xtracing).
	
initialize_debugger :-
	( xdebugging ->
	  call(debug) % avoid undef. warnings in runtime system
	; xtracing ->
	  call(trace) % avoid undef. warnings in runtime system
	; true
	).

% Insert the term expansion

:- multifile user:term_expansion/2.

user:term_expansion(Clause0, Clause) :-
	( running_dcgs ->
	  retractall(running_dcgs),
	  fail
	; prolog_load_context(module, expansion) -> % do not expand this module
	  fail
	; special_term_expansion(Clause0, Clause)
	).

:- multifile user:message_hook/3.
:- dynamic   user:message_hook/3.

:- dynamic new_file/0.

user:message_hook(loading(_,_), _, _) :- % SP
	assert(new_file),
	fail.

special_term_expansion(Clause0, Clause) :-
	( new_file ->
	  retractall(new_file),
	  Clause1 = [beginning_of_file,Clause0]
	; Clause1 = Clause0
	),
	initialize_debugger,
	clause_expansion_loop(Clause1, [], Clause2),
	simplify_and_flatten(Clause2, Clause3, []),
	( Clause3 = [Clause],
	  Clause = (:- module(_,_))
	  % module/2 test needed because of singleton warnings, see bug 5079
	->
	  % needed because module headers cannot be wrapped in a list
	  % see bug 5069
	  true 
	; Clause3 = [Clause],
	  Clause = (:- module(_,_,_)) -> true % SP
	; Clause3 == [Clause0] ->
	  Clause = Clause0
	; Clause = Clause3
	).

simplify_and_flatten([], Clause, Clause) :-
	!.
simplify_and_flatten(beginning_of_file, Clause, Clause) :-
	!.
simplify_and_flatten([Clause0|More], Clause1, Clause) :-
	!,
	simplify_and_flatten(Clause0, Clause1, Clause2),
	simplify_and_flatten(More, Clause2, Clause).
simplify_and_flatten(Clause0, [Clause0|Clause], Clause).

