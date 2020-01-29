/* Copyright (C) 1995, Swedish Institute of Computer Science. */

:- module(context, []).

% This module does not export any predicates. It defines clauses of
% user:goal_expansion/3 for generally useful expansions.

:- use_module(library(lists), [nth/3]).

:- multifile
	user:term_expansion/2,
	user:goal_expansion/3.
:- dynamic
	user:term_expansion/2,
	user:goal_expansion/3.

%%-----------------------------------------------------------------------
% Handling of context variables. Multiple lookups by key are replaced
% by a single unification. E.g:
%        ctxt(Ctxt,[key1-Value1,key3-Value3])
% is transformed into:
%        Ctxt = ctx(Value1,_,Value3,_,...)

% The item keys are defined in a list given as the directive
% ctxt_items/1 in the module being expanded. The directive is
% tranfered by means of term_expansion.

user:goal_expansion(ctxt(Ctxt,KeyValues), Module, Goal) :- !,
	clause(Module:ctxt_items(Keys), _),
	length(Keys, Len),
	functor(Ctxt0, ctxt, Len),
	(   find_items(KeyValues, Keys, Ctxt0) ->
	    Goal = (Ctxt=Ctxt0)
	;   print_message(warning, failed(Module:ctxt(Ctxt,KeyValues))), fail
	).

user:term_expansion((:-ctxt_items(Keys)), []) :- !,
	prolog_load_context(module, Module),
	retractall(Module:ctxt_items(_)),
	assertz(Module:ctxt_items(Keys)).

find_items([], _, _).
find_items([Key-Value|KeyValues], Keys, Ctxt) :-
	nth(Nr, Keys, Key),
	arg(Nr, Ctxt, Value),
	find_items(KeyValues, Keys, Ctxt).
