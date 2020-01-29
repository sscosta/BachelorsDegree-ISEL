/* Copyright(C) 1997, Swedish Institute of Computer Science */

:- attribute fd_attribute/3.
%% index, domain mutable, suspensions mutable.

:- meta_predicate
	fd_global(:, ?, ?).

is_fd_variable(X) :-
	get_atts(X, fd_attribute(_,_,_)).

get_fd_domain(X, Dom) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM).

get_fd_suspensions(X, Lists) :-
	get_atts(X, fd_attribute(_,_,ListsM)),
	get_mutable(Lists, ListsM).

get_mutable_rec(Val, v(_,_,Mut,_)) :- !,
	get_mutable(Mid, Mut),
	get_mutable_rec(Val, Mid).
get_mutable_rec(Val, Val).

solve(Constraint, DefPtr) :-
	check_arguments(Constraint, Attv),
	'$fd_begin',
	'$fd_get_indexicals'(DefPtr, 1, Constraint, _, _, Attv, Ixs),
	'$fd_enqueue_first'(Ixs),
        '$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

check_arguments(Constraint, Attv) :-
	'$fd_check_arguments'(Constraint, Attv), !.
check_arguments(Constraint, _) :-
	functor(Constraint, _, A),
	check_arguments_error(A, Constraint).

check_arguments_error(0, _) :- !.
check_arguments_error(A, Constraint) :-
	arg(A, Constraint, Arg),
	(   var(Arg)
	;   integer(Arg),
	    \+prolog:'$large_data'(0, Arg, _)
	), !,
	B is A-1,
	check_arguments_error(B, Constraint).
check_arguments_error(A, Constraint) :-
	arg(A, Constraint, Arg),
	fd_argument_error(Constraint, A, Arg).

fd_argument_error(Constraint, A, Arg) :-
	'$fd_minint_maxint'(Minint, Maxint),
	fd_illarg(domain(integer,between(Minint,Maxint)), Constraint, A, Arg).

% avoid throwing domain variables
fd_illarg(Error, Constraint, ArgNo) :-
	prolog:'$term_variables'(Constraint, Vars),
	purify_vars(Vars),
	prolog:illarg(Error, Constraint, ArgNo).

fd_illarg(Error, Constraint, ArgNo, Culprit) :-
	prolog:'$term_variables'(Constraint-Culprit, Vars),
	purify_vars(Vars),
	prolog:illarg(Error, Constraint, ArgNo, Culprit).

/* now unfolded
evaluate :-
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
*/
%% evaluate(RC, Global)
%% RC = -1 -- failure
%% RC = 0  -- done
%% RC = 1  -- indexicals to be run
%% RC = 2  -- Global to be run
evaluate(0, _).
evaluate(1, _) :-
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
evaluate(2, Global) :-
	dispatch_prune_and_enqueue(Global, []).

% FDBG puts advice on this!
prune_and_propagate(Pruned, Set) :-
	'$fd_in_set'(Pruned, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

silent_prune_and_propagate(Pruned, Set) :-
	'$fd_in_set'(Pruned, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
propagate_interval(Pruned, Min, Max) :-
	'$fd_in_interval'(Pruned, Min, Max, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

propagate_value(ListsM) :-
	'$fd_enqueue_all'(ListsM),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

%%% New global propagator API.

fd_global_tell(Var, FDSet) -->
	{'$fd_tell'(Var, FDSet, RC)},
	fd_global_told(RC, Var).

fd_global_told(0, _) --> [].
fd_global_told(1, Var) --> [dom(Var)].
fd_global_told(3, Var) --> [min(Var)].
fd_global_told(5, Var) --> [max(Var)].
fd_global_told(7, Var) --> [minmax(Var)].

/* Used in 3.9.beta and earlier.

verify_attributes(Var, Other, Goals) :-
	'$get_attributes'(Var, Atts, 1),
	verify_attributes1(Other, Atts, Goals).

verify_attributes1(Other, Atts, Goals) :-
	integer(Other),
	(   prolog:'$large_data'(0, Other, _) ->
	    fd_argument_error(_=Other, 2, Other)
	;   true
	),
	Atts = v(_,_,DomM,ListsM),
	get_mutable(Lists, ListsM),
	'$fd_set_singleton'(Other, DomM), % update_mutable(Other..Other, DomM),
	(   arg(2, Lists, 0) -> Goals = []
	;   Goals = [propagate_value(ListsM)]
	).
verify_attributes1(Other, _Atts, _Goals) :-
	var(Other),
	'$get_attributes'(Other, _, 1), !,
	prolog:illarg(representation('unified domain variable'), _=_, 0).
verify_attributes1(Other, Atts, Goals) :-
	var(Other),
	'$get_attributes'(Other, _, 1), !,
	'$put_attributes'(Fresh, Atts),
	Goals = [clpfd:'x=y'(Fresh,Other)].
verify_attributes1(Other, Atts, []) :-
	var(Other),
	'$put_attributes'(Other, Atts).
*/

/* This caused problems as the C code assumed fixed offsets between
   domain mutables and v/4 structures.

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, fd_attribute(_,DomM,ListsM)), % now guaranteed to succeed
	verify_attributes(Other, DomM, ListsM, Goals).

verify_attributes(Other, DomM, ListsM, Goals) :-
	integer(Other),
	get_mutable(Lists, ListsM),
	'$fd_set_singleton'(Other, DomM), % update_mutable(Other..Other, DomM),
	(   arg(2, Lists, 0) -> Goals = []
	;   Goals = [propagate_value(ListsM)]
	).
verify_attributes(Other, DomM, ListsM, Goals) :-
	var(Other),
	get_atts(Other, fd_attribute(_,_,_)), !,
	put_atts(Fresh, fd_attribute(0,DomM,ListsM)),
	Goals = [clpfd:'x=y'(Fresh,Other)].
verify_attributes(Other, DomM, ListsM, []) :-
	var(Other),
	put_atts(Other, fd_attribute(0,DomM,ListsM)).
*/

/* This is a more ambitious attempt to unify domain variables.
   It involves merging the suspension lists and intersecting the domains.
*/
% FDBG puts advice on this!
verify_attributes(Var, Other, Goals) :-
	get_atts(Var, fd_attribute(_,DomM,ListsM)), % now guaranteed to succeed
	check_arguments_error(2, _=Other),
	verify_attributes(Other, DomM, ListsM, Goals).

verify_attributes(Other, DomM, ListsM, Goals) :-
	integer(Other),
	get_mutable(Lists, ListsM),
	'$fd_set_singleton'(Other, DomM), % update_mutable(Other..Other, DomM),
	(   arg(2, Lists, 0) -> Goals = []
	;   Goals = [propagate_value(ListsM)]
	).
verify_attributes(Other, DomM, ListsM, Goals) :-
	var(Other),
	'$fd_arg_attribute'(Other, 0, OtherAttr),
	get_atts(Other, fd_attribute(_,OtherDomM,OtherListsM)),
	get_mutable(Dom, DomM),
	get_mutable(Lists, ListsM),
	get_mutable(OtherDom, OtherDomM),
	get_mutable(OtherLists, OtherListsM),
	update_mutable(OtherAttr, DomM),
	update_mutable([], ListsM),
	merge_lists(Lists, OtherLists, NewLists),
	update_mutable(NewLists, OtherListsM),
	arg(1, Dom, Set),
	arg(1, OtherDom, OtherSet),
	(   Set==OtherSet -> Goals = []
	;   '$fd_dom_union'(Set, OtherSet, Union),
	    '$fd_dom_intersection'(Set, OtherSet, Intersection),
	    Intersection \== [],
	    fdset_min(Union, Min),
	    fdset_max(Union, Max),
	    fdset_size(Union, Size),
	    dom_term(UDom, Union, Min, Max, Size),
	    update_mutable(UDom, OtherDomM),
	    Goals = [silent_prune_and_propagate(Other,Intersection)]
	).

dom_term(Dom, Set, Min, Max, Size) :-
	% prevent deref. chains
	Dom =.. [dom,Set,Min,Max,Size].

merge_lists(L1, L2, '$fdlists'(K,VW,C1s,C2s,C4s,C24s,C8s)) :-
	L1 = '$fdlists'(_, V,A1,A2,A4,A24,A8),
	L2 = '$fdlists'(_, W,B1,B2,B4,B24,B8),
	VW is V\/W,
	fdlists_globals(L1, G1),
	fdlists_globals(L2, G2),
	ord_intersection(G1, G2, G12),
	mark_coref(G12),
	append(A1, B1, C1),
	sort(C1, C1s),
	append(A2, B2, C2),
	sort(C2, C2s),
	append(A4, B4, C4),
	sort(C4, C4s),
	append(A24, B24, C24),
	sort(C24, C24s),
	append(A8, B8, C8),
	sort(C8, C8s),
	length(C1s, N1),
	length(C2s, N2),
	length(C4s, N4),
	length(C24s, N24),
	length(C8s, N8),
	K is N1+N2+N4+N24+N8.

fdlists_globals('$fdlists'(_,_,A1,A2,A4,A24,A8), Set) :-
	fdlists_globals(A1, S0, S1),
	fdlists_globals(A2, S1, S2),
	fdlists_globals(A4, S2, S3),
	fdlists_globals(A24, S3, S4),
	fdlists_globals(A8, S4, []),
	sort(S0, Set).

fdlists_globals([]) --> [].
fdlists_globals([G|Gs]) --> fdlists_global(G), fdlists_globals(Gs).

fdlists_global(_-G) --> !,
	fdlists_global(G).
fdlists_global(G) --> {G = global(_,_,_,_,_)}, !, [G].
fdlists_global(_) --> [].

mark_coref([]).
mark_coref([global(_,_,StatusM,_,_)|Gs]) :-
	get_mutable(Status, StatusM),
	Status1 is Status+4,
	update_mutable(Status1, StatusM),
	mark_coref(Gs).




%%% Support for project_attributes and B&B search.

purify_vars([]).
purify_vars([X|Xs]) :-
	var(X), !,
	put_atts(X, -fd_attribute(_,_,_)),
	purify_vars(Xs).
purify_vars([_|Xs]) :-
	purify_vars(Xs).

%%% Support for global (specialized) constraints.

% Susp is a list of F(Var) terms, each representing that the constraint should be
% suspended on the variable Var.  F denotes the circumstances under which the constraint
% should be resumed:
% dom - resume when dom(Var) changes
% min - resume when min(Var) changes
% max - resume when max(Var) changes
% minmax - resume when min(Var) or max(Var) changes
% val - resume when Var becomes nonvar
fd_global(ModConstraint, State, Susp) :-
	prolog:get_module(ModConstraint, Constraint, Module),
	fd_global6(Constraint, State, Susp, _, Module, 0).

fd_global6(Constraint, State, Susp, Global, Module, Base) :-
	(   \+'$fd_coref'(Susp) -> Status is Base\/1
	;   Status is Base\/5
	),
	'$fd_begin',
	Global = global(StateM,Constraint,StatusM,_Ent,Module),
	create_mutable(Status, StatusM),
	create_mutable(State, StateM),
	dispatch_prune_and_enqueue(Global, Susp).

dispatch_prune_and_enqueue(Global, Susp) :-
	Global = global(StateM,Constraint,_StatusM,_Ent,_Module),
	get_mutable(State, StateM),
	dispatch_global_fast(Constraint, State, NewState, Actions),
	update_mutable(NewState, StateM),
	'$fd_global_told'(Susp, Global),
	'$fd_prune_and_enqueue'(Actions, Global),
	'$fd_evaluate_indexical'(RC, Global1),
	evaluate(RC, Global1).

% clpfd:dispatch_global(+Constraint, +State0, -State, -Actions)
% calls a user-defined constraint solver for a particular kind of constraint.
% Constraint is the original constraint;
% State0 is a term representing aux. info about this constraint at the time of its
% latest invocation.
% State represents updated aux. info;
% Actions is a list of terms of the following form:
%	exit - the constraint has been found entailed
%	fail - the constraint has been found disentailed
%	call(G) - call the Prolog goal G
%	X = R   - call(X in R)
%	X in R   - call(X in R)
%	X in_set R  - call(X in_set R)

% FDBG puts advice on this!
dispatch_global_fast(in_set_iff(_,_,_), State, NewState, Actions) :- !,
	'$fd_in_set_iff'(State, NewState, Actions).
%% fdbg support
dispatch_global_fast(eq_iff(_,_,_), State, NewState, Actions) :- !,
	'$fd_eq_iff'(State, NewState, Actions).
dispatch_global_fast(le_iff(_,_,_), State, NewState, Actions) :- !,
	'$fd_le_iff'(State, NewState, Actions).
dispatch_global_fast(oneof(_,_,_), State, NewState, Actions) :- !,
	'$fd_oneof'(State, NewState, Actions).
dispatch_global_fast(abs(_,_), State, NewState, Actions) :- !,
	'$fd_abs'(State, NewState, Actions).
%% end fdbg support
dispatch_global_fast('x*x=y'(_,_), State, NewState, Actions) :- !,
	'$fd_square'(State, NewState, Actions).
dispatch_global_fast('x*y=z'(_,_,_), State, NewState, Actions) :- !,
	'$fd_product'(State, NewState, Actions).
dispatch_global_fast('x/y=z'(_,_,_), State, NewState, Actions) :- !,
	'$fd_quotient'(State, NewState, Actions).
dispatch_global_fast('x mod y=z'(_,_,_), State, NewState, Actions) :- !,
	'$fd_modulo'(State, NewState, Actions).
% dispatch_global_fast(count(I,_,Rel,S), State, NewState, Actions) :- !,
% 	'count solver'(I, State, NewState, Rel, S, Actions).
dispatch_global_fast(scalar_product(_,_,_,_), State, NewState, Actions) :- !,
	'$fd_linear'(State, NewState, Actions).
dispatch_global_fast(all_different(_), State, NewState, Actions) :- !,
	'$fd_all_different'(State, NewState, Actions).
dispatch_global_fast(knapsack(_,_,_), State, NewState, Actions) :- !,
	'$fd_knapsack'(State, NewState, Actions).
dispatch_global_fast(all_distinct(_), State, NewState, Actions) :- !,
	'$fd_all_distinct'(State, NewState, Actions).
dispatch_global_fast(circuit(_,_), State, NewState, Actions) :- !,
	'$fd_circuit'(State, NewState, Actions).
dispatch_global_fast(element(_,_,_), State, NewState, Actions) :- !,
	'$fd_element'(State, NewState, Actions).
% dispatch_global_fast(relation(_,_,_), State, NewState, Actions) :- !,
% 	'$fd_relation'(State, NewState, Actions).
dispatch_global_fast(assignment(_,_), State, NewState, Actions) :- !,
	'$fd_assignment'(State, NewState, Actions).
dispatch_global_fast(bool(_,_,_,_), State, NewState, Actions) :- !,
	'$fd_bool'(State, NewState, Actions).
dispatch_global_fast(task_order(_,_,_,_), State0, State, Actions) :- !,
	'$fd_cumulative'(State0, State, Actions).
dispatch_global_fast(disjoint1(_,_), State, NewState, Actions) :- !,
	'$fd_disjoint1'(State, NewState, Actions).
dispatch_global_fast(disjoint2(_,_), State, NewState, Actions) :- !,
	'$fd_disjoint2'(State, NewState, Actions).
dispatch_global_fast(case(_,_,_,_), State, NewState, Actions) :- !,
	'$fd_case'(State, NewState, Actions).
dispatch_global_fast(cumulatives(_,_,_), State, NewState, Actions) :- !,
	'$fd_cumulatives'(State, NewState, Actions).
dispatch_global_fast(global_cardinality(_,_), State, NewState, Actions) :- !,
	'$fd_gcc'(State, NewState, Actions).
dispatch_global_fast(Global, State, NewState, Actions) :-
	dispatch_global(Global, State, NewState, Actions).

:- multifile
	dispatch_global/4.

:- dynamic
	dispatch_global/4.

/* fd_closure/2 */

% Given a list Vars of domain variables, Closure is the set
% of variables (including Vars) that can be transitively reached
% via constraints.
fd_closure(Vars, Closure) :-
	sort(Vars, Open),
	fd_closure(Open, Open, Closure).

/*** worst case O(N^2) version

fd_closure([], Closure, Closure).
fd_closure([X|Open], Closed, Closure) :-
	fd_neighbors(X, Neighs),
	ord_union(Closed, Neighs, Closed1, New),
	ord_union(Open, New, Open1),
	fd_closure(Open1, Closed1, Closure).

fd_neighbors(X, Neighs) :-
	var(X),
	clpfd:get_atts(X, fd_attribute(_,_,ListM)), !,
	get_mutable(Lists, ListM),
	Lists = '$fdlists'(_,_,L1,L2,L3,L4,L5),
	fd_neighbors(L1, S0, S1),
	fd_neighbors(L2, S1, S2),
	fd_neighbors(L3, S2, S3),
	fd_neighbors(L4, S3, S4),
	fd_neighbors(L5, S4, []),
	sort(S0, Neighs).
fd_neighbors(_, []).

fd_neighbors([]) --> [].
fd_neighbors([Item|L]) --> fd_neighbors(Item, L).

fd_neighbors(_-Item, L) --> !, fd_neighbors(Item, L).
fd_neighbors(iff(Item,_,_,_), L) --> !, fd_neighbors(Item, L).
fd_neighbors(ix(_,C,_,Ent,B,_,_), L) --> {var(Ent)}, !,
	{prolog:'$term_variables'(C, Vars)},
	list(Vars),
	(   {clpfd:'$get_attributes'(B, _, 1)} -> [B]
	;   []
	),
	fd_neighbors(L).
fd_neighbors(global(_,C,_,Ent,_), L) --> {var(Ent)}, !,
	{prolog:'$term_variables'(C, Vars)},
	list(Vars),
	fd_neighbors(L).
fd_neighbors(_, L) -->
	fd_neighbors(L).

list([]) --> [].
list([X|Xs]) --> [X], list(Xs).

***/

fd_closure([], Closure, Closure).
fd_closure([X|Open], Closed, Closure) :-
	fd_list_constraints([X|Open], Cs, []),
	prolog:term_variables(Cs, Neighs),
	ord_union(Closed, Neighs, Closed1, Open1),
	fd_closure(Open1, Closed1, Closure).

fd_neighbors(X, Neighs) :-
	fd_list_constraints([X], Cs, []),
	prolog:term_variables(Cs, Neighs).

fd_list_constraints([]) --> [].
fd_list_constraints([X|Xs]) -->
	{var(X),
	 get_atts(X, fd_attribute(_,_,ListM)), !,
	 get_mutable(Lists, ListM), % avoid '$fdlists' in source code
	 arg(3, Lists, L3),
	 arg(4, Lists, L4),
	 arg(5, Lists, L5),
	 arg(6, Lists, L6),
	 arg(7, Lists, L7)},
	fd_neighbors(L3),
	fd_neighbors(L4),
	fd_neighbors(L5),
	fd_neighbors(L6),
	fd_neighbors(L7),
	fd_list_constraints(Xs).
fd_list_constraints([_|Xs]) -->
	fd_list_constraints(Xs).

%%% similar to suspensions/3, but we cannot assume that we
%%% have all zero-one variables already

fd_neighbors([]) --> [].
fd_neighbors([Item|L]) --> 
	fd_neighbors(Item, L).

fd_neighbors(_-Item, L) --> !, 
	fd_neighbors(Item, L).
fd_neighbors(iff(Item,_,_,_), L) --> !, 
	fd_neighbors(Item, L).
fd_neighbors(ix(_,C,_,Ent,B,_,_), L) --> {var(Ent)}, !,
	[B-C],
	fd_neighbors(L).
fd_neighbors(global(_,C,_,Ent,_), L) --> {var(Ent)}, !,
	[C],
	fd_neighbors(L).
fd_neighbors(_, L) -->
	fd_neighbors(L).

/* fd_copy_term/3 */

% Given a term Term containing domain variables, Template is a
% copy of the same term with all variables renamed to new variables
% such that executing Body will post constraints equivalent to those that
% Term is attached to.
% 
% For example:
% | ?- X in 0..1, Y in 10..11, X+5 #=< Y, clpfd:fd_copy_term(f(X,Y), Template, Body). 
% 
% Body = _A in_set[[0|1]],_B in_set[[10|11]],clpfd:'t>=u+c'(_B,_A,5),
% Template = f(_A,_B),
% X in 0..1,
% Y in 10..11 ? 
fd_copy_term(Term, Template, Body) :-
	prolog:'$term_variables'(Term, Vars0),
	fd_closure(Vars0, Vars0, Vars),
	all_suspensions(Vars, Goals, Goals2, Goals0, []),
	sort(Goals0, Goals2),
	andify(Goals, Body0),
	findall(Term-Body0, purify_vars(Vars), [Template-Body]).

all_suspensions([], S, S) --> [].
all_suspensions([X|Xs], S0, S) -->
	{get_atts(X, fd_attribute(_,DomM,ListsM))}, !,
	{get_mutable(Dom, DomM)},
	{get_mutable(Lists, ListsM)},
	{Dom = dom(Set,Min,Max,_)},
	{   Min==inf, Max==sup -> S0 = S1
	;   S0 = [X in_set Set | S1]
	},
	fdlists_suspensions(Lists),
	all_suspensions(Xs, S1, S).
all_suspensions([_|Xs], S0, S) -->
	all_suspensions(Xs, S0, S).

