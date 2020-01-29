/* Copyright(C) 1997, Swedish Institute of Computer Science */

%%% Target library for arithmetic expressions:
%%%	'x+y=t'/3
%%%	'ax=t'/3
%%%	'ax+y=t'/4
%%%	'ax+by=t'/5
%%%	't=c'/2	
%%%	't=<c'/2
%%%	't\\=c'/2
%%%	't>=c'/2
%%%	't=u+c'/3
%%%	't=<u+c'/3
%%%	't\\=u+c'/3
%%%	't>=u+c'/3
%%%	't+u=c'/3
%%%	't+u=<c'/3
%%%	't+u\\=c'/3
%%%	't+u>=c'/3
%%%	't=x+y+c'/4
%%%	'x+y=u+c'/4
%%%	'x+y+z=c'/4

/***
't=c'(T,C) +:
	T in {C}.

't=<c'(T,C) +:
	T in inf..C.

't>=c'(T,C) +:
	T in C..sup.

't\\=c'(T,C) +:
	T in \{C}.

Type checking done at compile time.
***/

't=c'(T,C) :-
	check_arguments_error(1, T#=C),
	propagate_interval(T, C, C).

't=<c'(T,C) :- 
	check_arguments_error(1, T#=<C),
	propagate_interval(T, inf, C).

't>=c'(T,C) :- 
	check_arguments_error(1, T#>=C),
	propagate_interval(T, C, sup).

't\\=c'(T,C) :-
	check_arguments_error(1, T#\=C),
	'$fd_range'(C, C, R, 1),
	'$fd_dom_complement'(R, R1),
	prune_and_propagate(T, R1).

%%% indexicals for arithmetic

'ax=t'(A,X,T) +:
	X in min(T) /> A..max(T) /< A,
	T in min(X) *  A..max(X) *  A !.

'x+y=t'(X,Y,T) +:
	X in min(T) - max(Y)..max(T) - min(Y) !,
	Y in min(T) - max(X)..max(T) - min(X) !,
	T in min(X) + min(Y)..max(X) + max(Y) !.

% Now domain consistent!
't+u=c'(T,U,C) +:
	T in C - dom(U) !,
	U in C - dom(T) !.

% Now domain consistent!
'x+c=y'(X,C,Y) +:
	X in dom(Y) - C !,
	Y in dom(X) + C !.

% Now domain consistent!
't=u+c'(T,U,C) +:
	T in dom(U) + C !,
	U in dom(T) - C !.

't=<u+c'(T,U,C) +:
	T in inf..max(U)+C,
	U in min(T) - C..sup.

't\\=u+c'(T,U,C) +:
	T in \{U + C},
	U in \{T - C}.

't>=u+c'(T,U,C) +:
	T in min(U) + C..sup,
	U in inf..max(T) - C.

'ax+c=t'(A,X,C,Y) +:
	X in (min(Y) - C) /> A..(max(Y) - C) /< A,
	Y in  min(X)*A + C   .. max(X)*A + C !.

'ax+y=t'(A,X,Y,Z) +:
	X in (min(Z) - max(Y)) /> A..(max(Z) - min(Y)) /< A,
	Y in  min(Z) - max(X)*A	  .. max(Z) - min(X)*A !,
	Z in  min(X)*A + min(Y)	  .. max(X)*A + max(Y) !.

't+u=<c'(T,U,C) +:
	T in inf..C - min(U),
	U in inf..C - min(T).

't+u\\=c'(T,U,C) +:
	T in \{C - U},
	U in \{C - T}.

't+u>=c'(T,U,C) +:
	T in C - max(U)..sup,
	U in C - max(T)..sup.

% obsolete, backward compatibility?
'ax+by=t'(A,X,B,Y,Z) +:
	X in  (min(Z) - max(Y)*B) /> A.. (max(Z) - min(Y)*B) /< A,
	Y in  (min(Z) - max(X)*A) /> B.. (max(Z) - min(X)*A) /< B,
	Z in min(X)*A + min(Y)*B     ..max(X)*A + max(Y)*B !.

'x+y=u+c'(X,Y,U,C) +:
	X in min(U) - max(Y) + C..max(U) - min(Y) + C !,
	Y in min(U) - max(X) + C..max(U) - min(X) + C !,
	U in min(X) + min(Y) - C..max(X) + max(Y) - C !.

't=x+y+c'(T,X,Y,C) :-
	'x+y+c=z'(X,Y,C,T).

'x+y+c=z'(X,Y,C,Z) +:
	X in min(Z) - max(Y) - C..max(Z) - min(Y) - C !,
	Y in min(Z) - max(X) - C..max(Z) - min(X) - C !,
	Z in min(X) + min(Y) + C..max(X) + max(Y) + C !.

'ax+y+c=z'(A,X,Y,C,Z) +:
	X in  (min(Z) -	  max(Y) - C)/>A.. (max(Z) -   min(Y) - C)/<A,
	Y in   min(Z) - max(X)*A - C	..  max(Z) - min(X)*A - C !,
	Z in min(X)*A +	  min(Y) + C	..max(X)*A +   max(Y) + C !.

'x+y+z=t'(X,Y,Z,T) +:
	X in min(T) - max(Y) - max(Z)..max(T) - min(Y) - min(Z) !,
	Y in min(T) - max(X) - max(Z)..max(T) - min(X) - min(Z) !,
	Z in min(T) - max(X) - max(Y)..max(T) - min(X) - min(Y) !,
	T in min(X) + min(Y) + min(Z)..max(X) + max(Y) + max(Z) !.

'x+y+z=c'(X,Y,Z,C) +:
	X in C - max(Y) - max(Z)..C - min(Y) - min(Z) !,
	Y in C - max(X) - max(Z)..C - min(X) - min(Z) !,
	Z in C - max(X) - max(Y)..C - min(X) - min(Y) !.

'ax+y+z=t'(A,X,Y,Z,T) +:
	X in  (min(T) -	  max(Y) - max(Z)) /> A..
	      (max(T) -	  min(Y) - min(Z)) /< A,

	Y in   min(T) - max(X)*A - max(Z).. 
	       max(T) - min(X)*A - min(Z) !,

	Z in   min(T) - max(X)*A - max(Y)..
	       max(T) - min(X)*A - min(Y) !,

	T in min(X)*A +	  min(Y) + min(Z)..
	     max(X)*A +	  max(Y) + max(Z) !.


%%% Utilities for globals.

arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 0, Attr), !.
arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

finite_arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 1, Attr), !.
finite_arg_attribute(X, _, Goal, ArgNo) :-
	var(X), !,
	raise_exception(instantiation_error(Goal,ArgNo)).
finite_arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

must_be_dvar_list(List, _, _) :-
	'$fd_dvar_list'(List, 0), !.
must_be_dvar_list(List, Goal, ArgNo) :-
	not_dvar_list(List, Goal, ArgNo).

not_dvar_list(List, Goal, ArgNo) :- var(List), !,
	fd_illarg(var, Goal, ArgNo, List).
not_dvar_list([Arg|List], Goal, ArgNo) :- !,
	arg_attribute(Arg, _, Goal, ArgNo),
	not_dvar_list(List, Goal, ArgNo).
not_dvar_list(List, Goal, ArgNo) :-
	fd_illarg(domain(term,list), Goal, ArgNo, List).

must_be_list_of_finite_dvar(List, _, _) :-
	'$fd_dvar_list'(List, 1), !.
must_be_list_of_finite_dvar(List, Goal, ArgNo) :-
	not_list_of_finite_dvar(List, Goal, ArgNo).

not_list_of_finite_dvar(L, Goal, ArgNo) :- var(L), !,
	fd_illarg(var, Goal, ArgNo, L).
not_list_of_finite_dvar([X|L], Goal, ArgNo) :- !,
	finite_arg_attribute(X, _, Goal, ArgNo),
	not_list_of_finite_dvar(L, Goal, ArgNo).
not_list_of_finite_dvar(L, Goal, ArgNo) :-
	fd_illarg(domain(term,list), Goal, ArgNo, L).

/****************************************************************/
/* new all_different/[1,2], all_distinct/[1,2]                  */
/****************************************************************/

all_different(Xs) :-
	all_different(Xs, [], opt(value,false), all_different(Xs)).

all_different(Xs, Opt) :-
	all_different(Xs, Opt, opt(value,false), all_different(Xs,Opt)).

all_distinct(Xs) :-
	all_different(Xs, [], opt(domain,true), all_distinct(Xs)).

all_distinct(Xs, Opt) :-
	all_different(Xs, Opt, opt(domain,true), all_distinct(Xs,Opt)).

all_different(Xs, Options, Opt0, Goal) :-
	all_diff_options(Options, Opt0, Opt, Goal, 2),
	must_be_list_of_finite_dvar(Xs, Goal, 1),
	Opt = opt(On,Complete),
	(   Complete==true
        ->  all_dist_init(Xs, Vec, On, Goal, 1, Susp, []),
	    fd_global(all_distinct(Xs), f(Vec,0,0,_Handle,0), Susp)
	;   all_diff_init(Xs, Vec, On, Goal, 1, Susp, []),
	    fd_global6(all_different(Xs), state(Vec,0,_Handle,0), Susp,
		       _, clpfd, 4) % propagates to self
	).

all_diff_init([], [], _On, _Goal, _ArgNo) --> [].
all_diff_init([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{arg_attribute(X, M, Goal, ArgNo)},
	all_diff_init(Xs, Vec, On, Goal, ArgNo).

all_dist_init([], [], _On, _Goal, _ArgNo) --> [].
all_dist_init([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{arg_attribute(X, M, Goal, ArgNo)},
	all_dist_init(Xs, Vec, On, Goal, ArgNo).

on(_, X) --> {nonvar(X)}, !.
on(domain, X) --> [dom(X)].
on(range, X) --> [minmax(X)].
on(value, X) --> [val(X)].

all_diff_options(L, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
all_diff_options([], Opt, Opt, _, _) :- !.
all_diff_options([X|L], Opt0, Opt, Goal, ArgNo) :- !,
	(   all_diff_option(X, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,all_diff_option), Goal, ArgNo, X)
        ),
	all_diff_options(L, Opt1, Opt, Goal, ArgNo).
all_diff_options(L, _, _, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Goal, ArgNo, L).

all_diff_option(on(On), opt(_,Complete), opt(On,Complete)) :-
	on(On, _, _, _).
all_diff_option(complete(Bool), opt(On,_), opt(On,Bool)) :-
	bool_option(Bool, _).


/****************************************************************/
/* element/3							*/
/****************************************************************/
% 
% A generalized version of element(X,L,Y):
% nonground L -> constructive disjunction style
% Propagator in C
% interval consistent in Y and L, domain consistent in X

element(X, L, Y) :-
	Goal = element(X,L,Y),
	must_be_dvar_list(L, Goal, 2),
	length(L, N),
	X in 1..N,
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 3),
	on_minmax(L, Susp, []),
	fd_global(Goal, elt(X-XM,L,Y-YM,0,0,_Handle,0), [dom(X),minmax(Y)|Susp]).

/****************************************************************/
/* circuit/[1,2]						*/
/****************************************************************/

circuit(Xs) :-
	circuit(Xs, _, circuit(Xs)).

circuit(Xs, Ys) :-
	circuit(Xs, Ys, circuit(Xs,Ys)).

circuit(Xs, Ys, Goal) :-
	must_be_dvar_list(Xs, Goal, 1),
	length(Xs, N),
	length(Ys, N),
	must_be_dvar_list(Ys, Goal, 2),
	(   N=:=1 -> Xs = [1], Ys = [1]
	;   '$fd_range'(1, N, Set, 1),
	    domain(Xs, Set),
	    domain(Ys, Set),
	    not_self(Xs, 0),
	    not_self(Ys, 0),
	    assignment_state(Xs, Ys, 0, XVec, YVec, domain, Goal, Susp, []),
	    fd_global(circuit(Xs,Ys), f(XVec,YVec,0,0,_Handle,0), Susp)
	).

not_self([], _).
not_self([Y|Ys], I) :-
	J is I+1,
	't\\=c'(Y, J),
	not_self(Ys, J).

/****************************************************************/
/* relation/3							*/
/****************************************************************/

/***
% 
% AC3-based implementation of relation/3
% 
relation(X, L0, Y) :-
	keysort(L0, L),				% must be keysorted
	map2list(L, L1, []),
	length(L1, N),
	Goal = relation(X,L0,Y),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 3),
	fd_global(relation(X,L0,Y), f(N,X-XM,L1,Y-YM), [dom(X),dom(Y)]).

map2list([]) --> [].
map2list([X-Expr|Map]) -->
	{set_expression(Expr, Set),
	 fdset_to_list(Set, List, [])},
	tag_list(List, X),
	map2list(Map).

tag_list([], _) --> [].
tag_list([X|Xs], K) --> [K-X], tag_list(Xs, K).
***/

%
% piggy-back on case/3
% 
relation(X, L0, Y) :-
	keysort(L0, L1),
	empty_assoc(A0),
	values_to_ids(L1, L2, A0, A1),
	assoc_to_list(A1, Alist),
	relation_root_node(L2, A, Node),
	relation_nodes(Alist, 0, B, Nodes),
	case(A-B, [X-Y], [Node|Nodes]).

values_to_ids([], [], A, A).
values_to_ids([Xi-Ys|L1], [Xi-ID|L2], A0, A) :-
	range_to_fdset(Ys, Zs),
	(   get_assoc(Zs, A0, ID) -> A1 = A0
	;   put_assoc(Zs, A0, ID, A1)
	),
	values_to_ids(L1, L2, A1, A).

relation_root_node(L2, A, node([],A,Children)) :-
	relation_root_children(L2, Children).

relation_root_children([], []).
relation_root_children([K-V|L1], [(K..K2)-V|L3]) :-
	relation_root_child(L1, K, V, K2, L2),
	relation_root_children(L2, L3).

relation_root_child([K3-V3|L1], K2, V, Klast, L2) :-
	V3==V, K3 =:= K2+1, !,
	relation_root_child(L1, K3, V, Klast, L2).
relation_root_child(L, K2, _, K2, L).

relation_nodes([], _, _, []).
relation_nodes([FDSet-ID|L1], ID0, B, [node(ID,B,Intervals)|L2]) :-
	ID is ID0+1,
	relation_intervals(FDSet, Intervals),
	relation_nodes(L1, ID, B, L2).

relation_intervals([], []).
relation_intervals([[A|B]|L1], [A..B|L2]) :-
	relation_intervals(L1, L2).

/****************************************************************/
/* assignment/[2,3]						*/
/****************************************************************/

% assignment(X1...Xn, Y1...Yn) is true if 
% all Xi,Yi in 1..n and Xi=j iff Yj=i

assignment(Xs, Ys) :-
	assignment(Xs, Ys, [], opt(domain,true), assignment(Xs,Ys)).

assignment(Xs, Ys, Options) :-
	assignment(Xs, Ys, Options, opt(domain,true), assignment(Xs,Ys,Options)).

assignment(Xs, Ys, Options, Opt0, Goal) :-
	all_diff_options(Options, Opt0, Opt, Goal, 3),
	must_be_dvar_list(Xs, Goal, 1),
	length(Xs, N),
	length(Ys, N),
	must_be_dvar_list(Ys, Goal, 2),
	'$fd_range'(1, N, Set, 1),
	domain(Xs, Set),
	domain(Ys, Set),
	Opt = opt(On,_Complete),
	assignment_state(Xs, Ys, 0, XVec, YVec, On, Goal, Susp, []),
	fd_global(assignment(Xs,Ys), f(XVec,YVec,0,0,_Handle,0), Susp).

assignment_state([], [], _, [], [], _On, _Goal) --> [].
assignment_state([X|Xs], [Y|Ys], I, [X-XM|XVec], [Y-YM|YVec], On, Goal) -->
	on(On, X),
	on(On, Y),
	{arg_attribute(X, XM, Goal, 1),
	 arg_attribute(Y, YM, Goal, 2),
	 J is I+1},
	assignment_state(Xs, Ys, J, XVec, YVec, On, Goal).

/****************************************************************/
/* serialized/2, cumulative/4 and friends			*/
/****************************************************************/

serialized(Ss, Ds) :-
	serialized(Ss, Ds, []).

serialized(Ss, Ds, Options) :-
	Goal = serialized(Ss,Ds,Options),
	serialized_options(Options, opt(_,[],0), opt(R,Ps,Flags), Goal, 3),
	must_be_list_of_finite_dvar(Ss, Goal, 1),
	must_be_list_of_finite_dvar(Ds, Goal, 2),
	R = resource(Tasks,Diffs,Global),
	ones(Ss, Rs),
	cumulative(Ss, Ds, Rs, Ps, Tasks, Diffs, 1, Flags, Global, Goal).


cumulative(Ss, Ds, Rs, Limit) :-
	cumulative(Ss, Ds, Rs, Limit, []).

cumulative(Ss, Ds, Rs, Limit, Options) :-
	Goal = cumulative(Ss,Ds,Rs,Limit,Options),
	serialized_options(Options, opt(_,[],0), opt(R,Ps,Flags0), Goal, 5),
	(   Flags0/\1 =:= 1 -> Flags is Flags0\/16
	;   Flags = Flags0
	),
	must_be_list_of_finite_dvar(Ss, Goal, 1),
	must_be_list_of_finite_dvar(Ds, Goal, 2),
	must_be_list_of_finite_dvar(Rs, Goal, 3),
	R = resource(Tasks,Diffs,Global),
	cumulative(Ss, Ds, Rs, Ps, Tasks, Diffs, Limit, Flags, Global, Goal).


serialized_options(L, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
serialized_options([], Opt, Opt, _, _) :- !.
serialized_options([X|L], Opt0, Opt, Goal, ArgNo) :- !,
	(   serialized_option(X, Goal, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,serialized_option), Goal, ArgNo, X)
        ),
	serialized_options(L, Opt1, Opt, Goal, ArgNo).
serialized_options(L, _, _, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Goal, ArgNo, L).

% opt(Resource,Precedences,2'EDCBA) where
% A = path_consistency
% B = static_sets
% C = edge_finder
% D = decomposition
% E = Dij vars must be used
serialized_option(-, _, _, _) :- !, fail.
serialized_option(precedences(Ps), _, opt(R,_,Flags0), opt(R,Ps,Flags)) :-
	Flags is (Flags0 /\ -17) \/ 16.
serialized_option(resource(R), _, opt(_,Ps,Flags0), opt(R,Ps,Flags)) :-
	Flags is (Flags0 /\ -17) \/ 16.
serialized_option(path_consistency(B), _, opt(R,Ps,Flags0), opt(R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -2) \/ Value.
serialized_option(static_sets(B), _, opt(R,Ps,Flags0), opt(R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -3) \/ (Value<<1).
serialized_option(edge_finder(B), _, opt(R,Ps,Flags0), opt(R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -5) \/ (Value<<2).
serialized_option(decomposition(B), _, opt(R,Ps,Flags0), opt(R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -9) \/ (Value<<3).
serialized_option(bounds_only(B), _, opt(R,Ps,Flags0), opt(R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -33) \/ ((1-Value)<<5).

/*
serialized_resource(Ss, Ds, resource(Tasks,Diffs,Global)) :-
	serialized(Ss, Ds, [], Tasks, Diffs, Global,
	           serialized_resource(Ss,Ds,resource(Tasks,Diffs,Global))).

serialized(Ss, Ds) :-
	serialized(Ss, Ds, [], _Tasks, _Diffs, _Global, serialized(Ss,Ds)).

serialized_precedence_resource(Ss, Ds, Ps, resource(Tasks,Diffs,Global)) :-
	serialized(Ss, Ds, Ps, Tasks, Diffs, Global,
	           serialized_precedence_resource(Ss,Ds,Ps,resource(Tasks,Diffs,Global))).

serialized_precedence(Ss, Ds, Ps) :-
	serialized(Ss, Ds, Ps, _Tasks, _Diffs, _Global,
		   serialized_precedence(Ss,Ds,Ps)).
*/

% guts

cumulative([], [], _Rs, _Ps, [], [], _, _, global, _Goal) :- !.
cumulative(Ss, Ds, Rs, Ps, Tasks, Diffs, Limit, Flags, Global, Goal) :-
	% inf_sup(Set1),
	% zero_sup(Set2),
	% must_be_dvar_list(Ss, Set1, Goal, 1),
	% must_be_dvar_list(Ds, Set2, Goal, 2),
	mktasks(Ss, Ds, Rs, Tasks, 1, Goal), % 1-based numbering
	empty_assoc(H0),
	(   Flags/\16 =:= 0 -> Diffs = []
	;   ti_delta_holes(Ps, H0, H),
	    fd_max(Limit, LimitUB),
	    ti_deltas(Tasks, 1, H, LimitUB, Goal, Diffs, []) % 1-based numbering
	),
	task_order(Tasks, Diffs, Limit, Flags, Global).

mktasks([], _, _, [], _NT, _).
mktasks([S|Ss], [D|Ds], [R|Rs], [task(S,SM,D,DM,R,RM,I,_/*Precedences*/)|Ts], I, Goal) :-
	finite_arg_attribute(S, SM, Goal, 1),
	finite_arg_attribute(D, DM, Goal, 2),
	finite_arg_attribute(R, RM, Goal, 3),
	J is I+1,
	mktasks(Ss, Ds, Rs, Ts, J, Goal).

ti_delta_holes([], H, H).
ti_delta_holes([d(I,J,D)|Ds], H0, H) :-	!, % Si before Sj implies Si+D =< Sj
	(   I<J ->
	    ti_one_less_pos(D, X),
	    ti_delta_hole(I-J, 1, X, H0, H1)
	;   ti_one_less_neg(D, X),
	    ti_delta_hole(J-I, X, -1, H0, H1)
	),
	ti_delta_holes(Ds, H1, H).
ti_delta_holes([J-I in R|Ds], H0, H) :-
	set_expression(R, S),
	(   I<J -> ti_delta_set(I-J, S, H0, H1)
	;   '$fd_negate'(S, 0, S1),
	    ti_delta_set(J-I, S1, H0, H1)
	),
	ti_delta_holes(Ds, H1, H).

ti_delta_set(Key, S, H0, H) :-
	get_assoc(Key, H0, Hole0, H, Hole), !,
	'$fd_dom_subtract'(Hole0, S, Hole).
ti_delta_set(Key, S, H0, H) :-
	'$fd_dom_complement'(S, Hole),
	put_assoc(Key, H0, Hole, H).

ti_delta_hole(_  , LB, UB, H0, H) :-
	integer(LB),
	integer(UB),
	LB > UB, !,
	H = H0.
ti_delta_hole(Key, LB, UB, H0, H) :-
	get_assoc(Key, H0, Hole0, H, Hole), !,
	fdset_parts(Part, LB, UB, []),
	'$fd_dom_union'(Hole0, Part, Hole).
ti_delta_hole(Key, LB, UB, H0, H) :-
	put_assoc(Key, H0, Hole, H),
	fdset_parts(Hole, LB, UB, []).

ti_one_less_pos(P, X) :- integer(P), !, X is P-1.
ti_one_less_pos(sup, sup).

ti_one_less_neg(P, X) :- integer(P), !, X is 1-P.
ti_one_less_neg(sup, inf).

ti_deltas([_], _, _H, _, _Goal) --> !.
ti_deltas([Taski|Tasks], I, H, Limit, Goal) -->
	{Taski = task(_,SM,_,DM,_,RM,_,_),
	 get_mutable_rec(SD, SM),
	 SD = dom(_,Mini,Maxi,_),
	 get_mutable_rec(DD, DM),
	 DD = dom(_,Duri,_,_),
	 get_mutable_rec(RD, RM),
	 RD = dom(_,Resi,_,_),
	 MaxRes is Limit-Resi,
	 J is I+1},
	ti_deltas(Tasks, I, J, H, Mini, Maxi, Duri, MaxRes, Goal),
	ti_deltas(Tasks, J, H, Limit, Goal).

ti_deltas([], _, _, _H, _, _, _, _, _Goal) --> [].
ti_deltas([Taskj|Tasks], I, J, H, Mini, Maxi, Duri, MaxRes, Goal) -->
	[d(I,J,Mij)],
	{Taskj = task(_,SM,_,DM,_,RM,_,_),
	 get_mutable_rec(SD, SM),
	 SD = dom(_,Minj,Maxj,_),
	 get_mutable_rec(DD, DM),
	 DD = dom(_,Durj,_,_),
	 get_mutable_rec(RD, RM),
	 RD = dom(_,Resj,_,_),
	 A is Minj-Maxi,
	 D is Maxj-Mini,
	 fdset_parts(Set0, A, D, []),
	 (   Resj>MaxRes ->
	     ti_constrain_dur(Duri, Durj, Set0, Set1)
	 ;   Set1 = Set0
	 ),
	 ti_constrain_precedence(I, J, H, Set1, Set),
	 Set \== [],
	 create_mutable(Set, Mij),
	 K is J+1},
	ti_deltas(Tasks, I, K, H, Mini, Maxi, Duri, MaxRes, Goal).

% For now, any task can be pre-empted by a zero-duration task.
ti_constrain_dur(0, _, Set, Set) :- !.
ti_constrain_dur(_, 0, Set, Set) :- !.
ti_constrain_dur(Duri, Durj, Set0, Set) :-
	Subl is 1-Durj,
	Subu is Duri-1,
	fdset_parts(Sub, Subl, Subu, []),
	'$fd_dom_subtract'(Set0, Sub, Set).

ti_constrain_precedence(I, J, H, Set0, Set) :-
	get_assoc(I-J, H, Hole), !,
	'$fd_dom_subtract'(Set0, Hole, Set).
ti_constrain_precedence(_, _, _, Set, Set).

task_order(Tasks, Diffs, Limit, Flags) :-
	task_order(Tasks, Diffs, Limit, Flags, _Global).

task_order(Tasks, Diffs, Limit, Flags, Global) :-
	task_order_susp1(Tasks, Susp, [max(Limit)]),
	length(Tasks, N),
	fd_global6(task_order(Tasks,Diffs,Limit,Flags),
		   f(N,Tasks,Diffs,Limit,Flags,N,0,_Handle,0),
		   Susp, Global, clpfd, 0).

task_order_susp1([]) --> [].
task_order_susp1([task(S,_,D,_,R,_,_,_)|L]) -->
	[minmax(S),min(D),min(R)],
	task_order_susp1(L).

/****************************************************************/
/* count/4							*/
/****************************************************************/

count(Value, Xs, Rel, Card) :-
	fdset_singleton(Set, Value),
	count_aux(Xs, Set, Bs),
	sum(Bs, Rel, Card).

count_aux([], _, []).
count_aux([X|Xs], Set, [B|Bs]) :-
	in_set_iff(X, Set, B),
	count_aux(Xs, Set, Bs).


/****************************************************************/
/* sum/3, scalar_product/4					*/
/****************************************************************/

sum(Xs, Rel, S) :-
	ones(Xs, Cs),
	scalar_product(Cs, Xs, Rel, S).

ones([], []).
ones([_|L1], [1|L2]) :- ones(L1, L2).


scalar_product(Cs, Xs, Rel, S) :-
	Goal = scalar_product(Cs,Xs,Rel,S),
	must_be_dvar_list(Xs, Goal, 2),
	must_be_dvar_list([S], Goal, 4),
	scalar_state(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal),
	length(Vec, N),
	(   Rel==(#=)
	->  sp_strength_reduce(N, Vec, Sum, Goal, Susp)
	;   scalar_bounds(Rel, LB, UB),
	    fd_global(Goal,
		      state(Vec,LB,UB,Sum/*RHS*/,0/*Nground*/,0/*Fast*/,_,0),
		      Susp)
        ).

%%% This is not an exhaustive list.

sp_strength_reduce(0, [], S, _Goal, _Susp) :- !, S = 0.
sp_strength_reduce(1, [f(C1,X1,_M1)], S, _Goal, _Susp) :- !,
	S mod C1 =:= 0,
	S1 is S//C1,
	't=c'(X1, S1).
sp_strength_reduce(2, [f(C1,X1,M1),f(C2,X2,M2)], S, Goal, Susp) :-
	'$fd_debugging'(0), !,
	sp_strength_reduce_2(C1, X1, M1, C2, X2, M2, S, Goal, Susp).
sp_strength_reduce(3, [f(C1,X1,M1),f(C2,X2,M2),f(C3,X3,M3)], S, Goal, Susp) :-
	'$fd_debugging'(0), !,
	sp_strength_reduce_3(C1, X1, M1, C2, X2, M2, C3, X3, M3, S, Goal, Susp).
sp_strength_reduce(_, Vec, Sum, Goal, Susp) :-
	fd_global(Goal, state(Vec,0,0,Sum/*RHS*/,0/*Nground*/,0/*Fast*/,_,0), Susp).


sp_strength_reduce_2(1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- !,
	't+u=c'(X1, X2, S).
sp_strength_reduce_2(-1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- !,
	't=u+c'(X2, X1, S).
sp_strength_reduce_2(1, X1, _M1, -1, X2, _M2, S, _Goal, _Susp) :- !,
	't=u+c'(X1, X2, S).
sp_strength_reduce_2(-1, X1, _M1, -1, X2, _M2, S, _Goal, _Susp) :- !,
	S1 is -S, 
	't+u=c'(X1, X2, S1).
sp_strength_reduce_2(1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C2>0, !,
	'ax+y=t'(C2, X2, X1, S).
sp_strength_reduce_2(1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C2<0, !,
	C3 is -C2,
	'ax+y=t'(C3, X2, S, X1).
sp_strength_reduce_2(C1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- C1>0, !,
	'ax+y=t'(C1, X1, X2, S).
sp_strength_reduce_2(C1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- C1<0, !,
	C3 is -C1,
	'ax+y=t'(C3, X1, S, X2).
% sp_strength_reduce_2(C1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C1>0, C2>0, !,
% 	S mod gcd(C1,C2) =:= 0,
% 	'ax+by=t'(C1, X1, C2, X2, S).
% sp_strength_reduce_2(C1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C1<0, C2<0, !,
% 	C3 is -C1, C4 is -C2, S1 is -S,
% 	S mod gcd(C3,C4) =:= 0,
% 	'ax+by=t'(C3, X1, C4, X2, S1).
sp_strength_reduce_2(C1, X1, M1, C2, X2, M2, Sum, Goal, Susp) :-
	fd_global(Goal, state([f(C1,X1,M1),f(C2,X2,M2)],0,0,Sum/*RHS*/,0/*Nground*/,0/*Fast*/,_,0), Susp).

sp_strength_reduce_3(1, X1, _M1, 1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y+z=c'(X1, X2, X3, S).
sp_strength_reduce_3(-1, X1, _M1, 1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X2, X3, X1, S).
sp_strength_reduce_3(1, X1, _M1, -1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X1, X3, X2, S).
sp_strength_reduce_3(-1, X1, _M1, -1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X3, X1, X2, S).
sp_strength_reduce_3(1, X1, _M1, 1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X1, X2, X3, S).
sp_strength_reduce_3(-1, X1, _M1, 1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X2, X3, X1, S).
sp_strength_reduce_3(1, X1, _M1, -1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X1, X3, X2, S).
sp_strength_reduce_3(-1, X1, _M1, -1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	S1 is -S,
	'x+y+z=c'(X1, X2, X3, S1).
sp_strength_reduce_3(C1, X1, M1, C2, X2, M2, C3, X3, M3, Sum, Goal, Susp) :-
	fd_global(Goal, state([f(C1,X1,M1),f(C2,X2,M2),f(C3,X3,M3)],0,0,Sum/*RHS*/,0/*Nground*/,0/*Fast*/,_,0), Susp).


scalar_bounds(#<, inf, -1).
scalar_bounds(#=<, inf, 0).
scalar_bounds(#>, 1, sup).
scalar_bounds(#>=, 0, sup).
scalar_bounds(#=, 0, 0).
scalar_bounds(#\=, inf, sup).

scalar_state(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal) :-
	integer(S), !,
	scalar_vector(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal).
scalar_state(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal) :-
	scalar_vector([-1|Cs], [S|Xs], Rel, 0, Vec, Sum, Susp, Goal).

scalar_vector(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal) :-
	scalar_pairs(Xs, Cs, L1),
	keysort(L1, L2),
	keyfuse(L2, L3),
	scalar_vector(L3, Rel, S, Vec, Sum, Susp, Goal).

scalar_pairs([], [], []).
scalar_pairs([X|Xs], [C|Cs], [X-C|XCs]) :- scalar_pairs(Xs, Cs, XCs).

scalar_vector([], _, Sum, [], Sum, [], _Goal).
scalar_vector([X-C|XCs], Rel, S0, Vec, S, Susp, Goal) :-
	integer(X), !,
	S1 is S0-C*X,
	scalar_vector(XCs, Rel, S1, Vec, S, Susp, Goal).
scalar_vector([X-C|XCs], Rel, S0, [f(C,X,M)|Vec], S, [Sus|Susp], Goal) :- 
	C=\=0, !,
	arg_attribute(X, M, Goal, 2),
	susp_type(Rel, C, X, Sus),
	scalar_vector(XCs, Rel, S0, Vec, S, Susp, Goal).
scalar_vector([_|XCs], Rel, S0, Vec, S, Susp, Goal) :-
	scalar_vector(XCs, Rel, S0, Vec, S, Susp, Goal).

susp_type(#<, C, X, Type) :- susp_type(#=<, C, X, Type).
susp_type(#=<, C, X, Type) :- C>0 -> Type = min(X); Type = max(X).
susp_type(#>, C, X, Type) :- susp_type(#>=, C, X, Type).
susp_type(#>=, C, X, Type) :- C>0 -> Type = max(X); Type = min(X).
susp_type(#=, _, X, minmax(X)).
susp_type(#\=, _, X, val(X)).
susp_type(dom, _, X, dom(X)).

/****************************************************************/
/* knapsack/3           					*/
/****************************************************************/

knapsack(Cs, Xs, S) :-
	Goal = knapsack(Cs,Xs,S),
	must_be_list_of_finite_dvar(Xs, Goal, 2),
	scalar_state(Cs, Xs, dom, 0, Vec, Sum, Susp, Goal),
	nonnegative_vector(Vec),
	arg_attribute(S, SD, Goal, 3),
	fd_global(Goal, state(Vec,S-SD,Sum/*RHS*/,0/*Nground*/,_,0), [minmax(S)|Susp]).

nonnegative_vector([]).
nonnegative_vector([f(C,X,_)|Vec]) :-
	C >= 0,
	't>=c'(X, 0),
	nonnegative_vector(Vec).

/****************************************************************/
/* 'x*x=y'/2							*/
/****************************************************************/

'x*x=y'(X, Y) :-
	integer(X), !,
	Y is X*X.
'x*x=y'(X, Y) :-
	integer(Y), !,
	(   Y =:= 0 -> '$fd_range'(0, 0, R, 1)
	;   Y >= 0,
	    X1 is integer(sqrt(Y)),
	    X1*X1 =:= Y,
	    X0 is -X1,
	    '$fd_range'(X0, X0, R0, 1),
	    '$fd_dom_insert'(R0, X1, R)
	),
	prune_and_propagate(X, R).
'x*x=y'(X, Y) :-
	Goal = 'x*x=y'(X,Y),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	Y in 0..sup,
	on_minmax([X,Y], Susp, []),
	fd_global(Goal, state(X,XM,Y,YM), Susp).

/****************************************************************/
/* 'x*y=z'/3							*/
/****************************************************************/

'x*y=z'(X, Y, Z) :-
	integer(X),
	'$fd_debugging'(0), !,
	(   X=:=0 -> Z = 0
	;   X > 0 -> 'ax=t'(X, Y, Z)
	;   X1 is -X, '-ax=t'(X1, Y, Z)
	).
'x*y=z'(X, Y, Z) :-
	integer(Y),
	'$fd_debugging'(0), !,
	'x*y=z'(Y, X, Z).
'x*y=z'(X, Y, Z) :-
	Goal = 'x*y=z'(X,Y,Z),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	on_minmax([X,Y,Z], Susp, []),
	fd_global(Goal, state(X,XM,Y,YM,Z,ZM), Susp).

'-ax=t'(A,X,T) +:
	X in -max(T) /< A .. -min(T) /> A,
	T in -max(X) *	A .. -min(X) *	A !.

/****************************************************************/
/* 'x/y=z'/3							*/
/****************************************************************/

:- block 'x/y=z'(?, -, ?).
'x/y=z'(X, Y, Z) :-
	integer(X), !,
	Y =\= 0,
	Z is X//Y.
'x/y=z'(X, Y, Z) :-
	Y>0,
	fd_min(X, Xa), integer(Xa), Xa>=0,
	'$fd_debugging'(0), !,
	'x/k=z'(X, Y, Z).
'x/y=z'(X, Y, Z) :-
	Goal = 'x/y=z'(X,Y,Z),
	Y =\= 0,
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Z, ZM, Goal, 3),
	on_minmax([X,Z], Susp, []),
	fd_global(Goal, state(X,XM,Y,Z,ZM), Susp). 

'x/k=z'(X, K, Z) +:
	Z in min(X)/<K .. max(X)/<K,
	X in min(Z)*K  .. (max(Z)+1)*K - 1.

/****************************************************************/
/* 'x mod y=z'/3						*/
/****************************************************************/

:- block 'x mod y=z'(?, -, ?).
'x mod y=z'(X, Y, Z) :-
	integer(X), !,
	Y =\= 0,
	Z is X mod Y.
'x mod y=z'(X, Y, Z) :-
	Goal = 'x mod y=z'(X,Y,Z),
	Y =\= 0,
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Z, ZM, Goal, 3),
	B is abs(Y)-1,
	A is -B,
	Z in A..B,
	on_minmax([X,Z], Susp, []),
	fd_global(Goal, state(X,XM,Y,Z,ZM), Susp).

/****************************************************************/
/* min/3, max/3 etc						*/
/****************************************************************/

'min(x,y)=z'(X, Y, Z) :-
	X #>= Z,
	Y #>= Z,
	'oneof(x,y)=z'(X, Y, Z).

'max(x,y)=z'(X, Y, Z) :-
	X #=< Z,
	Y #=< Z,
	'oneof(x,y)=z'(X, Y, Z).

'oneof(x,y)=z'(X, Y, Z) :-
	'$fd_debugging'(1), !,
	oneof(X, Y, Z).
'oneof(x,y)=z'(X, Y, Z) :-
	'oneof(x,y)=z IND'(X, Y, Z).

'oneof(x,y)=z IND'(X, Y, Z) +:
	X in ((dom(Y)/\dom(Z)) ? (inf..sup)) \/ dom(Z) !,
	Y in ((dom(X)/\dom(Z)) ? (inf..sup)) \/ dom(Z) !,
	Z in (dom(X)\/dom(Y)) !.

'|x|=y'(X,Y) :-
	'$fd_debugging'(1), !,
	abs(X, Y).
'|x|=y'(X,Y) :-
	't>=c'(Y, 0),				% do this outside the loop
	'|x|=y 1'(X,Y).

'|x|=y 1'(X,Y) +:
	X in dom(Y) \/ (0-dom(Y)) !,
	Y in dom(X) \/ (0-dom(X)) !.

%%% Support for reified constraints (domain reasoning for now)

% optimized for common cases
'x=y'(X, Y) :- integer(Y), !,
	't=c'(X, Y).
'x=y'(X, Y) :- integer(X), !,
	't=c'(Y, X).
'x=y'(X, Y) :-
	't=u'(X, Y).


% Now domain consistent!
't=u'(T, U) :-
	'$fd_debugging'(1), !,
	T = U.
't=u'(T, U) :-
	't=u IND'(T, U).

't=u IND'(X,Y) +:
	X in dom(Y) !,
	Y in dom(X) !.
't=u IND'(X,Y) -:
	X in \{Y},
	Y in \{X}.
% 't=u IND'(X,Y) +?				% covered by next rule
%	X in {Y}.
't=u IND'(X,Y) -?
	X in \dom(Y).

% Now domain consistent!
% just the negation of the above
'x\\=y'(T, U) :-
	'$fd_debugging'(1), !,
	eq_iff(T, U, 0).
'x\\=y'(T, U) :-
	'x\\=y IND'(T, U).

'x\\=y IND'(X,Y) -:
	X in dom(Y) !,
	Y in dom(X) !.
'x\\=y IND'(X,Y) +:
	X in \{Y},
	Y in \{X}.
% 'x\\=y IND'(X,Y) -?			% covered by next rule
%	X in {Y}.
'x\\=y IND'(X,Y) +?
	X in \dom(Y).

'x=<y'(T, U) :-
	'$fd_debugging'(1), !,
	le_iff(T, U, 1).
'x=<y'(T, U) :-
	'x=<y IND'(T, U).

'x=<y IND'(X,Y) +:
	X in inf..max(Y),
	Y in min(X)..sup.
'x=<y IND'(X,Y) -:
	X in (min(Y)+1)..sup,
	Y in inf..(max(X)-1).
'x=<y IND'(X,Y) +?
	X in inf..min(Y).
'x=<y IND'(X,Y) -?				% NOT covered by prev rule
	X in (max(Y)+1)..sup.

%%% Reified constraints.

iff_aux(Constraint0, B) :-
	prolog:get_module(Constraint0, Constraint, Module),
	iff_aux(Constraint, Module, B).

iff_aux('x=y'(X,Y), clpfd, B) :-
	'$fd_debugging'(1), !,
	eq_iff(X, Y, B).
iff_aux('x\\=y'(X,Y), clpfd, B) :-
	'$fd_debugging'(1), !,
	eq_iff(X, Y, B1),
	'\\p'(B1, B).
iff_aux('x=<y'(X,Y), clpfd, B) :-
	'$fd_debugging'(1), !,
	le_iff(X, Y, B).
iff_aux('x=y'(X,Y), clpfd, B) :- !,
	iff_aux('t=u IND'(X,Y), clpfd, B).
iff_aux('x\\=y'(X,Y), clpfd, B) :- !,
	iff_aux('x\\=y IND'(X,Y), clpfd, B).
iff_aux('x=<y'(X,Y), clpfd, B) :- !,
	iff_aux('x=<y IND'(X,Y), clpfd, B).
iff_aux(X in Expr, _, B) :- !,
        fd_goal_expansion(X in Expr #<=> B, clpfd, Goal),
	call(Goal).
iff_aux(Constraint, _, B) :-
	fd_expandable(Constraint, _, _, _), !,
	fd_goal_expansion(Constraint #<=> B, clpfd, Goal),
	call(Goal).
iff_aux(Constraint, Module, B) :-
	'$fd_find_definition'(Constraint, Module, DefPtr),
	(   DefPtr =\= 0
	->  check_arguments(Constraint, Attv),
	    iff_aux(DefPtr, Constraint, B, Attv)
	;   functor(Constraint, Name, Arity),
	    fd_illarg(existence(constraint,Module:Name/Arity,0), Constraint, 0)
	).

iff_aux(Def, Constraint, B, Attv) :-
	var(B), !,
	propagate_interval(B, 0, 1),
	'$fd_begin',
	enqueue_ix(Def, 2, Constraint, A, B, Attv),
	enqueue_ix(Def, 3, Constraint, A, B, Attv),
	'$fd_get_indexicals'(Def, 0, Constraint, Ent, _, Attv, Ix0s),
	'$fd_link_iff'(Ix0s, B, 0, A),
	'$fd_get_indexicals'(Def, 1, Constraint, Ent, _, Attv, Ix1s),
	'$fd_link_iff'(Ix1s, B, 1, A),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
iff_aux(Def, Constraint, B, Attv) :-
	integer(B),
	B /\ -2 =:= 0,
	'$fd_begin',
	enqueue_ix(Def, B, Constraint, _, _, Attv),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

enqueue_ix(DefPtr, B, Constraint, A, ZeroOne, Attv) :-
	'$fd_get_indexicals'(DefPtr, B, Constraint, A, ZeroOne, Attv, Ixs),
	'$fd_enqueue_first'(Ixs).


in_aux_rt(X, Expr) :-
	ground(Expr),
	set_expression(Expr, Set), !,
	prune_and_propagate(X, Set).
in_aux_rt(X, Expr) :-
	ill_formed_constraint(X in Expr), fail.

in_aux_rt(X, Expr, B) :-
	ground(Expr),
	set_expression(Expr, Set), !,
	in_set_iff(X, Set, B).
in_aux_rt(X, Expr, B) :-
	ill_formed_constraint(X in Expr #<=> B), fail.


in_set_aux_rt(X, Set) :-
	'$fd_size'(Set, _, 1), !,
	prune_and_propagate(X, Set).
in_set_aux_rt(X, Set) :-
	ill_formed_constraint(X in_set Set), fail.

in_set_aux_rt(X, Set, B) :-
	'$fd_size'(Set, _, 1), !,
	in_set_iff(X, Set, B).
in_set_aux_rt(X, Set, B) :-
	ill_formed_constraint(X in_set Set #<=> B), fail.

in_set_iff(X, Set, B) :-
	Goal = in_set_iff(X,Set,B),
	propagate_interval(B, 0, 1),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(B, BMut, Goal, 3),
	fd_global(Goal, state(X,XMut,Set,B,BMut), [dom(X),dom(B)]).

%% fdbg support

eq_iff(X, Y, B) :-
	Goal = eq_iff(X,Y,B),
	propagate_interval(B, 0, 1),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(B, BMut, Goal, 3),
	(   B==1 -> X = Y
	;   B==0
	->  fd_global(Goal, state(X,XMut,Y,YMut,B,BMut), [val(X),val(Y),dom(B)])
	;   fd_global(Goal, state(X,XMut,Y,YMut,B,BMut), [dom(X),dom(Y),dom(B)])
	).

le_iff(X, Y, B) :-
	Goal = le_iff(X,Y,B),
	propagate_interval(B, 0, 1),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(B, BMut, Goal, 3),
	fd_global(Goal, state(X,XMut,Y,YMut,B,BMut), [minmax(X),minmax(Y),dom(B)]).

oneof(X, Y, B) :-
	Goal = oneof(X,Y,B),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(B, BMut, Goal, 3),
	fd_global(Goal, state(X,XMut,Y,YMut,B,BMut), [dom(X),dom(Y),dom(B)]).

abs(X, Y) :-
	Goal = abs(X,Y),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	fd_global(Goal, state(X,XMut,Y,YMut), [dom(X),dom(Y)]).

%% end fdbg support

/****************************************************************/
/* bool/4							*/
/****************************************************************/

bool(Fun, X, Y, Z) :-
	Goal = bool(Fun,X,Y,Z),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(Z, ZMut, Goal, 3),
	propagate_interval(X, 0, 1),
	propagate_interval(Y, 0, 1),
	propagate_interval(Z, 0, 1),
	on_dom([X,Y,Z], Susp, []),
	fd_global(Goal, state(X,XMut,Y,YMut,Z,ZMut,Fun), Susp).

% FOR BACKWARD COMPATIBILITY!

'\\p'(P, B) :- bool(6, P, B, 1).

'p/\\q'(P, Q, B) :- bool(0, P, Q, B).

'p\\q'(P, Q, B) :- bool(6, P, Q, B).

'p\\/q'(P, Q, B) :- bool(3, P, Q, B).

'p=>q'(P, Q, B) :- bool(4, Q, P, B).

'p<=>q'(P, Q, B) :- bool(7, P, Q, B).

/****************************************************************/
/* domain/3							*/
/****************************************************************/

domain(Vars, Min, Max) :-
	set_expression(Min..Max, Set), !,
	domain(Vars, Set).
domain(Vars, Min, Max) :-
	fd_illarg(domain(term,set_expression), domain(Vars,Min,Max), 0, Min..Max).

% FDBG puts advice on this!
domain(Vars, Set) :-
	domain1(Vars, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

domain1([], _, _).
domain1([X|Xs], Set, Init) :-
	'$fd_in_set'(X, Set, Init),
	domain1(Xs, Set, 0).

%%% predicates corresponding to macro-expanded constraints

X in Expr :-
	in_aux_rt(X, Expr).

X in_set Set :-
	in_set_aux_rt(X, Set).

X #= Y :-
	fd_goal_expansion(X #= Y, clpfd, Goal),
	Goal.

X #\= Y :-
	fd_goal_expansion(X #\= Y, clpfd, Goal),
	Goal.

X #< Y :-
	fd_goal_expansion(X #< Y, clpfd, Goal),
	Goal.

X #=< Y :-
	fd_goal_expansion(X #=< Y, clpfd, Goal),
	Goal.

X #> Y :-
	fd_goal_expansion(X #> Y, clpfd, Goal),
	Goal.

X #>= Y :-
	fd_goal_expansion(X #>= Y, clpfd, Goal),
	Goal.

:- meta_predicate #\(:).
#\ Q :-
	fd_goal_expansion(#\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #/\(:,:).
P #/\ Q :-
	fd_goal_expansion(P #/\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\(:,:).
P #\ Q :-
	fd_goal_expansion(P #\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\/(:,:).
P #\/ Q :-
	fd_goal_expansion(P #\/ Q, clpfd, Goal),
	Goal.

:- meta_predicate #=>(:,:).
P #=> Q :-
	fd_goal_expansion(P #=> Q, clpfd, Goal),
	Goal.

:- meta_predicate #<=(:,:).
P #<= Q :-
	fd_goal_expansion(P #<= Q, clpfd, Goal),
	Goal.

:- meta_predicate #<=>(:,:).
P #<=> Q :-
	fd_goal_expansion(P #<=> Q, clpfd, Goal),
	Goal.

/****************************************************************/
/* disjoint1/[1,2]                          			*/
/****************************************************************/

disjoint1(Items) :-
	disjoint1(Items, []).

disjoint1(Items, Options) :-
	Goal = disjoint1(Items,Options),
	disjoint1_options(Options, opt(0,inf,sup,[]), Opt, Goal, 2),
	(   Opt = opt(Flags,Min,B,_),
	    Flags /\ 2 =:= 2
	->  Max is B-1
	;   Min = inf, Max = sup
	),
	mkitems(Items, Items2, Min, Max, Goal, Susp, []),
	length(Items2, N),
	fd_global(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp).


disjoint1_options(L, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
disjoint1_options([], Opt, Opt, _, _) :- !.
disjoint1_options([X|L], Opt0, Opt, Goal, ArgNo) :- !,
	(   disjoint1_option(X, Goal, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,disjoint1_option), Goal, ArgNo, X)
        ),
	disjoint1_options(L, Opt1, Opt, Goal, ArgNo).
disjoint1_options(L, _, _, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Goal, ArgNo, L).

% opt(2'AMXD,Min,Max,Margins) where
% A = global
% M = Margins \== []
% X = wrap-around
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff) = list of extra margins
disjoint1_option(-, _, _, _) :- !, fail.
disjoint1_option(decomposition(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -2) \/ Value.
disjoint1_option(global(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -9) \/ (Value<<3).
disjoint1_option(wrap(Min,Max), _, opt(Flags0,_,_,Ms), opt(Flags,Min,Max,Ms)) :-
	(   Min==inf, Max==sup -> Flags is Flags0 /\ -3
	;   integer(Min),
	    integer(Max),
	    Min < Max,
	    Flags is (Flags0 /\ -3) \/ 2
	).
disjoint1_option(margin(T1,T2,D), _, opt(Flags0,Min,Max,Tail), opt(Flags,Min,Max,[margin(T1,T2,D)|Tail])) :-
	Flags is (Flags0 /\ -5) \/ 4.
disjoint1_option(lean(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),	% optimize for the incremental case
	Flags is (Flags0 /\ -33) \/ (Value<<5).


mkitems([], [], _, _, _) --> [].
mkitems([X|Xs], [item(S,SM,D,DM,Type)|Items], Min, Max, Goal) -->
	[minmax(S),min(D)],
	{arg(1,X,S), arg(2,X,D)},
	{arg(3,X,Type) -> true; Type=0},
	{propagate_interval(S, Min, Max)},
	{finite_arg_attribute(S, SM, Goal, 1)},
	{finite_arg_attribute(D, DM, Goal, 1)},
	mkitems(Xs, Items, Min, Max, Goal).

/****************************************************************/
/* disjoint2/[1,2]                          			*/
/****************************************************************/

disjoint2(Items) :-
	disjoint2(Items, []).

disjoint2(Items, Options) :-
	Goal = disjoint2(Items,Options),
	disjoint2_options(Options, opt(0,inf,sup,inf,sup,[]), Opt, Goal, 2),
	Opt = opt(Flags,Min1,B1,Min2,B2,_),
	(   Flags /\ 2 =:= 2
	->  Max1 is B1-1
	;   Min1 = inf, Max1 = sup
	),
	(   Flags /\ 4 =:= 4
	->  Max2 is B2-1
	;   Min2 = inf, Max2 = sup
	),
	mkitems(Items, Items2, Min1, Max1, Min2, Max2, Goal, Susp, []),
	length(Items2, N),
	fd_global(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp).


disjoint2_options(L, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
disjoint2_options([], Opt, Opt, _, _) :- !.
disjoint2_options([X|L], Opt0, Opt, Goal, ArgNo) :- !,
	(   disjoint2_option(X, Goal, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,disjoint2_option), Goal, ArgNo, X)
        ),
	disjoint2_options(L, Opt1, Opt, Goal, ArgNo).
disjoint2_options(L, _, _, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Goal, ArgNo, L).

% opt(2'AMYXD,Min1,Max1,Min2,Max2,Margins) where
% A = global
% M = Margins \== []
% Y = wrap-around in Y dim
% X = wrap-around in X dim
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff1,Diff2) = list of extra margins
disjoint2_option(-, _, _, _) :- !, fail.
disjoint2_option(decomposition(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -2) \/ Value.
disjoint2_option(global(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -17) \/ (Value<<4).
disjoint2_option(wrap(Min1,Max1,Min2,Max2), _, 
		 opt(Flags0,_,_,_,_,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	(   Min1==inf, Max1==sup -> Flags1 is (Flags0 /\ -3)
	;   integer(Min1),
	    integer(Max1),
	    Min1 < Max1,
	    Flags1 is (Flags0 /\ -3) \/ 2
	),
	(   Min2==inf, Max2==sup -> Flags is (Flags1 /\ -5)
	;   integer(Min2),
	    integer(Max2),
	    Min2 < Max2,
	    Flags is (Flags1 /\ -5) \/ 4
	).
disjoint2_option(margin(T1,T2,D1,D2), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Tail), 
		 opt(Flags,Min1,Max1,Min2,Max2,[margin(T1,T2,D1,D2)|Tail])) :-
	Flags is (Flags0 /\ -9) \/ 8.
disjoint2_option(lean(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),	% optimize for the incremental case
	Flags is (Flags0 /\ -33) \/ (Value<<5).
disjoint2_option(synchronization(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -65) \/ (Value<<6).


% S1,S2 - start variables
% SM1,SM2 - start domain mutables
% D1,D2 - durations
% Type - type of object (optional)
mkitems([], [], _, _, _, _, _) --> [].
mkitems([X|Xs], [item(S1,SM1,D1,DM1,S2,SM2,D2,DM2,Type)|Items],
	Min1, Max1, Min2, Max2, Goal) -->
	[minmax(S1),min(D1),minmax(S2),min(D2)],
	{arg(1,X,S1), arg(2,X,D1), arg(3,X,S2), arg(4,X,D2)},
	{arg(5,X,Type) -> true; Type=0},
	{propagate_interval(S1, Min1, Max1)},
	{finite_arg_attribute(S1, SM1, Goal, 1)},
	{finite_arg_attribute(D1, DM1, Goal, 1)},
	{propagate_interval(S2, Min2, Max2)},
	{finite_arg_attribute(S2, SM2, Goal, 1)},
	{finite_arg_attribute(D2, DM2, Goal, 1)},
	mkitems(Xs, Items, Min1, Max1, Min2, Max2, Goal).

/****************************************************************/
/* case/[3,4]							*/
/****************************************************************/

case(Template, Tuples, Dag) :-
	case(Template, Tuples, Dag, []).

case(Template, Tuples, Dag, Options) :-
	Goal = case(Template,Tuples,Dag,Options),
	prolog:term_variables(Template, Vars),
	length(Vars, NVars),
	doms(NVars, Doms),
	case_options(Options, opt(Doms,Doms), opt(On,Prune), Vars, Goal, 4),
	length(Dag, NNodes),
	case_compile(Dag, Vars, Dag1, NChildren),
	case_post(Tuples, Template, Vars, NVars, NNodes, NChildren, Dag1, On, Prune, Goal).

doms(0, []) :- !.
doms(I, [dom|L]) :- J is I-1, doms(J, L).

case_options(L, _, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
case_options([], Opt, Opt, _, _, _) :- !.
case_options([X|L], Opt0, Opt, Vars, Goal, ArgNo) :- !,
	(   case_option(X, Vars, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,case_option), Goal, ArgNo, X)
        ),
	case_options(L, Opt1, Opt, Vars, Goal, ArgNo).
case_options(L, _, _, Vars, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Vars, Goal, ArgNo, L).

case_option(-, _, _, _) :- !, fail.
case_option(on(Spec), Vars, opt(On0,Prune), opt(On,Prune)) :-
	case_spec(Spec, F, Var),
	var_nth(Var, Vars, 0, N),
	replace_nth(N, On0, F, On).
case_option(prune(Spec), Vars, opt(On,Prune0), opt(On,Prune)) :-
	case_spec(Spec, F, Var),
	var_nth(Var, Vars, 0, N),
	replace_nth(N, Prune0, F, Prune).

case_spec(dom(X), dom, X).
case_spec(min(X), min, X).
case_spec(max(X), max, X).
case_spec(minmax(X), minmax, X).
case_spec(val(X), val, X).
case_spec(none(X), [], X).

replace_nth(0, [_|L], F, [F|L]) :- !.
replace_nth(I, [X|L1], F, [X|L2]) :-
	J is I-1,
	replace_nth(J, L1, F, L2).

case_post([], _, _, _, _, _, _, _, _, _).
case_post([Tuple|Tuples], Template, Vars, NVars, NNodes, NChildren, Dag1, On, Prune, Goal) :-
	copy_term(Template-Vars, Tuple-TVars),
	case_susp(TVars, TPairs, On, Susp, Goal),
	fd_global(Goal, state(f(NVars,NNodes,NChildren,TPairs,Dag1,Prune),_Handle,0), Susp),
	case_post(Tuples, Template, Vars, NVars, NNodes, NChildren, Dag1, On, Prune, Goal).

case_susp([], [], [], [], _).
case_susp([X|Xs], [X-XM|Ys], [none|On], Zs, Goal) :- !,
	arg_attribute(X, XM, Goal, 2),
	case_susp(Xs, Ys, On, Zs, Goal).
case_susp([X|Xs], [X-XM|Ys], [F|On], [FX|Zs], Goal) :-
	FX =.. [F,X],
	case_spec(FX, F, X),
	arg_attribute(X, XM, Goal, 2),
	case_susp(Xs, Ys, On, Zs, Goal).

% TODO: error handling
% TODO: check all paths complete
case_compile(Dag, Vars, Dag1, NChildren) :-
	empty_assoc(ID2Index0),
	case_map(Dag, 0, Dag1, ID2Index0, ID2Index),
	case_compile(Dag, Vars, Dag1, 0, NChildren, ID2Index),
	list_to_tree(Dag1, NodeTree),
	Dag1 = [Node|_],
	descendants(Node, _, NodeTree).

case_compile([], _, [], NC, NC, _).
case_compile([Node|Nodes], Vars, [Node1|Nodes1], NC0, NC, ID2Index) :-
	Node = node(_,Var,Children),
	Node1 = node(N,_,Children3),
	var_nth(Var, Vars, 0, N),
	case_children(Children, ID2Index, Children0, Children1),
	keysort(Children1, Children2),
	append(Children0, Children2, Children3),
	length(Children3, Len3),
	NC1 is NC0+Len3,
	case_compile(Nodes, Vars, Nodes1, NC1, NC, ID2Index).

case_children([], _, [], []).
case_children([inf..Max|L1], ID2Index, [(inf..Max)-[]|Ch0], Ch2) :- !,
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([(inf..Max)-ID|L1], ID2Index, [(inf..Max)-Index|Ch0], Ch2) :- !,
	(   ID\==[], get_assoc(ID, ID2Index, Index) -> true
	;   Index = []
	),
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([Min..Max|L1], ID2Index, Ch0, [(Min..Max)-[]|Ch2]) :- !,
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([(Min..Max)-ID|L1], ID2Index, Ch0, [(Min..Max)-Index|Ch2]) :-
	(   ID\==[], get_assoc(ID, ID2Index, Index) -> true
	;   Index = []
	),
	case_children(L1, ID2Index, Ch0, Ch2).

case_map([], _, [], A, A).
case_map([node(ID,_,_)|Nodes], I, [_|Nodes1], A0, A) :-
	put_assoc(ID, A0, I, A1),
	J is I+1,
	case_map(Nodes, J, Nodes1, A1, A).

/*** bitmask set representation

descendants(node(Index,Set,Children), Set, NodeTree) :-
	(   nonvar(Set) -> true
	;   Set0 is 1<<Index,
	    children_descendants(Children, Set0, Set, NodeTree)
	).

children_descendants([], Set, Set, _).
children_descendants([_-Ix|Children], Set0, Set, NodeTree) :-
	(   integer(Ix)
	->  J is Ix+1,
	    get_label(J, NodeTree, Node),
	    descendants(Node, Set1, NodeTree),
	    Set2 is Set0\/Set1
	;   Set2 = Set0
	),
	children_descendants(Children, Set2, Set, NodeTree).

***/

/*** fdset representation ***/

descendants(node(Index,Set,Children), Set, NodeTree) :-
	(   nonvar(Set) -> true
	;   fdset_singleton(Set0, Index),
	    children_descendants(Children, Set0, Set, NodeTree)
	).

children_descendants([], Set, Set, _).
children_descendants([_-Ix|Children], Set0, Set, NodeTree) :-
	(   integer(Ix)
	->  J is Ix+1,
	    get_label(J, NodeTree, Node),
	    descendants(Node, Set1, NodeTree),
	    fdset_union(Set0, Set1, Set2)
	;   Set2 = Set0
	),
	children_descendants(Children, Set2, Set, NodeTree).


/****************************************************************/
/* cumulatives/[2,3]  						*/
/****************************************************************/

cumulatives(Tasks0, Machines) :-
	cumulatives(Tasks0, Machines, []).
		
cumulatives(Tasks, Machines, Options) :-
	Goal = cumulatives(Tasks, Machines, Options),
	cumulatives_options(Options, 0, Opt, Goal, 3),
	cum_minmax_tasks(Tasks, Susp, []),
	cum_minmax_fields(Tasks, L1, L2, L3, L4, L5),
	must_be_list_of_finite_dvar(L1, Goal, 1),
	must_be_list_of_finite_dvar(L2, Goal, 1),
	must_be_list_of_finite_dvar(L3, Goal, 1),
	must_be_list_of_finite_dvar(L4, Goal, 1),
	must_be_list_of_finite_dvar(L5, Goal, 1),
	length(Tasks, NT),
	length(Machines, NM),
	fd_global(Goal, f(NT,NM,Opt,Tasks,Machines,NT,0,_Handle,0), Susp).

cumulatives_options(L, _, _, Goal, ArgNo) :- 
	var(L), !,
	fd_illarg(var, Goal, ArgNo).
cumulatives_options([], Opt, Opt, _, _) :- !.
cumulatives_options([X|L], Opt0, Opt, Goal, ArgNo) :- !,
	(   cumulatives_option(X, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,cumulatives_option), Goal, ArgNo, X)
        ),
	cumulatives_options(L, Opt1, Opt, Goal, ArgNo).
cumulatives_options(L, _, _, Goal, ArgNo) :- 
	fd_illarg(domain(term,list), Goal, ArgNo, L).

%%% Valid options:
%%% bound(lower|upper)   ==> 0x0|0x1
%%% generalization(Bool) ==> 0x0|0x2
%%% task_intervals(Bool) ==> 0x0|0x4
%%% prune(all|next)      ==> 0x0|0x8

cumulatives_option(-, _, _) :- !, fail.
cumulatives_option(bound(B), Opt0, Opt) :-
	aux_option(bound, B, Value),
	Opt is (Opt0 /\ -2) \/ Value.
cumulatives_option(generalization(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -3) \/ (Value<<1).
cumulatives_option(task_intervals(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -9) \/ (Value<<2).
cumulatives_option(prune(B), Opt0, Opt) :-
	aux_option(prune, B, Value),
	Opt is (Opt0 /\ -17) \/ (Value<<3).

cum_minmax_tasks([]) --> [].
cum_minmax_tasks([task(Org,Dur,End,Height,Machine)|Tasks]) -->
	on_minmax([Org,Dur,End,Height]),
	on_dom([Machine]),
	cum_minmax_tasks(Tasks).

cum_minmax_fields([], [], [], [], [], []).
cum_minmax_fields([task(A,B,C,D,E)|Tasks],
		  [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es]) :-
	cum_minmax_fields(Tasks, As, Bs, Cs, Ds, Es).

/****************************************************************/
/* global_cardinality/2						*/
/****************************************************************/

global_cardinality(Vars, Vals) :-
	Goal = global_cardinality(Vars,Vals),
	must_be_dvar_list(Vars, Goal, 1),
	length(Vars, N),
	val_keys(Vals, Goal, 2, DomList, Counts),
	list_to_fdset(DomList, DomSet),
	length(DomList, M),
	'$fd_size'(DomSet, M, 1), !, % no duplicates
	keysort(Vals, SVals),
	domain(Vars, DomSet),
	domain(Counts, 0, N),
	on_dom(Vars, Susp, Susp1),
	on_minmax(Counts, Susp1, []),
	fd_global(global_cardinality(Vars,Vals),
		  f(N,M,Vars,SVals,_Handle,0),
		  Susp).
global_cardinality(Vars, Vals) :-
	fd_illarg(domain(list(callable),global_cardinality_value_list),
		  global_cardinality(Vars,Vals), 2).

val_keys(V, _Goal, _ArgNo, _, _) :- var(V), !, fail.
val_keys([], _Goal, _ArgNo, [], []).
val_keys([Val|Vals], Goal, ArgNo, [K|Ks], [Count|Counts]) :-
	nonvar(Val), Val = K-Count,
	arg_attribute(Count, _, Goal, ArgNo),
	val_keys(Vals, Goal, ArgNo, Ks, Counts).

on_dom([]) --> [].
on_dom([X|Xs]) --> [dom(X)], on_dom(Xs).

on_minmax([]) --> [].
on_minmax([X|Xs]) --> [minmax(X)], on_minmax(Xs).

%%% Option parsing

bool_option(-, _) :- !, fail.
bool_option(false, 0).
bool_option(true, 1).

aux_option(_, -, _) :- !, fail.
aux_option(bound, lower, 0) :- !.
aux_option(bound, upper, 1).
aux_option(prune, all, 0) :- !.
aux_option(prune, next, 1).

