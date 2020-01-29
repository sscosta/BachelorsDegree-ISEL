/* Copyright(C) 1997, Swedish Institute of Computer Science */

:- meta_predicate
	labeling(:, ?),
	minimize(:, ?),
	maximize(:, ?).

:- dynamic incumbent/2.

labeling(Options, Variables) :-
	Goal = labeling(Options,Variables),
	Options = _:Options2,
	labeling_options(Options2, opt(leftmost,all,step(up),_,33554431),
				   opt(Sel,Sol,Enum,K,DU),
				   Variables, Goal),
	must_be_list_of_finite_dvar(Variables, Goal, 2),
	labeling(Sol, Variables, Sel, Enum, K, DU).

labeling_options(L, _, _, _, Goal) :- 
	var(L), !,
	fd_illarg(var, Goal, 1).
labeling_options([], Opt, Opt, _, _) :- !.
labeling_options([X|L], Opt0, Opt, Vs, Goal) :- !,
	(   labeling_option(X, Goal, Opt0, Opt1) -> true
        ;   fd_illarg(domain(term,labeling_options), Goal, 1, X)
        ),
	labeling_options(L, Opt1, Opt, Vs, Goal).
labeling_options(_, _, _, _, Goal) :- 
	fd_illarg(domain(term,labeling_options), Goal, 1).

labeling_option(-, _, _, _) :- !, fail.
% statistics option:
labeling_option(assumptions(K), _, opt(Sel,Sol,Enum,_,DU), opt(Sel,Sol,Enum,K,DU)).
% variable choice options:
labeling_option(leftmost, _, opt(_,Sol,Enum,K,DU), opt(leftmost,Sol,Enum,K,DU)).
labeling_option(min, _, opt(_,Sol,Enum,K,DU), opt(min,Sol,Enum,K,DU)).
labeling_option(max, _, opt(_,Sol,Enum,K,DU), opt(max,Sol,Enum,K,DU)).
labeling_option(ff, _, opt(_,Sol,Enum,K,DU), opt(ff,Sol,Enum,K,DU)).
labeling_option(ffc, _, opt(_,Sol,Enum,K,DU), opt(ffc,Sol,Enum,K,DU)).
labeling_option(variable(Sel), Goal, opt(_,Sol,Enum,K,DU),
				     opt(variable(M:Sel),Sol,Enum,K,DU)) :-
	Goal = labeling(M:_,_).
% value choice options
labeling_option(discrepancy(DU), _, opt(Sel,Sol,Enum,K,_), opt(Sel,Sol,Enum,K,DU)).
labeling_option(enum, _, opt(Sel,Sol,Enum,K,DU), opt(Sel,Sol,enum(A),K,DU)) :-
	arg(1, Enum, A).
labeling_option(step, _, opt(Sel,Sol,Enum,K,DU), opt(Sel,Sol,step(A),K,DU)) :-
	arg(1, Enum, A).
labeling_option(bisect, _, opt(Sel,Sol,Enum,K,DU), opt(Sel,Sol,bisect(A),K,DU)) :-
	arg(1, Enum, A).
labeling_option(up, _, opt(Sel,Sol,Enum0,K,DU), opt(Sel,Sol,Enum,K,DU)) :-
	Enum0 =.. [F|_], Enum =.. [F,up].
labeling_option(down, _, opt(Sel,Sol,Enum0,K,DU), opt(Sel,Sol,Enum,K,DU)) :-
	Enum0 =.. [F|_], Enum =.. [F,down].
labeling_option(value(Enum), Goal, opt(Sel,Sol,_,K,DU),
				   opt(Sel,Sol,value(M:Enum),K,DU)) :-
	Goal = labeling(M:_,_).
% solution options
labeling_option(all, _, opt(Sel,_,Enum,K,DU), opt(Sel,all,Enum,K,DU)).
labeling_option(minimize(X), Goal, opt(Sel,_,Enum,K,DU),
	                           opt(Sel,minimize(X,Goal),Enum,K,DU)) :-
	arg_attribute(X, _, Goal, 1).
labeling_option(maximize(X), Goal, opt(Sel,_,Enum,K,DU),
	                           opt(Sel,maximize(X,Goal),Enum,K,DU)) :-
	arg_attribute(X, _, Goal, 1).

labeling(all, Variables, Sel, Enum, K, DU) :-
	'$fd_debugging'(FDBG),
	nobb_labeling(Variables, param(Sel,Enum,FDBG), 0, K, nobb(DU)).
labeling(minimize(Value,Goal), Variables, Sel, Enum, K, DU) :-
	list_of_zeros(Variables, Zeros),
	'$fd_minint_maxint'(_Minint, Maxint),
	asserta(incumbent(Maxint,Zeros), Ref),
	call_cleanup(bb_labeling(Variables, Sel, Enum, 0, K,
	                         bb(minimize(Value),Ref,Goal,DU)),
	             erase(Ref)).
labeling(maximize(Value,Goal), Variables, Sel, Enum, K, DU) :-
	list_of_zeros(Variables, Zeros),
	'$fd_minint_maxint'(Minint, _Maxint),
	asserta(incumbent(Minint,Zeros), Ref),
	call_cleanup(bb_labeling(Variables, Sel, Enum, 0, K,
	                         bb(maximize(Value),Ref,Goal,DU)),
	             erase(Ref)).

list_of_zeros([], []).
list_of_zeros([_|L1], [0|L2]) :- list_of_zeros(L1, L2).

bb_labeling(Variables, Sel, Enum, 0, K, BB) :-
	BB = bb(MinMax,Ref,Goal,_),
	'$fd_debugging'(FDBG),
	nobb_labeling(Variables, param(Sel,Enum,FDBG), 0, K, BB),
	arg(1, MinMax, Value),
	(   var(Value) -> fd_illarg(var, Goal, 1)
        ;   prolog:'$ptr_ref'(Ptr, Ref),
	    '$fd_update_incumbent'(Ptr, Value, Variables)
        ),
	fail.
bb_labeling(Variables, _, _, _, _, bb(minimize(Value),Ref,_,_)) :- !,
	% purify_vars([Value|Variables]), -- leaks permanent term refs
	clause(incumbent(Value,Variables), true, Ref),
	'$fd_minint_maxint'(_Minint, Maxint),
	Value < Maxint.
bb_labeling(Variables, _, _, _, _, bb(maximize(Value),Ref,_,_)) :-
	% purify_vars([Value|Variables]), -- leaks permanent term refs
	clause(incumbent(Value,Variables), true, Ref),
	'$fd_minint_maxint'(Minint, _Maxint),
	Value > Minint.


nobb_labeling([], _, K, K, _).
nobb_labeling(LL, Param, I, K, BB) :-
	LL = [X|L],
	var(X), !,
	Param = param(Selector,Enum,FDBG),
	delete(Selector, X, L, X1, L1),
	fdbg_start_labeling(FDBG, X1),
	labeling_cont(Enum, X1, L1, LL, R, BB, BB1),
	J is I+1,
	nobb_labeling(R, Param, J, K, BB1).
nobb_labeling([_|L], Param, I, K, BB) :-
	nobb_labeling(L, Param, I, K, BB).


%% SzT 2001.09.10, changes for FDBG

% to indicate the start and failure of labeling
% FDBG puts advice on this!
fdbg_start_labeling(0, _Var).
fdbg_start_labeling(1, _Var).
fdbg_start_labeling(1, _Var) :- fail.

% to indicate one labeling step in user-defined labeling
% FDBG puts advice on this!
fdbg_labeling_step(_Var, _Step).

% the built-in labeling uses the following predicates to indicate a
% labeling step and to make the appropriate narrowing (reducing the
% domain to a singleton, changing the maximum, or changing the minimum,
% respectively)

% FDBG puts advice on this!
labeling_singleton(T, C, _Mode) :-
	'$fd_in_interval'(T, C, C, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
labeling_max(T, C, _Mode) :-
	'$fd_in_interval'(T, inf, C, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
labeling_min(T, C, _Mode) :-
	'$fd_in_interval'(T, C, sup, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).


labeling_cont(value(M:Enum), X, L, LL, LL, BB, BB1) :-
	call(Enum, M, X, L, BB, BB1).
labeling_cont(enum(Arg), X, L, _LL, L, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,_Min,_Max,_Size),
	indomain(Arg, X, Set, BB, BB1).
labeling_cont(step(Arg), X, L, LL, R, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_step(Arg, Min, Max, X, L, LL, R, BB, BB1).
labeling_cont(bisect(Arg), X, _L, LL, LL, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_bisect(Arg, Min, Max, X, BB, BB1).

/*** obsolete as of 3.9
labeling_dual(up, X, _, Var, BB, BB) :-
	labeling_singleton(Var, X, dual).
labeling_dual(up, _, X, Var, BB, BB1) :-
	later_bound(BB, BB1),
	labeling_singleton(Var, X, dual).
labeling_dual(down, _, X, Var, BB, BB) :-
	labeling_singleton(Var, X, dual).
labeling_dual(down, X, _, Var, BB, BB1) :-
	later_bound(BB, BB1),
	labeling_singleton(Var, X, dual).
***/

labeling_step(up, Min, _, X, L, _,  L, BB, BB) :-
	labeling_singleton(X, Min, step).
labeling_step(up, Min, _, X,   _, LL, LL, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: keeps X
	Min1 is Min+1, 
	labeling_min(X, Min1, step).
labeling_step(down, _, Max, X, L, _,  L, BB, BB) :-
	labeling_singleton(X, Max, step).
labeling_step(down, _, Max, X,   _, LL, LL, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: keeps X
	Max1 is Max-1, 
	labeling_max(X, Max1, step).

labeling_bisect(up, Min, Max, X, BB, BB) :-
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).
labeling_bisect(up, Min, Max, X, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: var count?
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).

labeling_bisect(down, Min, Max, X, BB, BB) :-
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).
labeling_bisect(down, Min, Max, X, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: var count?
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).


indomain(X) :-
	integer(X), !.
indomain(X) :-
	var(X),
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,_Min,_Max,Size),
	integer(Size), !,
	BB = nobb(Size),
	'$fd_debugging'(FDBG),
	fdbg_start_labeling(FDBG, X),
	indomain(up, X, Set, BB, _).
indomain(X) :-
	fd_argument_error(indomain(X), 1, X).

% precondition: X is not connected as in Constraint #<=> X
indomain(up, X, [[A|B]|R], BB, BB1) :-
	indomain(R, A, B, Val, BB, BB1),
	labeling_singleton(X, Val, indomain_up).
indomain(down, X, R, BB, BB1) :-
	reverse(R, [[A|B]|R1]),
	indomain_rev(R1, A, B, Val, BB, BB1),
	labeling_singleton(X, Val, indomain_down).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain([], A, A, V, BB, BB1) :- !,
	BB1 = BB,
	V = A.
indomain(_, A, _, A, BB, BB).
indomain(R, A, B, V, BB, BB1) :-
	A < B, !,
	A1 is A+1,
	indomain_later(R, A1, B, V, BB, BB1).
indomain([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_rev([], B, B, V, BB, BB1) :- !,
	BB1 = BB,
	V = B.
indomain_rev(_, _, B, B, BB, BB).
indomain_rev(R, A, B, V, BB, BB1) :-
	A < B, !,
	B1 is B-1,
	indomain_rev_later(R, A, B1, V, BB, BB1).
indomain_rev([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_rev_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_later([], A, A, V, BB, BB1) :- !,
	later_bound(BB, BB1),
	V = A.
indomain_later(_, A, _, V, BB, BB1) :-
	later_bound(BB, BB1),
	V = A.
indomain_later(R, A, B, V, BB, BB1) :-
	A < B, !,
	A1 is A+1,
	indomain_later(R, A1, B, V, BB, BB1).
indomain_later([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_rev_later([], B, B, V, BB, BB1) :- !,
	later_bound(BB, BB1),
	V = B.
indomain_rev_later(_, _, B, V, BB, BB1) :-
	later_bound(BB, BB1),
	V = B.
indomain_rev_later(R, A, B, V, BB, BB1) :-
	A < B, !,
	B1 is B-1,
	indomain_rev_later(R, A, B1, V, BB, BB1).
indomain_rev_later([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_rev_later(R, A, B, V, BB, BB1).

%%% The bounding rule if branch-and-bound search.

first_bound(BB, BB).

%% [MC] 3.8.6: made determinate
later_bound(nobb(DU0), nobb(DU)) :-
	DU0>0, DU is DU0-1.
later_bound(bb(minimize(Value),Ref,Goal,DU0),
	    bb(minimize(Value),Ref,Goal,DU)) :- !,
	DU0>0, DU is DU0-1,
	prolog:'$ptr_ref'(Ptr, Ref),
	'$fd_incumbent_bound'(Ptr, Bound),
	Min is Bound-1,
	'$fd_in_interval'(Value, inf, Min, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
later_bound(bb(maximize(Value),Ref,Goal,DU0),
	    bb(maximize(Value),Ref,Goal,DU)) :-
	DU0>0, DU is DU0-1,
	prolog:'$ptr_ref'(Ptr, Ref),
	'$fd_incumbent_bound'(Ptr, Bound),
	Max is Bound+1,
	'$fd_in_interval'(Value, Max, sup, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).


domain_set(dom(X,_,_,_), X).

domain_min(dom(_,X,_,_), X).

domain_max(dom(_,_,X,_), X).

domain_size(dom(_,_,_,X), X).

fd_min(X, Min) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_min(Dom, Min)
	;   Min = inf
	).
fd_min(X, Min) :-
	integer(X), !, Min = X.
fd_min(X, Min) :-
	fd_argument_error(fd_min(X,Min), 1, X).

fd_max(X, Max) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_max(Dom, Max)
	;   Max = sup
	).
fd_max(X, Max) :-
	integer(X), !, Max = X.
fd_max(X, Max) :-
	fd_argument_error(fd_max(X,Max), 1, X).

fd_size(X, Size) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_size(Dom, Size)
	;   Size = sup
	).
fd_size(X, Size) :-
	integer(X), !, Size = 1.
fd_size(X, Size) :-
	fd_argument_error(fd_size(X,Size), 1, X).

fd_degree(X, Degree) :-
	var(X), !,
	(   get_fd_suspensions(X, Lists)
	->  arg(1, Lists, Degree)
	;   Degree = 0
	).
fd_degree(X, Degree) :-
	integer(X), !, Degree = 0.
fd_degree(X, Degree) :-
	fd_argument_error(fd_degree(X,Degree), 1, X).

fd_set(X, Set) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_set(Dom, Set)
	;   Set = [[inf|sup]]
	).
fd_set(X, Set) :-
	integer(X),
	\+prolog:'$large_data'(0, X, _), !,
	Set = [[X|X]].
fd_set(X, Set) :-
	fd_argument_error(fd_set(X,Set), 1, X).

fd_dom(X, R) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_set(Dom, Set), fdset_to_range(Set, R)
	;   R = (inf..sup)
	).
fd_dom(X, R) :-
	integer(X),
	\+prolog:'$large_data'(0, X, _), !,
	R = {X}.
fd_dom(X, Dom) :-
	fd_argument_error(fd_dom(X,Dom), 1, X).

delete(leftmost, X, L, X, L).
delete(min, X, L, X1, L1) :-
	fd_min(X, Rank),
	deletemin(L, Rank, X, L1, L2, X1, L2).
delete(max, X, L, X1, L1) :-
	fd_max(X, Rank),
	deletemax(L, Rank, X, L1, L2, X1, L2).
delete(ff, X, L, X1, L1) :-
	fd_rank(X, Rank),
	deleteff(L, Rank, X, L1, L2, X1, L2).
delete(ffc, X, L, X1, L1) :-
	fd_rankc(X, Rank),
	deleteffc(L, Rank, X, L1, L2, X1, L2).
delete(variable(M:Sel), X, L, X1, L1) :-
	call(Sel, M, [X|L], X1, L1).

call(M:Goal, _, A, B, C) :- !,
	call(Goal, M, A, B, C).
call(Goal, M, A, B, C) :-
	functor(Goal, F, Ar),
	Ar1 is Ar+1,
	Ar2 is Ar+2,
	Ar3 is Ar+3,
	functor(Call, F, Ar3),
	arg(Ar1, Call, A),
	arg(Ar2, Call, B),
	arg(Ar3, Call, C),
	copy_args(0, Ar, Goal, Call),
	M:Call.

call(M:Goal, _, A, B, C, D) :- !,
	call(Goal, M, A, B, C, D).
call(Goal, M, A, B, C, D) :-
	functor(Goal, F, Ar),
	Ar1 is Ar+1,
	Ar2 is Ar+2,
	Ar3 is Ar+3,
	Ar4 is Ar+4,
	functor(Call, F, Ar4),
	arg(Ar1, Call, A),
	arg(Ar2, Call, B),
	arg(Ar3, Call, C),
	arg(Ar4, Call, D),
	copy_args(0, Ar, Goal, Call),
	M:Call.

copy_args(Ar, Ar, _, _) :- !.
copy_args(I, N, Goal, Call) :-
	J is I+1,
	arg(J, Goal, X),
	arg(J, Call, X),
	copy_args(J, N, Goal, Call).
	

% deletexx(+Vars, +Rank, +Best, -Before, +After, -Var, -Suffix).

deletemin([], _, X, L, L, X, []).
deletemin([X0|L0], Rank1, X1, B, A, X, S) :-
	(   integer(X0)
	->  deletemin(L0, Rank1, X1, B, A, X, S)
	;   fd_min(X0, Rank0),
	    (   Rank0 < Rank1
	    ->  B = [X1|A],
	        deletemin(L0, Rank0, X0, S, S1, X, S1)
	    ;   S = [X0|S1],			
		deletemin(L0, Rank1, X1, B, A, X, S1)
	    )
	).

deletemax([], _, X, L, L, X, []).
deletemax([X0|L0], Rank1, X1, B, A, X, S) :-
	(   integer(X0)
	->  deletemax(L0, Rank1, X1, B, A, X, S)
	;   fd_max(X0, Rank0),
	    (   Rank0 > Rank1
	    ->  B = [X1|A],
	        deletemax(L0, Rank0, X0, S, S1, X, S1)
	    ;   S = [X0|S1],		
		deletemax(L0, Rank1, X1, B, A, X, S1)
	    )
	).

deleteff(L0, 2, X, L, L, X, L0) :- !.
deleteff([], _, X, L, L, X, []).
deleteff([X0|L0], Rank1, X1, B, A, X, S) :-
	(   integer(X0)
	->  deleteff(L0, Rank1, X1, B, A, X, S)
	;   fd_rank(X0, Rank0),
	    (   Rank0 @< Rank1			% @=< was better for
	    ->					%  queens,eq20,donald,suudoku
	        B = [X1|A],
	        deleteff(L0, Rank0, X0, S, S1, X, S1)  % @< was better for
	    ;					%  eq10,partit,crypta,alpha,
		S = [X0|S1],			%  squares
		deleteff(L0, Rank1, X1, B, A, X, S1)
	    )
	).

deleteffc([], _, X, L, L, X, []).
deleteffc([X0|L0], Rank1, X1, B, A, X, S) :-
	(   integer(X0)
	->  deleteffc(L0, Rank1, X1, B, A, X, S)
	;   fd_rankc(X0, Rank0),
	    (   Rank0 @< Rank1
	    ->  B = [X1|A],
	        deleteffc(L0, Rank0, X0, S, S1, X, S1)
	    ;   S = [X0|S1],
		deleteffc(L0, Rank1, X1, B, A, X, S1)
	    )
	).

fd_rank(X, Dsize) :-
	get_fd_domain(X, Dom),
	domain_size(Dom, Dsize).

fd_rankc(X, Dsize-NegDegree) :-
	get_fd_domain(X, Dom),
	domain_size(Dom, Dsize),
	get_fd_suspensions(X, Lists),
	arg(1, Lists, Degree),
	NegDegree is -Degree.

minimize(Goal, Var) :-
	findall(Goal-Var, (Goal -> true), [Best1-UB1]),
	minimize(Goal, Var, Best1, UB1).

minimize(Goal, Var, _, UB) :- var(UB), !,
	fd_illarg(var, minimize(Goal,Var), 2).
minimize(Goal, Var, _, UB) :-
	Var #< UB,
	findall(Goal-Var, (Goal -> true), [Best1-UB1]), !,
	minimize(Goal, Var, Best1, UB1).
minimize(Goal, Var, Goal, Var).


maximize(Goal, Var) :-
	findall(Goal-Var, (Goal -> true), [Best1-LB1]),
	maximize(Goal, Var, Best1, LB1).

maximize(Goal, Var, _, LB) :- var(LB), !,
	fd_illarg(var, maximize(Goal,Var), 2).
maximize(Goal, Var, _, LB) :-
	Var #> LB,
	findall(Goal-Var, (Goal -> true), [Best1-LB1]), !,
	maximize(Goal, Var, Best1, LB1).
maximize(Goal, Var, Goal, Var).


%   order_resource(+Options, +Resource) 
%   where Options is a list of key words specifying a heuristic,
%   Resource represents a resource as returned by
%   serialized_resource/3 on which tasks must be serialized.  True if
%   a total ordering can be imposed on the task; enumerating all such
%   orderings via backtracking.
%   [first,est] (the default) and [last,lct] can be good heuristics.

order_resource(Options, Resource) :-
	order_options(Options, opt(first,est), opt(Edge,Key)),
	order_resource(Edge, Key, Resource).

order_options([]) --> [].
order_options([X|Xs]) --> order_option(X), order_options(Xs).

order_option(first, opt(_,Key), opt(first,Key)).
order_option(last, opt(_,Key), opt(last,Key)).
order_option(est, opt(Edge,_), opt(Edge,est)).
order_option(lst, opt(Edge,_), opt(Edge,lst)).
order_option(ect, opt(Edge,_), opt(Edge,ect)).
order_option(lct, opt(Edge,_), opt(Edge,lct)).


order_resource(first, Key, resource(Tasks,Matrix,Global)) :-
	order_resource_first(Tasks, Matrix, Global, Key).
order_resource(last, Key, resource(Tasks,Matrix,Global)) :-
	order_resource_last(Tasks, Matrix, Global, Key).

tasks_slack(Tasks, Slack) :-
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_slack(Tasks, 0, Dur, Maxint, EST, Minint, LCT),
	Slack is LCT-EST-Dur.

% useful in JSP heuristics
tasks_slack([], Dur, Dur, EST, EST, LCT, LCT).
tasks_slack([T|Tasks], Dur0, Dur, EST0, EST, LCT0, LCT) :-
	task(dur, T, Dur1),
	task(est, T, EST1),
	task(lct, T, LCT1),
	Dur2 is Dur0+Dur1,
	EST2 is min(EST0,EST1),
	LCT2 is max(LCT0,LCT1),
	tasks_slack(Tasks, Dur2, Dur, EST2, EST, LCT2, LCT).

tasks_duration([], Dur, Dur).
tasks_duration([T|Tasks], Dur0, Dur) :-
	task(dur, T, Dur1),
	Dur2 is Dur0 + Dur1,
	tasks_duration(Tasks, Dur2, Dur).

% Val1 (Val2) is the smallest (next smallest) Key value of Tasks
tasks_smallest([], _, Val1, Val1, Val2, Val2).
tasks_smallest([T|Tasks], Key, Val1a, Val1, Val2a, Val2) :-
	task(Key, T, New),
	Val1b is min(Val1a,New),
	Val2b is max(Val1a,min(Val2a,New)),
	tasks_smallest(Tasks, Key, Val1b, Val1, Val2b, Val2).

% Val1 (Val2) is the greatest (next greatest) Key value of Tasks
tasks_greatest([], _, Val1, Val1, Val2, Val2).
tasks_greatest([T|Tasks], Key, Val1a, Val1, Val2a, Val2) :-
	task(Key, T, New),
	Val1b is max(Val1a,New),
	Val2b is min(Val1a,max(Val2a,New)),
	tasks_greatest(Tasks, Key, Val1b, Val1, Val2b, Val2).

% {t in Tasks | total duration =< LCT(Tasks)-EST(t)} can be first in Tasks
order_resource_first([], _Mat, _Global, _Key) :- !.
order_resource_first([_], _Mat, _Global, _Key) :- !.
order_resource_first(_Tasks, _Mat, Global, _Key) :-
	Global = global(_,_,_,Ent,_), nonvar(Ent), !.
order_resource_first(Tasks, Mat, Global, Key) :-
	Order = [],
	/* too expensive:
           tasks_order(Tasks, Mat, Order),
	   */
	can_be_first(Tasks, Order, Key, First0),
	keysort(First0, First),
	order_resource_first(Tasks, First, Mat, Global, Key).

% {t in Tasks | total duration =< LCT(t)-EST(Tasks)} can be last in Tasks
order_resource_last([], _Mat, _Global, _Key) :- !.
order_resource_last([_], _Mat, _Global, _Key) :- !.
order_resource_last(_Tasks, _Mat, Global, _Key) :-
	Global = global(_,_,_,Ent,_), nonvar(Ent), !.
order_resource_last(Tasks, Mat, Global, Key) :-
	Order = [],
	/* too expensive:
           tasks_order(Tasks, Mat, Order),
	   */
	can_be_last(Tasks, Order, Key, Last0),
	keysort(Last0, Last),
	order_resource_last(Tasks, Last, Mat, Global, Key).

order_resource_first(Tasks, [_-F|_], Mat, Global, Key) :-
	task(id, F, I),
	tell_after(Tasks, I, Mat, Rest),
	'$fd_global_enqueue'(Global),
	evaluate(2, Global),
	order_resource_first(Rest, Mat, Global, Key).
order_resource_first(Tasks, [_|First], Mat, Global, Key) :-
	order_resource_first(Tasks, First, Mat, Global, Key).

order_resource_last(Tasks, [_-F|_], Mat, Global, Key) :-
	task(id, F, I),
	tell_before(Tasks, I, Mat, Rest),
	'$fd_global_enqueue'(Global),
	evaluate(2, Global),
	order_resource_last(Rest, Mat, Global, Key).
order_resource_last(Tasks, [_|Last], Mat, Global, Key) :-
	order_resource_last(Tasks, Last, Mat, Global, Key).

can_be_first(Tasks, Order, Key, First0) :-
	tasks_duration(Tasks, 0, Dur),
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_smallest(Tasks, lst, Maxint, LST1, Maxint, LST2),
	tasks_greatest(Tasks, lct, Minint, LCT1, Minint, LCT2),
	can_be_first(Tasks, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First0).

can_be_first([], _, _, _, _, _, _, _, []).
can_be_first([T|Rest], Order, Dur, LCT1, LCT2, LST1, LST2, Key, [Rank-T|First]) :-
	task(all, T, _Id, EST, LST, ECT, LCT),
	(   LCT=:=LCT1 -> LCT2-EST >= Dur; LCT1-EST >= Dur   ),
	(   LST=:=LST1 -> ECT =< LST2; ECT =< LST1   ),
	/* too expensive:
	   get_assoc(Id, Order, Min-_), Min<0,
	   */ !,
	task(Key, T, Rank),
	can_be_first(Rest, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First).
can_be_first([_|Rest], Order, Dur, LCT1, LCT2, LST1, LST2, Key, First) :-
	can_be_first(Rest, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First).

can_be_last(Tasks, Order, Key, Last0) :-
	tasks_duration(Tasks, 0, Dur),
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_greatest(Tasks, ect, Minint, ECT1, Minint, ECT2),
	tasks_smallest(Tasks, est, Maxint, EST1, Maxint, EST2),
	can_be_last(Tasks, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last0).

can_be_last([], _, _, _, _, _, _, _, []).
can_be_last([T|Rest], Order, Dur, EST1, EST2, ECT1, ECT2, Key, [Rank-T|Last]) :-
	task(all, T, _Id, EST, LST, ECT, LCT),
	(   EST=:=EST1 -> LCT-EST2 >= Dur; LCT-EST1 >= Dur   ),
	(   ECT=:=ECT1 -> LST >= ECT2; LST >= ECT1   ),
	/* too expensive:
           get_assoc(Id, Order, _-Max), Max>0,
	   */ !,
	task(Key, T, Rank0), Rank is -Rank0,
	can_be_last(Rest, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last).
can_be_last([_|Rest], Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last) :-
	can_be_last(Rest, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last).

tell_after([], _I, _Mat, []).
tell_after([Tj|Tasks], I, Mat, [Tj|Rest]) :-
	task(id, Tj, J),
	I =\= J, !,
	dvar_gtz(I, J, Mat),
	tell_after(Tasks, I, Mat, Rest).
tell_after([_|Tasks], I, Mat, Rest) :-
	tell_after(Tasks, I, Mat, Rest).

tell_before([], _I, _Mat, []).
tell_before([Tj|Tasks], I, Mat, [Tj|Rest]) :-
	task(id, Tj, J),
	I =\= J, !,
	dvar_gtz(J, I, Mat),
	tell_before(Tasks, I, Mat, Rest).
tell_before([_|Tasks], I, Mat, Rest) :-
	tell_before(Tasks, I, Mat, Rest).

task(id, task(_,_,_,_,_,_,I,_), I).
task(dur, task(_,_,_,DurM,_,_,_,_), Dur) :-
	get_mutable_rec(DurD, DurM),
	DurD = dom(_,Dur,_,_).
task(est, task(_,SM,_,_,_,_,_,_), Est) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,Est,_,_).
task(lst, task(_,SM,_,_,_,_,_,_), Lst) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,_,Lst,_).
task(ect, task(_,SM,_,DurM,_,_,_,_), Ect) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,Est,_,_),
	get_mutable_rec(DurD, DurM),
	DurD = dom(_,Dur,_,_),
	Ect is Est + Dur.
task(lct, task(_,SM,_,DurM,_,_,_,_), Lct) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,_,Lst,_),
	get_mutable_rec(DurD, DurM),
	DurD = dom(_,Dur,_,_),
	Lct is Lst + Dur.
task(all, task(_,SM,_,DurM,_,_,Id,_), Id, Est, Lst, Ect, Lct) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,Est,Lst,_),
	get_mutable_rec(DurD, DurM),
	DurD = dom(_,Dur,_,_),
	Ect is Est + Dur,
	Lct is Lst + Dur.


dvar_gtz(I, J, Diffs) :- I < J, !,
	memberchk(d(I,J,M), Diffs),
	get_mutable(Set0, M),
	'$fd_dom_intersection'(Set0, [[1|sup]], Set), % D #> 0
	update_mutable(Set, M).
dvar_gtz(I, J, Diffs) :-
	memberchk(d(J,I,M), Diffs),
	get_mutable(Set0, M),
	'$fd_dom_intersection'(Set0, [[inf|-1]], Set), % D #< 0
	update_mutable(Set, M).

end_of_file.

tasks_order(Tasks, Mat, Order) :-
	initial_order(Tasks, List),
	ord_list_to_assoc(List, Order0),
	get_order_from_matrix(Mat, 0, Order0, Order).

initial_order([], []).
initial_order([T|Tasks], [I-(Minint-Maxint)|Order]) :-
	'$fd_minint_maxint'(Minint, Maxint),
	task(id, T, I),
	initial_order(Tasks, Order).

get_order_from_matrix([], _, Order, Order).
get_order_from_matrix([Row|Mat], I, Order0, Order) :-
	J is I+1,
	get_assoc(I, Order0, IPair0, Order1, IPair), !,
	get_order_from_row(Row, J, IPair0, IPair, Order1, Order2),
	I1 is I+1,
	get_order_from_matrix(Mat, I1, Order2, Order).
get_order_from_matrix([_|Mat], I, Order0, Order) :-
	I1 is I+1,
	get_order_from_matrix(Mat, I1, Order0, Order).

get_order_from_row([], _, IPair, IPair, Order, Order).
get_order_from_row([Elt|Row], J, IPair0, IPair, Order0, Order) :-
	get_order_from_elt(Elt, J, IPair0, IPair1, Order0, Order1),
	J1 is J+1,
	get_order_from_row(Row, J1, IPair1, IPair, Order1, Order).

get_order_from_elt(Elt, J, IPair0, IPair, Order0, Order) :-
	get_assoc(J, Order0, JPair0, Order, JPair), !,
	get_order_from_elt(Elt, IPair0, IPair, JPair0, JPair).
get_order_from_elt(_, _, IPair, IPair, Order, Order).

get_order_from_elt(_-M, Imin0-Imax0, Imin-Imax, Jmin0-Jmax0, Jmin-Jmax) :-
	get_mutable(D, M),
	D = dom(_,Min,Max,_),
	Imin is max(Imin0,-Max),
	Imax is min(Imax0,-Min),
	Jmin is max(Jmin0,Min),
	Jmax is min(Jmax0,Max).
