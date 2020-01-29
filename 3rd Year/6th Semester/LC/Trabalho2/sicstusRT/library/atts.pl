/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : atts.pl
%   Author     : Christian Holzbaur
%   Updated    : 3 September 1999
%   Purpose    : Expansions and library for programs using 
%              : attributed variables.

% This file has to be loaded at compile time in order to define
% expansions. It is also needed at runtime for expansion of dynamic calls to
% {get,put}_atts/2.

:- module(attributes, [atts_subset/3]).

:- use_module(library(lists), [
	member/2,
	memberchk/2
			      ]).

:- op(1150, fx, [attribute]).

% -----------------------------------------------------------------------

:- multifile
	user:term_expansion/2,
	user:goal_expansion/3.
:- dynamic
	user:term_expansion/2,
	user:goal_expansion/3.


user:term_expansion((:- attribute(Decls)), Exp) :- !,
	attribute_exp(Decls, Exp, []).

user:goal_expansion(get_atts(Var,Spec), Module, Exp) :- !,
	expand_get_atts(Var, Spec, Module, Exp).
user:goal_expansion(put_atts(Var,Spec), Module, Exp):- !,
	expand_put_atts(Var, Spec, Module, Exp).

% -----------------------------------------------------------------------

attribute_exp(Decls) -->
	[(:- prolog:'$save_attribute_info'(Module)),
	 (:- use_module(attributes, library(atts), [atts_subset/3]))],
	{prolog_load_context(module, Module),
	 conj_to_list_functor(Decls, Attrs, 1, Size),
	 functor(Vector, v, Size)},
	compute_att_mask(Attrs, Module, Vector).

conj_to_list_functor((N/A,B), [H|Rest], I, K) :- !,
	functor(H, N, A),
	J is I+A,
	conj_to_list_functor(B, Rest, J, K).
conj_to_list_functor(N/A, [H], I, K) :- 
	functor(H, N, A),
	K is I+A.

% -----------------------------------------------------------------------
% Preprocessing of the set of attributes producing data used by
% the subsequent inline expansion of get/put_attr goals

compute_att_mask(Atts, Module, Vector) -->
	[(:- dynamic '$t2v'/3),
	 '$t2v'('$bitv'(Mask),Vector,0)],
	{arg(1, Vector, Mask),
	 retractall(Module:'$t2v'(_,_,_)),
	 assert(Module:'$t2v'('$bitv'(Mask),Vector,0))},
	compute_att_mask(Atts, Module, Vector, 1, 1),
	[(:- dynamic '$v2l'/2),
	 '$v2l'(Vector,Atts)],
	{retractall(Module:'$v2l'(_,_)),
	 assert(Module:'$v2l'(Vector,Atts))}.

compute_att_mask([], _, _, _, _) --> [].
compute_att_mask([Att|Atts], Module, Vector, I, M) -->
	['$t2v'(Att,Vector,M)],
	{functor(Att, _, A),
	 J is I+A,
	 equate_args(A, Att, J, Vector),
	 assert(Module:'$t2v'(Att,Vector,M)),
	 N is M<<1},
	compute_att_mask(Atts, Module, Vector, J, N).

equate_args(0, _, _, _) :- !.
equate_args(A, T1, C, T2) :-
	arg(A, T1, X),
	arg(C, T2, X),
	B is A-1, D is C-1,
	equate_args(B, T1, D, T2).



% -----------------------------------------------------------------------
% Goal expansion of {get,put}_atts/2

expand_get_atts(Var, Spec, Module, Exp) :-
	nonvar(Spec), !,
	partition_spec(Spec, SpecP, [], SpecN, []),
	sanity(SpecP, get_atts(Var,Spec), Module),
	sanity(SpecN, get_atts(Var,Spec), Module),
	l2v(SpecP, Module, Vector, 0, SpecPM),
	l2v(SpecN, Module, Vector, 0, SpecNM),
	all_mask(Module, SpecAll),
	expand_get_atts(SpecPM, SpecNM, SpecAll, get_atts(Var,Spec), Module,
	                Vector, Exp).
expand_get_atts(Var, Spec, _,
	        ('$get_attributes'(Var,Vector,Mask),
		 (var(Vector) -> Spec=[] ;
	         '$v2l'(Vector,Atts),
	         atts_subset(Atts,Mask,Spec)))).

expand_get_atts(P, N, _, Goal, Module, _, _) :-
	P /\ N =\= 0, !,
	expand_exception(Goal, Module).
expand_get_atts(0, 0, _, Goal, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = ('$get_attributes'(Var,_,_)).
expand_get_atts(0, All, All, Goal, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = ('$get_attributes'(Var,_,0)).
expand_get_atts(0, N, _, Goal, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = ('$get_attributes'(Var,_,Mask),
		Mask /\ N =:= 0).
expand_get_atts(P, N, All, Goal, _, Vector, Body) :-
	P\/N =:= All, !,
	arg(1, Goal, Var),
	Body = ('$get_attributes'(Var,Vector0,Mask),
		Mask = P,
		Vector0 = Vector).
expand_get_atts(P, N, _, Goal, _, Vector, Body) :-
	arg(1, Goal, Var),
	PN is P\/N,
	Body = ('$get_attributes'(Var,Vector0,Mask),
		Mask /\ PN =:= P,
		Vector0=Vector).



expand_put_atts(Var, Spec, Module, Exp) :-
	nonvar(Spec), !,
	partition_spec(Spec, SpecP, [], SpecN, []),
	sanity(SpecP, put_atts(Var,Spec), Module),
	sanity(SpecN, put_atts(Var,Spec), Module),
	all_atts(Module, All),
	put_exp(All, SpecP, SpecN, VOld, VNew, 0, PM, 0, NM),
	all_mask(Module, SpecAll),
	expand_put_atts(PM, NM, SpecAll, put_atts(Var,Spec), Module, VOld, VNew, Exp).
expand_put_atts(Var, Spec, _, _) :-
	prolog:illarg(var, put_atts(Var,Spec), 2).
   
expand_put_atts(P, N, _, Goal, Module, _, _, _) :-
	P /\ N =\= 0, !,
	expand_exception(Goal, Module).
expand_put_atts(0, 0, _, Goal, _, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = '$get_attributes'(Var,_,_).
expand_put_atts(0, All, All, Goal, _, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = '$delete_attributes'(Var).
expand_put_atts(0, N, _, Goal, _, VOld, VNew, Body) :- !,
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = ('$get_attributes'(Var,V,OldMask),
		NewMask is OldMask /\ NN,
		NewMask =\= OldMask ->
		V = VOld,
		'$put_attributes'(Var, VNew)
	       ;true
	       ).
expand_put_atts(P, N, All, Goal, _, _, VNew, Body) :-
	P\/N =:= All, !,
	arg(1, Goal, Var),
	arg(1, VNew, P),
	Body = '$put_attributes'(Var, VNew).
expand_put_atts(P, 0, _, Goal, _, VOld, VNew, Body) :- !,
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	Body = ('$get_attributes'(Var,V,OldMask),
		NewMask is OldMask \/ P,
		V = VOld,
		'$put_attributes'(Var, VNew)
	       ).
expand_put_atts(P, N, _, Goal, _, VOld, VNew, Body) :-
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = ('$get_attributes'(Var,V,OldMask),
		NewMask is (OldMask \/ P) /\ NN,
		V = VOld,
		'$put_attributes'(Var, VNew)
	       ).

partition_spec(X, _, _, _, _) :- var(X), !, fail. 
partition_spec(+(X), [X|P], P, N, N) :- !.
partition_spec(-(X), P, P, [Y|N], N) :- !,
	functor(X, F, A),
	functor(Y, F, A).
partition_spec([], P0, P0, N0, N0) :- !.
partition_spec([S|Ss], P2, P0, N2, N0) :- !,
	partition_spec(S, P1, P0, N1, N0),
	partition_spec(Ss, P2, P1, N2, N1).
partition_spec(X, [X|P], P, N, N).


expand_exception(Goal, Module) :-
	arg(2, Goal, Spec),
	partition_spec(Spec, Pos, [], Neg, []),
	member(Culprit1, Pos),
	functor(Culprit1, N, A),
	functor(Culprit2, N, A),
	member(Culprit2, Neg),
	prolog:illarg(consistency(+Culprit1,-Culprit2,clash), Module:Goal, 2, Spec).

sanity( [], _, _).
sanity( [S|Ss], Goal, Module) :-
	( is_t2v(Module, '$t2v'(S,_,_)) ->
	    sanity(Ss, Goal, Module)
	;
	    Module:'$v2l'(_,All),
	    prolog:illarg(domain(term,one_of(All)), Module:Goal, 2, S)
	).

l2v([], _, _, M0, M0).
l2v([T|Ts], Module, V, M0, M2) :-
	clause(Module:'$t2v'(T,V,Mask), _), !,
	M1 is M0 \/ Mask,
	l2v(Ts, Module, V, M1, M2).

all_mask(Module, Mask) :-
	findall(Bits, clause(Module:'$t2v'(_,_,Bits),_), All),
	all_mask(All, 0, Mask).

all_mask([], Mask, Mask).
all_mask([Bits|L], Mask0, Mask) :-
	Mask1 is Mask0\/Bits,
	all_mask(L, Mask1, Mask).

all_atts(Module, All) :-
	findall(T2V, is_t2v(Module,T2V), All).

is_t2v(Module, '$t2v'(T,Vect,Mask)) :-
	Module:'$t2v'(T,Vect,Mask), Mask =\= 0.

put_exp([], _, _, _, _, PM, PM, NM, NM).
put_exp(['$t2v'(T,Vec,Mask)|Ts], SpecP, SpecN, VOld, VNew, PM0, PM, NM0, NM) :-
	(   memberchk(T,SpecP) ->
	    PM1 is PM0 \/ Mask
	;   PM1 = PM0
	),
	(   memberchk(T,SpecN) ->
	    NM1 is NM0 \/ Mask
	;   NM1 = NM0
	),
	(   (PM1\/NM1)/\Mask =:= Mask -> true
	;   copy_term(T-Vec, T-VOld)
	),
	Vec = VNew,
	put_exp(Ts, SpecP, SpecN, VOld, VNew, PM1, PM, NM1, NM).

% exported - called by expanded code
atts_subset([], _, []).
atts_subset([Att|Atts1], Mask, Present) :-
	(   Mask/\1 =:= 1 -> Present = [Att|Present1]
	;   Present = Present1
	),
	Mask1 is Mask>>1,
	atts_subset(Atts1, Mask1, Present1).
