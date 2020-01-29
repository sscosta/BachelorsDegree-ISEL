/* Copyright(C) 1993-95, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Name runlib.pl
% Author: Seif Haridi
% Implementors: Mahmoud Rafea, Khaled Shaalan, Kent Boortz, and Seif Haridi
% Date: 1992, December 29
% Purpose: runtime routines for SICStus Objects
%
% Attributes & Instances added, inheritance mechanism changed. /san -94
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic super_sub/2.

%--------------------------------------------------------------------------
% Toplevel calls

(:: Msg) :-					% self not specified
	call_object(object, Msg, _).

(<: Msg) :-					% self not specified
	call_object(object, Msg, _).

(Object :: Msg) :-				% ordinary call
	call_object(Object, Msg, Object).

(Object <: Msg) :-				% delegation
	call_object(Object, Msg, _).


%--------------------------------------------------------------------------
% Runtime support

create_object(Object, Supers0, StatPreds, DynPreds, Attributes, SuperTree) :-
	Goal = create_object(Object,_,_,_,_,_),
	object_module(Object, Module),
	check_redefine(Module, Object, Goal),
	peel_of_nis(Supers0, Supers),
	check_supers(Supers, SuperTree, Goal),
	clear_object(Object, StatPreds, DynPreds),
	length(StatPreds, NSP),
	length(DynPreds, NDP),
	NrPreds0 is NSP + NDP,
	inherit_supers(Supers0, Module, Object, StatPreds, DynPreds, ISP, CDP, NrPreds0, NrPreds),
%	write(NrPreds), nl,
	prolog:'$make_lightweight_module'(Module, _, NrPreds),
	inh_sp(ISP),
	cp_dp(CDP),
	assert_super_sub(Supers, Object),
	init_attributes(Module, Attributes).

inh_sp([]).
inh_sp([import_static_preds(Type, SModule, Module, ImportPreds)|ISP]) :-
	import_static_preds(Type, SModule, Module, ImportPreds),
	inh_sp(ISP).

cp_dp([]).
cp_dp([copy_dynamic_preds(CopyPreds, SModule, Module, Super, Object)|CDP]) :-
	copy_dynamic_preds(CopyPreds, SModule, Module, Super, Object),
	cp_dp(CDP).

check_redefine(M, Object, _) :-
%% Existing object
	current_module(M),
	call_if_def('$so_type'(OldObject,_), M), !,
	(   source_file(M:'$so_type'(_,_), File),
	    prolog_load_context(file, File) ->
	    true
	;   print_message(warning, so_redefining(M,OldObject,Object))
	).
check_redefine(M, _, Goal) :-
%% Existing non-empty non-object module
	current_module(M),
	predicate_property(M:predicate_property(_,_), built_in),
	predicate_property(M:H, _),
	\+ predicate_property(M:H, built_in), !,
	perm_error(Goal, redefine, module, M).
check_redefine(prolog, _, Goal) :- !,
	perm_error(Goal, redefine, module, prolog).
check_redefine(_, _, _).
	
check_supers(Supers, tree(Object,SuperTrees), Goal) :-
	check_supers1(Supers, Object, SuperTrees, Goal).

peel_of_nis([], []).
peel_of_nis([S-_|Ss0], [S|Ss]) :- peel_of_nis(Ss0, Ss).

check_supers1([], _, Trees, Goal) :-
	(   Trees=[] -> true
	;   Trees=[tree(O,_)|_],
	    prolog:illarg(existence(object,O,0), Goal, 0)
	).
check_supers1([Super|Supers], Object, Trees0, Goal) :-
	(   Trees0 = [] ->
	    print_message(warning, so_additional_object(Object,Super))
	;   Trees0 = [tree(Super,STrees)|Trees],
	    object_module(Super, Module),
	    findall(S, call_if_def(super(S,_,_,_),Module), Ss),
	    check_supers1(Ss, Super, STrees, Goal),
	    check_supers1(Supers, Object, Trees, Goal)
	).

% This version is for dynamic objects
clear_object(Object) :-
	functor(Object, Name, Ar),
	functor(Skel, Name, Ar),
	object_module(Object, M),
	(   call_if_def(super(S,_,_,_), M),
	    retract(super_sub(S,Skel)),
	    fail
	;   has_instance(Object, Instance),
	    prolog:'$remove_module'(Instance, 0),
	    fail
	;   prolog:'$remove_module'(M, 0)
	).

% This version is for static objects where we can't abolish everything.
% If loading a .po file, the methods are installed before the create_object/6
% goal is encountered.
% StatPreds and DynPreds are ordsets of predicates to _keep_.
clear_object(Object, StatPreds, DynPreds) :-
	functor(Object, Name, Ar),
	functor(Skel, Name, Ar),
	object_module(Object, M),
	(   call_if_def(super(S,_,_,_), M),
	    retract(super_sub(S,Skel)),
	    fail
	;   has_instance(Object, Instance),
	    prolog:'$remove_module'(Instance, 0),
	    fail
	;   findall(Pred, current_pred(M, Pred), Preds0),
	    sort(Preds0, Preds1),
	    ord_subtract(Preds1, StatPreds, Preds2),
	    ord_subtract(Preds2, DynPreds, Preds),
	    abolish(M:Preds)
	).
	
current_pred(M, N/A) :-
	current_predicate(_, M:Head),
	functor(Head, N, A),
	N/A \== '$so_type'/2.

% unsatisfactory !
has_instance(Object, Instance) :-
	current_module(Instance),
	instance_class(Instance, Object).

assert_super_sub([], _).
assert_super_sub([S|Supers], Object) :-
	assert(super_sub(S,Object)),
	assert_super_sub(Supers, Object).

ensure_defined([]).
ensure_defined([M|Ms]) :-
	prolog:'$make_lightweight_module'(M, _, 2),
	ensure_defined(Ms).

%--------------------------------------------------------------------------

inherit_supers([], _, _, _, _, [], [], NP, NP).
inherit_supers([Super-NIS|Supers], Module, Object, StatPreds0, DynPreds0, ISP, CDP, NP0, NP) :-
	object_module(Super, SModule),
	static_methods_of(SModule, StatPreds1),
	ord_subtract(StatPreds1, NIS, StatPreds2),
	ord_subtract(StatPreds2, DynPreds0, StatPreds3),
	ord_union(StatPreds0, StatPreds3, StatPreds4, ImportPreds),
	call_apply('$so_type'(Super,Type), SModule),
	ISP = [import_static_preds(Type, SModule, Module, ImportPreds)|ISP0],
	length(ImportPreds, NSP),
	dynamic_methods_of(SModule, DynPreds1),
	ord_subtract(DynPreds1, NIS, DynPreds2),
	ord_subtract(DynPreds2, StatPreds0, DynPreds3),
	ord_union(DynPreds0, DynPreds3, DynPreds4, CopyPreds),
	CDP = [copy_dynamic_preds(CopyPreds, SModule, Module, Super, Object)|CDP0],
	length(CopyPreds, NDP),
	NP1 is NP0 + NSP + NDP,
	inherit_supers(Supers, Module, Object, StatPreds4, DynPreds4, ISP0, CDP0, NP1, NP).

static_methods_of(Module, Preds) :-
	findall(N/A, static_method(Module,N,A),	Preds0),
	sort(Preds0, Preds).

static_method(Module, N, A) :-
	visible_static(Head, Module),
	exclude(Head),
	functor(Head, N, A).

dynamic_methods_of(Module, Preds) :-
	findall(N/A, dynamic_method(Module,N,A), Preds0),
	sort(Preds0, Preds).

dynamic_method(Module, N, A) :-
	visible_dynamic(Head, Module),
	exclude(Head),
	functor(Head, N, A).

exclude(super(_,_,_,_)) :- !, fail.
exclude(attributes(_,_,_)) :- !, fail.
exclude('$so_type'(_,_)) :- !, fail.
%exclude('$default'(_,_,_)) :- !, fail.   Now inherit also default method
exclude('$fix_param'(_,_)) :- !, fail.
exclude(:-(_,_,_,_)) :- !, fail.
exclude(_).

import_static_preds(static, SModule, Module, ImportPreds) :-
	prolog:'$import_preds'(SModule, Module, ImportPreds).
import_static_preds(dynamic, SModule, Module, ImportPreds) :-
	import_from_static(ImportPreds, SModule, Module).

% Never import static methods directly from a dynamic objects since it
% may be abolished.
import_from_static([], _, _).
import_from_static([N/A|Ps], SModule, Module) :-
	functor(Head, N, A),
	predicate_property(SModule:Head, imported_from(SModule1)),
	prolog:'$import_preds'(SModule1, Module, [N/A]),
	import_from_static(Ps, SModule, Module).

copy_dynamic_preds([], _, _, _, _).
copy_dynamic_preds([N/A|Preds], SModule, Module, Super, Object) :-
	functor(Head, N, A),
	assert(Module:Head, Ref),	% define (if no clauses)
	erase(Ref),
	(   clause(SModule:Head, Body),
	    assertz(Module:(Head:-Body)),
	    fail
	;   true
	),
	copy_dynamic_preds(Preds, SModule, Module, Super, Object).


%--------------------------------------------------------------------------

init_attributes(Module, Attributes0) :-
	Attributes=['$class'([])|Attributes0],
	prolog:'$init_module_data'(Module, _, Attributes).

%--------------------------------------------------------------------------
% Handle calls with, at compile time, unknown method

call_object(Object, Msg, Self) :-
	expand_literal(Msg, ExpMsg, Self, Object),
	object_module(Object, Module),
	prolog:call_module(ExpMsg, Module, []/*no PC*/).


%--------------------------------------------------------------------------
% super::Method

call_super(Object, Msg, Self, S) :-
	expand_literal(Msg, ExpMsg, Self, S),
	call_super_exp(Object, ExpMsg, S).

call_super_exp(Object, Msg0, S) :-
	object_module(Object, Module),
	(   call_if_def(super(S,_,_,Object), Module),
	    object_module(S, SModule),
	    (   Msg=Msg0
	    ;   contract_literal(Msg0, ContrMsg, Self, ObjVar),
		Msg='$default'(ContrMsg,Self,ObjVar)
	    ),
	    visible(Msg, SModule) ->
	    prolog:call_module(Msg, SModule, []/*no PC*/)
	;   functor(Msg0, N, A),
	    prolog:illarg(existence(procedure,supers_of(Module):N/A,0), Msg0, 0)
	).


%--------------------------------------------------------------------------
% Unknown goal within body of method

call_from_body((G1,G2), Self, Myself, SourceModule) :- !,
	(   call_from_body(G1, Self, Myself, SourceModule),
	    call_from_body(G2, Self, Myself, SourceModule)
	).
call_from_body((G1->G2;G3), Self, Myself, SourceModule) :- !,
	(   call_from_body(G1, Self, Myself, SourceModule)
	->  call_from_body(G2, Self, Myself, SourceModule)
	;   call_from_body(G3, Self, Myself, SourceModule)
	).
call_from_body((G1->G2), Self, Myself, SourceModule) :- !,
	(   call_from_body(G1, Self, Myself, SourceModule)
	->  call_from_body(G2, Self, Myself, SourceModule)
	).
call_from_body((G1;G2), Self, Myself, SourceModule) :- !,
	(   call_from_body(G1, Self, Myself, SourceModule)
	;   call_from_body(G2, Self, Myself, SourceModule)
	).
call_from_body((\+G1), Self, Myself, SourceModule) :- !,
	\+ call_from_body(G1, Self, Myself, SourceModule).
call_from_body(if(G1,G2,G3), Self, Myself, SourceModule) :- !,
	if(call_from_body(G1, Self, Myself, SourceModule),
	   call_from_body(G2, Self, Myself, SourceModule),
	   call_from_body(G3, Self, Myself, SourceModule)).
call_from_body((_^G1), Self, Myself, SourceModule) :- !,
	call_from_body(G1, Self, Myself, SourceModule).
call_from_body((:: Msg), Self, Myself, _) :- !,
	call_object(Myself, Msg, Self).
call_from_body((<: Msg), Self, Myself, _) :- !,
	call_object(Myself, Msg, Self).
call_from_body((self :: Msg), Self, _, _) :- !,
	call_object(Self, Msg, Self).
call_from_body((self <: Msg), Self, _, _) :- !,
	call_object(Self, Msg, Self).
call_from_body((super :: Msg), Self, _, _) :- !,
	call_super(Self, Msg, Super, Super).
call_from_body((super <: Msg), Self, _, _) :- !,
	call_super(Self, Msg, Self, _).
call_from_body((Object :: Msg), _Self, _, _) :- !,
	call_object(Object, Msg, Object).
call_from_body((Object <: Msg), Self, _, _) :- !,
	call_object(Object, Msg, Self).
call_from_body(M:Goal, _, _, _) :- !,
	M:Goal.
call_from_body(:Goal, _, _, SourceModule) :- !,
	SourceModule:Goal.
call_from_body(Msg, Self, _, _) :-		% default is Self::Msg
	call_object(Self, Msg, Self).


%--------------------------------------------------------------------------
% The default clause, if any, is called when no other definition,
% local or inherited, matches a call.

:- multifile
	user:unknown_predicate_handler/3.

:- dynamic
	user:unknown_predicate_handler/3.

user:unknown_predicate_handler(Goal0, Module, Goal) :-
	call_if_def('$so_type'(_,_), Module), !, % Module is an object
	contract_literal(Goal0, Method, Self, ObjVar),
	Goal = '$default'(Method,Self,ObjVar),
	visible(Goal, Module).


%--------------------------------------------------------------------------
% Support for methods in 'object'.

% Check or enumerate objects & methods

ext_object(Object, Type) :-
	nonvar(Object),
	object_module(Object, M),
	instance_class(M, []),
	call_if_def('$so_type'(Object,Type), M).
ext_object(Object, Type) :-
	var(Object),
	current_module(M),
	instance_class(M, []),
	call_if_def('$so_type'(Object,Type), M).

ext_method(Object, N/A, Type) :-
	atom(N), integer(A), !,
	A2 is A+2,
	functor(Fu2, N, A2),
	object_module(Object, M),
	method_predicate(Fu2, M, Type).
ext_method(Object, N/A, Type) :-
	object_module(Object, M),
	method_predicate(Fu2, M, Type),
	functor(Fu2, N, A2),
	A is A2-2.

method_predicate(Fu2, M, Type) :-
	current_predicate(_, M:Fu2),
	exclude(Fu2),
	(   predicate_property(M:Fu2, dynamic) ->
	    Type = dynamic
	;   Type = static
	).


%--------------------------------------------------------------------------
%  new/2 - create a dynamic object

ext_new(Object, Supers0) :-
	Goal = object::new(Object,Supers0),
	trans_supers(Supers0, Goal, Supers, SupersNIS),
	(   var(Object) ->
	    Module = Object
	;   check_object(Object, Goal, 2),
	    object_module(Object, Module),
	    check_redefine(Module, Object, Goal),
	    clear_object(Object)
	),
	prolog:'$make_lightweight_module'(Module, _, 2),
	assert_super_sub(Supers, Object),
	super_dependencies_rt(Supers, Object, Object, Module, Goal,
	                      [], Attributes),
	init_attributes(Module, Attributes),
	assertz(Module:'$so_type'(Object,dynamic)),
	(   member(Super-NIS1, SupersNIS), % NIS1 is an ordered list of method specs
	    assertz(Module:super(Super,NIS1,_,Object)),
	    fail
	;   nis_mets_to_preds(SupersNIS, SupersNIS2),
	    inherit_supers(SupersNIS2, Module, Object, [], [], ISP, CDP, 0, _),
	    inh_sp(ISP),
	    cp_dp(CDP)

	).

ext_new1(Object, Super) :-
	Goal = Super::new(Object),
	(   var(Object) ->
	    Module = Object
	;   check_object(Object, Goal, 2),
	    object_module(Object, Module),
	    check_redefine(Module, Object, Goal),
	    clear_object(Object)
	),
	prolog:'$make_lightweight_module'(Module, _, 2),
	assert_super_sub([Super], Object),
	super_dependencies_rt([Super], Object, Object, Module, Goal,
	                      [], Attributes),
	init_attributes(Module, Attributes),
	assertz(Module:'$so_type'(Object,dynamic)),
	assertz(Module:super(Super,[],_,Object)),
	inherit_supers1(Super, Module, Object).

% Specialized version (one super and no local def's)
inherit_supers1(Super, Module, Object) :-
	object_module(Super, SModule),
	static_methods_of(SModule, StatPreds),
	call_apply('$so_type'(Super,Type), SModule),
	import_static_preds(Type, SModule, Module, StatPreds),
	dynamic_methods_of(SModule, DynPreds),
	copy_dynamic_preds(DynPreds, SModule, Module, Super, Object).

trans_supers(Ss, Goal, _, _) :- var(Ss), !,
	prolog:illarg(var, Goal, 2).
trans_supers([], _, Ss, SNs) :- !, Ss=[], SNs=[].
trans_supers([S0|Ss0], Goal, Ss, SNs) :- !,
	trans_super_rt(S0, Goal, S, NIS),
	Ss = [S|Ss1],
	SNs = [S-NIS|SNs0],
	trans_supers(Ss0, Goal, Ss1, SNs0).
trans_supers(Ss, Goal, _, _) :-
	prolog:illarg(type(list), Goal, 2, Ss).

trans_super_rt(S0-NIS0, Goal, S, NIS) :- !,
	S=S0,
	check_object_rt(S, Goal, 2),
	check_nis(NIS0, Goal),
	sort(NIS0, NIS).
trans_super_rt(S, Goal, S, []) :-
	check_object_rt(S, Goal, 2).

super_dependencies_rt(Ss, Object, BObject, BModule, Goal, Atts0, Atts) :-
	assert_fix_param(Object, BObject, BModule),
	super_dependencies_rt1(Ss, BObject, BModule, Goal, Atts0, Atts).

super_dependencies_rt1([], _, _, _, Atts, Atts).
super_dependencies_rt1([S|Ss], BO, BM, Goal, Atts0, Atts) :-
	object_module(S, M),
	(   loaded_object_info(M, S, Supers, Attributes0) ->
	    true
	;   prolog:illarg(existence(object,S,0), Goal, 0)
	),
	super_dependencies_rt(Supers, S, BO, BM, Goal, Attributes0, Attributes),
	add_atts(Attributes, Atts0, Atts2),
	super_dependencies_rt1(Ss, BO, BM, Goal, Atts2, Atts).

assert_fix_param(Object, _, _) :- atomic(Object), !.
assert_fix_param(Object, BObject, BModule) :-
	assertz(BModule:('$fix_param'(Object,X):-
			 objects:object_class(BObject,X))).


%--------------------------------------------------------------------------
% assert & augment methods

ext_asserta_method(Head, Object, Ref) :-
	object_module(Object, Module),
	assert_method(Head, true, Object, Module, Clause),
	asserta(Clause, Ref).

ext_assertz_method(Head, Object, Ref) :-
	object_module(Object, Module),
	assert_method(Head, true, Object, Module, Clause),
	assertz(Clause, Ref).

ext_augment({Body}, Object, AZ) :-
	object_module(Object, Module),
	ext_augment(Body, Object, Module, AZ).

ext_augment((M1 & M2), Object, Module, AZ) :- !,
	ext_augment(M2, Object, Module, AZ),
	ext_augment(M1, Object, Module, AZ).
ext_augment(RawMethod, Object, Module, AZ) :-
	expand_term(RawMethod, Method),
	(   Method = (:- Body) ->
	    translate_method_dynamic(_, Body, Object, Module, (_:-MBody)),
	    call(Module:MBody)
	;   var(Method) ->
	    assert_method(Method, true, Object, Module, Clause),
	    augment1(AZ, Clause)
	;   Method = (H:-B) ->
	    assert_method(H, B, Object, Module, Clause),
	    augment1(AZ, Clause)
	;   assert_method(Method, true, Object, Module, Clause),
	    augment1(AZ, Clause)
	).

augment1(a, Clause) :- asserta(Clause).
augment1(z, Clause) :- assertz(Clause).


%--------------------------------------------------------------------------

locked_method(Method, M) :-
	functor(Method, N, A),
	A2 is A+2,
	functor(Fu2, N, A2),
	(   exclude(Fu2) ->
	    (   predicate_property(M:Fu2, dynamic) -> fail
	    ;   predicate_property(M:Fu2, _) -> true
	    ;   call_apply('$so_type'(_,static), M)
	    )
	;   true
	).

assert_method(Method, Body, Object, Module, _Clause) :-
	locked_method(Method, Module), !,
	functor(Method, N, A),
	perm_error(Object::assert((Method:-Body)), assert, 'static or locked', Module:N/A).
assert_method(Head, Body, Object, Module, Module:Method) :-
	translate_method_dynamic(Head, Body, Object, Module, Method).


%--------------------------------------------------------------------------
% retract & abolish methods

ext_retract_method(Head, Object) :-
	object_module(Object, Module),
	locked_method(Head, Module), !,
	functor(Head, N, A),
	perm_error(Object::retract(Head), retract, 'static or locked', Module:N/A).
ext_retract_method(Head, Object) :-
	expand_literal(Head, EHead, _, Object),
	object_module(Object, Module),
	retract(Module:EHead).

ext_abolish(Object) :-
	object_module(Object, Module),
	instance_class(Module, Class),
	(   Class = [] ->
	    ext_abolish1(Object, Object)
	;   prolog:'$remove_module'(Object, 0)
	).

ext_abolish1(Object, Module) :-
	call_apply('$so_type'(Object,static), Module), !,
	perm_error(Object::abolish, abolish, static_object, Object).
ext_abolish1(Object, _) :-
	super_sub(Object, _), !,
	perm_error(Object::abolish, abolish, object_with_subs, Object).
ext_abolish1(Object, Module) :-
	call_if_def(super(S,_,_,_), Module),
	retract(super_sub(S,Object)),
	fail.
ext_abolish1(Object, _) :-
	has_instance(Object, Instance),
	prolog:'$remove_module'(Instance, 0),
	fail.
ext_abolish1(_, Module) :-
	prolog:'$remove_module'(Module, 0).


ext_retractall(Head, Object) :-
	nonvar(Head), !,
	functor(Head, N, A),
	object_module(Object, Module),
	(   locked_method(Head, Module) ->
	    perm_error(Object::retractall(Head), retractall, 'static or locked', Module:N/A)
	;   A2 is A+2,
	    functor(Fu2, N, A2),
	    retractall(Module:Fu2)
	).
ext_retractall(Head, Object) :-
	var(Head),
	object_module(Object, Module),
	method_predicate(Fu2, Module, dynamic),
	functor(Fu2, N, A2),
	A is A2-2,
	functor(Head, N, A),
	retractall(Module:Fu2).

ext_ancestor(Anc, Anc, K, K).
ext_ancestor(Object, Anc, I, K) :-
	object_module(Object, Module),
	call_if_def(super(Super,_,_,Object), Module),
	J is I+1,
	ext_ancestor(Super, Anc, J, K).

ext_descendant(Desc, Desc, K, K).
ext_descendant(Object, Desc, I, K) :-
	super_sub(Object, Sub),
	J is I+1,
	ext_descendant(Sub, Desc, J, K).


%--------------------------------------------------------------------------
% Runtime translation of one method (used by assert/retract)

translate_method_dynamic(MHead, MBody, Myself, Module, Clause) :-
	var(MHead), !,
	Head = '$default'(MHead,Self,ObjVar),
	tmd(Head, MBody, Myself, Module, Self, ObjVar, Clause).
translate_method_dynamic(MHead, MBody, Myself, Module, Clause) :-
	expand_literal(MHead, Head, Self, ObjVar),
	tmd(Head, MBody, Myself, Module, Self, ObjVar, Clause).

tmd(Head, MBody, Myself, Module, Self, ObjVar, (Head:-Body)) :-
	make_context(Module, Myself, Self, _, Info),
	translate_method_body(MBody, Info, Body0, _, []),
	parameter_fix(Myself, Body0, Head, ObjVar, Body).


%--------------------------------------------------------------------------
% Make an instance

ext_instance(I, Object) :-
	var(I), !,
	object_module(Object, Module),
	(   instance_class(Module, []) ->
	    instance(Object, Module, I)
	;   perm_error(Object::instance(I), make_instance_of,
	               instance, Object)
	).
ext_instance(I, Object) :-
	callable(I), !,
	Goal = Object::instance(I),
	check_object(I, Goal, 2),
	check_redefine(I, I, Goal),
	clear_object(I),
	object_module(Object, Module),
	instance(Object, Module, I).
ext_instance(I, Object) :-
	prolog:illarg(type(callable), Object::instance(I), 2, I).

instance(Object, Module, I) :-
	    prolog:'$make_lightweight_module'(I, Module, _),
	    prolog:'$init_module_data'(I, true, Module),
	    prolog:'$set_module_data'(I, '$class', Object).
	

%--------------------------------------------------------------------------

ext_get(1, Module, Key, Arity, X) :- !,
	arg(1, X, Val),
	prolog:'$get_module_data'(Module, Key, Arity, 0, Val).
ext_get(N, Module, Key, Arity, X) :-
	arg(N, X, Val),
	N1 is N-1,
	prolog:'$get_module_data'(Module, Key, Arity, N1, Val),
	ext_get(N1, Module, Key, Arity, X).

ext_set(1, Module, Key, Arity, X) :- !,
	arg(1, X, Val),
	prolog:'$set_module_data'(Module, Key, Arity, 0, Val).
ext_set(N, Module, Key, Arity, X) :-
	arg(N, X, Val),
	N1 is N-1,
	prolog:'$set_module_data'(Module, Key, Arity, N1, Val),
	ext_set(N1, Module, Key, Arity, X).

ext_has_attribute(N/A, Module) :-
	atom(N), integer(A), !,
	functor(Head, N, A),
	not_reserved(Head),
	prolog:'$current_module_data'(Module, Head).
ext_has_attribute(N/A, Module) :-
	prolog:'$current_module_data'(Module, Head),
	not_reserved(Head),
	functor(Head, N, A).

not_reserved('$class'(_)) :- !, fail.
not_reserved(_).


%--------------------------------------------------------------------------
% Support

perm_error(Goal, Op, Type, Culprit) :-
	prolog:illarg(permission(Op,Type,0), Goal, 0, Culprit).

%% Get the class parameters of an instance if any
%% stored as a reserved attribute, which is otherwise '[]'
%% Any other object is its own class

object_class(Class, Obj) :-
	object_module(Obj, Module),
	instance_class(Module, InstanceClass),
	(   InstanceClass = [] ->
	    Obj=Class
	;   InstanceClass=Class
	).

