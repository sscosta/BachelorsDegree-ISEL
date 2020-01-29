/* Copyright(C) 1993-95, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Name: expand.pl
% Author: Seif Haridi
% Implementors : Dr. Mahmoud Rafeh, Khaled Shaalan and Kent Boortz
% Date: 17 Nov  1992
% Purpose: SICStus Objects  expander and runtime predicates
%
% Attributes & Instances added, inheritance mechanism changed. /san -94
% Enhanced with layout info. /Rob Scott, IQSoft, March 1998
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:term_expansion/4.

:- dynamic
	user:term_expansion/4.

user:term_expansion(ObjectDefinition, Lay0, ExpandedCode, Lay) :-
	ObjectDefinition = _::_, !,
	obj_term_expansion(ObjectDefinition, Lay0, ExpandedCode, Lay).
user:term_expansion(end_of_file, _, _, _) :-
	prolog_load_context(file, File),
	retractall(compile_time_info(_,_,File,_,_)),
	% fail to allow other modules to expand on end_of_file
	fail.

obj_term_expansion(ObjectDefinition, Lay0, ExpandedCode, Lay) :-
	ObjectDefinition = Obj::Methods,
	prolog:decomp_layout2(Lay0, LayObj, LayMethods),    % Obj, Methods
	check_object(Obj, ObjectDefinition, 1),
	objects:object_module(Obj, Module),
	compile_time_check(Module, ObjectDefinition),
	(   nonvar(Methods),
	    object_expansion(Methods, Obj, Module, ObjectDefinition, LayMethods, Lay1, C, []) ->
	    ExpandedCode = Module:C,
	    % put back layout for Module:C
	    prolog:condense_layout(LayObj, LayObjC),
	    prolog:comp_layout2(Lay0, LayObjC, Lay1, Lay)
	;   var(Methods) ->
	    raise_exception(instantiation_error(ObjectDefinition, 2))
	;   (   Lay0==[]
	    ->	prolog_load_context(stream, Stream),
		line_count(Stream, LC),
		Where = between(LC,LC,LC)
	    ;   prolog:condense_layout(Lay0, LA),
		last_layout(Lay0, LB),
		Where = between(LA,LB,LA)
	    ),
	    raise_exception(syntax_error(ObjectDefinition, Where,
	                                 [object,expansion,failed],[],0))

	).

compile_time_check(M, Goal) :-
	clause(compile_time_info(M,_,_,_,_), _), !,
	raise_exception(permission_error(Goal,redefine,object_module,M,0)).
compile_time_check(_, _).

last_layout([L], Z) :- !, last_layout(L, Z).
last_layout([_|L], Z) :- !, last_layout(L, Z).
last_layout(Z, Z).


%--------------------------------------------------------------------------
  
object_expansion({}, Object, Module, Goal, Lay0, Lay) --> !,
	{prolog:arg1_layout(Lay0, Lay1)},  	% remove layout for {}
	object_expansion1([], Object, Module, Goal, Lay1, Lay).
object_expansion({Methods}, Object, Module, Goal, Lay0, Lay) -->
	{prolog:arg1_layout(Lay0, Lay1)},	        % remove layout for {_}
	{prolog:comp_layout2(Lay1, Lay1, [], Lay2)}, % add layout for eob
	{normalize_body((Methods & end_of_object), Object, Lay2, Lay3, [], MethodList, [])},
	object_expansion1(MethodList, Object, Module, Goal, Lay3, Lay), !.

normalize_body((Method & Methods), Object, LayIn, Lay0, Lay) --> !,
	{prolog:decomp_layout2(LayIn, LL, RR)},
	normalize_body(Method, Object, LL, Lay0, Lay1),
	normalize_body(Methods, Object, RR, Lay1, Lay).
normalize_body(RawMethod, Object, LayIn0, Lay0, Lay) -->
	{expand_method(RawMethod, Object, ExpMethods, LayIn0, LayIn1)},
	expand_terms(ExpMethods, LayIn1, Lay0, Lay).

expand_method(RawMethods, Object, ExpMethods, _, []/*undef layout*/) :-
	visible(method_expansion(_,_,_), user),
	user:method_expansion(RawMethods, Object, ExpMethods), !.
expand_method(end_of_object, _, ExpMethods, Lay0, Lay) :- !,
	ExpMethods = [],
	Lay0 = Lay.
expand_method(RawMethods, _, RawMethods, Lay, Lay).

expand_terms([], _, Lay, Lay) --> !.
expand_terms([M|Ms], LayIn, Lay0, Lay) --> !,
	{prolog:decomp_layout2(LayIn, LL, RR)},
	{prolog:expand_term_layout(M, EM, LL, LL1)},
	normalize_list(EM, LL1, Lay0, Lay1),
	expand_terms(Ms, RR, Lay1, Lay).
expand_terms(M, LayIn, Lay0, Lay) -->
	{prolog:expand_term_layout(M, EM, LayIn, LayIn1)},
	normalize_list(EM, LayIn1, Lay0, Lay).

normalize_list([], _, Lay, Lay) --> !.
normalize_list([H|T], LayIn, Lay0, Lay) --> !, 
	[H],
	{prolog:decomp_layout2(LayIn, LL, RR),
	 prolog:comp_layout2(LayIn, LL, Lay1, Lay0)},
 	normalize_list(T, RR, Lay1, Lay).
normalize_list(X, LayIn, Lay0, Lay) --> [X],
	{prolog:comp_layout2(LayIn, LayIn, Lay, Lay0)}.


%%% Warning: the following code should use difference layouts througout.
%%%          Currently it assumes that during translate_methods
%%%          that each term gets translated to one term only.
%%%          This is a dangerous assumption and will be fixed
%%%          in a future version.

object_expansion1(MethodList, Object, Module, Goal, LayIn, LayOut) -->
	[(:-objects:create_object(Object,SupersNIS,StatPreds,DynPreds,Attributes,SuperTree)),
	 '$so_type'(Object,Type)],
	{classify_methods(MethodList, Goal, Type, SupersNIS0, Dynamics0,
	                  Clauses0, Attributes0, LayIn, LayIn0),
	 check_attributes(Attributes0, Goal, [], Attributes1),
	 augment_supers(SupersNIS0, Object, SupersNIS1, Clauses0, Clauses),
	 nis_mets_to_preds(SupersNIS1, SupersNIS),
	 objects:peel_of_nis(SupersNIS1, Supers),
	 record_object(Module, Object, Supers, Attributes1)},
        {prolog:condense_layout(LayIn, LNIn)},
 	super_dependencies(Supers, Object, Object,
	                   Attributes1, Attributes, SuperTree, LNIn, LSX, DLX),
	{clauses_functors(Clauses, Functors0, Dynamics0),
	 sort(Dynamics0, Dynamics1),
	 sort(Functors0, Functors)},
	{   Type = static ->
	    Dynamics = Dynamics1,
	    objects:ord_subtract(Functors, Dynamics, Statics)
	;   Dynamics = Functors, Statics = []
	},
	{mets_to_preds(Statics, StatPreds),
	 mets_to_preds(Dynamics, DynPreds)},
	create_dynamic_entries(DynPreds, LNIn, DLX, LayOut0),
	translate_methods(Clauses, Module, Object, LayIn0, LayOut0),
	% 2 dummy layouts added 'cos two declarations inserted at the start
	{prolog:condense_layout(LayOut0, LN),
         prolog:comp_layout2(LN, LN, LSX, LayOut1),
         prolog:comp_layout2(LN, LN, LayOut1, LayOut)}.

%--------------------------------------------------------------------------

check_object(Obj, Goal, ArgNo) :- integer(Obj), !,
	raise_exception(type_error(Goal,ArgNo,object_spec,Obj)).
check_object(Obj, Goal, ArgNo) :-
	check_object_rt(Obj, Goal, ArgNo).
	
check_object_rt(Obj, _, _) :- callable(Obj), !.
check_object_rt(Obj, Goal, ArgNo) :- integer(Obj), !,
	(   Obj >= -16'2000000,
	    Obj < 16'2000000 -> true
	;   raise_exception(domain_error(Goal,ArgNo,small_int,Obj))
	).
check_object_rt(Obj, Goal, ArgNo) :- var(Obj), !,
	raise_exception(instantiation_error(Goal, ArgNo)).
check_object_rt(Obj, Goal, ArgNo) :-
	raise_exception(type_error(Goal,ArgNo,object_spec,Obj)).

check_attributes([], _, Atts, Atts).
check_attributes([AList|ALists], Goal, Atts0, Atts) :-
	check_attributes1(AList, Goal, Atts0, Atts1),
	check_attributes(ALists, Goal, Atts1, Atts).

check_attributes1(AList, Goal, _, _) :- var(AList), !,
	raise_exception(instantiation_error(Goal,2)).
check_attributes1([], _, Atts0, Atts) :- !, Atts0=Atts.
check_attributes1([Att|AList], Goal, Atts0, Atts) :- !,
	valid_attribute(Att, Goal),
	key(Att, AttKey),
	(   memberchk(AttKey, Atts0) ->
	    print_message(warning, so_duplicate_attribute(Att)),
	    check_attributes1(AList, Goal, Atts0, Atts)
	;   check_attributes1(AList, Goal, [Att|Atts0], Atts)	    
	).
check_attributes1(AList, Goal, _, _) :-
	raise_exception(type_error(Goal,2,list,AList)).

valid_attribute(Att, Goal) :- var(Att), !,
	raise_exception(instantiation_error(Goal,2)).
valid_attribute(Att, Goal) :- Att=[_], !,
	raise_exception(type_error(Goal,2,attribute_spec,Att)).
valid_attribute(Att, Goal) :- atomic(Att), !,
	raise_exception(type_error(Goal,2,attribute_spec,Att)).
valid_attribute(_, _).

check_nis(Ms, Goal) :- var(Ms), !,
	raise_exception(instantiation_error(Goal,2)).
check_nis([], _) :- !.
check_nis([M|Ms], Goal) :- !,
	check_spec(M, Goal),
	check_nis(Ms, Goal).
check_nis(Ms, Goal) :-
	raise_exception(type_error(Goal,2,list,Ms)).

check_spec(Spec, Goal) :- var(Spec), !,
	raise_exception(instantiation_error(Goal,2)).
check_spec(N/A, _) :-
	atom(N),
	integer(A),
	!.
check_spec(Spec, Goal) :-
	raise_exception(type_error(Goal,2,predicate_spec,Spec)).
	

%--------------------------------------------------------------------------

augment_supers([], _, [], Clauses0, Clauses0) :- !.
augment_supers(Supers, _, Supers, Clauses, Clauses) :-
	Supers = [_|_].

nis_mets_to_preds([], []).
nis_mets_to_preds([Super-NIS0|SN0], [Super-NIS|SN]) :-
	mets_to_preds(NIS0, NIS),
	nis_mets_to_preds(SN0, SN).

mets_to_preds([], []).
mets_to_preds([N/A|Ms], [N/A2|Ps]) :- A2 is A+2, mets_to_preds(Ms, Ps).

create_dynamic_entries([], _, LX, LX) --> [].
create_dynamic_entries([N/A|Ps], LN, LX, LY) --> 
	[(:- dynamic N/A)],
	{prolog:comp_layout2(LN, LN, LZ, LX)},
	create_dynamic_entries(Ps, LN, LZ, LY).

record_object(Module, Object, Supers, Attributes1) :-
	prolog_load_context(file, File), !,
	assertz(compile_time_info(Module,Object,File,Supers,Attributes1)).
record_object(_, _, _, _).	% allow usage through expand_term/2


%--------------------------------------------------------------------------

classify_methods([], _, static, [], [], [], [], Layout, Layout).
classify_methods([Met|Mets], Goal, Type, Supers, Dyns, Cls, Atts, LayIn, LayOut) :-
	prolog:decomp_layout2(LayIn, L1, L2),
	classify_methods(Met, Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut).

classify_methods(Var, Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :-
	var(Var), !,
	Cls = [(Var:-true)|Cls0],
	prolog:condense_layout(L1, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut1),
	prolog:comp_layout2(LN, LayOut1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers, Dyns, Cls0, Atts, L2, LayOut2).
classify_methods(dynamic, Mets, Goal, Type, Supers, Dyns, Cls, Atts, _, L2, LayOut) :- !,
	Type = dynamic,
	classify_methods(Mets, Goal, _, Supers, Dyns, Cls, Atts, L2, LayOut).
classify_methods(super(S), Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :- !,
	check_object(S, Goal, 2),
	Supers = [S-[]|Supers0],
	Cls = [(super(S,[]):-true)|Cls0],
	prolog:condense_layout(L1, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut1),
	prolog:comp_layout2(LN, LayOut1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers0, Dyns, Cls0, Atts, L2, LayOut2).

classify_methods(super(S,NIS0), Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :- !,
	check_object(S, Goal, 2),
	check_nis(NIS0, Goal),
	sort(NIS0, NIS),
	Supers = [S-NIS|Supers0],
	Cls = [(super(S,NIS):-true)|Cls0],
	prolog:condense_layout(L1, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut1),
	prolog:comp_layout2(LN, LayOut1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers0, Dyns, Cls0, Atts, L2, LayOut2).
classify_methods(dynamic(S), Mets, Goal, Type, Supers, Dyns, Cls, Atts, _, L2, LayOut) :- !,
	Dyns = [S|Dyns0],
	(   S = super/1 ->
	    raise_exception(permission_error(Goal,declare_dynamic,method,S,0))
	;   true
	),
	classify_methods(Mets, Goal, Type, Supers, Dyns0, Cls, Atts, L2, LayOut).
classify_methods(attributes(As), Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :- !,
	Atts = [As|Atts0],
	Cls = [(attributes(As):-true)|Cls0],
	prolog:condense_layout(L1, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut1),
	prolog:comp_layout2(LN, LayOut1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers, Dyns, Cls0, Atts0, L2, LayOut2).
classify_methods((:-B), Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :- !,
	Cls = [(:-B)|Cls0],
	prolog:comp_layout2(L1, L1, LayOut0, LayOut),
	classify_methods(Mets, Goal, Type, Supers, Dyns, Cls0, Atts, L2, LayOut0).
classify_methods((H:-B), Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :- !,
	Cls = [(H:-B)|Cls0],
	prolog:comp_layout2(L1, L1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers, Dyns, Cls0, Atts, L2, LayOut2).
classify_methods(X, Mets, Goal, Type, Supers, Dyns, Cls, Atts, L1, L2, LayOut) :-
	Cls = [(X:-true)|Cls0],
	prolog:condense_layout(L1, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut1),
	prolog:comp_layout2(LN, LayOut1, LayOut2, LayOut),
	classify_methods(Mets, Goal, Type, Supers, Dyns, Cls0, Atts, L2, LayOut2).

clauses_functors([]) --> [].
clauses_functors([Clause|Clauses]) -->
	clause_functor(Clause),
	clauses_functors(Clauses).

clause_functor((H:-_)) --> !, clause_functor1(H).
clause_functor(_) --> [].

clause_functor1(H) --> {nonvar(H)}, !, {functor(H, N, A)}, [N/A].
clause_functor1(H) --> {var(H)}, ['$default'/1].


%--------------------------------------------------------------------------
% super_dependencies(+Supers, +Object, +BaseObject,
%                    +Attributes0, -Attributes, -SuperTree)
%  - traverses the super hierarchy in order to:
%   1. Collect attributes
%   2. Generate parameter passing clauses
%   3. Build the (compile time) super hierarchy to be passed to
%      the load time check
%   4. Signal an error if a cyclic dependency is found

super_dependencies(Ss, O, BO, Atts0, Atts, Tree, LNIn, LX, LY) -->
	{Tree = tree(O,Trees)},
	fix_param(O, BO, LNIn, LX, LZ),
	super_dependencies1(Ss, BO, Atts0, Atts, Trees, LNIn, LZ, LY).

super_dependencies1([], _,      Atts, Atts, [], _, LS, LS) --> [].
super_dependencies1([S|_], BO, _, _, _, _, _, _) -->
	{functor(S, F, _), functor(BO, F, _)}, !,
	{raise_exception(consistency_error(_Goal,BO,super(F),self_ancestor))}.
super_dependencies1([S|Ss], BO, Atts0, Atts, [Tree|Trees], LNIn, LX, LY) -->
	{copy_term(S-BO, S1-BO1),
	 object_info(S1, Supers, Attributes0)},
	 super_dependencies(Supers, S1, BO1, Attributes0, Attributes, Tree, LNIn, LX, LZ),
	{add_atts(Attributes, Atts0, Atts2)},
	super_dependencies1(Ss, BO, Atts2, Atts, Trees, LNIn, LZ, LY).

fix_param(S, _, _, LF, LF) --> {atom(S), !}.
fix_param(S, BO, LNIn, LX, LY) -->
	[('$fix_param'(S,Obj):-objects:object_class(BO,Obj))],
	{prolog:comp_layout2(LNIn, LNIn, LY, LX)}.

object_info(Object, Supers, Attributes) :-
	objects:object_module(Object, M),
	(   clause(compile_time_info(M,Object,_,Supers,Attributes), _) ->
	    true
	;   loaded_object_info(M, Object, Supers, Attributes) ->
	    true
	;   raise_exception(existence_error(_Goal,0,object,Object,_))

	).

loaded_object_info(Module, Object, Supers, Attributes) :-
	objects:call_if_def('$so_type'(Object,_), Module),
	(   objects:call_if_def(attributes(Attributes,_,Object), Module) ->
	    true
	;   Attributes = []
	),
	findall(Super, objects:call_if_def(super(Super,_,_,Object),Module), Supers).

add_atts([], Atts, Atts).
add_atts([Att|AddAtts], Atts0, Atts) :-
	key(Att, Key),
	(   memberchk(Key, Atts0) -> Atts = Atts1
	;   Atts = [Att|Atts1]
	),
	add_atts(AddAtts, Atts0, Atts1).
	

%--------------------------------------------------------------------------
% Modules may be created by references in the body at load-time. To
% make sure they become lw-modules, directly referenced modules are
% collected and the ensure_defined/1 call is emitted.
% ensure_defined/1 is also run at compile time in order to avoid
% incorrect meta-expansions for e.g. abolish/2.

translate_methods(Mlist, Module, Object, LayIn, LayOut) -->
	{make_context(Module, Object, _, _, Info)},
	[(:-objects:ensure_defined(ED))],
	translate_methods1(Mlist, Info, ED0, [], LayIn, LayIn0),
	{sort([Module|ED0], ED),
	 objects:ensure_defined(ED),    % run at compile time as well
	 prolog:condense_layout(LayIn0, LN),
         prolog:comp_layout2(LN, LN, LayIn0, LayOut)}.

translate_methods1([], _, ED, ED, Layout, Layout) --> [].
translate_methods1([Msg0|Ms], Info0, ED, ED0, LayIn, LayOut) -->
	{copy_term(Info0-Msg0, Info-Msg)},
	{prolog:arg1_layout(LayIn, L0),
	 prolog:arg2_layout(LayIn, R0)},
	translate_method(Msg, Info, ED, ED1, L0, L1),
	translate_methods1(Ms, Info0, ED1, ED0, R0, R1),
	{prolog:comp_layout2(LayIn, L1, R1, LayOut)}.

make_context(Module, Object, Self, SelfModule, Context) :-
	Context = context(SourceModule,Module,Object,Self,SelfModule),
	(   prolog_load_context(module, SourceModule) -> true
	;   prolog_flag(typein_module, SourceModule)
	).

context_srcmodule(    context(S,_,_,_,_), S).
context_module(       context(_,S,_,_,_), S).
context_object(       context(_,_,S,_,_), S).
context_self(         context(_,_,_,S,_), S).
context_selfmodule(   context(_,_,_,_,S), S).


%--------------------------------------------------------------------------

translate_method((MHead :- MBody), Info, ED, ED0, LayIn, LayOut) -->
	{prolog:decomp_layout2(LayIn, L0, R0),
	 prolog:condense_layout(LayIn, LN)},
	translate_method(MHead, MBody, Info, ED, ED0, LN, L0, R0, LayOut).
translate_method((:- MBody), Info, ED, ED0, LayIn, LayOut) -->
	{prolog:arg1_layout(LayIn, L0)},
	{translate_method_body(MBody, Info, Body, L0, L1, ED, ED0)},
	[(:- Body)],
	{prolog:comp_layout1(LayIn, L1, LayOut)}.
translate_method(ANY, MBody, Info, ED, ED0, LN, L0, R0, LayOut) --> % match any message
	{var(ANY), !,
	 context_object(Info, Object),
	 context_self(Info, Self),
	 translate_method_body(MBody, Info, Body0, R0, R1, ED, ED0),
	 parameter_fix(Object, Body0, [], ObjVar, Body, R1, R2)},
	[('$default'(ANY,Self,ObjVar):-Body)],
	{prolog:condense_layout(L0, LL),
	 prolog:comp_layout2(LN, LL, R2, LayOut)}.
translate_method(super(S,NIS), MBody, Info, ED, ED, LN, L0, R0, LayOut) --> !,
	{context_object(Info, Object),
	 expand_literal(super(S,NIS), Head, _, Object)},
	[(Head:-MBody)],		% assuming super body is Prolog only
	{prolog:comp_layout2(LN, L0, R0, LayOut)}.
translate_method(MHead, MBody, Info, ED, ED0, LN, L0, R0, LayOut) -->
	{context_object(Info, Object),
	 context_self(Info, Self),
	 expand_literal(MHead, Head, Self, ObjVar),	 
	 translate_method_body(MBody, Info, Body0, R0, R1, ED, ED0),
	 parameter_fix(Object, Body0, Head, ObjVar, Body, R1, R2)},
	[(Head:-Body)],
        {prolog:condense_layout(L0, LL),
	 prolog:comp_layout2(LN, LL, R2, LayOut)}.


%--------------------------------------------------------------------------
% Since the 2nd additional arg, used for parameter passing, can not be
% changed before inheritance call, a special call is inserted for 
% parameter transfer. It is pushed down as far as possible.

parameter_fix(Object, Body0, _, _, Body) :-
 	parameter_fix(Object, Body0, _, _, Body, [], _).

parameter_fix(Object, Body0, _, _, Body, LayIn, LayOut) :-
	atomic(Object), !,
	Body = Body0,
	LayOut = LayIn.
parameter_fix(Object, Body0, Head, ObjVar, Body, LayIn, LayOut) :-
	variables_of(Object, Params),
	pre_goal_exp_layout(object_module(ObjVar,M), objects,
	             objects:call_apply('$fix_param'(Object,ObjVar),M), Fix, LayIn, FixLay),
	(   variables_of(Head, HVars),
	    objects:ord_disjoint(Params, HVars) ->
	    fix_body(Body0, Params, Fix, _, Body, LayIn, FixLay, LayOut)
	;   Body = (Fix,Body0),
	    prolog:comp_layout2(LayIn, FixLay, LayIn, LayOut)
	).

fix_body(G, Params, Fix, Flag, NG, LayIn, FixLay, LayOut) :-
	var(G), !,
	fix_body(G, [G], Params, Fix, Flag, NG, LayIn, FixLay, LayOut).
fix_body((G1,G2), Params, Fix, Flag, NG, LayIn, FixLay, LayOut) :- !,
	prolog:decomp_layout2(LayIn, L0, R0),
	NG=(NG1,NG2),
	fix_body(G1, Params, Fix, Flag, NG1, L0, FixLay, L1),
	(   nonvar(Flag) -> 
	    NG2 = G2,
	    prolog:comp_layout2(LayIn, L1, R0, LayOut)
	;   fix_body(G2, Params, Fix, Flag, NG2, R0, FixLay, R1),
	    prolog:comp_layout2(LayIn, L1, R1, LayOut)
	).
fix_body(G, Params, Fix, Flag, NG, LayIn, FixLay, LayOut) :-
	variables_of(G, Vars),
	fix_body(G, Vars, Params, Fix, Flag, NG, LayIn, FixLay, LayOut).

fix_body(G, Vars, Params, _, _, NG, LayIn, _, LayOut) :-
	objects:ord_disjoint(Params, Vars), !,
	NG=G,
	LayOut=LayIn.
fix_body(G, _, _, Fix, Flag, NG, LayIn, FixLay, LayOut) :-
	NG=(Fix,G),
	Flag=true,
	prolog:comp_layout2(LayIn, FixLay, LayIn, LayOut).

variables_of(Term, Vars) :-
	prolog:'$term_variables'(Term, Vars0),
	sort(Vars0, Vars).

%--------------------------------------------------------------------------


translate_method_body(G, Info, NG) --> 
	translate_method_body(G, Info, NG, [], _).


translate_method_body(G, Info, NG, LayIn, LayOut) -->
	{var(G), !,
	 context_module(Info, Module),
	 context_self(Info, Self),
	 context_srcmodule(Info, Src),
	 NG = objects:call_from_body(G, Self, Module, Src)},
        {prolog:condense_layout(LayIn, LayOut)}.
translate_method_body(true, _, NG, LayIn, LayOut) --> !, 
	{NG=true, LayIn=LayOut}.
translate_method_body('::'(G, S0, S), Info, NG, LayIn, LayOut) --> !, % fix DCG exp
	{expand_literal(G, NG0, S0, S)},
	translate_method_body((:: NG0), Info, NG, LayIn, LayOut).
translate_method_body('<:'(G, S0, S), Info, NG, LayIn, LayOut) --> !, % fix DCG exp
	{expand_literal(G, NG0, S0, S)},
	translate_method_body((<: NG0), Info, NG, LayIn, LayOut).
translate_method_body('::'(O, G, S0, S), Info, NG, LayIn, LayOut) --> !, % fix DCG exp
	{expand_literal(G, NG0, S0, S)},
	translate_method_body((O :: NG0), Info, NG, LayIn, LayOut).
translate_method_body('<:'(O, G, S0, S), Info, NG, LayIn, LayOut) --> !, % fix DCG exp
	{expand_literal(G, NG0, S0, S)},
	translate_method_body((O <: NG0), Info, NG, LayIn, LayOut).
translate_method_body((G1,G2), Info, NG, LayIn, LayOut) --> !,
	{prolog:decomp_layout2(LayIn, L0, R0)},
	translate_method_body(G1, Info, NG1, L0, L1),
	{   NG1=true ->
	    NG=NG2
	;   NG=(NG1,NG2)
	},
	translate_method_body(G2, Info, NG2, R0, R1),
	{prolog:comp_layout2(LayIn, L1, R1, LayOut)}.
translate_method_body((G1;G2), Info, NG, LayIn, LayOut) --> !,
	{prolog:decomp_layout2(LayIn, L0, R0)},
	{NG=(NG1;NG2)},
	translate_method_body(G1, Info, NG1, L0, L1),
	translate_method_body(G2, Info, NG2, R0, R1),
	{prolog:comp_layout2(LayIn, L1, R1, LayOut)}.
translate_method_body((G1->G2), Info, NG, LayIn, LayOut) --> !,
	{prolog:decomp_layout2(LayIn, L0, R0)},
	{NG=(NG1->NG2)},
	translate_method_body(G1, Info, NG1, L0, L1),
	translate_method_body(G2, Info, NG2, R0, R1),
	{prolog:comp_layout2(LayIn, L1, R1, LayOut)}.
translate_method_body(if(G1,G2,G3), Info, NG, LayIn, LayOut) --> !,
	{prolog:decomp_layout3(LayIn, L0, M0, R0)},
	{NG=if(NG1,NG2,NG3)},
	translate_method_body(G1, Info, NG1, L0, L1),
	translate_method_body(G2, Info, NG2, M0, M1),
	translate_method_body(G3, Info, NG3, R0, R1),
	{prolog:comp_layout3(LayIn, L1, M1, R1, LayOut)}.
translate_method_body((\+ G), Info, NG, LayIn, LayOut) --> !,
	{prolog:arg1_layout(LayIn, L0)},
	{NG=(\+ NG1)},
	translate_method_body(G, Info, NG1, L0, L1),
	{prolog:comp_layout1(LayIn, L1, LayOut)}.
translate_method_body(!, _, NG, L0, L1) --> !,
	{NG=!, L0=L1}.          % regard ! as control
translate_method_body((A=B), Info, NG, LayIn, LayOut) --> !,
	{context_srcmodule(Info, Src),
	 NG = Src:(A=B)},	% regard explicit unification as "native"
	{prolog:condense_layout(LayIn, LN),
	 prolog:comp_layout2(LN, LN, LN, LayOut)}.
translate_method_body((:: G), Info, NG, LayIn, LayOut) --> !,
	{context_object(Info, O)},
	translate_method_send(O, G, O, Info, NG, LayIn, LayOut).
translate_method_body((<: G), Info, NG, LayIn, LayOut) --> !,
	{context_object(Info, O),
	 context_self(Info, Self)},
	translate_method_send(O, G, Self, Info, NG, LayIn, LayOut).
translate_method_body(O :: G, Info, NG, LayIn, LayOut) --> !,
	translate_method_send(O, G, O, Info, NG, LayIn, LayOut).
translate_method_body(O <: G, Info, NG, LayIn, LayOut) --> !,
	{context_self(Info, Self)},
	translate_method_send(O, G, Self, Info, NG, LayIn, LayOut).
translate_method_body(self(X), Info, NG, LayIn, LayOut) --> !,
	{context_srcmodule(Info, Src),
	 NG=Src:(X=Self),
	 context_self(Info, Self)},
	{prolog:condense_layout(LayIn, LN),
	 prolog:comp_layout2(LN, LN, LN, LayOut)}.
translate_method_body(M:G, Info, NG, LayIn, LayOut) --> !, 
	{prolog:decomp_layout2(LayIn, L0, R0)},
	translate_prolog_call(G, M, Info, NG, L0, R0, LayOut).
translate_method_body(:G, Info, NG, LayIn, LayOut) --> !,
	{context_srcmodule(Info, Src)},
	{prolog:arg1_layout(LayIn, L0)},
	translate_prolog_call(G, Src, Info, NG,  L0, [], LayOut).
translate_method_body(G, Info, NG, LayIn, LayOut) -->
	{context_self(Info, Self)},
	translate_method_send(Self, G, Self, Info, NG, LayIn, LayOut).


%--------------------------------------------------------------------------

translate_method_send(Object, Msg, Self, Info, NG, LayIn, LayOut) -->
	{var(Object)}, !,
	translate_method_send1(Msg, Object, Self, Info, NG, LayIn, LayOut).
translate_method_send(self, Msg, _, Info, NG, LayIn, LayOut) --> !,
	{context_self(Info, Self)},
	translate_method_send1(Msg, Self, Self, Info, NG, LayIn, LayOut).
translate_method_send(super, Msg, Self, Info, NG, LayIn, LayOut) --> !,
	{context_object(Info, Myself)},
	{   Self == super ->
	    translate_super_send(Msg, Myself, ObjVar, ObjVar, NG, LayIn, LayOut)
	;   translate_super_send(Msg, Myself, Self, _, NG, LayIn, LayOut)
	}.
translate_method_send(Object, Msg, Self, Info, NG, LayIn, LayOut) -->
	translate_method_send1(Msg, Object, Self, Info, NG, LayIn, LayOut).

% The fact that we don't know the methods of the supers at compile
% time makes super-calls inefficient
translate_super_send(Msg, Myself, Self, ObjVar, NG, LayIn, LayOut) :-
	nonvar(Msg), !,
	NG = objects:call_super_exp(Myself,ExpMsg,ObjVar),
	expand_literal(Msg, ExpMsg, Self, ObjVar),
	prolog:condense_layout(LayIn, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut).
translate_super_send(Msg, Myself, Self, ObjVar, NG, LayIn, LayOut) :-
	var(Msg),
	NG = objects:call_super(Myself,Msg,Self,ObjVar),
	prolog:condense_layout(LayIn, LN),
	prolog:comp_layout2(LN, LN, LN, LayOut).

translate_method_send1(Msg, Object, Self, _, NG, LayIn, LayOut) -->
	{var(Msg), !,
	 NG = objects:call_object(Object, Msg, Self)},
	{prolog:condense_layout(LayIn, LN),
         prolog:comp_layout2(LN, LN, LN, LayOut)}.
translate_method_send1(get(X), Object, Self, Info, NG, LayIn, LayOut) --> !,
	(   {var(X)} ->
	    transl_method_send_general(Object, get(X,Self,Object), Info, NG, LayIn, LayOut)
	;   {functor(X, Key, Ar),
	     (   Ar=1 ->
		 arg(1, X, Val),
		 G = prolog:'$get_module_data'(Module,Key,Val)
	     ;   gen_get(Ar, Module, Key, Ar, X, G)
	     ),
	     pre_goal_exp_layout(object_module(Self,Module), objects, G, NG, LayIn, LayOut)}
	).
translate_method_send1(set(X), Object, Self, Info, NG, LayIn, LayOut) --> !,
	(   {var(X)} ->
	    transl_method_send_general(Object, set(X,Self,Object), Info, NG, LayIn, LayOut)
	;   {functor(X, Key, Ar),
	     (   Ar=1 ->
		 arg(1, X, Val),
		 G = prolog:'$set_module_data'(Module,Key,Val)
	     ;   gen_set(Ar, Module, Key, Ar, X, G)
	     ),
	     pre_goal_exp_layout(object_module(Self,Module), objects, G, NG, LayIn, LayOut)}
	).
translate_method_send1(Msg, Object, Self, Info, NG, LayIn, LayOut) -->
	{expand_literal(Msg, NG0, Self, Object)},
	transl_method_send_general(Object, NG0, Info, NG, LayIn, LayOut).

transl_method_send_general(Object, NG0, _, NG, LayIn, LayOut) -->
	{nonvar(Object), !,
	 objects:object_module(Object, M),
	 NG = M:NG0,
	 prolog:condense_layout(LayIn, LN),
	 prolog:comp_layout2(LN, LN, LN, LayOut)},
	[M].
transl_method_send_general(Object, NG0, _, NG, LayIn, LayOut) -->
	{NG=objects:call_apply(NG0,Object),
	 prolog:condense_layout(LayIn, LN),
	 prolog:comp_layout2(LN, LN, LN, LayOut)}.


%--------------------------------------------------------------------------
% Inlining of get/set attribute, multiple args

gen_get(1, Module, Key, Arity, X, G) :- !,
	arg(1, X, Val),
	G = prolog:'$get_module_data'(Module,Key,Arity,0,Val).
gen_get(N, Module, Key, Arity, X, G) :-
	arg(N, X, Val),
	N1 is N-1,
	G = (prolog:'$get_module_data'(Module,Key,Arity,N1,Val),G0),
	gen_get(N1, Module, Key, Arity, X, G0).

gen_set(1, Module, Key, Arity, X, G) :- !,
	arg(1, X, Val),
	G = prolog:'$set_module_data'(Module,Key,Arity,0,Val).
gen_set(N, Module, Key, Arity, X, G) :-
	arg(N, X, Val),
	N1 is N-1,
	G = (prolog:'$set_module_data'(Module,Key,Arity,N1,Val),G0),
	gen_set(N1, Module, Key, Arity, X, G0).


% Replaces an undocumented feature.
translate_prolog_call(G, Module, _, Module:G, L0, R0, LayOut) -->
	{prolog:comp_layout2(L0, L0, R0, LayOut)}.

%--------------------------------------------------------------------------
% Expands a method call/head with two args, Self and Proto. Since it
% is also used for some runtime goals, it is speeded up a bit by
% unrolling.

expand_literal(M, EM, Self, Proto) :-
	functor(M, N, A),
	A1 is A+1,
	A2 is A1+1,
	functor(EM, N, A2),
	arg(A1, EM, Self),
	arg(A2, EM, Proto),
	copy_args(A, M, EM).

contract_literal(EM, M, Self, Proto) :-
	functor(EM, N, A2),
	A1 is A2-1,
	A1 > 0,
	A is A1-1,
	functor(M, N, A),
	arg(A1, EM, Self),
	arg(A2, EM, Proto),
	copy_args(A, EM, M).

copy_args(0, _, _) :- !.
copy_args(1, M, EM) :- !,
	arg(1, M, A1), arg(1, EM, A1).
copy_args(2, M, EM) :- !,
	arg(1, M, A1), arg(1, EM, A1),
	arg(2, M, A2), arg(2, EM, A2).
copy_args(3, M, EM) :- !,
	arg(1, M, A1), arg(1, EM, A1),
	arg(2, M, A2), arg(2, EM, A2),
	arg(3, M, A3), arg(3, EM, A3).
copy_args(4, M, EM) :- !,
	arg(1, M, A1), arg(1, EM, A1),
	arg(2, M, A2), arg(2, EM, A2),
	arg(3, M, A3), arg(3, EM, A3),
	arg(4, M, A4), arg(4, EM, A4).
copy_args(5, M, EM) :- !,
	arg(1, M, A1), arg(1, EM, A1),
	arg(2, M, A2), arg(2, EM, A2),
	arg(3, M, A3), arg(3, EM, A3),
	arg(4, M, A4), arg(4, EM, A4),
	arg(5, M, A5), arg(5, EM, A5).
copy_args(N, M, EM) :-
	arg(N, M, AN), arg(N, EM, AN),
	N1 is N-1,
	copy_args(N1, M, EM).

/* Alternative
expand_literal(M, EM, Self, Proto) :-
	prolog:dcg_translate_dcg_atom(M, EM, Self, Proto).
*/

%--------------------------------------------------------------------------
% This is primarily intended for expanding object_module/2 to true in
% advance in order to be able to peephole away the trues.

pre_goal_exp_layout(Goal, Module, SecGoal, ConGoal, LayIn, LayOut) :-
	user:goal_expansion(Goal, Module, ExpGoal), !,
	(   ExpGoal = true ->
	    ConGoal = SecGoal,
	    prolog:condense_layout(LayIn, LayOut)
	;   ConGoal = (Module:ExpGoal,SecGoal),
	    prolog:condense_layout(LayIn, LN),
	    prolog:comp_layout2(LN, LL, LN, LayOut),
	    prolog:comp_layout2(LN, LN, LN, LL)	    
	).
pre_goal_exp_layout(Goal, Module, SecGoal, (Module:Goal,SecGoal), LayIn, LayOut) :-
	prolog:condense_layout(LayIn, LN),
        prolog:comp_layout2(LN, LL, LN, LayOut),
	prolog:comp_layout2(LN, LN, LN, LL).

%--------------------------------------------------------------------------
% This is primarily intended for expanding object_module/2 to true in
% advance in order to be able to peephole away the trues.

pre_goal_exp(Goal, Module, SecGoal, ConGoal) :-
	user:goal_expansion(Goal, Module, ExpGoal), !,
	(   ExpGoal = true ->
	    ConGoal = SecGoal
	;   ConGoal = (Module:ExpGoal,SecGoal)
	).
pre_goal_exp(Goal, Module, SecGoal, (Module:Goal,SecGoal)).

end_of_file.

% Undocumented pre-3.7 code:

%--------------------------------------------------------------------------
% All this for expanding object calls inside Prolog goals
% (e.g. findall(X, object::m(X), Xs))

translate_prolog_call(G, Module, _, NG, L0, R0, LayOut) -->
	{var(G), !, 
	NG=Module:G },
	{prolog:comp_layout2(L0, L0, R0, LayOut)}.
translate_prolog_call((G1,G2), Module, Info, NG, L0, R0, LayOut) --> !,
	{NG=(NG1,NG2)},
	{prolog:decomp_layout2(R0, L1, R1)},
	translate_prolog_call(G1, Module, Info, NG1, L0, L1, PCL0),
	translate_prolog_call(G2, Module, Info, NG2, L0, R1, PCL1),
	{prolog:comp_layout2(PCL0, PCL0, PCL1, LayOut)}.
translate_prolog_call((G1;G2), Module, Info, NG, L0, R0, LayOut) --> !,
	{prolog:decomp_layout2(R0, L1, R1)},
	{NG=(NG1;NG2)},
	translate_prolog_call(G1, Module, Info, NG1, L0, L1, PCL0),
	translate_prolog_call(G2, Module, Info, NG2, L0, R1, PCL1),
	{prolog:comp_layout(PCL0, PCL0, PCL1, LayOut)}.
translate_prolog_call((G1->G2), Module, Info, NG, L0, R0, LayOut) --> !,
	{prolog:decomp_layout2(R0, L1, R1)},
	{NG=(NG1->NG2)},
	translate_prolog_call(G1, Module, Info, NG1, L0, L1, PCL0),
	translate_prolog_call(G2, Module, Info, NG2, L0, R1, PCL1),
	{prolog:comp_layout(PCL0, PCL0, PCL1, LayOut)}.
translate_prolog_call(if(G1,G2,G3), Module, Info, NG, L0, R0, LayOut) --> !,
	{prolog:decomp_layout3(R0, L1, M1, R1)},
	{NG=if(NG1,NG2,NG3)},
	translate_prolog_call(G1, Module, Info, NG1, L0, L1, PCL0),
	translate_prolog_call(G2, Module, Info, NG2, L0, M1, PCL1),
	translate_prolog_call(G3, Module, Info, NG3, L0, R1, PCL2),
	{prolog:comp_layout3(PCL0, PCL0, PCL1, PCL2, LayOut)}.
translate_prolog_call((\+ G), Module, Info, NG, L0, R0, LayOut) --> !,
	{prolog:arg1_layout(R0, L1)},
	{NG=(\+ NG1)},
	translate_prolog_call(G, Module, Info, NG1, L0, L1, PCL),
	{prolog:comp_layout1(PCL, PCL, LayOut)}.
translate_prolog_call(G, Module, Info, NG, L0, R0, LayOut) -->
	{functor(G, Name, Arity),
	 functor(Decl, Name, Arity),
	 builtin_meta_predicate(Decl), !,
	 functor(NG1, Name, Arity),
	 NG = Module:NG1,
	 context_module(Info, ContxtModule)},
	translate_meta(Arity, G, Decl, NG1, Info, ContxtModule),
	{   R0 = [] ->
	    prolog:condense_layout(L0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
	;   prolog:condense_layout(R0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
	}.
translate_prolog_call(G, Module, Info, NG, L0, R0, LayOut) -->
	{predicate_property(Module:G, (meta_predicate Decl)),
	 \+ predicate_property(Module:G, built_in), !,
	 NG = Module:NG1,
	 functor(G, Name, Arity),
	 functor(NG1, Name, Arity),
	 context_module(Info, ContxtModule)},
	translate_meta(Arity, G, Decl, NG1, Info, ContxtModule),
	{   R0 = [] ->
	    prolog:condense_layout(L0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
	;   prolog:condense_layout(R0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
        }.
translate_prolog_call(G, Module, _, NG, L0, R0, LayOut) --> 
	{NG=Module:G},
	{   R0 = [] ->
	    prolog:condense_layout(L0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
	;   prolog:condense_layout(R0, LN),
	    prolog:comp_layout2(LN, LN, L0, LayOut)
        }.


translate_meta(0, _, _, _, _, _) --> !.
translate_meta(N, G, Decl, NG, Info, Module) -->
	{arg(N, Decl, DeclArg),
	 arg(N, G, SubG)},
	translate_meta(DeclArg, SubG, Info, Module, NSubG),
	{arg(N, NG, NSubG),
	 NN is N - 1},
	translate_meta(NN, G, Decl, NG, Info, Module).

translate_meta(:, SubG, Info, Module, NSubG) --> !,
	translate_method_body(SubG, Info, NSubG0, [], _),
	{   NSubG0 = _:_ ->
	    NSubG = NSubG0
	;   NSubG = Module:NSubG0
	}.
translate_meta(_, SubG, _, _, SubG) --> [].

builtin_meta_predicate(time_out(:,?,?)).
builtin_meta_predicate(freeze(:,?)).
builtin_meta_predicate(on_exception(?,:,:)).
builtin_meta_predicate(setof(?,:,?)).
builtin_meta_predicate(bagof(?,:,?)).
builtin_meta_predicate(findall(?,:,?)).
builtin_meta_predicate(findall(?,:,?,?)).
builtin_meta_predicate(call(:)).
builtin_meta_predicate(incore(:)).
builtin_meta_predicate(? ^ :).
builtin_meta_predicate(when(?,:)).
builtin_meta_predicate(call_residue(:,?)).
builtin_meta_predicate(call_cleanup(:,:)).
