/* Copyright(C) 1997, Swedish Institute of Computer Science */

:- multifile
	user:term_expansion/2,
	user:goal_expansion/3.

:- dynamic
	user:term_expansion/2,
	user:goal_expansion/3.

user:term_expansion((Head-:Body), Expansion) :- !,
        fd_expansion(Head, Body, 0, Expansion).
user:term_expansion((Head+:Body), Expansion) :- !,
        fd_expansion(Head, Body, 1, Expansion).
user:term_expansion((Head-?Body), Expansion) :- !,
        fd_expansion(Head, Body, 2, Expansion).
user:term_expansion((Head+?Body), Expansion) :- !,
        fd_expansion(Head, Body, 3, Expansion).

type_neck(0, -:).
type_neck(1, +:).
type_neck(2, -?).
type_neck(3, +?).

fd_expansion(Head1, Body, Type,
	     [(:-clpfd:'$fd_install'(Spec, Module, Type,
	                              InfoLength, Info))]) :-
	(   Head1=Module:Head -> true
	;   Head1=Head,
	    prolog_load_context(module, Module)
	),
	functor(Head, Functor, Arity),
	Spec = Functor/Arity,
	Head =..[Functor|M],
	numbervars(Head, 0, _),
	type_neck(Type, Neck),
	(   on_exception(Error,
	                 compile(Body, Type, Info, M),
		         handle_compile_exception(Error, Spec, Neck))
	->  true
	;   error_info(Spec, Neck, Info)
	),
	length(Info, InfoLength),
	check_info(Type, Info, Spec, Neck).

check_info(0, Info, Spec, Neck) :-
	check_info(1, Info, Spec, Neck).
check_info(1, Info, Spec, Neck) :-
	info_lhs(Info, Lhs),
	sort(Lhs, Set),
	length(Lhs, N1),
	length(Set, N2),
	(   N1=:=N2 -> true
	;   print_message(warning, format('multiple propagating indexicals for an argument of ~q (clause ~a)',[Spec,Neck]))
	).
check_info(2, Info, Spec, Neck) :-
	check_info(3, Info, Spec, Neck).
check_info(3, Info, Spec, Neck) :-
	length(Info, N),
	(   N=:=1 -> true
	;   print_message(warning, format('multiple checking indexicals for ~q (clause ~a)',[Spec,Neck]))
	).

info_lhs([], []).
info_lhs([info(_,_,P,_,_,_,_)|L1], [P|L2]) :- info_lhs(L1, L2).

error_info(Spec,Neck, [info([],0,0,Code,Len,[],0)]) :-
	print_message(warning, format('compilation of FD predicate clause failed for ~q (clause ~a)',[Spec,Neck])),
	emit(error, Len, Code, []).


handle_compile_exception(domain(D), Spec,Neck) :- !,
	print_message(warning, format('~q - bad domain in finite domain range in ~q (clause ~a)',[D,Spec,Neck])),
	fail.
handle_compile_exception(term(T), Spec,Neck) :- !,
	print_message(warning, format('~q - bad term in finite domain range in ~q (clause ~a)',[T,Spec,Neck])),
	fail.
handle_compile_exception(idmember(X), Spec,Neck) :- !,
	print_message(warning, format('~q - unknown variable in finite domain range in ~q (clause ~a)',[X,Spec,Neck])),
	fail.
handle_compile_exception(Excp) :-
	print_message(error, Excp),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FD COMPILER
%% Generating DCG's
%%

compile((Expr1,Expr2), Type, Codes, M) :- !,
	compile(Expr1, Type, Codes1, M),
	compile(Expr2, Type, Codes2, M),
	append(Codes1, Codes2, Codes).
compile(X in R, Type, Codes, _) :- !,
	Codes = [info(Linkage,LinkageLength,Pruned,Code,CodeLength,Lit,LitLength)],
	indexical(X, R, Type, Linkage, LinkageLength,
	          Pruned, CodeLength, [], Lit, Code, []),
	length(Lit, LitLength).
compile(element(X,L,Y), Type, Codes, M) :- !,
	compile_element(X, L, Y, Body),
	compile(Body, Type, Codes, M).
compile(relation(X,L,Y), Type, Codes, M) :- !,
	compile_relation(X, L, Y, Body),
	compile(Body, Type, Codes, M).
compile(Constraint, Type, Codes, M) :-
	fd_expandable(Constraint, L, R, Rel),
	compile_arith(Rel, L, R, M, Body),
	compile(Body, Type, Codes, M).

indexical('$VAR'(Pruned), R, Type, Linkage, LinkageLength, Pruned, Length, L0, L) -->
	{   integer(Pruned) -> true
        ;   raise_exception(idmember('$VAR'(Pruned)))
        },
						% ground(Mon) if used for tell
						% ground(Amon) if used for ask
	{decode_pragma(R, R1, P)},
	domain_top_level(R1, Op, [], state(Linkage1,0,Length0,Mon,Amon), L0, L),
	emit(ret(Type,Op,P), Unit),
	{map_const(Mon, 8, Mon1),
	 map_const(Amon, 16, Amon1),
	 merge(Mon1, Linkage1, Linkage2),
	 merge(Amon1, Linkage2, Linkage3),
	 merge_for_ask(Type, R1, Pruned, Linkage3, Linkage),
	 length(Linkage, LinkageLength),
	 Length is Length0+Unit}.

decode_pragma(R!, R, 1) :- !.
decode_pragma(R, R, 0).

domain_top_level(R, 7, Qs, State, L0, L) -->
	{simple(R)}, !,
	domain(R, Qs, State, L0, L).
domain_top_level('$VAR'(X), 7, Qs, State, L0, L) --> !,
	domain('$VAR'(X), Qs, State, L0, L).
domain_top_level(R0, 6, Qs, State, L0, L) -->
	{unit_term(R0, R)}, !,
	{state_copy(State, State1, MonAmon, MonAmon, S, G)},
	term(R, Qs, State1, L0, L),
	{ord_union(S, G, MonAmon)}.
domain_top_level(Expr, 7, Qs, State, L0, L) -->
	{set_expression(Expr, _)}, !,
	domain(Expr, Qs, State, L0, L).
domain_top_level({R}, 7, Qs, State, L0, L) --> !,
	domain({R}, Qs, State, L0, L).
domain_top_level(Min..Max, 0, Qs, State, L0, L) -->
	{Min==inf, Max==sup}, !,
	void(Qs, State, L0, L).
domain_top_level(Min..Max, 1, Qs, State, L0, L) -->
	{Min==inf}, !,
	term(Max, Qs, State, L0, L).
domain_top_level(Min..Max, 2, Qs, State, L0, L) -->
	{Max==sup}, !,
	{state_copy(State, State1, G, S, S, G)},
	term(Min, Qs, State1, L0, L).
domain_top_level(Min..Max, 3, Qs, State, L0, L) --> !,
	{State = state(Linkage,CodeOffset,Length,Mon,Amon)},
	term(Min, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1),
	{CodeOffset1 is CodeOffset + Length1},
	term(Max, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(G1, S2, Mon),
	 ord_union(S1, G2, Amon),
	 Length is Length1 + Length2}.
domain_top_level(\R0, 4, Qs, State, L0, L) -->
	{unit_term(R0, R)}, !,
	{state_copy(State, State1, MonAmon, MonAmon, S, G)},
	term(R, Qs, State1, L0, L),
	{ord_union(S, G, MonAmon)}.
domain_top_level(\gdom('$VAR'(No)), 5, _, State, L, L) --> !, % unused
	{State = state([],_,Unit,[],[])},
	emit(gdom(No), Unit).
domain_top_level(gdom('$VAR'(No)), 7, _, State, L, L) --> !, % unused
	{State = state([],_,Unit,[],[])},
	emit(gdom(No), Unit).
domain_top_level(\R, 5, Qs, State, L0, L) --> !,
	{state_copy(State, State1, G, S, S, G)},
	domain(R, Qs, State1, L0, L).
domain_top_level(R, 7, Qs, State, L0, L) -->
	domain(R, Qs, State, L0, L).

unit_term({R}, R) :- 
	(   var(R) -> true
	;   R='$VAR'(_) -> true
        ;   R=(_,_) -> fail
        ;   true
        ).

void(_, state([],_,0,[],[]), L, L) --> [].

state_copy(state(Linkage,CodeOffset,Length,A,B),
	   state(Linkage,CodeOffset,Length,C,D), A, B, C, D).
	

term_or_domain(R0, Qs, State, L0, L, term) -->
	{unit_term(R0, R)}, !,
	{State = state(Linkage,CodeOffset,Length,MonAmon,MonAmon)},
	term(R, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L),
	{ord_union(S, G, MonAmon)}.
term_or_domain(R, Qs, State, L0, L, domain) -->
	domain(R, Qs, State, L0, L).

is_domain(-) :- !, fail.
is_domain(dom(_)).
is_domain({_}).
is_domain(_.._).
is_domain(\_).
is_domain(_/\_).
is_domain(_\/_).
is_domain(_?_).
is_domain(R1+R2) :- is_domain(R1); is_domain(R2).
is_domain(R1-R2) :- is_domain(R1); is_domain(R2).
is_domain(-R2) :- is_domain(R2).
is_domain(R1 mod R2) :- is_domain(R1); is_domain(R2).
is_domain(unionof(_,_,_)).
is_domain(switch(_,_)).

domain(Dom, _, _, _, _) -->
	{simple(Dom)}, !,
	{raise_exception(domain(Dom))}.
domain('$VAR'(X), _, _, _, _) --> !,
	{raise_exception(domain('$VAR'(X)))}.
domain(D, Qs, State, L0, L) -->
	{unit_term(D, T)}, !,
	{State = state(Linkage,CodeOffset,Length,MonAmon,MonAmon)},
	term(T, Qs, state(Linkage,CodeOffset,Length1,S,G), L0, L),
	emit(dup_range, Unit),
	{ord_union(S, G, MonAmon),
	 Length is Length1 + Unit}.
domain(D, _, State, L0, L) -->
	{set_expression(D, Set)}, !,
	{State = state([],_,Unit,[],[])},
	{lookup_literal(L0, d(Set), 0, X, L)},
	emit(const_dom(X), Unit).
domain({D1,D2}, Qs, State, L0, L) --> !,
	domain({D1}\/{D2}, Qs, State, L0, L).
domain(dom(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-1],_CodeOffset,Unit,[],[No])},
            {L = L0},
	    emit(dom(No), Unit)
	;   {var(X)}
	->  {State = state([],_,Unit,[],[])},
            {L = L0},
	    {var_nth(X, Qs, 0, No)},
            emit(qval(No), Unit1),
	    emit(dup_range, Unit2),
	    {Unit is Unit1+Unit2}
	;   domain({X}, Qs, State, L0, L)
	).
domain(Min .. Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L).
domain(D1 /\ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{nonvar(D2), D2 = \ND2}, !,
	term_or_domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(ND2, Qs, state(Linkage2,CodeOffset1,Length2,Amon2,Mon2), L1, L, Op2),
	emit(subtract(Op1,Op2), Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1, Mon2, Mon),
	  ord_union(Amon1, Amon2, Amon),
	  Length is Length1 + Length2 + Unit}.
domain(D1 /\ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L, Op2),
	emit(inter(Op1,Op2), Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1, Mon2, Mon),
	  ord_union(Amon1, Amon2, Amon),
	  Length is Length1 + Length2 + Unit}.
domain(D1 ? Univ \/ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Univ == (inf..sup)}, !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(check_union(Offset2), Unit),
	{CodeOffset1 is CodeOffset + Length1 + Unit},
	domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	{Offset2 is CodeOffset1+Length2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon),
	 ord_union(Amon1,Amon2,Amon),
	 Length is Length1+Length2+Unit}.
domain(D1 \/ D2,Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D1,Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(D2,Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L, Op2),
	emit(union(Op1,Op2), Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(Mon1, Mon2, Mon),
	 ord_union(Amon1, Amon2, Amon),
	 Length is Length1 + Length2 + Unit}.
domain(\D, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D, Qs, state(Linkage,CodeOffset,Length1,Amon,Mon), L0, L, Op),
	emit(compl(Op), Unit),
	{Length is Length1 + Unit}.
/*
domain(R1 + R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1+B), Qs, State, L0, L).
*/
domain(R1 + R2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(R1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	domain(R2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(setplus, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1,Mon2,Mon),
	  ord_union(Amon1,Amon2,Amon),
	Length is Length1 + Length2 + Unit}.
domain(R + T, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setadd, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(S2,G2,Tmp),
	  ord_union(Mon1,Tmp,Mon),
	  ord_union(Amon1,Tmp,Amon),
	Length is Length1 + Length2 + Unit}.
domain(- R, Qs, State, L0, L) --> !,
	domain(0 - R, Qs, State, L0, L).
domain(T - R, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_term(T), is_domain(R)}, !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setneg, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(S2,G2,Tmp),
	  ord_union(Mon1,Tmp,Mon),
	  ord_union(Amon1,Tmp,Amon),
	Length is Length1 + Length2 + Unit}.
/*
domain(R1 - R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1-B), Qs, State, L0, L).
*/
domain(R1 - R2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(R1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	domain(R2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(setminus, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1,Mon2,Mon),
	  ord_union(Amon1,Amon2,Amon),
	Length is Length1 + Length2 + Unit}.
domain(R - T, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setsub, Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(S2,G2,Tmp),
	 ord_union(Mon1,Tmp,Mon),
	 ord_union(Amon1,Tmp,Amon),
	 Length is Length1 + Length2 + Unit}.
domain(R1 mod R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1 mod B), Qs, State, L0, L).
domain(D mod T, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	domain(D, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setmod, Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	ord_union(S2,G2,Tmp),
	ord_union(S1,Tmp,S),
	ord_union(G1,Tmp,G),
	Length is Length1 + Length2 + Unit}.
domain(D1 ? D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(check(Offset2), Unit),
	{CodeOffset1 is CodeOffset + Length1 + Unit},
	domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	{Offset2 is CodeOffset1+Length2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon),
	 ord_union(Amon1,Amon2,Amon),
	 Length is Length1+Length2+Unit}.
domain(unionof(B,D1,D2), Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(unionof(CodeOffset2), Unit1),
	{CodeOffset1 is CodeOffset + Length1 + Unit1},
	{length(Qs, BIndex), append(Qs, [B], Qs1)},
	domain(D2, Qs1, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(unionof_next(BIndex,CodeOffset1), Unit2),
	{CodeOffset2 is CodeOffset1 + Length2 + Unit2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon3),
	 ord_union(Amon1,Amon2,Amon3),
	 ord_del_element(Mon3, B, Mon),
	 ord_del_element(Amon3, B, Amon),
	 Length is Length1+Length2+Unit1+Unit2}.
domain(switch(T,Keylist), Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term(T, Qs, state(Linkage0,CodeOffset,Length0,S0,G0), L0, L1),
	emit(switch(Index,CodeOffset2), Unit),
	{CodeOffset1 is CodeOffset+Length0+Unit},
	table(Keylist, Qs, state(Linkage1,CodeOffset1,Length1,S1,G1), Table, L1, L2),
	{Length is Length0+Unit+Length1, CodeOffset2 is CodeOffset+Length},
	{merge(Linkage0,Linkage1,Linkage),
	 ord_union(S0,S1,Mon),
	 ord_union(G0,G1,Amon),
	 lookup_literal(L2, h(Table), 0, Index, L)}.
domain(Dom, _, _, _, _) -->
	{raise_exception(domain(Dom))}.

range(Min, Max, _, state([],_,Unit,[],[]), L, L) -->
	{Min==inf, Max==sup}, !,
	emit(range(open,open), Unit).
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Min==inf}, !,
	term(Max, Qs, state(Linkage,CodeOffset,Length2,Mon,Amon), L0, L),
	emit(range(open,closed), Unit),
	{Length is Length2 + Unit}.
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Max==sup}, !,
	term(Min, Qs, state(Linkage,CodeOffset,Length1,Amon,Mon), L0, L),
	emit(range(closed,open), Unit),
	{Length is Length1 + Unit}.
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	term(Min, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1),
	{CodeOffset1 is CodeOffset + Length1},
	term(Max, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(range(closed,closed), Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(G1, S2, Mon),
	 ord_union(S1, G2, Amon),
	 Length is Length1 + Length2 + Unit}.


is_term(N) :- simple(N).
is_term('$VAR'(_)).
is_term(min(_)).
is_term(max(_)).
is_term(card(_)).
is_term(T1+T2) :- is_term(T1), is_term(T2).
is_term(T1-T2) :- is_term(T1), is_term(T2).
is_term(-T) :- is_term(T).
is_term(_*_).
is_term(_/>_).
is_term(_/<_).
is_term(T1 mod T2) :- is_term(T1), is_term(T2).

term(N, Qs, state([],_,Unit,[],[]), L, L) --> 
	{var(N)}, !,
	{var_nth(N, Qs, 0, No)},
	emit(qval(No), Unit).
term(N, _, state([],_,Unit,[],[]), L0, L) -->
	{simple(N)}, !,
	{lookup_literal(L0, N, 0, X, L)},
	emit(const(X), Unit).
term('$VAR'(No), _, state([],_,Unit,[No],[No]), L, L) --> !,
	emit(val(No), Unit).
term(min(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-2],_,Unit,[No],[])},
            {L = L0},
            emit(min(No), Unit)
	;   term(X, Qs, State, L0, L)
        ).
term(max(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-4], _,Unit,[],[No])},
            {L = L0},
            emit(max(No), Unit)
	;   term(X, Qs, State, L0, L)
        ).
term(card(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-1], _,Unit,[],[No])},
            {L = L0},
	    emit(card(No), Unit)
	;   term(1, Qs, State, L0, L)
	).
term(T1+T2, Qs, Tuple, L0, L) --> !,
	binary_term(add, sym, T1, T2, Qs, Tuple, L0, L).
term(T1-T2, Qs, Tuple, L0, L) --> !,
	binary_term(sub, asym, T1, T2, Qs, Tuple, L0, L).
term(-T2, Qs, Tuple, L0, L) --> !,
	binary_term(sub, asym, 0, T2, Qs, Tuple, L0, L).
term(T1*T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(mult, sym, T1, T2, Qs, Tuple, L0, L).
term(T1/>T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(divu, asym, T1, T2, Qs, Tuple, L0, L).
term(T1/<T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(divd, asym, T1, T2, Qs, Tuple, L0, L).
term(T1 mod T2, Qs, Tuple, L0, L) --> !,
	binary_term(mod, amon, T1, T2, Qs, Tuple, L0, L).
term(Term, _, _, _, _) -->
	{raise_exception(term(Term))}.

binary_term(Op, Mon, T1, T2, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	term(T1, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T2, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(Op, Unit),
	{merge(Linkage1, Linkage2, Linkage),
	 binary_monotonicity(Mon, S1, G1, S2, G2, S, G),
	 Length is Length1 + Length2 + Unit}.

binary_term_val(Op, Mon, T1, T2, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	term(T1, Qs, state(Linkage,CodeOffset,Length1,S1,G1), L0, L1), 
	(   {integer(T2), T2>=0}
	->  {SG2 = []},
	    {lookup_literal(L1, T2, 0, X, L)},
	    emit_imm(Op, X, Unit)
	;   {nonvar(T2), T2 = '$VAR'(No)}
	->  {SG2 = [No], L = L1},
	    emit_val(Op, No, Unit)
	;   {var(T2)}
	->  {SG2 = [], L = L1},
	    {var_nth(T2, Qs, 0, No)},
	    emit_qval(Op, No, Unit)
	),
	{binary_monotonicity(Mon, S1, G1, SG2, SG2, S, G),
	 Length is Length1 + Unit}.

binary_monotonicity(sym, S1, G1, S2, G2, S, G) :-
	ord_union(S1, S2, S),
	ord_union(G1, G2, G).
binary_monotonicity(asym, S1, G1, S2, G2, S, G) :-
	ord_union(S1, G2, S),
	ord_union(G1, S2, G).
binary_monotonicity(amon, S1, G1, S2, G2, SG, SG) :-
	ord_union([S1,G1,S2,G2], SG).

unary_term(Op, T1, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	term(T1, Qs, state(Linkage,CodeOffset,Length1,S,G), L0, L),
	emit(Op, Unit),
	{Length is Length1 + Unit}.

table([], Qs, State, [], L0, L) --> void(Qs, State, L0, L).
table([Con-D|Keylist], Qs, State, [Con-CodeOffset|Table], L0, L) -->
	{State = state(Linkage,CodeOffset,Length,S,G),
	 State1 = state(Linkage1,CodeOffset,Length1,S1,G1)},
	domain(D, Qs, State1, L0, L1),
	emit(popj, Unit),
	{CodeOffset1 is CodeOffset+Length1+Unit},
	{State2 = state(Linkage2,CodeOffset1,Length2,S2,G2)},
	table(Keylist, Qs, State2, Table, L1, L),
	{merge(Linkage1, Linkage2, Linkage),
	 ord_union(S1, S2, S),
	 ord_union(G1, G2, G),
	 Length is Length1+Length2+Unit}.



% xref fd_insn.h for now!  Operands:
% a(I) - argreg offset
% l(I) - code offset, becomes immediate label
% t(I) - literal offset, becomes immediate tagged
% h(I) - literal offset, becomes immediate hash table
%% [MC] 3.8.6: made determinate
emit(dup_range, 1) --> [0].
emit(range(open,open), 1) --> !, [1].
emit(range(open,closed), 1) --> !, [2].
emit(range(closed,open), 1) --> !, [3].
emit(range(closed,closed), 1) --> [4].
emit(setadd, 1) --> [5].
emit(setsub, 1) --> [6].
emit(setmod, 1) --> [7].
emit(setneg, 1) --> [73].
emit(setplus, 1) --> [74].
emit(setminus, 1) --> [75].
emit(compl(term), 1) --> !, [8].
emit(compl(domain), 1) --> [9].
emit(union(term,term), 1) --> !, [10].
emit(union(term,domain), 1) --> !, [11].
emit(union(domain,term), 1) --> !, [12].
emit(union(domain,domain), 1) --> [13].
emit(inter(term,term), 1) --> !, [14].
emit(inter(term,domain), 1) --> !, [15].
emit(inter(domain,term), 1) --> !, [16].
emit(inter(domain,domain), 1) --> [17].
emit(subtract(term,term), 1) --> !, [76].
emit(subtract(term,domain), 1) --> !, [77].
emit(subtract(domain,term), 1) --> !, [78].
emit(subtract(domain,domain), 1) --> [79].
emit(qval(X), 2) --> [18,a(X)].
emit(add, 1) --> [19].
emit(sub, 1) --> [20].
% 21, 22, 23 -- see below
emit(mod, 1) --> [24].
emit(val(0), 1) --> !, [26].
emit(val(1), 1) --> !, [27].
emit(val(2), 1) --> !, [28].
emit(val(X), 2) --> [25,a(X)].
emit(dom(0), 1) --> !, [30].
emit(dom(1), 1) --> !, [31].
emit(dom(2), 1) --> !, [32].
emit(dom(X), 2) --> [29,a(X)].
emit(gdom(1), 1) --> !, [71].			% unused
emit(min(0), 1) --> !, [34].
emit(min(1), 1) --> !, [35].
emit(min(2), 1) --> !, [36].
emit(min(X), 2) --> [33,a(X)].
emit(max(0), 1) --> !, [38].
emit(max(1), 1) --> !, [39].
emit(max(2), 1) --> !, [40].
emit(max(X), 2) --> [37,a(X)].
emit(const(C), 2) --> [41,t(C)].
emit(const_dom(C), 2) --> [41,d(C)].
% 42, 43, 44 -- see below
emit(ret(Type,Op,P), 2) --> {Type<2}, !, [B,a(P)], {B is 45+Op+8*(Type>>1)}.
emit(ret(Type,Op,_), 1) --> [B], {B is 45+Op+8*(Type>>1)}.
emit(check_union(Op), 2) --> [61,l(Op)].
emit(check(Op), 2) --> [62,l(Op)].
emit(unionof(Op), 2) --> [63,l(Op)].
emit(unionof_next(Bi,Op), 3) --> [64,a(Bi),l(Op)].
emit(card(0), 1) --> !, [66].
emit(card(1), 1) --> !, [67].
emit(card(2), 1) --> !, [68].
emit(card(X), 2) --> [65,a(X)].
emit(switch(Index,Join), 3) --> [69,h(Index),l(Join)].
emit(popj, 1) --> [70].
emit(error, 1) --> [72].

emit_imm(mult, I, 2) --> [21,t(I)].
emit_imm(divd, I, 2) --> [22,t(I)].
emit_imm(divu, I, 2) --> [23,t(I)].

emit_val(mult, X, 2) --> [42,a(X)].
emit_val(divd, X, 2) --> [43,a(X)].
emit_val(divu, X, 2) --> [44,a(X)].

emit_qval(mult, X, 2) --> [80,a(X)].
emit_qval(divd, X, 2) --> [81,a(X)].
emit_qval(divu, X, 2) --> [82,a(X)].


lookup_literal([], Lit, I, I, [Lit]).
lookup_literal([X|L1], Lit, I, K, [X|L2]) :-
	(   X==Lit
	->  K = I, L2 = L1
	;   J is I+1,
	    lookup_literal(L1, Lit, J, K, L2)
	).

var_nth(X, [Y|_], M, N) :- X == Y, !, N = M.
var_nth(X, [_|Ys], I, N) :-
	J is I+1,
	var_nth(X, Ys, J, N).

map_const([], _C, []).
map_const([Var|Links1], C, [Var-C|Links2]) :-
	map_const(Links1, C, Links2).

merge_for_ask(Type, R, Pruned, S, [Pruned-Ix|S]) :-
	Type /\ 2 =:= 2, !,			% for ask
	merge_for_ask(R, Ix).
merge_for_ask(_, _, _, S, S).

merge_for_ask(R, Ix) :-
	simple(R), !, Ix = 16.
merge_for_ask('$VAR'(_), Ix) :- !,
	Ix = 16.
merge_for_ask(A..B, Ix) :- !,
	(   A==inf -> Ix = 4
	;   B==sup -> Ix = 2
	;   Ix = 6
	).
merge_for_ask(_, 1).


merge([], L, L).
merge([X|Xs], X2, [E|X3]) :-
	merge2(X, X2, E, LeftOvers),
	merge(Xs, LeftOvers, X3).

%% [MC] 3.8.6: made determinate
merge2(V, [], V, []) :- !.
merge2(V1-N1, [V2-N2|R], V1-N3, R) :-
	V1 == V2, !,
	N3 is N1 \/ N2.
merge2(V1, [X|Xs], X2s, [X|LeftOvers]) :-
	merge2(V1, Xs, X2s, LeftOvers).


in_set_goal(X, Set, Goal) :-
	'$fd_size'(Set, _, 1), !,
	in_set_goal_1(X, Set, Goal).
in_set_goal(X, Set, clpfd:in_set_aux_rt(X,Set)).

in_set_goal_1(X, Set, Goal) :-
	integer(X), !,
	(   '$fd_dom_contains'(Set, X) -> Goal = true
	;   Goal = false
	).
in_set_goal_1(X, [[Min|Max]], Goal) :- !,
	Goal = clpfd:propagate_interval(X,Min,Max).
in_set_goal_1(X, Set, clpfd:prune_and_propagate(X,Set)).


expand_arith(Rel, X, Y, Goal) :-
 	(   trans_goal(X, Rel, Y, List)
	->  (   List = [Elt] -> single_goal(Elt, Goal)
            ;   andify(List, Goal)
	    )
	;   Goal0 =.. [Rel,X,Y],
	    ill_formed_constraint(Goal0)
	).

ill_formed_constraint(Constraint) :-
	prolog:illarg(domain(term,constraint), Constraint, 0).

single_goal(T=C, Goal) :- fd_dvar(T), fd_integer(C), !,
	Goal = 't=c'(T,C).
single_goal(X=Y, Goal) :- !, Goal = 'x=y'(X,Y).
single_goal(Goal, Goal).

fd_dvar(Arg) :- var(Arg), !.
fd_dvar(Arg) :-
	integer(Arg),
	\+prolog:'$large_data'(0, Arg, _).

fd_integer(Arg) :-
	integer(Arg),
	\+prolog:'$large_data'(0, Arg, _).

set_expression(-, _) :- !, fail.
set_expression({S}, Mask) :-
	union_expression(S, Mask).
set_expression(A..B, Set) :-
	atomic(A),
	atomic(B),
	'$fd_range'(A, B, Set, 1).
set_expression(\D, Mask) :-
	set_expression(D, Mask1),
	'$fd_dom_complement'(Mask1, Mask).
set_expression(D1/\D2, Mask) :-
	set_expression(D1, Mask1),
	set_expression(D2, Mask2),
	'$fd_dom_intersection'(Mask1, Mask2, Mask).
set_expression(D1\/D2, Mask) :-
	set_expression(D1, Mask1),
	set_expression(D2, Mask2),
	'$fd_dom_union'(Mask1, Mask2, Mask).

union_expression(-, _) :- !, fail.
union_expression(I, Set) :-
	integer(I), !,
	'$fd_range'(I, I, Set, 1).
union_expression((S1,S2), Set) :-
	union_expression(S1, Set1),
	union_expression(S2, Set2),
	'$fd_dom_union'(Set1, Set2, Set).


%%% indexical expansion of element(X,L,Y)
%%% Warning! Assumes X and Y have reasonable domains.

compile_element(X, L, Y, (X in XExpr!, Y in YExpr!)) :-
	XExpr = unionof(B,dom(Y) /* /\YSet */, switch(B,Keylist)),
	values_indices(L, 1, Keylist0),
	keysort(Keylist0, Keylist1),
	keymerge(Keylist1, Keylist2),
	commafy_values(Keylist2, Keylist),
	sort(L, Values),
	length(L, Ln),
	length(Values, Vn),
	(   Vn<<1 =< Ln
        ->  element_disjuncts(Keylist2, X, YExpr)
	;   YExpr = unionof(B,dom(X) /* /\(1..Ln) */, switch(B,Poslist)),
	    indices_values(L, 1, Poslist)
	).

values_indices([], _, []).
values_indices([V|L1], I, [V-[I]|L2]) :-
	J is I+1,
	values_indices(L1, J, L2).

commafy_values([], []).
commafy_values([K-V1|L1], [K-{V2}|L2]) :-
	commafy(V1, V2),
	commafy_values(L1, L2).

element_disjuncts([V-Ixs], X, ((dom(X)/\{Set}) ? {V})) :- !,
	commafy(Ixs, Set).
element_disjuncts([V-Ixs|L], X, ((dom(X)/\{Set}) ? {V})\/Union) :-
	commafy(Ixs, Set),
	element_disjuncts(L, X, Union).


indices_values([], _, []).
indices_values([V|L1], I, [I-{V}|L2]) :-
	J is I+1,
	indices_values(L1, J, L2).


%%% indexical expansion of relation/3

compile_relation(X, R, Y, (X in XExpr!, Y in YExpr!)) :-
	XExpr = unionof(B,dom(Y) /* /\ (YMin..YMax)*/,switch(B,R1)),
	YExpr = unionof(B,dom(X) /* /\ (XMin..XMax)*/,switch(B,R)),
	relation_inverse(R, R1),
	/* keys(R, XD),
	   min_max(XD, XMin, XMax),
	   keys(R1, YD),
	   min_max(YD, YMin, YMax),
	*/
	true.

min_max(List, Min, Max) :-
	List = [Min|_],
	last(List, Max).

relation_inverse(R, R1) :-
	inverted_pairs(R, L1, []),
	keysort(L1, L2),
	inverted_relation(L2, R1).
 
inverted_pairs([]) --> [].
inverted_pairs([Key-Set|L]) -->
	{set_expression(Set, Expr)},
	domain_pairs(Expr, Key),
	inverted_pairs(L).

domain_pairs([], _) --> !.
domain_pairs([[L|U]|D], Key) -->
	range_pairs(L, U, Key),
	domain_pairs(D, Key).

range_pairs(U, U, Key) --> !, [U-Key].
range_pairs(L, U, Key) --> [L-Key],
	{L1 is L+1},
	range_pairs(L1, U, Key).

inverted_relation([], []).
inverted_relation([Key-Elt|L1], [Key-{S}|L]) :-
	inverted_relation(L1, Key, Set, L2),
	commafy([Elt|Set], S),
	inverted_relation(L2, L).

inverted_relation([Key-Elt|L1], Key, [Elt|Set], L) :- !,
	inverted_relation(L1, Key, Set, L).
inverted_relation(L, _, _, L).

commafy([], true).
commafy([X|L], Conj) :- commafy(L, X, Conj).

commafy([], X, X).
commafy([Y|L], X, (X,Conj)) :- commafy(L, Y, Conj).




fd_expandable(E1 #= E2, E1, E2, #=).
fd_expandable(E1 #< E2, E1, E2-1, #=<).
fd_expandable(E1 #=< E2, E1, E2, #=<).
fd_expandable(E1 #\= E2, E1, E2, #\=).
fd_expandable(E1 #> E2, E2+1, E1, #=<).
fd_expandable(E1 #>= E2, E2, E1, #=<).

andify([], true).
andify([X=Y|L], Conj) :- !, X=Y, andify(L, Conj).
andify([X|L], Conj) :- andify(L, X, Conj).

andify([], X, X).
andify([X=Y|L], X0, Conj) :- !, X=Y, andify(L, X0, Conj).
andify([Y|L], X, (X,Conj)) :- andify(L, Y, Conj).

reify(T = U,			true,	'x=y'(T,U)).
reify('x=y'(T,U),		true,	'x=y'(T,U)).
reify('x\\=y'(T,U),		true,	'x\\=y'(T,U)).
reify('x=<y'(T,U),		true,	'x=<y'(T,U)).
reify('x+y=t'(X,Y,T),		'x+y=t'(X,Y,Z),	'x=y'(T,Z)).
reify('ax=t'(A,X,T),		'ax=t'(A,X,Z),	'x=y'(T,Z)).
reify('ax+y=t'(A,X,Y,T),	'ax+y=t'(A,X,Y,Z),	'x=y'(T,Z)).
reify('ax+by=t'(A,X,B,Y,T),	'ax+by=t'(A,X,B,Y,Z),	'x=y'(T,Z)).
reify('t=c'(T,C),		true,		'x=y'(T,C)).
reify('t=<c'(T,C),		true,		'x=<y'(T,C)).
reify('t\\=c'(T,C),		true,		'x\\=y'(T,C)).
reify('t>=c'(T,C),		true,		'x=<y'(C,T)).
reify('t=u+c'(T,U,C),	't=u+c'(Z,U,C),	'x=y'(T,Z)).
reify('t=<u+c'(T,U,C),	't=u+c'(Z,U,C),	'x=<y'(T,Z)).
reify('t\\=u+c'(T,U,C),	't=u+c'(Z,U,C),	'x\\=y'(T,Z)).
reify('t>=u+c'(T,U,C),	't=u+c'(Z,U,C),	'x=<y'(Z,T)).
reify('t+u=c'(T,U,C),	'x+y=t'(T,U,Z),	'x=y'(Z,C)).
reify('t+u=<c'(T,U,C),	'x+y=t'(T,U,Z),	'x=<y'(Z,C)).
reify('t+u\\=c'(T,U,C),	'x+y=t'(T,U,Z),	'x\\=y'(Z,C)).
reify('t+u>=c'(T,U,C),	'x+y=t'(T,U,Z),	'x=<y'(C,Z)).
reify('t=x+y+c'(T,X,Y,C),	't=x+y+c'(U,X,Y,C),	'x=y'(T,U)).
reify('x+y=u+c'(X,Y,U,C),	('x+y=t'(X,Y,T), 't=u+c'(Z,U,C)), 'x=y'(T,Z)).
reify('x+y+z=c'(X,Y,Z,C),	('x+y=t'(X,Y,T), 'x+y=t'(T,Z,U)), 'x=y'(U,C)).
reify(scalar_product(As,Xs,#=,S1),      scalar_product([-1|As],[T|Xs],#=,0), 'x=y'(T,S1)) :- !.
reify(scalar_product(As,Xs,Rel,S1),     scalar_product([-1|As],[T|Xs],#=,0), Goal) :-
	t_rel_u(Rel, T, S1, [Goal], []).

trans_goal(E1, Rel, E2, Goals) :-
	linearize_rel(E1, Rel, E2, S, []),
	trans_goal(S, Goals).

trans_goal([], []).
trans_goal([nonlinear(C)|S], [C|G]) :- !,
	trans_goal(S, G).
/*
trans_goal([linear(E1,Rel,E2)|S], Goals0) :-
	normalize(E1-E2, 1, Poly0, []),
	keysort(Poly0, Poly1),
	keyfuse(Poly1, Poly2),
	split(Poly2, LHS0, [], RHS0, []),
	get_constant(LHS0, LCon1, LHS1, 0, GCD0),
	get_constant(RHS0, RCon1, RHS1, GCD0, GCD1),
	Con1 is LCon1-RCon1,
	GCD is gcd(GCD1,Con1),
	(   GCD =< 1
	->  LHS = LHS1, RHS = RHS1, Con = Con1
	;   scale_poly(LHS1, GCD, LHS),
	    scale_poly(RHS1, GCD, RHS),
	    Con is Con1//GCD
	),
	trans_rel(LHS, Con, RHS, 0, Rel, Goals0, Goals),
	trans_goal(S, Goals).
*/
% now translating to linear/4 constraints!
trans_goal([linear(E1,Rel,E2)|S], Goals0) :-
	normalize(E1-E2, 1, Poly0, []),
	keysort(Poly0, Poly1),
	keyfuse(Poly1, Poly2),
	isolate(Poly2, A, X, As1, Xs1, Sum),
	(   A =< 0 -> As2 = As1, Rel1 = Rel, Sum1 = Sum
	;   Sum1 is -Sum,
	    rel_inverse(Rel, Rel1),
	    neg_all(As1, As2)
	),
	trans_linear(As2, Xs1, Sum1, Rel1, X, Goals0, Goals),
	trans_goal(S, Goals).

neg_all([], []).
neg_all([X|Xs], [Y|Ys]) :- Y is -X, neg_all(Xs, Ys).

trans_linear([], [], S, Rel, X) --> !,
	{rel_inverse(Rel, Inv)},
	t_rel_c(Inv, X, S).
trans_linear([1], [Y], S, Rel, X) --> {integer(X)}, !,
	{HL is X-S},
	t_rel_c(Rel, Y, HL).
trans_linear([1], [Y], 0, Rel, X) --> !,
	t_rel_u(Rel, Y, X).
trans_linear([1], [Y], S, Rel, X) -->
	{'$fd_debugging'(0)}, !,
	{S1 is -S},
	t_rel_u_c(Rel, Y, X, S1).
trans_linear([-1], [Y], S, Rel, X) -->
	{'$fd_debugging'(0)}, !,
	{rel_inverse(Rel, Rel1)},
	t_u_rel_c(Rel1, X, Y, S).
trans_linear(As, Xs, S, Rel, X) -->
	(   {integer(X)}
	->  {S1 is X-S},
	    peep_linear(As, Xs, Rel, S1)
	;   {S1 is -S},
	    peep_linear([-1|As], [X|Xs], Rel, S1)
	).
/*
trans_linear(As, Xs, S, Rel, X) -->
	(   {integer(X)}
	->  {S1 is X-S},
	    peep_linear([-1|As], [Temp|Xs], S1)
	;   {S1 is -S},
	    peep_linear([-1,-1|As], [Temp,X|Xs], S1)
	),
	t_rel_c(Rel, Temp, 0).
*/

peep_linear([-1|As], Xs, Rel, S) --> !,
	{neg_all(As, As1), S1 is -S},
	{rel_inverse(Rel, Inv)},
	peep_linear([1|As1], Xs, Inv, S1).
peep_linear([A], [X], #=, S) --> !,
	(   {A =:= 0, S =:= 0} -> []
	;   {A =:= 0} -> [fail]
	;   {S mod A =:= 0} -> {Q is S//A}, [X = Q]
	;   [fail]
	).
peep_linear(As, Xs, Rel, S) -->
	{'$fd_debugging'(1)}, !,
	[scalar_product(As,Xs,Rel,S)].
peep_linear([1,1], [X,Y], Rel, S) --> !, t_u_rel_c(Rel,X,Y,S).
peep_linear([1,-1], [X,Y], Rel, 0) --> !, t_rel_u(Rel,X,Y).
peep_linear([1,-1], [X,Y], Rel, S) --> !, t_rel_u_c(Rel,X,Y,S).
peep_linear([1,1,1], [X,Y,Z], #=, S) --> !, ['x+y+z=c'(X,Y,Z,S)].
peep_linear([1,1,-1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(X,Y,Z)].
peep_linear([1,1,-1], [X,Y,Z], #=, S) --> !, ['x+y=u+c'(X,Y,Z,S)].
peep_linear([1,-1,1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(X,Z,Y)].
peep_linear([1,-1,1], [X,Y,Z], #=, S) --> !, ['x+y=u+c'(X,Z,Y,S)].
peep_linear([1,-1,-1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(Y,Z,X)].
peep_linear([1,-1,-1], [X,Y,Z], #=, S) --> !, ['t=x+y+c'(X,Y,Z,S)].
peep_linear(As, Xs, Rel, S) --> [scalar_product(As,Xs,Rel,S)].


isolate([], 0, 0, [], [], 0).
isolate([X- -1|Poly], -1, X, As, Xs, S) :- var(X), !,
	isolate(Poly, As, Xs, S).
isolate([X-1|Poly], 1, X, As, Xs, S) :- var(X), !,
	isolate(Poly, As, Xs, S).
isolate([N-S], 0, 0, [], [], S) :- N==1, !.
isolate([_-0|Poly], A1, X1, As1, Xs1, S) :- !,
	isolate(Poly, A1, X1, As1, Xs1, S).
isolate([X-A|Poly], A1, X1, [A|As1], [X|Xs1], S) :-
	isolate(Poly, A1, X1, As1, Xs1, S).


isolate([], [], [], 0).
isolate([N-S], [], [], S) :- N==1, !.
isolate([_-0|Poly], As1, Xs1, S) :- !,
	isolate(Poly, As1, Xs1, S).
isolate([X-A|Poly], [A|As1], [X|Xs1], S) :-
	isolate(Poly, As1, Xs1, S).


linearize_rel(E1, Rel, E2) -->
	linearize(E1, 1, L1),
	linearize(E2, 1, L2),
	[linear(L1, Rel, L2)].

linearize(E,   I, J) --> {integer(E)}, !, {J is I*E}.
linearize(E,   I, I*E) --> {simple(E)}, !.
linearize(K*E, I, L) --> {integer(K)}, !,
	{J is I*K},
	linearize(E, J, L).
linearize(E*K, I, L) --> {integer(K)}, !,
	{J is I*K},
	linearize(E, J, L).
linearize(E1*E2, I, I*Y) -->
	{E1 == E2}, !,
	linearize_rel(E1, #=, X),
	[nonlinear('x*x=y'(X,Y))].
linearize(E1*E2, I, I*Z) -->
	linearize_rel(E1, #=, X),
	linearize_rel(E2, #=, Y),
	[nonlinear('x*y=z'(X,Y,Z))].
linearize(E1/E2, I, I*Z) -->
	linearize_rel(E1, #=, X),
	linearize_rel(E2, #=, Y),
	[nonlinear('x/y=z'(X,Y,Z))].
linearize(E1 mod E2, I, I*Z) -->
	linearize_rel(E1, #=, X),
	linearize_rel(E2, #=, Y),
	[nonlinear('x mod y=z'(X,Y,Z))].
linearize(min(E1,E2), I, I*Z) -->
	linearize_rel(E1, #=, X),
	linearize_rel(E2, #=, Y),
	[nonlinear('min(x,y)=z'(X,Y,Z))].
linearize(max(E1,E2), I, I*Z) -->
	linearize_rel(E1, #=, X),
	linearize_rel(E2, #=, Y),
	[nonlinear('max(x,y)=z'(X,Y,Z))].
linearize(abs(E1), I, I*Y) -->
	linearize_rel(E1, #=, X),
	[nonlinear('|x|=y'(X,Y))].
linearize(E1+E2, I, L1+L2) -->
	linearize(E1, I, L1),
	linearize(E2, I, L2).
linearize(E1-E2, I, L1-L2) -->
	linearize(E1, I, L1),
	linearize(E2, I, L2).


normalize(I, Sign) --> {I = '$VAR'(_)}, !, [I-Sign].
normalize(I, Sign) --> {atomic(I)}, !,
	[Item],
	{   integer(I) -> J is I*Sign, Item = 1-J
	;   Item = I-Sign
	}.
normalize(K*I, Sign) --> !, {integer(K), J is K*Sign}, [I-J].
normalize(E1+E2, Sign) --> !,
	normalize(E1, Sign),
	normalize(E2, Sign).
normalize(E1-E2, Sign) --> !,			% unofficial
	{Neg is -Sign},
	normalize(E1, Sign),
	normalize(E2, Neg).

keys([], []).
keys([K-_|L1], [K|L2]) :- keys(L1, L2).

values([], []).
values([_-V|L1], [V|L2]) :- values(L1, L2).

keyfuse([], []).
keyfuse([K1-I,K2-J|L1], L2) :- K1==K2, !, IJ is I+J, keyfuse([K1-IJ|L1], L2).
keyfuse([X|L1], [X|L2]) :- keyfuse(L1, L2).

keymerge([], []).
keymerge([K-I,K-J|L1], L2) :- !, append(I,J,IJ), keymerge([K-IJ|L1], L2).
keymerge([X|L1], [X|L2]) :- keymerge(L1, L2).

split([], LHS, LHS, RHS, RHS).
split([K-I|Poly], LHS0, LHS, RHS0, RHS) :-
	compare(Key, I, 0),
	split(Key, K, I, Poly, LHS0, LHS, RHS0, RHS).

split(<, K, I, Poly, LHS0, LHS, [K-J|RHS0], RHS) :-
	J is -I,
	split(Poly, LHS0, LHS, RHS0, RHS).
split(=, _, _, Poly, LHS0, LHS, RHS0, RHS) :-
	split(Poly, LHS0, LHS, RHS0, RHS).
split(>, K, I, Poly, [K-I|LHS0], LHS, RHS0, RHS) :-
	split(Poly, LHS0, LHS, RHS0, RHS).

get_constant([], 0, [], GCD, GCD).
get_constant([V-C|Poly], C, Poly, GCD0, GCD) :- V==1, !,
	get_constant(Poly, GCD0, GCD).
get_constant([V-C|Poly0], Con, [V-C|Poly], GCD0, GCD) :-
	GCD1 is gcd(GCD0,C),
	get_constant(Poly0, Con, Poly, GCD1, GCD).

get_constant([], GCD, GCD).
get_constant([_-C|Poly], GCD0, GCD) :-
	GCD1 is gcd(GCD0,C),
	get_constant(Poly, GCD1, GCD).

scale_poly([], _, []).
scale_poly([V-C1|L1], D, [V-C2|L2]) :-
	C2 is C1//D,
	scale_poly(L1, D, L2).

trans_rel([], C, [], D, Rel) --> !,
	{compare(Key, C, D),
	 key_code(Key, KeyCode),
	 rel_code(Rel, RelCode)},
	(   {KeyCode/\RelCode>0} -> []
	;   [fail]
	).
trans_rel([], C, RHS, D, Rel) --> !,
	{rel_inverse(Rel, Rel1)},
	{Con is C-D},
	trans_poly_rel_c(RHS, Rel1, Con).
trans_rel(LHS, C, [], D, Rel) --> !,
	{Con is D-C},
	trans_poly_rel_c(LHS, Rel, Con).
trans_rel(LHS, C, RHS, D, #=) --> !,
	{compare(Key, C, D),
	 length(LHS, Ln),
	 length(RHS, Rn)},
	{trans_poly(Ln, LHS, [], T, S0, S)},
	{trans_poly(Rn, RHS, [], U, S, [])},
	trans_eq_by_two(Key, S0, T, C, U, D).
trans_rel(LHS, C, RHS, D, Rel) -->
	{compare(Key, C, D),
	 length(LHS, Ln),
	 length(RHS, Rn)},
	trans_poly(Ln, LHS, [], T),
	trans_poly(Rn, RHS, [], U),
	trans_rel_by_two(Key, T, C, U, D, Rel).

% divide and conquer translation
trans_poly_rel_c([U-C], Rel, Con) --> !,
	trans_poly_1(C, U, T),
	t_rel_c(Rel, T, Con).
trans_poly_rel_c(Poly0, #=, Con) --> !,
	{length(Poly0, I)},
	{J is I>>1, K is I-J},
	{trans_poly(J, Poly0, Poly1, T0, S0, S)},
	{trans_poly(K, Poly1, [], T1, S, [])},
	t_u_eq_c(S0, T0, T1, Con).
trans_poly_rel_c(Poly0, Rel, Con) -->
	{length(Poly0, I)},
	{J is I>>1, K is I-J},
	trans_poly(J, Poly0, Poly1, T0),
	trans_poly(K, Poly1, [], T1),
	t_u_rel_c(Rel, T0, T1, Con).

% peep hole opt:
% x+y=t, t+z=c --> x+y+z=c
% y+z=t, x+z=c --> x+y+z=c
t_u_eq_c([], T0, T1, C) --> ['t+u=c'(T0,T1,C)].
t_u_eq_c(['x+y=t'(X,Y,T)|L], T0, T1, C) -->
	{T==T0}, !,
	dlist(L),
	['x+y+z=c'(X,Y,T1,C)].
t_u_eq_c(['x+y=t'(X,Y,T)|L], T0, T1, C) -->
	{T==T1}, !,
	dlist(L),
	['x+y+z=c'(T0,X,Y,C)].
t_u_eq_c([X|L], T0, T1, C) --> [X],
	t_u_eq_c(L, T0, T1, C).

trans_poly(1, [U-C|Poly], Poly, T) --> !, trans_poly_1(C, U, T).
trans_poly(2, [U-C,V-D|Poly], Poly, T) --> !, trans_poly_2(C, U, D, V, T).
trans_poly(3, [U-C,V-D,W-E|Poly], Poly, T) --> !,
	trans_poly_2(C, U, D, V, T0),
	trans_poly_2(1, T0, E, W, T).
trans_poly(I, Poly0, Poly, T) -->
	{J is I>>1, K is I-J},
	trans_poly(J, Poly0, Poly1, T0),
	trans_poly(K, Poly1, Poly, T1),
	['x+y=t'(T0,T1,T)].

trans_poly_1(1, U, U) --> !.
trans_poly_1(C, U, T) --> ['ax=t'(C,U,T)].

trans_poly_2(1, U, 1, V, T) --> !, ['x+y=t'(U,V,T)].
trans_poly_2(1, U, D, V, T) --> !, ['ax+y=t'(D,V,U,T)].
trans_poly_2(C, U, 1, V, T) --> !, ['ax+y=t'(C,U,V,T)].
trans_poly_2(C, U, D, V, T) --> ['ax+by=t'(C,U,D,V,T)].

trans_rel_by_two(<, T, C, U, D, Rel) -->
	{Con is D-C},
	t_rel_u_c(Rel, T, U, Con).
% trans_rel_by_two(=, T, _, U, _, #=) --> !, [T=U]. why not?
trans_rel_by_two(=, T, _, U, _, Rel) -->
	t_rel_u(Rel, T, U).
trans_rel_by_two(>, T, C, U, D, Rel) -->
	{Con is C-D,
	 rel_inverse(Rel, Rel1)},
	t_rel_u_c(Rel1, U, T, Con).

trans_eq_by_two(<, S, T, C, U, D) -->
	{Con is D-C},
	t_eq_u_c(S, T, U, Con).
% trans_eq_by_two(=, S, T, _, U, _) --> [T=U], %% #=
% 	dlist(S).
trans_eq_by_two(=, S, T, _, U, _) -->
	t_eq_u(S, U, T).
trans_eq_by_two(>, S, T, C, U, D) -->
	{Con is C-D},
	t_eq_u_c(S, U, T, Con).

% peep hole opt:
% x+y=t, t=u+c --> x+y=u+c
% x+y=u, t=u+c --> t=x+y+c
t_eq_u_c([], T0, T1, C) --> ['t=u+c'(T0,T1,C)].
t_eq_u_c(['x+y=t'(X,Y,T)|L], T0, T1, C) -->
	{T==T0}, !,
	dlist(L),
	['x+y=u+c'(X,Y,T1,C)].
t_eq_u_c(['x+y=t'(X,Y,T)|L], T0, T1, C) -->
	{T==T1}, !,
	dlist(L),
	['t=x+y+c'(T0,X,Y,C)].
t_eq_u_c([X|L], T0, T1, C) --> [X],
	t_eq_u_c(L, T0, T1, C).

t_eq_u([], T0, T1) --> [T0=T1 /*'t=u'(T0,T1)*/].
t_eq_u(['x+y=t'(X,Y,T)|L], T0, T1) -->
	{T==T0}, !,
	dlist(L),
	['x+y=t'(X,Y,T1)].
t_eq_u(['x+y=t'(X,Y,T)|L], T0, T1) -->
	{T==T1}, !,
	dlist(L),
	['x+y=t'(X,Y,T0)].
t_eq_u([X|L], T0, T1) --> [X],
	t_eq_u(L, T0, T1).

t_rel_c(#=, T, Con) -->
	[T=Con /*'t=c'(T,Con)*/].
t_rel_c(#=<, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t=<c'(T,Con)]; ['x=<y'(T,Con)]).
t_rel_c(#\=, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t\\=c'(T,Con)]; ['x\\=y'(T,Con)]).
t_rel_c(#>=, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t>=c'(T,Con)]; ['x=<y'(Con,T)]).

t_rel_u_c(#=, T, U, Con) --> ['t=u+c'(T,U,Con)].
t_rel_u_c(#=<, T, U, Con) --> ['t=<u+c'(T,U,Con)].
t_rel_u_c(#\=, T, U, Con) --> ['t\\=u+c'(T,U,Con)].
t_rel_u_c(#>=, T, U, Con) --> ['t>=u+c'(T,U,Con)].

t_rel_u(#=, T, U) --> [T=U /*'x=y'(T,U) bad for linear eq.*/].
t_rel_u(#=<, T, U) --> ['x=<y'(T,U)].
t_rel_u(#\=, T, U) --> ['x\\=y'(T,U)].
t_rel_u(#>=, T, U) --> ['x=<y'(U,T)].

t_u_rel_c(#=, T, U, Con) --> ['t+u=c'(T,U,Con)].
t_u_rel_c(#=<, T, U, Con) --> ['t+u=<c'(T,U,Con)].
t_u_rel_c(#\=, T, U, Con) --> ['t+u\\=c'(T,U,Con)].
t_u_rel_c(#>=, T, U, Con) --> ['t+u>=c'(T,U,Con)].

dlist([]) --> [].
dlist([X|L]) --> [X], dlist(L).

rel_code(#=,  2'010).
rel_code(#=<, 2'110).
rel_code(#\=, 2'101).
rel_code(#>=, 2'011).

key_code(<,   2'100).
key_code(=,   2'010).
key_code(>,   2'001).

rel_inverse(#=,  #=).
rel_inverse(#=<, #>=).
rel_inverse(#\=, #\=).
rel_inverse(#>=, #=<).

%%% Wholesale translation to indexicals.

compile_arith(Rel, L, R, Vars, Body) :-
	normalize(L-R, 1, Poly0, []),
	keysort(Poly0, Poly1),
	keyfuse(Poly1, Poly2),
	split(Poly2, LHS0, [], RHS0, []),
	get_constant(LHS0, LCon1, LHS1, 0, GCD0),
	get_constant(RHS0, RCon1, RHS1, GCD0, GCD1),
	Con1 is RCon1-LCon1,
	GCD is gcd(GCD1,Con1),
	(   GCD =< 1
	->  LHS = LHS1, RHS = RHS1, Con = Con1
	;   scale_poly(LHS1, GCD, LHS),
	    scale_poly(RHS1, GCD, RHS),
	    Con is Con1//GCD
	),
	compile_arith(Rel, Vars, LHS, RHS, Con, Body).

compile_arith(#\=, Vars, LHS, RHS, Con, Body) :- !,
	rhs_append(RHS, LHS, Poly),
	diseq_to_indexicals(Vars, Poly, Con, List, []),
	andify(List, Body).
compile_arith(Rel, Vars, LHS, RHS, Con, Body) :-
	rhs_append(RHS, LHS, Poly),
	rel_to_indexicals(Vars, Poly, Con, Rel, List, []),
	andify(List, Body).

rhs_append([], Poly, Poly).
rhs_append([X-N|RHS], LHS, [X-M|Poly]) :-
	M is -N,
	rhs_append(RHS, LHS, Poly).

diseq_to_indexicals([], _, _) --> [].
diseq_to_indexicals([X|Xs], Poly, Con) --> 
	{select_var(X-N, Poly, Poly1), !,
	 compare(Key, N, 0),
	 Con1 is Con-1,
	 rel_to_ix(Key, N, Con1, Poly1, Below1, Above1, #=<),
	 Con2 is Con+1,
	 rel_to_ix(Key, N, Con2, Poly1, Below2, Above2, #>=)},
	emit_indexical(N, X, (Below1..Above1)\/(Below2..Above2)),
	diseq_to_indexicals(Xs, Poly, Con).
diseq_to_indexicals([_|Xs], Poly, Con) -->
	diseq_to_indexicals(Xs, Poly, Con).

rel_to_indexicals([], _, _, _) --> [].
rel_to_indexicals([X|Xs], Poly, Con, Rel) -->
	{select_var(X-N, Poly, Poly1), !,
	 compare(Key, N, 0),
	 rel_to_ix(Key, N, Con, Poly1, Below, Above, Rel)},
	emit_indexical(N, X, Below..Above),
	rel_to_indexicals(Xs, Poly, Con, Rel).
rel_to_indexicals([_|Xs], Poly, Con, Rel) -->
	rel_to_indexicals(Xs, Poly, Con, Rel).

rel_to_ix(<, N, Con, Poly, Below2, Above2, Rel) :-
	M is -N,
	Con1 is -Con,
	rel_inverse(Rel, Ler),
        poly_below(Poly, 1, Con1, Below),
	poly_above(Poly, 1, Con1, Above),
	scaled_bounds(M, Below, Above, Below1, Above1),
	select_below_above(Ler, Below1, Above1, Below2, Above2).
rel_to_ix(>, N, Con, Poly, Below2, Above2, Rel) :-
	poly_above(Poly, -1, Con, Above),
	poly_below(Poly, -1, Con, Below),
	scaled_bounds(N, Below, Above, Below1, Above1),
	select_below_above(Rel, Below1, Above1, Below2, Above2).

select_below_above(#=<, _,     Above,   inf, Above).
select_below_above(#= , Below, Above, Below, Above).
select_below_above(#>=, Below, _,     Below, sup).

scaled_bounds(1, Below, Above, Below, Above) :- !.
scaled_bounds(N, Below, Above, Below/>N, Above/<N).

poly_below([], _, Expr, Expr).
poly_below([X-N|Poly], S, Expr0, Expr) :-
	M is S*N,
	(   M>0
        ->  Expr1 = Expr0+Term,
	    coefficient(M, min(X), Term)
        ;   Expr1 = Expr0-Term,
	    M1 is -M,
	    coefficient(M1, max(X), Term)
        ),
	poly_below(Poly, S, Expr1, Expr).


poly_above([], _, Expr, Expr).
poly_above([X-N|Poly], S, Expr0, Expr) :-
	M is S*N,
	(   M>0
        ->  Expr1 = Expr0+Term,
	    coefficient(M, max(X), Term)
        ;   Expr1 = Expr0-Term,
	    M1 is -M,
	    coefficient(M1, min(X), Term)
        ),
	poly_above(Poly, S, Expr1, Expr).

coefficient(1, Term, Term) :- !.
coefficient(I, Term, Term*I).

select_var(X-N, [Y-N|Tail], Tail) :- X==Y, !.
select_var(Element, [Head|Tail1], [Head|Tail2]) :- 
	select_var(Element, Tail1, Tail2).

emit_indexical(-1, X, R) --> !, [X in R!].
emit_indexical(1, X, R) --> !, [X in R!].
emit_indexical(_, X, R) --> [X in R].

fd_commafy([], true).
fd_commafy([true|L], Conj) :- !, fd_commafy(L, Conj).
fd_commafy([X|L], Conj) :- fd_ify(X, X1), fd_commafy(L, X1, Conj).

fd_commafy([], X, X).
fd_commafy([true|L], X, Conj) :- !, fd_commafy(L, X, Conj).
fd_commafy([Y|L], X, (X,Conj)) :- fd_ify(Y, Y1), fd_commafy(L, Y1, Conj).

fd_ify(M:P, M:P) :- !.
fd_ify(X=Y, X=Y) :- !.
fd_ify(G, clpfd:G).


dlist_foldeq([Last], Last) --> !.
dlist_foldeq([X=Y|L], Last) --> !, {X=Y}, dlist_foldeq(L, Last).
dlist_foldeq([Goal|L], Last) --> [Goal], dlist_foldeq(L, Last).

user:goal_expansion(X in Expr, _, Goal) :- !,
	fd_goal_expand_in(X, Expr, Goal).
user:goal_expansion(X in_set Set, _, Goal) :- !,
	in_set_goal(X, Set, Goal).
user:goal_expansion(X #= Y, _, clpfd:Goal) :- !,
	expand_arith(#=, X, Y, Goal).
user:goal_expansion(X #< Y, _, clpfd:Goal) :- !,
	expand_arith(#=<, X, Y-1, Goal).
user:goal_expansion(X #=< Y, _, clpfd:Goal) :- !,
	expand_arith(#=<, X, Y, Goal).
user:goal_expansion(X #\= Y, _, clpfd:Goal) :- !,
	expand_arith(#\=, X, Y, Goal).
user:goal_expansion(X #> Y, _, clpfd:Goal) :- !,
	expand_arith(#=<, Y+1, X, Goal).
user:goal_expansion(X #>= Y, _, clpfd:Goal) :- !,
	expand_arith(#=<, Y, X, Goal).
user:goal_expansion(Prop iff B, M, Expanded) :- !, % compatibility
	user:goal_expansion(Prop #<=> B, M, Expanded).
user:goal_expansion(#\ Y, M, Expanded) :- !,
	fd_goal_expand_prop(#\ Y, M, Expanded).
user:goal_expansion(X #/\ Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #/\ Y, M, Expanded).
user:goal_expansion(X #\ Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #\ Y, M, Expanded).
user:goal_expansion(X #\/ Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #\/ Y, M, Expanded).
user:goal_expansion(X #=> Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #=> Y, M, Expanded).
user:goal_expansion(X #<= Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #<= Y, M, Expanded).
user:goal_expansion(X #<=> Y, M, Expanded) :- !,
	fd_goal_expand_prop(X #<=> Y, M, Expanded).

fd_goal_expansion(Goal, M, Expanded) :-
	user:goal_expansion(Goal, M, Expanded).

fd_goal_expand_in(X, Expr, Goal) :-
	(   ground(Expr),
	    set_expression(Expr, Set) 
	->  in_set_goal_1(X, Set, Goal)
	;   Goal = clpfd:in_aux_rt(X,Expr)
	).

%%% propositional constraints

fd_goal_expand_prop(Prop, M, Expanded) :-
	bool_decompose(Prop, M, Expr, S0, S1),
	bool_normalize(Expr, Expr1),
	bool_connect_top(Expr1, Prop, S1, []),
	fd_commafy(S0, Expanded).

bool_decompose(P, _, B) --> {simple(P)}, !, {B=P}.
bool_decompose(M:Constraint, _, B) --> !,
	bool_decompose(Constraint, M, B).
bool_decompose( #\  P, M, not(P1)) --> !,
	bool_decompose(P, M, P1).
bool_decompose(P #/\ Q, M, and(P1,Q1)) --> !,
	bool_decompose(P, M, P1),
	bool_decompose(Q, M, Q1).
bool_decompose(P #\  Q, M, xor(P1,Q1)) --> !,
	bool_decompose(P, M, P1),
	bool_decompose(Q, M, Q1).
bool_decompose(P #\/ Q, M, or(P1,Q1)) --> !,
	bool_decompose(P, M, P1),
	bool_decompose(Q, M, Q1).
bool_decompose(P #=> Q, M, or(Q1,not(P1))) --> !,
	bool_decompose(P, M, P1),
	bool_decompose(Q, M, Q1).
bool_decompose(Q #<= P, M, B) --> !,
	bool_decompose(P #=> Q, M, B).
bool_decompose(P #<=> Q, M, xor(P1,not(Q1))) --> !,
	bool_decompose(P, M, P1),
	bool_decompose(Q, M, Q1).
bool_decompose(X in Expr, _, B) --> !,
	(   {ground(Expr)}
	->  (   {set_expression(Expr, Set)}
	    ->  [in_set_iff(X,Set,B)]
	    ;   {ill_formed_constraint(X in Expr #<=> B)}
            )
	;   [in_aux_rt(X,Expr,B)]
	).
bool_decompose(X in_set Set, _, B) --> !,
	(   {'$fd_size'(Set, _, 1)} -> [in_set_iff(X,Set,B)]
	;   [in_set_aux_rt(X,Set,B)]
	).
bool_decompose(Goal0, M, B) -->
	{fd_expandable(Goal0, X, Y, Rel)}, !,
 	(   {trans_goal(X, Rel, Y, List)}
	->  (   {List==[]} -> [B=1]
            ;   {List==[fail]} -> [B=0]
	    ;   dlist_foldeq(List, Goaln), [Goal2, iff_aux(clpfd:Goal3,B)],
	        {reify(Goaln, Goal2, Goal3)}
	    )
	;   {ill_formed_constraint(M:Goal0 #<=> B)}
        ).
bool_decompose(Goal, M, B) --> [iff_aux(M:Goal,B)].

% push negations down
% remove 0s and 1s as operands
bool_normalize(P, P) :- simple(P), !.
bool_normalize(not(P), R) :-
	bool_norm_neg(P, R).
bool_normalize(and(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(and(P1,Q1), Expr).
bool_normalize(or(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(or(P1,Q1), Expr).
bool_normalize(xor(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(xor(P1,Q1), Expr).

bool_norm(and(P1,Q1), Expr) :-
	(   P1==0 -> Expr = 0
        ;   P1==1 -> Expr = Q1
        ;   Q1==0 -> Expr = 0
        ;   Q1==1 -> Expr = P1
	;   Expr = and(P1,Q1)
        ).
bool_norm(or(P1,Q1), Expr) :-
	(   P1==1 -> Expr = 1
        ;   P1==0 -> Expr = Q1
        ;   Q1==1 -> Expr = 1
        ;   Q1==0 -> Expr = P1
	;   Expr = or(P1,Q1)
        ).
bool_norm(xor(P1,Q1), Expr) :-
	(   P1==0 -> Expr = Q1
        ;   P1==1 -> bool_norm_neg(Q1, Expr)
        ;   Q1==0 -> Expr = P1
        ;   Q1==1 -> bool_norm_neg(P1, Expr)
	;   Expr = xor(P1,Q1)
        ).


bool_norm_neg(P, Q) :- simple(P), !,
	(   P==0 -> Q = 1
        ;   P==1 -> Q = 0
        ;   Q = not(P)
        ).
bool_norm_neg(not(P), R) :-
	bool_normalize(P, R).
bool_norm_neg(and(P,Q), Expr) :-
	bool_norm_neg(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(or(P1,Q1), Expr).
bool_norm_neg(or(P,Q), Expr) :-
	bool_norm_neg(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(and(P1,Q1), Expr).
bool_norm_neg(xor(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(xor(P1,Q1), Expr).


bool_connect_top(V, _) --> {simple(V)}, !, [V=1].
bool_connect_top(not(V), _) --> !, [V=0].
bool_connect_top(xor(P,Q), Context) -->		% eliminate redundant xorn constraints
	{nonvar(Q),				% for Constraint #<=> B
	 Q = not(Q1)}, !,
	{prolog:'$term_variables'(Context, Vars)},
	bool_connect(P, V, Ps),
	(   {Ps=:=0, free_of_var(Vars, V)} -> {V = Q1}
	;   {Inv is (Ps<<1)+1},
	    xor_connective(Inv, V, Q1, 1)
	).
bool_connect_top(Expr, _) -->
	bool_connect(Expr, 1, 0).

bool_connect(V, V, 0) --> {var(V)}, !.
bool_connect(not(V), V, 1) --> [].
bool_connect(and(P,Q), Z, 0) --> 
	bool_connect(P, X, Ps),
	bool_connect(Q, Y, Qs),
	{Inv is (Ps<<1)+Qs},
	and_connective(Inv, X, Y, Z).
bool_connect(or(P,Q), Z, 0) --> 
	bool_connect(P, X, Ps),
	bool_connect(Q, Y, Qs),
	{Inv is (Ps<<1)+Qs},
	or_connective(Inv, X, Y, Z).
bool_connect(xor(P,Q), Z, 0) --> 
	bool_connect(P, X, Ps),
	bool_connect(Q, Y, Qs),
	{Inv is (Ps<<1)+Qs},
	xor_connective(Inv, X, Y, Z).

and_connective(0, X, Y, Z) --> [bool(0,X,Y,Z)].	% AND
and_connective(1, X, Y, Z) --> [bool(1,X,Y,Z)].	% ANDN
and_connective(2, X, Y, Z) --> [bool(1,Y,X,Z)].	% ANDN
and_connective(3, X, Y, Z) --> [bool(5,X,Y,Z)].	% NOR

or_connective(0, X, Y, Z) --> [bool(3,X,Y,Z)].	% OR
or_connective(1, X, Y, Z) --> [bool(4,X,Y,Z)].	% ORN
or_connective(2, X, Y, Z) --> [bool(4,Y,X,Z)].	% ORN
or_connective(3, X, Y, Z) --> [bool(2,X,Y,Z)].	% NAND

xor_connective(0, X, Y, Z) --> [bool(6,X,Y,Z)].	% XOR
xor_connective(1, X, Y, Z) --> [bool(7,X,Y,Z)].	% XORN
xor_connective(2, X, Y, Z) --> [bool(7,Y,X,Z)].	% XORN
xor_connective(3, X, Y, Z) --> [bool(6,X,Y,Z)].	% XOR

free_of_var([], _).
free_of_var([X1|Xs], X) :- X\==X1, free_of_var(Xs, X).
