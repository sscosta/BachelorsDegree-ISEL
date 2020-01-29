/* Copyright(C) 1997, Swedish Institute of Computer Science */

%   File       : clpfd.pl
%   Author     : Mats Carlsson
%   Updated    : 9 October 2000
%   Purpose    : Finite domains constraint solver

:- module(clpfd, [
	% enumeration
	indomain/1,
	labeling/2,
	first_bound/2,
	later_bound/2,
	minimize/2,
	maximize/2,
	order_resource/2,
	% reflection
	fd_min/2,
	fd_max/2,
	fd_size/2,
	fd_set/2,
	fd_dom/2,
	fd_degree/2,
	fd_statistics/0,
	fd_statistics/2,
	fd_neighbors/2,
	fd_closure/2,
	fd_copy_term/3,
	% constraints
	domain/3,
	iff/2,					% for compatibility
	in/2,
	in_set/2,
	all_different/1,
	all_different/2,
	all_distinct/1,
	all_distinct/2,
	element/3,
	relation/3,
	circuit/1,
	circuit/2,
	assignment/2,
	assignment/3,
	serialized/2,
	serialized/3,
	cumulative/4,
	cumulative/5,
        disjoint1/1,
        disjoint1/2,
        disjoint2/1,
        disjoint2/2,
        case/3,
        case/4,
	cumulatives/2,
	cumulatives/3,
	global_cardinality/2,
	count/4,
	sum/3,
	scalar_product/4,
	knapsack/3,
	#= /2,
	#\= /2,
	#< /2,
	#=< /2,
	#> /2,
	#>= /2,
	#\ /1,
	#/\ /2,
	#\ /2,
	#\/ /2,
	#=> /2,
	#<= /2,
	#<=> /2,
	% programming interface
	fd_global/3,
	is_fdset/1,
	empty_fdset/1,
	fdset_parts/4,
	empty_interval/2,
	fdset_interval/3,
	fdset_singleton/2,
	fdset_min/2,
	fdset_max/2,
	fdset_size/2,
	list_to_fdset/2,
	fdset_to_list/2,
	range_to_fdset/2,
	fdset_to_range/2,
	fdset_add_element/3,
	fdset_del_element/3,
	fdset_disjoint/2,
	fdset_intersect/2,
	fdset_intersection/3,
	fdset_intersection/2,
	fdset_member/2,
	fdset_eq/2,
	fdset_subset/2,
	fdset_subtract/3,
	fdset_union/3,
	fdset_union/2,
	fdset_complement/2
		 ]).

:- use_module(library(atts)).

:- use_module(library(assoc), [
	del_assoc/4,
        empty_assoc/1, 
	get_assoc/3,
	get_assoc/5,
	ord_list_to_assoc/2,
	assoc_to_list/2,
	put_assoc/4
			      ]).

:- use_module(library(lists), [
	append/3,
	last/2,
	member/2,
	memberchk/2,
	nth/3,
	reverse/2
			      ]).

:- use_module(library(ordsets), [
	ord_subset/2,
	ord_subtract/3,
	ord_intersection/3,
	ord_union/2,
	ord_union/3,
	ord_union/4,
	ord_del_element/3
				]).

:- use_module(library(trees), [
	get_label/3,
	put_label/4,
	list_to_tree/2
			      ]).

:- op(1200, xfx, [+:,-:,+?,-?]).  %% as :-
:- op(900,  xfx, iff).
:- op(760,  yfx, #<=>).
:- op(750,  xfy, #=>).
:- op(750,  yfx, #<=).
:- op(740,  yfx, #\/).
:- op(730,  yfx, #\).
:- op(720,  yfx, #/\).
:- op(710,   fy, #\).
:- op(700,  xfx, [in,in_set]).
:- op(700,  xfx, [#=,#\=,#<,#=<,#>,#>=]).
:- op(600,  xf,  !).
:- op(550,  xfx, ..). %% higher than +,-,*...
:- op(500,  yfx, \).				% same as \/, /\
:- op(500,   fy, \).				% same as \/, /\
:- op(490,  yfx, ?).				% tighter than \/, /\
:- op(400,  yfx, [/>,/<]).			% divu, divd

:- dynamic
	full_answer/0.

%% extremely crude version: just filter out any non-query vars
project_attributes(_, _) :- full_answer, !.
project_attributes(QueryVars, AttVars) :-
	sort(QueryVars, QueryVars1),
	sort(AttVars, AttVars1),
	ord_subtract(AttVars1, QueryVars1, ElimVars),
	purify_vars(ElimVars).

attribute_goal(X, Goal) :-
	fd_set(X, Set),
	\+fdset_singleton(Set, _),		% temp created by fdvar=fdvar
	fdset_to_range(Set, Dom1),
	(   full_answer ->
	    get_fd_suspensions(X, Lists), % avoid '$fdlists' in source code
	    fdlists_suspensions(Lists, All, []),
	    sort(All, Sorted),
	    commafy(Sorted, X in Dom1, Goal)
	;   Goal = (X in Dom1)
	).

fdlists_suspensions(Lists) -->
	{arg(3, Lists, L3)},
	{arg(4, Lists, L4)},
	{arg(5, Lists, L5)},
	{arg(6, Lists, L6)},
	{arg(7, Lists, L7)},
	suspensions(L3),
	suspensions(L4),
	suspensions(L5),
	suspensions(L6),
	suspensions(L7).

suspensions([]) --> [].
suspensions([Item|L]) -->
	suspension(Item),
	suspensions(L).

suspension(_-Item) --> !,
	suspension(Item).
suspension(iff(Ix,B,K,A)) -->
	{var(A)},
	{Ix = ix(Ptr,Constraint,_,Ent,_,_,_)},
	{var(Ent)}, !,
	{'$fd_indexical_data'(Ptr, _, Module)},
	(   {var(B)} -> [Module:Constraint #<=> B]
	;   {B==0, K==0} -> [#\ Module:Constraint]
	;   {B==1, K==1} -> [Module:Constraint]
	;   []
	).
suspension(ix(Ptr,Constraint,_,Ent,ZeroOne,_,_)) -->
	{var(Ent), var(ZeroOne)}, !,
	{'$fd_indexical_data'(Ptr, Type, Module)},
	suspension_item(Type, Module:Constraint).
suspension(global(_,Constraint,_,Ent,Module)) -->
	{var(Ent)}, !,
	[Module:Constraint].
suspension(_) --> [].

suspension_item(0, C) --> [#\ C].
suspension_item(1, C) --> [C].
suspension_item(2, _) --> [].
suspension_item(3, _) --> [].

fd_statistics :-
	'$fd_statistics'(0, S0),
	'$fd_statistics'(1, S1),
	'$fd_statistics'(2, S2),
	'$fd_statistics'(3, S3),
	'$fd_statistics'(4, S4),
	format(user_error,
	       'Resumptions: ~d\n\c
	        Entailments: ~d\n\c
	        Prunings: ~d\n\c
		Backtracks: ~d\n\c
	        Constraints created: ~d\n', [S0,S1,S2,S3,S4]).

fd_statistics(Key, Value) :-
	statistics_code(Key, Code),
	'$fd_statistics'(Code, Value).

statistics_code(resumptions, 0).
statistics_code(entailments, 1).
statistics_code(prunings, 2).
statistics_code(backtracks, 3).
statistics_code(constraints, 4).

%% used by foreign resource's init function
%% called back from C
call_action(call(Goal)) :- Goal.
call_action(X in R) :- 
	set_expression(R, S),
	'$fd_in_set'(X, S, 0).
call_action(X in_set S) :- 
	'$fd_in_set'(X, S, 0).
call_action(X=I) :-
	'$fd_range'(I, I, Set, 1),
	'$fd_in_set'(X, Set, 0).


:- dynamic foreign/3, foreign_resource/2.


foreign_resource(clpfd,
        [init(fd_initialize),
	 deinit(fd_deinitialize),
	 prolog_fd_size,
	 prolog_fd_range,
	 prolog_fd_cons,
	 prolog_fd_dom_complement,
	 prolog_fd_dom_subtract,
	 prolog_fd_dom_intersection,
	 prolog_fd_dom_union,
	 prolog_fd_dom_contains,
	 prolog_fd_dom_insert,
	 prolog_fd_dom_delete,
	 prolog_fd_dom_intersect,
	 prolog_fd_negate,
	 prolog_fd_arg_attribute,
	 prolog_fd_dvar_list,
	 prolog_fd_coref,
	 prolog_fd_begin,
	 prolog_fd_tell,
	 prolog_fd_check_arguments,
	 prolog_fd_install,
	 prolog_fd_get_indexicals,
	 prolog_fd_mon_vars,
	 prolog_fd_link_iff,
	 prolog_fd_enqueue_first,
	 prolog_fd_prune_and_enqueue,
	 prolog_fd_find_definition,
	 prolog_fd_indexical_data,
	 prolog_fd_global_told,
	 prolog_fd_global_enqueue,
	 prolog_fd_statistics,
	 prolog_fd_debug,
	 prolog_fd_debugging,
	 prolog_fd_set_singleton,
	 prolog_fd_in_set,
	 prolog_fd_in_interval,
	 prolog_fd_evaluate_indexical,
	 prolog_fd_enqueue_all,
	 prolog_fd_update_incumbent,
	 prolog_fd_incumbent_bound,
	 prolog_fd_minint_maxint,
	 prolog_fd_mark_variable,
	 prolog_fd_trailed_mutables,
	 prolog_fd_linear,
	 prolog_fd_knapsack,
	 prolog_fd_square,
	 prolog_fd_product,
	 prolog_fd_quotient,
	 prolog_fd_modulo,
	 prolog_fd_cumulative,
	 prolog_fd_all_different,
	 prolog_fd_all_distinct,
	 prolog_fd_assignment,
	 prolog_fd_circuit,
	 prolog_fd_relation,
	 prolog_fd_element,
	 prolog_fd_in_set_iff,
	 % new
	 prolog_fd_eq_iff,
	 prolog_fd_le_iff,
	 prolog_fd_oneof,
	 prolog_fd_abs,
	 % end new
	 prolog_fd_bool,
	 prolog_fd_disjoint1,
	 prolog_fd_disjoint2,
	 prolog_fd_case,
	 prolog_fd_cumulatives,
	 prolog_fd_gcc
	]).

foreign(prolog_fd_size, '$fd_size'(+term,-term,[-integer])).
foreign(prolog_fd_range, '$fd_range'(+term,+term,-term,[-integer])).
foreign(prolog_fd_cons, '$fd_cons'(+term,+term,+term,-term,[-integer])).
foreign(prolog_fd_dom_complement, '$fd_dom_complement'(+term,-term)).
foreign(prolog_fd_dom_subtract, '$fd_dom_subtract'(+term,+term,-term)).
foreign(prolog_fd_dom_intersection, '$fd_dom_intersection'(+term,+term,-term)).
foreign(prolog_fd_dom_union, '$fd_dom_union'(+term,+term,-term)).
foreign(prolog_fd_dom_contains, '$fd_dom_contains'(+term,+term)).
foreign(prolog_fd_dom_insert, '$fd_dom_insert'(+term,+term,-term)).
foreign(prolog_fd_dom_delete, '$fd_dom_delete'(+term,+term,-term)).
foreign(prolog_fd_dom_intersect, '$fd_dom_intersect'(+term,+term,[-integer])).
foreign(prolog_fd_negate, '$fd_negate'(+term,+term,-term)).
foreign(prolog_fd_arg_attribute, '$fd_arg_attribute'(+term,+integer,-term)).
foreign(prolog_fd_dvar_list, '$fd_dvar_list'(+term,+integer)).
foreign(prolog_fd_coref, '$fd_coref'(+term)).
foreign(prolog_fd_begin, '$fd_begin').
foreign(prolog_fd_tell, '$fd_tell'(+term,+term,[-integer])).
foreign(prolog_fd_check_arguments, '$fd_check_arguments'(+term,-term)).
foreign(prolog_fd_install, '$fd_install'(+term,+atom,+integer,+integer,+term)).
foreign(prolog_fd_get_indexicals, '$fd_get_indexicals'(+integer,+integer,+term,+term,+term,+term,-term)).
foreign(prolog_fd_mon_vars, '$fd_mon_vars'(+term,-term)).
foreign(prolog_fd_link_iff, '$fd_link_iff'(+term,+term,+term,+term)).
foreign(prolog_fd_enqueue_first, '$fd_enqueue_first'(+term)).
foreign(prolog_fd_prune_and_enqueue, '$fd_prune_and_enqueue'(+term,+term)).
foreign(prolog_fd_find_definition, '$fd_find_definition'(+term,+atom,[-integer])).
foreign(prolog_fd_indexical_data, '$fd_indexical_data'(+integer,-integer,[-atom])).
foreign(prolog_fd_global_told, '$fd_global_told'(+term,+term)).
foreign(prolog_fd_global_enqueue, '$fd_global_enqueue'(+term)).
foreign(prolog_fd_statistics, '$fd_statistics'(+integer,[-integer])).
foreign(prolog_fd_debug, '$fd_debug'(+integer)).
foreign(prolog_fd_debugging, '$fd_debugging'([-integer])).
foreign(prolog_fd_set_singleton, '$fd_set_singleton'(+term,+term)).
foreign(prolog_fd_in_set, '$fd_in_set'(+term,+term,+integer)).
foreign(prolog_fd_in_interval, '$fd_in_interval'(+term,+term,+term,+integer)).
foreign(prolog_fd_evaluate_indexical, '$fd_evaluate_indexical'([-integer],-term)).
foreign(prolog_fd_enqueue_all, '$fd_enqueue_all'(+term)).
foreign(prolog_fd_update_incumbent, '$fd_update_incumbent'(+integer,+term,+term)).
foreign(prolog_fd_incumbent_bound, '$fd_incumbent_bound'(+integer,-term)).
foreign(prolog_fd_minint_maxint, '$fd_minint_maxint'(-integer,-integer)).
foreign(prolog_fd_mark_variable, '$fd_mark_variable'(+term,+integer)).
foreign(prolog_fd_trailed_mutables, '$fd_trailed_mutables'(+integer,-term)).

%% dispatch_global_fast/4 targets:

foreign(prolog_fd_linear, '$fd_linear'(+term,-term,-term)).
foreign(prolog_fd_knapsack, '$fd_knapsack'(+term,-term,-term)).
foreign(prolog_fd_square, '$fd_square'(+term,-term,-term)).
foreign(prolog_fd_product, '$fd_product'(+term,-term,-term)).
foreign(prolog_fd_quotient, '$fd_quotient'(+term,-term,-term)).
foreign(prolog_fd_modulo, '$fd_modulo'(+term,-term,-term)).
foreign(prolog_fd_cumulative, '$fd_cumulative'(+term,-term,-term)).
foreign(prolog_fd_all_different, '$fd_all_different'(+term,-term,-term)).
foreign(prolog_fd_all_distinct, '$fd_all_distinct'(+term,-term,-term)).
foreign(prolog_fd_assignment, '$fd_assignment'(+term,-term,-term)).
foreign(prolog_fd_circuit, '$fd_circuit'(+term,-term,-term)).
foreign(prolog_fd_relation, '$fd_relation'(+term,-term,-term)).
foreign(prolog_fd_element, '$fd_element'(+term,-term,-term)).
foreign(prolog_fd_in_set_iff, '$fd_in_set_iff'(+term,-term,-term)).
foreign(prolog_fd_eq_iff, '$fd_eq_iff'(+term,-term,-term)).
foreign(prolog_fd_le_iff, '$fd_le_iff'(+term,-term,-term)).
foreign(prolog_fd_oneof, '$fd_oneof'(+term,-term,-term)).
foreign(prolog_fd_abs, '$fd_abs'(+term,-term,-term)).
foreign(prolog_fd_bool, '$fd_bool'(+term,-term,-term)).
foreign(prolog_fd_disjoint1, '$fd_disjoint1'(+term,-term,-term)).
foreign(prolog_fd_disjoint2, '$fd_disjoint2'(+term,-term,-term)).
foreign(prolog_fd_case, '$fd_case'(+term,-term,-term)).
foreign(prolog_fd_cumulatives, '$fd_cumulatives'(+term,-term,-term)).
foreign(prolog_fd_gcc, '$fd_gcc'(+term,-term,-term)).

:- load_foreign_resource(library(system(clpfd))).

:- ensure_loaded('clpfd/fdsets').
:- ensure_loaded('clpfd/ixq').
:- ensure_loaded('clpfd/enum').
:- ensure_loaded('clpfd/compiler').
:- ensure_loaded('clpfd/lib').
