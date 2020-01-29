/* Copyright(C) 1993-95, Swedish Institute of Computer Science */

%   File       : objects.pl
%   Author     : Kent Boortz
%   Updated    : 21 October 1999
%   Purpose    : Provides object-oriented programming within Prolog

:- module(objects, [
	(::)/1,
	(<:)/1,
	(::)/2,
	(<:)/2
	]).

:- op(1200, xfy, [&]).
:- op(1198, xfx, [:-]).
:- op(1198,  fx, [:-]).
:- op(550,  xfx, [::, <:]).
:- op(550,   fx, [::, <:, :]).

:- use_module(library(ordsets), [
	ord_disjoint/2,
	ord_subtract/3,
	ord_add_element/3,
	ord_union/4
	]).
:- use_module(library(lists), [
	member/2,
	memberchk/2
	]).

%--------------------------------------------------------------------------

:- multifile
	user:goal_expansion/3,
	user:portray_message/2.

:- dynamic
	user:goal_expansion/3,
	user:portray_message/2.

% Hide some internal calls with macros
user:goal_expansion(visible(Goal,Module), objects, Exp) :- !,
	Exp = prolog:'$predicate_property'(Goal,_,_,Module,_).
user:goal_expansion(visible_static(Goal,Module), objects, Exp) :- !,
	Exp = (prolog:'$predicate_property'(Goal,_,Bits,Module,_),
	       Bits /\ 2 =:= 0).
user:goal_expansion(visible_dynamic(Goal,Module), objects, Exp) :- !,
	Exp = (prolog:'$predicate_property'(Goal,_,Bits,Module,_),
	       Bits /\ 2 =\= 0).
user:goal_expansion(visible(Goal,Module,DynFlag), objects, Exp) :- !,
	Exp = (prolog:'$predicate_property'(Goal,_,Bits,Module,_),
	       DynFlag is Bits /\ 2).
user:goal_expansion(call_if_def(Goal,Module), objects, Exp) :- !,
	(   var(Goal) ->
	    Exp = (objects:visible(Goal,Module),
	           Module:Goal)
	;   key(Goal, Key),
	    Exp = (objects:visible(Key,Module),
	           objects:call_apply(Goal,Module))
	).
user:goal_expansion(call_apply(Goal,Module), objects, Exp) :- !,
	Exp = '$object call'(Goal,Module).
user:goal_expansion(object_module(Object,Module), objects, Exp) :- !,
	Object = Module,
	Exp = true.
user:goal_expansion(instance_class(Module,Class), objects, Exp) :- !,
	Exp = prolog:'$get_module_data'(Module, '$class', Class).

key(X, Key) :- functor(X, N, A), functor(Key, N, A).

call_if_def(Goal, Module) :-
	prolog:'$predicate_property'(Goal,_,_,Module,_),
	Module:Goal.

call_apply(Goal, Module) :-
	Module:Goal.

object_module(Object, Module) :- atomic(Object), !, Module=Object.
object_module(Object, Module) :- functor(Object, Module, _).


user:portray_message(warning, so_redefining(Module,OldObject,Object)) :- !,
	print_message(warning, format('Module ~w is previously used by object ~w, now redefined by ~w', [Module,OldObject,Object])).
user:portray_message(warning, so_additional_object(Object,Super)) :- !,
	print_message(warning, format('Unexpected super(~w) found in ~w', [Super,Object])).
user:portray_message(warning, so_duplicate_attribute(Att)) :- !,
	print_message(warning, format('Discarded duplicate attribute: ~w', [Att])).

%--------------------------------------------------------------------------
:- ensure_loaded(library('objects/expand')).
:- ensure_loaded(library('objects/runlib')).
:- ensure_loaded(library('objects/object')).

/*
	Low level support required:
        --------------------------

'$make_lightweight_module'(?Module, ?PredTableModule, +NrPreds)
Makes a module with no built-in predicates imported. Module is either
a struct/list/atom or a variable in wich case a name is generated and the
variable is bound to the name. (For now: the name must be an integer).
    PredTableModule can be either a module or a variable indicating no
module. If a module is specified, the newly created module is not
given its own predicate table but will share the one of
PredTableModule. This is used for instances.

'$remove_module'(+Module, +Force)
Removes a module.

'$import_preds'(+ModuleFrom, +ModuleTo, +PredicateList)
Imports PredicateList from ModuleFrom to ModuleTo.

Module Data primitives, used (and designed) for Object Attributes:

'$init_module_data'(+Module, +Flag, +Data)
Initiates an array for term storage for the module Module.  Data is
either another module or a list, determined by Flag:
 Case module (nonvar(Flag): A copy of the other modules data is
            attached to Module.
 Case list (var(Flag): For each argument of each element in the list,
            a slot in the data array is reserved and filled in. Each
            element the functor is used as key for storing the offset
            of the first slot in the array.

'$get_module_data'(+Module, +KeyName, +KeyArity, +ArgNo, ?Value)
'$set_module_data'(+Module, +KeyName, +KeyArity, +ArgNo, ?Value)
Access the value of argument ArgNo for the element KeyName/KeyArity.

'$get_module_data'(+Module, +KeyName, ?Value)
'$set_module_data'(+Module, +KeyName, ?Value)
Special case of above where ArgNo==1, KeyArity==1.

'$term_variables'(Term, Vars)
Used to extract the variables of an arbitrary term. (Should be a built-in!)

Further on, an efficient implementation of calls to known predicates
in unknown modules (M:p(...)) is essential for performance.

*/
