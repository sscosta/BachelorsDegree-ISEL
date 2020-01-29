%%% -*- Mode: Prolog; Module: comclient1; -*-

:- module(comclient1,
          [
           comclient_is_object/1,
           comclient_valid_object/1,

           comclient_clsid_from_progid/2,
           comclient_clsid_from_progid/3,

           comclient_progid_from_clsid/2,
           comclient_progid_from_clsid/3,

           comclient_iid_from_name/2,
           comclient_iid_from_name/3,
           comclient_name_from_iid/2,
           comclient_name_from_iid/3,
          
           comclient_create_instance/2,
           %% not much point at present.
           % comclient_create_instance/3

           comclient_get_active_object/2,
           %% not much point at present.
           %% comclient_get_active_object/3,

           comclient_invoke_method_fun/3,
           comclient_invoke_method_proc/2,
           comclient_invoke_put/3,
           
           comclient_release/1,
           comclient_is_exception/1,
           comclient_exception_code/2,
           comclient_exception_culprit/2,
           comclient_exception_description/2,

           comclient_garbage_collect/0, % a no-op
           comclient_equal/2
           ]).

:- use_module(library(lists), [member/2, append/3, is_list/1]).

/***
**** FOREIGN FUNS
***/

%% CLSID and IID are GUIDs in textual form (e.g., from comclient_CLSIDStringFromProgID
foreign(comclient_create_instance, '$comclient_create_instance'(+term, +term, -term, -integer)).

%% wrapper for to GetActiveObject
%% CLSID and IID as for comclient_create_instance
foreign(comclient_get_active_object, '$comclient_get_active_object'(+term, +term, -term, -integer)).


%% comclient_CLSIDFromProgID(+ProgID:term, -CLSID:term, +atom?:integer, -result:integer)
foreign(comclient_CLSIDFromProgID, '$comclient_CLSIDFromProgID'(+term, -term, +integer, -integer)).
foreign(comclient_ProgIDFromCLSID, '$comclient_ProgIDFromCLSID'(+term, -term, +integer, -integer)).

%% comclient_INameToIID(+Name:term, -IID:term, +atom?:integer, -result:integer)
foreign(comclient_INameToIID, '$comclient_INameToIID'(+term, -term, +integer, -integer)).
%% comclient_IIDToName(+IID:term, -Name:term, +atom?:integer, -result:integer)
foreign(comclient_IIDToName, '$comclient_IIDToName'(+term, -term, +integer, -integer)).

%% '$finalize_external_object'(+Obj:term, -Existed:integer).
foreign('finalize_external_object', '$finalize_external_object'(+term, -integer)).

%% comclient_invoke(+Obj:term, +Goal:term, +Flags:term, -Result:term)
%% #define DISPATCH_METHOD         0x1
%% #define DISPATCH_PROPERTYGET    0x2
%% #define DISPATCH_PROPERTYPUT    0x4
%% #define DISPATCH_PROPERTYPUTREF 0x8
foreign(comclient_invoke, '$comclient_invoke'(+term, +term, +term, -term, -integer)).
foreign(comclient_invoke_ex, '$comclient_invoke_ex'(+term, +term, +term, -term, -term, -integer)).
foreign(comclient_equal, '$comclient_equal'(+term, +term, -integer)).
foreign(comclient_valid, '$comclient_valid'(+term, -integer)).

foreign(comclient_garbage_collect, '$comclient_garbage_collect'([-integer])).

foreign(comclient_breakpoints_flag, '$comclient_breakpoints_flag'(+integer, [-integer])).

foreign_resource(comclient, [
                             comclient_CLSIDFromProgID,
                             comclient_IIDToName,
                             comclient_INameToIID,
                             comclient_ProgIDFromCLSID,
                             comclient_create_instance,
                             comclient_get_active_object,
                             comclient_invoke,
                             comclient_invoke_ex,
                             comclient_equal,
                             comclient_valid,
                             comclient_garbage_collect,
                             comclient_breakpoints_flag,
                             
                             init(comclient_init),
                             deinit(comclient_deinit),
                             'finalize_external_object'

                             ]).

:- load_foreign_resource(library(system(comclient))).


/****
***** Prolog Code
****/



%% PUBLIC
comclient_garbage_collect :-
   comclient_garbage_collect(_).

comclient_garbage_collect(Collected) :-
   garbage_collect_atoms,
   '$comclient_garbage_collect'(Collected0),
   Collected = Collected0.


%% PUBLIC
comclient_is_object(Obj) :- var(Obj), !,
   fail.
comclient_is_object('$comclient_object'(Ref)) :- 
   integer(Ref).

%% PUBLIC
comclient_valid_object(Obj) :-
   %% is_object guards against other types of external objects
   %% (Currently a non-issue though).
   comclient_is_object(Obj),
   '$comclient_valid'(Obj, Result),
   Result == 1.

   
%% PUBLIC
comclient_equal(Obj1, Obj2) :-
   %% is_object guards against other types of external objects
   %% (Currently a non-issue though).
   comclient_is_object(Obj1),
   comclient_is_object(Obj2),
   '$comclient_equal'(Obj1, Obj2, Result),
   Result == 1.
   

%% PUBLIC
comclient_clsid_from_progid(ProgID, CLSID) :-
   WantAtom = true,
   comclient_clsid_from_progid(ProgID, CLSID, WantAtom).

%% PUBLIC
comclient_clsid_from_progid(ProgID, CLSID, WantAtom) :-
   Culprit = comclient_clsid_from_progid(ProgID, CLSID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exceptions before this point

   '$comclient_CLSIDFromProgID'(ProgID, CLSID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   CLSID = CLSID1.

%% PUBLIC
comclient_progid_from_clsid(CLSID, ProgID) :-
   WantAtom = true,
   comclient_progid_from_clsid(CLSID, ProgID, WantAtom).

%% PUBLIC
comclient_progid_from_clsid(CLSID, ProgID, WantAtom) :-
   Culprit = comclient_progid_from_clsid(CLSID, ProgID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exceptions before this point

   '$comclient_ProgIDFromCLSID'(CLSID, ProgID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   ProgID = ProgID1.



%% PUBLIC
comclient_iid_from_name(InterfaceName, IID) :-
   WantAtom = true,
   comclient_iid_from_name(InterfaceName, IID, WantAtom).

%% PUBLIC
comclient_iid_from_name(InterfaceName, IID, WantAtom) :-
   Culprit = comclient_iid_from_name(InterfaceName, IID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_INameToIID'(InterfaceName, IID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   IID = IID1.

%% PUBLIC
comclient_name_from_iid(IID, InterfaceName) :-
   WantAtom = true,
   comclient_name_from_iid(IID, InterfaceName, WantAtom).

%% PUBLIC
comclient_name_from_iid(IID, InterfaceName, WantAtom) :-
   Culprit = comclient_name_from_iid(IID, InterfaceName, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_IIDToName'(IID, InterfaceName1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   InterfaceName = InterfaceName1.



%% PUBLIC
comclient_create_instance(CLSID, Obj) :-
   %% comclient_iid_from_name('IDispatch', IID),
   IID = '{00020400-0000-0000-C000-000000000046}', % i.e., IDispatch
   comclient_create_instance(CLSID, IID, Obj).

%% PUBLIC
comclient_create_instance(CLSID, IID, Obj) :-
   Culprit = comclient_create_instance(CLSID, IID, Obj),
   %% More type tests later
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_create_instance'(CLSID, IID, Obj1, Result),
   comclient_process_result(Result, Culprit),
   Obj = Obj1.

%% PUBLIC
comclient_get_active_object(CLSID, Obj) :-
   %% comclient_iid_from_name('IDispatch', IID),
   IID = '{00020400-0000-0000-C000-000000000046}', % i.e., IDispatch
   comclient_get_active_object(CLSID, IID, Obj).

%% PUBLIC
comclient_get_active_object(CLSID, IID, Obj) :-
   Culprit = comclient_get_active_object(CLSID, IID, Obj),
   %% More type tests later
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_get_active_object'(CLSID, IID, Obj1, Result),
   comclient_process_result(Result, Culprit),
   Obj = Obj1.


   
%% PUBLIC
comclient_invoke_method_fun(Obj, Goals, Value) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_method_fun(Obj, G, Value)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_method_fun(Obj1, Gs, Value0),
       comclient_release(Obj1),
       Value = Value0
   ).

comclient_invoke_method_fun(Obj, Goal, Value) :-
   Culprit = comclient_invoke_method_fun(Obj, Goal, Value),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   
   %% comclient_flags(['SPCOM_RETVAL', 'PROPERTY_METHOD', 'PROPERTY_GET'], Flags),
   Flags = 67,                  % 'PROPERTY_METHOD' \/ PROPERTY_GET \/ 'SPCOM_RETVAL'
   
   comclient_preprocess_method_goal(Goal, 0, GoalIn, Culprit, 2),
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),

   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2),
   functor(Goal, _MethodName, Arity),
   RetvalIdx is Arity+1,
   arg(RetvalIdx, GoalOut, Value).

%% PUBLIC
comclient_invoke_method_proc(Obj, Goals) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_method_proc(Obj, G)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_method_proc(Obj1, Gs),
       comclient_release(Obj1)
   ).

comclient_invoke_method_proc(Obj, Goal) :-
   Culprit = comclient_invoke_method_proc(Obj, Goal),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   
   %% comclient_flags(['PROPERTY_METHOD'], Flags),
   Flags = 1,                   % 'PROPERTY_METHOD'
   
   comclient_preprocess_method_goal(Goal, 0, GoalIn, Culprit, 2),
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),

   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2).

%% PUBLIC
comclient_invoke_put(Obj, Goals, Value) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_put(Obj, G, Value)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_put(Obj1, Gs, Value),
       comclient_release(Obj1)
   ).

comclient_invoke_put(Obj, Goal, Value) :-
   Culprit = comclient_invoke_put(Obj, Goal, Value),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   comclient_check_type(comvalue, Value, Culprit, 3),
   
   %% comclient_flags(['PROPERTY_PUT'], Flags),
   Flags = 4,                  % 'PROPERTY_PUT'

   comclient_preprocess_method_goal(Goal, 1, GoalIn, Culprit, 2),
   functor(GoalIn, _, ArityIn),
   arg(ArityIn, GoalIn, Value),
   %% Goal =.. [Method|Args],
   %% append(Args, [Value], Args1),
   %% Goal1 =.. [Method|Args1],
   
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),

   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2).



comclient_preprocess_method_goal(Goal, ExtraArgs, GoalIn, Culprit,ArgNo) :-
   functor(Goal, MethodName, Arity),
   ArityIn is Arity+ExtraArgs,
   functor(GoalIn, MethodName, ArityIn),

   Limit is Arity+1,
   comclient_preprocess_method_goal1(1,Limit, Goal, GoalIn, Culprit, ArgNo).
      
comclient_preprocess_method_goal1(I,Limit, _Goal, _GoalIn, _Culprit, _ArgNo) :- I==Limit, !,
   true.
comclient_preprocess_method_goal1(I,Limit, Goal, GoalIn, Culprit, ArgNo) :-
   arg(I, Goal, Arg),
   comclient_preprocess_method_arg(Arg, ArgIn, Goal, I, Culprit, ArgNo),
   arg(I, GoalIn, ArgIn),
   I1 is I+1,
   comclient_preprocess_method_goal1(I1,Limit, Goal, GoalIn, Culprit, ArgNo).


comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- var(Arg), !,
   ArgIn = _.
comclient_preprocess_method_arg(Arg, ArgIn, Goal, I, Culprit, ArgNo) :- is_mutable(Arg), !,
   get_mutable(Arg1, Arg),
   comclient_preprocess_method_inout_arg(Arg1, ArgIn, Goal, I, Culprit, ArgNo).
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- atomic(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- comclient_is_object(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- Arg = [_|_],
   is_list(Arg),
   !,
   %% Should verify that it is a list of (UNICODE) char codes
   ArgIn = Arg.
comclient_preprocess_method_arg(_Arg, _ArgIn, Goal, I, Culprit, ArgNo) :-
   comclient_argument_exception(invoke_method(Goal, I), Culprit, ArgNo).


comclient_preprocess_method_inout_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- atomic(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_inout_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- Arg = [_|_],
   %% Should also verify that it is a list of (UNICODE) char codes
   is_list(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_inout_arg(_Arg, _ArgIn, Goal, I, Culprit, ArgNo) :-
   comclient_argument_exception(invoke_method(Goal, I), Culprit, ArgNo).


comclient_postprocess_method_goal(Goal, GoalOut, Culprit,ArgNo) :-
   functor(Goal, _MethodName, Arity),

   Limit is Arity+1,
   comclient_postprocess_method_goal1(1,Limit, Goal, GoalOut, Culprit, ArgNo).


comclient_postprocess_method_goal1(I,Limit, _Goal, _GoalOut, _Culprit, _ArgNo) :- I==Limit, !,
   true.
comclient_postprocess_method_goal1(I,Limit, Goal, GoalOut, Culprit, ArgNo) :-
   arg(I, Goal, Arg),
   comclient_postprocess_method_arg(Arg, ArgOut, Goal, I, Culprit, ArgNo),
   arg(I, GoalOut, ArgOut),
   I1 is I+1,
   comclient_postprocess_method_goal1(I1,Limit, Goal, GoalOut, Culprit, ArgNo).


comclient_postprocess_method_arg(Arg, ArgOut, _Goal, _I, _Culprit, _ArgNo) :- var(Arg), !,
   Arg = ArgOut.
comclient_postprocess_method_arg(Arg, ArgOut, _Goal, _I, _Culprit, _ArgNo) :- is_mutable(Arg), !,
   update_mutable(ArgOut, Arg).
comclient_postprocess_method_arg(_Arg, _ArgOut, _Goal, _I, _Culprit, _ArgNo).


/*
comclient_transfer_result(Goal, Result) :-
   functor(Goal, _, Arity),
   Limit is Arity+1,
   comclient_transfer_result1(1,Limit, Goal, Result).

comclient_transfer_result1(I,Limit, _, _) :- I==Limit, !,
   true.
comclient_transfer_result1(I,Limit, Goal, Result) :-
   arg(I, Goal, Arg),
   %% Arguments that are variables are assumed to be output args.
   %% They are bound to the corresponding value in the Result.
   ( var(Arg) ->
       arg(I, Result, Arg)
   ; true
   ),
   I1 is I+1,
   comclient_transfer_result1(I1,Limit, Goal, Result).

*/
   
   

comclient_flags(Flags, Bits) :-
   comclient_flags1(Flags, 0,Bits).

comclient_flags1([], Bits0,Bits) :-
   Bits = Bits0.
comclient_flags1([Flag|Flags], Bits0,Bits) :-
   comclient_flag(Flag, FlagBits),
   Bits1 is Bits0 \/ FlagBits,
   comclient_flags1(Flags, Bits1,Bits).
   

comclient_flag(Name, Bits) :- var(Name), !,
   comclient_argument_exception(nonvar, comclient_flag(Name, Bits), 1).
comclient_flag('SPCOM_RETVAL', Bits) :- !,
   Bits = 64.
comclient_flag('PROPERTY_METHOD', Bits) :- !,
   Bits = 1.
comclient_flag('PROPERTY_GET_', Bits) :- !,
   Bits = 2.
comclient_flag('PROPERTY_GET', Bits) :- !,
   Bits = 66.                   % 2 \/ SPCOM_RETVAL
comclient_flag('PROPERTY_PUT', Bits) :- !,
   Bits = 4.
comclient_flag(Name, Bits) :-
   Flags = ['SPCOM_RETVAL',
            'PROPERTY_METHOD', 'PROPERTY_GET', 'PROPERTY_GET_', 'PROPERTY_PUT'
            ],
   comclient_argument_exception(oneof(Flags), comclient_flag(Name, Bits), 1).

   



%% PUBLIC
comclient_release(Obj) :-
   Culprit = comclient_release(Obj),
   comclient_check_type(com_object, Obj, Culprit, 1),
   '$finalize_external_object'(Obj, Existed),
   ( Existed == 1 ->
       true
   ; otherwise ->
       %% Did not exist (anymore, possibly)
       comclient_raise_exception(arg, Culprit)
   ).



%% xref WinError.h
comclient_process_result(HRESULT, Culprit) :-
   comclient_process_result(HRESULT, _, Culprit).

comclient_process_result(HRESULT, Excp, Culprit) :- var(HRESULT), !,
   comclient_raise_exception(HRESULT, Excp, Culprit).
comclient_process_result(0, _Excp, _Culprit) :- !, % S_OK
   true.
comclient_process_result(1, _Excp, _Culprit) :- !, % S_FALSE (we (will) use it to signal fail)
   !, fail.
comclient_process_result(HRESULT, _Excp, Culprit) :- HRESULT < 0, !, 
   comclient_signed_to_unsigned_i4(HRESULT, HRESULT1),
   comclient_raise_exception(HRESULT1, _Excp, Culprit).

comclient_process_result(HRESULT, _Excp,_Culprit) :- HRESULT > 0,
   %% Other success values are unexpected but I will treat them as S_OK for now
   true.


comclient_raise_exception(HR, Excp, Culprit) :- var(Excp), !,
   comclient_raise_exception(HR, Culprit).
comclient_raise_exception(HR, Excp, Culprit) :- Excp == [], !,
   comclient_raise_exception(HR, Culprit).
comclient_raise_exception(HR, Excp, Culprit) :-
   raise_exception(comclient_error(HR, Excp, Culprit)).


comclient_raise_exception(HR, Culprit) :-
   raise_exception(comclient_error(HR, Culprit)).

%% Generic argument error at unknown position and type
comclient_argument_exception(Culprit) :-
   comclient_raise_exception(arg, Culprit).

%% Signal that argument ArgNo of goal Culprit was not of the ExpectedType
comclient_argument_exception(ExpectedType, Culprit, ArgNo) :-
   comclient_raise_exception(arg(ArgNo, ExpectedType), Culprit).



comclient_check_type(nonvar, Obj, Culprit, ArgNo) :-
   var(Obj), !,
   comclient_argument_exception(nonvar, Culprit, ArgNo).
comclient_check_type(nonvar, _Obj, _Culprit, _ArgNo) :- !,
   true.
comclient_check_type(com_object, X, Culprit, ArgNo) :- !,
   % for now
   comclient_check_type(match('$comclient_object'(_)), X, Culprit, ArgNo).
comclient_check_type(match(Pattern), X, _Culprit, _ArgNo) :-
   nonvar(X),
   \+ \+ X = Pattern,
   !,
   true.
comclient_check_type(match(Pattern), _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(match(Pattern), Culprit, ArgNo).
comclient_check_type(oneof(List), X, _Culprit, _ArgNo) :-
   nonvar(X),
   \+ \+ member(X, List),
   !,
   true.
comclient_check_type(oneof(List), _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(oneof(List), Culprit, ArgNo).
comclient_check_type(comvalue, X, _Culprit, _ArgNo) :-
   ground(X), !,                % for now
   true.
comclient_check_type(comvalue, _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(comvalue, Culprit, ArgNo).
comclient_check_type(Type, X, Culprit, ArgNo) :-
   \+ Culprit = comclient_check_type(_,_,_,_),
   Culprit1 = comclient_check_type(Type, X, Culprit, ArgNo),
   KnownTypes = [nonvar, match(_), oneof(_), com_object],
   comclient_check_type(oneof(KnownTypes), Type, Culprit1, 1).


%% PUBLIC
%% Use this to distinguish a comclient exception from other exceptions
comclient_is_exception(ExceptionTerm) :- var(ExceptionTerm), !,
   fail.
comclient_is_exception(comclient_error(_HR, _Culprit)).
comclient_is_exception(comclient_error(_HR, _Exception, _Culprit)).

%% PUBLIC
comclient_exception_code(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_code(comclient_error(HR, _Culprit), HR).
comclient_exception_code(comclient_error(HR, _Exception, _Culprit), HR).

%% PUBLIC
comclient_exception_culprit(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_culprit(comclient_error(_HR, Culprit), Culprit).
comclient_exception_culprit(comclient_error(_HR, _Exception, Culprit), Culprit).

%% PUBLIC
comclient_exception_description(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_description(comclient_error(_HR, Exception, _Culprit), Exception).





comclient_boolean_to_int(Boolean, BooleanFlag, _Culprit, _ArgNo) :-
   comclient_boolean_to_int(Boolean, BooleanFlag1), !,
   BooleanFlag = BooleanFlag1.
comclient_boolean_to_int(_Boolean, _BooleanFlag, Culprit, ArgNo) :-
   comclient_argument_exception(oneof([true, false]), Culprit, ArgNo).
   

%% Chould fail on illegal arg
comclient_boolean_to_int(Boolean, _) :- var(Boolean), !,
   fail.
comclient_boolean_to_int(true, 1).
comclient_boolean_to_int(false, 0).


%% Useful for HRESULT (best presented as an unsigned, 32 bit, long)
comclient_signed_to_unsigned_i4(SL, UL) :- SL >= 0, !,
   UL = SL.
comclient_signed_to_unsigned_i4(SL, UL) :-
   UL is SL + 1<<31 + 1<<31.

