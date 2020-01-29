/* Copyright(C) 1993-95, Swedish Institute of Computer Science */

[].						% for make_index

object :: {
	super(X) :-
	        super(X, _) &

	self(Self) :- self(Self) &		% inlined

%       super(X, NonInheritanceSpec)   .... part of every object

%	object(O) - enumerates all objects

	object(O) :-
                :ext_object(O, _) &

%	all the rest refer to "self"

	sub(X) :-
                self(Self),
		:super_sub(Self, X) &

	dynamic :-
                self(Self),
                :ext_object(Self, dynamic) &

	static :-
                self(Self),
                :ext_object(Self, static) &

	method(Method) :-
                self(Self),
                :ext_method(Self, Method, _) &

	dynamic(Method) :-
                self(Self),
                :ext_method(Self, Method, dynamic) &

	static(Method) :-
                self(Self),
                :ext_method(Self, Method, static) &

	asserta(Method) :-
                self(Self),
		:ext_asserta_method(Method, Self, _) &

	asserta(Method, Ref) :-
                self(Self),
		:ext_asserta_method(Method, Self, Ref) &

	assertz(Method) :-
                self(Self),
		:ext_assertz_method(Method, Self, _) &

	assertz(Method, Ref) :-
                self(Self),
		:ext_assertz_method(Method, Self, Ref) &

	assert(Method) :-
                self(Self),
		:ext_assertz_method(Method, Self, _) &

	assert(Method, Ref) :-
                self(Self),
		:ext_assertz_method(Method, Self, Ref) &

	retract(Method) :-
                self(Self),
		:ext_retract_method(Method, Self) &
		
	update(H) :-
		:functor(H, N, A),
		:functor(H1, N, A),
		(<: retract(H1) -> :true; :true),
		<: asserta(H) &

        new(Instance) :-
                self(Self),
                :ext_new1(Instance, Self) &

	new(Instance, Supers) :-
		:ext_new(Instance, Supers) &

	instance(Instance) :-
                self(Self),
		:ext_instance(Instance, Self) &

        has_instance(Instance) :-
                self(Self),
		:has_instance(Self, Instance) &

        get(Head) :-				% inlined
                self(Self),
		objects:object_module(Self,M),
		:nonvar(Head),
		:functor(Head, Key, Ar),
		objects:ext_get(Ar, M, Key, Ar, Head) &

	set(Head) :-				% inlined
                self(Self),
		objects:object_module(Self,M),
		:nonvar(Head),
		:functor(Head, Key, Ar),
		objects:ext_set(Ar, M, Key, Ar, Head) &
		
        has_attribute(Attribute) :-
                self(Self),
		objects:object_module(Self,M),
		objects:ext_has_attribute(Attribute, M) &
                 
	retractall(A) :-
                self(Self),
		:ext_retractall(A, Self) &

	abolish :-
                self(Self),
		:ext_abolish(Self) &

	augmenta(Body) :- 
                self(Self),
		:ext_augment(Body, Self, a) &

	augmentz(Body) :- 
                self(Self),
		:ext_augment(Body, Self, z) &

	augment(Body) :- 
                self(Self),
		:ext_augment(Body, Self, z)
	}.


utility ::
	{
	super(object) &
	
	subs(Objects) :-
		self(Self),
		:findall(S, utility:sub(S,Self,utility), Objects) &
	
	supers(Os) :-
		self(Self),
		:findall(O, Self:super(O,_,Self,Self), Os) &
	
	objects(Os) :-
		self(Self),
		:findall(O, ext_object(O, _), Os) &
	
	dynamic_objects(Os) :-
		self(Self),
		:findall(O, ext_object(O, dynamic), Os) &
	
	static_objects(Os) :-
		self(Self),
		:findall(O, ext_object(O, static), Os) &
	
	methods(Os) :-
		self(Self),
		:findall(O, ext_method(Self, O, _), Os) &
	
	dynamic_methods(Os) :-
		self(Self),
		:findall(O, ext_method(Self, O, dynamic), Os) &
	
	static_methods(Os) :-
		self(Self),
		:findall(O, ext_method(Self, O, static), Os) &
	
	descendant(Object) :-
		<: sub(Sub),
		:ext_descendant(Sub, Object, 1, _) &
	
	descendant(Object, Level) :-
		<: sub(Sub),
		:ext_descendant(Sub, Object, 1, Level) &
	
	descendants(Objects) :-
		self(Self),
		:findall(S, utility:descendant(S,Self,utility), Objects) &
	
	descendants(Objects, Level) :-
		self(Self),
		:findall(S, utility:descendant(S,Level,Self,utility), Objects) &
	
	ancestor(Object) :-
		self :: super(Super, _),
		:ext_ancestor(Super, Object, 1, _) &
	
	ancestor(Object, Level) :-
		self :: super(Super, _),
		:ext_ancestor(Super, Object, 1, Level) &
	
	ancestors(Os) :-
		self(Self),
		:findall(O, utility:ancestor(O,Self,utility), Os) &
			
	ancestors(Objects, Level) :-
		self(Self),
		:findall(S, utility:ancestor(S, Level,Self,utility), Objects) &
	
        restart :- 
                :ext_object(O, dynamic),
		:ext_abolish(O),
		:fail &
        restart &
	
        and_cast([], _M) &
        and_cast([O|Os], M) :- O :: M, <: and_cast(Os, M) &
	
        or_cast([O|_], M) :- O :: M &
        or_cast([_|Os], M) :- <: or_cast(Os, M)
	}.
