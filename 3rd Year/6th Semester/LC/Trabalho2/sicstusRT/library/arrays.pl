/* Copyright(C) 1988, Swedish Institute of Computer Science */
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Name: arrays.pl
%  Maintainer: Lena Flood
%  Date: 8 November 1988
%  Purpose: Extendable arrays with logarithmic access time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%  Adapted from shared code written by David Warren and Fernando Pereira.
 
%  Array extends from 0 to 2**Size - 1, where Size is a multiple of 2.
%  Note that 2**Size = 1<<Size.
 
:- module(arrays, [
		       new_array/1,
		       is_array/1,
		       aref/3,
		       arefa/3,
		       arefl/3,
		       aset/4,
		       array_to_list/2
		   ]).

:- mode
	new_array(-),
	is_array(+),
	aref(+, +, ?),
	arefa(+, +, ?),
	arefl(+, +, ?),
	aset(+, +, +, -),
	array_to_list(+, -),
	subarray_to_list(+, +, +, +, ?, ?),
	check_int(+, +),
	enlarge_array(+, +, +, -, -),
	array_item(+, +, +, -),
	update_array_item(+, +, +, +, -),
	update_subarray(+, +, -, -, -).


%  new_array(-Array) 
%  returns an empty new array Array
 
new_array(array($($,$,$,$),2)).

%  is_array(+Array) 
%  is true when Array actually is an array.
 
is_array(array(_,_)).


%  aref(+Index, +Array, ?Element) 
%  unifies Element to Array[Index], or fails if Array[Index] has 
%  not been set.
	
aref(Index, array(Array,Size), Item) :-
	check_int(Index, aref(Index,array(Array,Size),Item)),
	Index < 1<<Size,
	array_item(Size, Index, Array, Item).

%   arefa(+Index, +Array, ?Element) 
%   is as aref/3, except that it unifies Element with a new array 
%   if Array[Index] is undefined. This is useful for multidimensional 
%   arrays implemented as arrays of arrays.
 
arefa(Index, array(Array,Size), Item) :-
	check_int(Index, arefa(Index,array(Array,Size),Item)),
	Index < 1<<Size,
	array_item(Size, Index, Array, Item0), !,
	Item = Item0.
arefa(_, _, Item) :- new_array(Item).

%   arefl(Index, Array, Element) 
%   is as aref/3, except that Element appears as '[]' for undefined 
%   cells.
 
arefl(Index, array(Array,Size), Item) :-
	check_int(Index, arefl(Index,array(Array,Size),Item)),
	Index < 1<<Size,
	array_item(Size, Index, Array, Item0), !,
	Item = Item0.
arefl(_, _, []).

%   aset(Index, Array, Element, NewArray) 
%   unifies NewArray with the result of setting Array[Index] to Element.
 
aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-
	check_int(Index, aset(Index,array(Array0,Size0),Item,array(Array,Size))),
	enlarge_array(Index, Size0, Array0, Size, Array1), 
	update_array_item(Size, Index, Array1, Item, Array).


%   array_to_list(+Array, -List) 
%   returns a list of pairs Index-Element of all the elements of Array
%   that have been set.
 
array_to_list(array($(A0,A1,A2,A3),Size), L0) :-
	N is Size-2,
	subarray_to_list(0, N, 0, A0, L0, L1), 
	subarray_to_list(1, N, 0, A1, L1, L2), 
	subarray_to_list(2, N, 0, A2, L2, L3), 
	subarray_to_list(3, N, 0, A3, L3, []).

subarray_to_list(K, 0, M, Item, [N-Item|L0], L) :-
	Item \== $, !,
	N is K+M,
	L = L0.
subarray_to_list(K, N, M, $(A0,A1,A2,A3), L0, L) :-
	N>0, !,
	N1 is N-2,
	M1 is (K+M)<<2,
	subarray_to_list(0, N1, M1, A0, L0, L1),
	subarray_to_list(1, N1, M1, A1, L1, L2),
	subarray_to_list(2, N1, M1, A2, L2, L3),
	subarray_to_list(3, N1, M1, A3, L3, L).
subarray_to_list(_, _, _, _, L, L).


%   check_int(+Number, +Goal)
%   checks that Number is a positive integer, if not an error is raised.
 
check_int(I, _) :- integer(I), I >= 0, !.
check_int(I, Goal) :- prolog:illarg(domain(integer,>=(0)), Goal, 1, I).

enlarge_array(I, Size0, Array0, Size, Array) :-
	I < 1<<Size0, !,
	Size = Size0,
	Array = Array0.
enlarge_array(I, Size0, Array0, Size, Array) :-
	Size1 is Size0 + 2,
	enlarge_array(I, Size1, $(Array0,$,$,$), Size, Array).


array_item(0, _, Array, Item) :- !,
        Array \== $,
	Item = Array.
array_item(N, Index, Array, Item) :-
        N1 is N-2,
        Subindex is ((Index>>N1)/\3)+1,
        arg(Subindex, Array, Array1),
        array_item(N1, Index, Array1, Item).


update_array_item(0, _, _, NewItem, NewArray) :- !, NewArray = NewItem.
update_array_item(N, Index, Array, NewItem, NewArray) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	update_subarray(Subindex, Array, Array1, NewArray1, NewArray),
	update_array_item(N1, Index, Array1, NewItem, NewArray1).

update_subarray(I, $, X, X1, Array) :- !,
	update_subarray(I, $($,$,$,$), X, X1, Array).
update_subarray(0, $(W,X,Y,Z), W, W1, $(W1,X,Y,Z)).
update_subarray(1, $(W,X,Y,Z), X, X1, $(W,X1,Y,Z)).
update_subarray(2, $(W,X,Y,Z), Y, Y1, $(W,X,Y1,Z)).
update_subarray(3, $(W,X,Y,Z), Z, Z1, $(W,X,Y,Z1)).
