/* Copyright(C) 1989, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : heaps.pl                                                         %
%   Maintainer:  Lena Flood                                                   %
%   Updated: 12 January 1989                                                  %
%   Purpose: Implement heaps in Prolog.                                       %
% Jonas Barklund (Microsoft Corp.), 1999: Bug in delete_from_heap/4 fixed.    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(heaps, [
	add_to_heap/4,
	delete_from_heap/4,
	get_from_heap/4,
	empty_heap/1,
	heap_size/2,
	heap_to_list/2,
	is_heap/1,
	list_to_heap/2,
	min_of_heap/3,
	min_of_heap/5
  ]).


/*  Adapted from shared code written by Richard A O'Keefe. */

/*  A heap is a labelled binary tree where the key of each node is less
    than or equal to the keys of its sons.  The point of a heap is that
    we can keep on adding new elements to the heap and we can keep on
    taking out the minimum element. 

    A heap is represented as a triple t(N, Free, Tree) where N is the
    number of elements in the tree, Free is a list of integers which
    specifies unused positions in the tree, and Tree is a tree made of
	t			        terms for empty subtrees and
        t(Key,Datum,Lson,Rson)		terms for the rest
    The nodes of the tree are notionally numbered like this:
				    1
		     2				    3
	     4               6               5               7
	 8      12      10     14       9       13      11     15
      ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..
    The idea is that if the maximum number of elements that have been in
    the heap so far is M, and the tree currently has K elements, the tree
    is some subtree of the tree of this form having exactly M elements,
    and the Free list is a list of K-M integers saying which of the 
    positions in the M-element tree are currently unoccupied.  
*/


%   add_to_heap(+OldHeap, +Key, +Datum, -?NewHeap)
%   inserts the new Key-Datum pair into the heap.  The insertion is
%   not stable, that is, if you insert several pairs with the same
%   Key it is not defined which of them will come out first, and it
%   is possible for any of them to come out first depending on the 
%   history of the heap.  

	
add_to_heap(t(M,[],OldTree), Key, Datum, t(N,[],NewTree)) :- !,
	N is M+1,
	add_to_heap(N, Key, Datum, OldTree, NewTree).
add_to_heap(t(M,[H|T],OldTree), Key, Datum, t(N,T,NewTree)) :-
	N is M+1,
	add_to_heap(H, Key, Datum, OldTree, NewTree).


add_to_heap(1, Key, Datum, _, t(Key,Datum,t,t)) :- !.
add_to_heap(N, Key, Datum, t(K1,D1,L1,R1), t(K2,D2,L2,R2)) :-
	E is N /\ 1,
	M is N >> 1,
	compare(C, Key, K1),
	sort2(C, Key, Datum, K1, D1, K2, D2, K3, D3),
	add_to_heap(E, M, K3, D3, L1, R1, L2, R2).


add_to_heap(0, N, Key, Datum, L1, R, L2, R) :-
	add_to_heap(N, Key, Datum, L1, L2).
add_to_heap(1, N, Key, Datum, L, R1, L, R2) :-
	add_to_heap(N, Key, Datum, R1, R2).


sort2(<, Key1, Datum1, Key2, Datum2, Key1, Datum1, Key2, Datum2).
sort2(=, Key1, Datum1, Key2, Datum2, Key2, Datum2, Key1, Datum1).
sort2(>, Key1, Datum1, Key2, Datum2, Key2, Datum2, Key1, Datum1).



%   delete_from_heap(+OldHeap, +Key, ?Datum, ?NewHeap)
%   deletes a single Key-Datum pair from the heap OldHeap producing
%   the new heap NewHeap.  This is useful if you want to e.g. change
%   the priority of Datum.

delete_from_heap(t(N,Free,Tree0), Key, Datum, Heap) :-
	delete_from_heap(Tree0, Key, Datum, Tree, Hole), !,
	M is N-1,
	(   M =:= 0 -> empty_heap(Heap)
	;   Heap = t(M,[Hole|Free],Tree)
	).

delete_from_heap(t(K,D,L,R), Key, Datum, Tree, Hole) :-
	compare(C, Key, K),
	delete_from_heap(C, K, D, L, R, Key, Datum, Tree, Hole).

%% [MC] 3.8.6: made determinate
delete_from_heap(=, _, Datum, L, R, _, Datum, Tree, Hole) :-
	repair_heap(L, R, Tree, Hole).
delete_from_heap(>, K, D, L, R, Key, Datum, t(K,D,L1,R), Hole) :-
	delete_from_heap(L, Key, Datum, L1, Hole0), !,
	Hole is (Hole0<<1).	% Jonas Barklund 1999-09-29.
delete_from_heap(>, K, D, L, R, Key, Datum, t(K,D,L,R1), Hole) :-
	delete_from_heap(R, Key, Datum, R1, Hole0),
	Hole is (Hole0<<1) \/ 1. % Jonas Barklund 1999-09-29.



%   get_from_heap(+OldHeap, ?Key, ?Datum, -NewHeap)
%   returns the Key-Datum pair in OldHeap with the smallest Key, and
%   also a New Heap which is the Old Heap with that pair deleted.

get_from_heap(t(N,Free,t(Key,Datum,L,R)), Key, Datum, Heap) :-
	M is N-1,
	(   M=:=0 -> empty_heap(Heap)
	;   Heap = t(M,[Hole|Free],Tree),
	    repair_heap(L, R, Tree, Hole)
	).

repair_heap(t(K1,D1,L1,R1), t(K2,D2,L2,R2), t(K2,D2,t(K1,D1,L1,R1),R3), N) :-
	K2 @< K1, !,
	repair_heap(L2, R2, R3, M),
	N is (M<<1)+1.
repair_heap(t(K1,D1,L1,R1), t(K2,D2,L2,R2), t(K1,D1,L3,t(K2,D2,L2,R2)), N) :- !,
	repair_heap(L1, R1, L3, M),
	N is (M<<1).
repair_heap(t(K1,D1,L1,R1), t, t(K1,D1,L3,t), N) :- !,
	repair_heap(L1, R1, L3, M),
	N is (M<<1).
repair_heap(t, t(K2,D2,L2,R2), t(K2,D2,t,R3), N) :- !,
	repair_heap(L2, R2, R3, M),
	N is (M<<1)+1.
repair_heap(t, t, t, 1).



%   empty_heap(?Heap)
%   is true when Heap is the empty heap.

empty_heap(t(0,[],t)).



%   heap_size(+Heap, ?Size)
%   is true when Size is the number of elements currently in the heap.

heap_size(t(Size,_,_), Size).



%   heap_to_list(+Heap, -List)
%   returns the current set of Key-Datum pairs in the Heap as a
%   List, sorted into ascending order of Keys.  
 
heap_to_list(t(_,_,Tree), List) :-
	heap_tree_to_list(Tree, List).


heap_tree_to_list(t, []).
heap_tree_to_list(t(Key,Datum,Lson,Rson), [Key-Datum|Merged]) :-
	heap_tree_to_list(Lson, Llist),
	heap_tree_to_list(Rson, Rlist),
	heap_tree_to_list(Llist, Rlist, Merged).


heap_tree_to_list([H1|T1], [H2|T2], [H2|T3]) :-
	H2 @< H1, !,
	heap_tree_to_list([H1|T1], T2, T3).
heap_tree_to_list([H1|T1], T2, [H1|T3]) :-
	heap_tree_to_list(T1, T2, T3).
heap_tree_to_list([], T, T).



%   is_heap(+Heap)
%   is true when Heap is a valid heap.

is_heap(t(Size, _, Tree)) :-
	integer(Size),
	(   Size =:= 0 -> Tree = t
	;   Size > 0,
	    Tree = t(K,_,L,R),
	    is_heap(L, K, 1, Size1),
	    is_heap(R, K, Size1, Size)
	).


is_heap(t, _, Size, Size).
is_heap(t(K,_,L,R), Min, Size0, Size) :-
	K @>= Min,
	Size1 is Size0+1,
	is_heap(L, K, Size1, Size2),
	is_heap(R, K, Size2, Size).



%   list_to_heap(+List, -Heap)
%   takes a list of Key-Datum pairs (such as keysort could be used to
%   sort) and forms them into a heap.  


list_to_heap(List, Heap) :-
	list_to_heap(List, 0, t, Heap).

list_to_heap([], N, Tree, t(N,[],Tree)).
list_to_heap([Key-Datum|Rest], M, OldTree, Heap) :-
	N is M+1,
	add_to_heap(N, Key, Datum, OldTree, MidTree),
	list_to_heap(Rest, N, MidTree, Heap).




%   min_of_heap(+Heap, ?Key, ?Datum)
%   returns the Key-Datum pair at the top of the heap (which is of
%   course the pair with the smallest Key), but does not remove it
%   from the heap.  It fails if the heap is empty.

min_of_heap(t(_,_,t(Key,Datum,_,_)), Key, Datum).



%   min_of_heap(+Heap, ?Key1, ?Datum1, ?Key2, ?Datum2)
%   returns the smallest (Key1) and second smallest (Key2) pairs in
%   the heap, without deleting them.  It fails if the heap does not
%   have at least two elements.

min_of_heap(t(_,_,t(Key1,Datum1,Lson,Rson)), Key1, Datum1, Key2, Datum2) :-
	min_of_heap(Lson, Rson, Key2, Datum2).

min_of_heap(t(Ka,_,_,_), t(Kb,Db,_,_), Key, Datum) :-
	Kb @< Ka, !,
	Key = Kb,
	Datum = Db.
min_of_heap(t(Ka,Da,_,_), _, Ka, Da).
min_of_heap(t, t(Kb,Db,_,_), Kb, Db).
