/* Copyright(C) 1989, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  File: trees.pl							     %
%  Maintainer: Lena Flood						     %
%  Date: 9 November 1988						     %
%  Purpose: Updatable binary trees					     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(trees, [
	gen_label/3,
	get_label/3,
	list_to_tree/2,
	map_tree/3,
	put_label/4,
	put_label/5,
	tree_size/2,
	tree_to_list/2
   ]).

:- meta_predicate
	map_tree(:, ?, ?).

%  Adapted from shared code written by Richard O'Keefe



%   get_label(+Index, +Tree, ?Label)
%   treats the tree as an array of N elements and returns the Index-th.
%   If Index < 1 or > N it simply fails, there is no such element.

get_label(N, Tree, Label) :-
	find_node(N, Tree, t(Label,_,_)).


find_node(1, Tree, Node) :- !, Node = Tree.
find_node(N, Tree, Node) :-
	N > 1,
	M is N >> 1,
	(   N/\1 =:= 0 -> find_node(M, Tree, t(_,Node,_))
	;   find_node(M, Tree, t(_,_,Node))
	).



%   put_label(+Index, +OldTree, +Label, -NewTree)
%   constructs a new tree the same shape as the old which moreover has the
%   same elements except that the Index-th one is Label.  

put_label(N, Old, Label, New) :-
	find_node(N, Old, t(_,Left,Right), New, t(Label,Left,Right)).


%   put_label(+Index, +OldTree, +OldLabel, -NewTree, -NewLabel)
%   constructs a new tree the same shape as the old which moreover has
%   the same elements except that the Index-th element is changed from
%   OldLabel to NewLabel.

put_label(N, Old, OldLabel, New, NewLabel) :-
	find_node(N, Old, t(OldLabel,Left,Right), New, t(NewLabel,Left,Right)).


find_node(1, Old, OldSub, New, NewSub) :- !, Old = OldSub, New = NewSub.
find_node(N, Old, OldSub, New, NewSub) :-
	N > 1,
	M is N >> 1,
	(   N/\1 =:= 0 ->
	    find_node(M, Old, t(Label,OldSub,Same), New, t(Label,NewSub,Same))
	;   find_node(M, Old, t(Label,Same,OldSub), New, t(Label,Same,NewSub))
	).



%   tree_size(+Tree, ?Size)
%   calculates the number of elements in the Tree.  All trees made by
%   list_to_tree that are the same size have the same shape.

tree_size(Tree, Size) :-
	tree_size(Tree, 0, Total),
	Size = Total.

tree_size(t, Accum, Accum).
tree_size(t(_,Left,Right), SoFar, Total) :-
	tree_size(Right, SoFar, M),
	N is M+1,
	tree_size(Left, N, Total).



%   map_tree(:Pred, ?OldTree, ?NewTree)
%   is true when OldTree and NewTree are binary trees of the same shape
%   and Pred(Old,New) is true for corresponding elements of the two trees.

map_tree(MPred, OldTree, NewTree) :-
	prolog:get_module(MPred, Pred, M),
	map_tree(OldTree, NewTree, M, Pred).

map_tree(t, t, _, _).
map_tree(t(Old,OLeft,ORight), t(New,NLeft,NRight), M, Pred) :-
	map_tree(OLeft, NLeft, M, Pred),
	prolog:dcg_translate_dcg_atom(Pred, Goal, Old, New),
	M:Goal,
	map_tree(ORight, NRight, M, Pred).



%   list_to_tree(+List, -Tree)
%   takes a given List of N elements and constructs a binary Tree
%   where get_label(K, Tree, Lab) <=> Lab is the Kth element of List.

list_to_tree(List, Tree) :-
	list_to_tree(List, [Tree|Tail], Tail).

list_to_tree([], Qhead, []) :-
	list_to_tree(Qhead).
list_to_tree([Head|Tail], [t(Head,Left,Right)|Qhead], [Left,Right|Qtail]) :-
	list_to_tree(Tail, Qhead, Qtail).

list_to_tree([]).
list_to_tree([t|Qhead]) :- list_to_tree(Qhead).



%   tree_to_list(+Tree, ?List)
%   is the converse operation to list_to_tree.	Any mapping or checking
%   operation can be done by converting the tree to a list, mapping or
%   checking the list, and converting the result, if any, back to a tree.
%   It is also easier for a human to read a list than a tree, as the
%   order in the tree goes all over the place.

tree_to_list(Tree, List) :-
	tree_to_list([Tree|Tail], Tail, List, []).

tree_to_list([], []) --> [].
tree_to_list([t|_], _) --> !.
tree_to_list([t(Head,Left,Right)|Qhead], [Left,Right|Qtail]) --> 
	[Head],
	tree_to_list(Qhead, Qtail).



%   gen_label(?Index, +Tree, ?Value)
%   assumes that Tree is a proper binary tree, and is true when Value
%   is the Index-th element in Tree.  Can be used to enumerate
%   all Values by ascending Index.

gen_label(Index, Tree, Value) :-
	gen_label([Tree|Tail], Tail, 1, Index, Value).

gen_label([t(Head,_,_)|_], _, I, I, Head).
gen_label([t(_,Left,Right)|Qhead], [Left,Right|Qtail], I, Index, Value) :-
	J is I+1,
	gen_label(Qhead, Qtail, J, Index, Value).

