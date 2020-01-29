/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Name: queues.pl                                                          %
%  Maintainer: Lena Flood                                                   %
%  Date: 8 November 1988                                                    %
%  Purpose: Queue operations package                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(queues, [
        empty_queue/1,
        is_queue/1,
        queue/2,
        queue_head/3,
        queue_head_list/3,
        queue_last/3,
        queue_last_list/3,
        list_queue/2,
        queue_length/2
	]).

%   Adapted from shared code written by Richard A O'Keefe      


%  empty_queue(?Queue)
%  is true when Queue is a queue with no elements.

empty_queue(q(0,B,B)).


%  is_queue(+Queue)
%  is true when Queue is a valid queue.

is_queue(q(N,Front,Back)) :-
	is_queue(N, Front, Back).

is_queue(0, Front, Back) :- Front==Back.
is_queue(s(N), Front, Back) :-
	nonvar(Front),
	Front = [_|Tail],
	is_queue(N, Tail, Back).


%  queue(?X, ?Queue)
%  is true when Queue is a queue with just one element.

queue(X, q(s(0),[X|B],B)).


%  queue_head(?Head, ?Queue, ?NewQueue)
%  is true when Queue and NewQueue are the same queues except that NewQueue
%  has Head inserted in front of it. It can be used to enqueue Head from
%  NewQueue.

queue_head(X, q(N,F,B), q(s(N),[X|F],B)).


%  queue_head_list(?HeadList, ?Queue1, ?Queue2)
%  is true when Queue1 and Queue2 have the same elements in them except 
%  that Queue2 has HeadList inserted in front of it.

queue_head_list([], Queue, Queue).
queue_head_list([X|Xs], Queue, Queue0) :-
	queue_head(X, Queue1, Queue0),
	queue_head_list(Xs, Queue, Queue1).


%  queue_last(?Last, ?Queue1, ?Queue2)
%  is true when Queue2 is like Queue1 but has Last as the last element
%  in the queue

queue_last(X, q(N,F,[X|B]), q(s(N),F,B)).


%  queue_last_list(?LastList, ?Queue1, ?Queue2)
%  is true when Queue1 and Queue2 are the same queues except that Queue2
%  has the list of elements List last in the queue.

queue_last_list([], Queue, Queue).
queue_last_list([X|Xs], Queue1, Queue) :-
	queue_last(X, Queue1, Queue2),
	queue_last_list(Xs, Queue2, Queue).


%  list_queue(+List, ?Queue)
%  is true when Queue is the queue representation of the elements in List.

list_queue(List, q(Count,Front,Back)) :-
	list_queue(List, Count, Front, Back).

list_queue([], 0, B, B).
list_queue([X|Xs], s(N), [X|F], B) :-
	list_queue(Xs, N, F, B).


%  queue_length(?Queue, ?Length)
%  is true when Length is the number of elements in Queue.

queue_length(q(Count,Front,Back), Length) :-
	var(Length), !,
	queue_length(Count, Front, Back, 0, Length).
queue_length(q(Count,Front,Back), Length) :-
	integer(Length),
	Length >= 0,
	queue_length(Count, Front, Back, 0, Length), !.

queue_length(0, Back, Back, Length, Length).
queue_length(s(N), [_|Front], Back, L0, Length) :-
	L1 is L0 + 1,
	queue_length(N, Front, Back, L1, Length).

