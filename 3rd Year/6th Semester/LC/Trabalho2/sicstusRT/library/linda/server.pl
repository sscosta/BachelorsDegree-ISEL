/* Copyright (C) 1990 Swedish Institute of Computer Science */
/* Modified 2000 by Malcolm Ryan to include the 'quit' message */

:- module(linda, [
	linda/0,
	linda/1
   ]).

:- use_module(library(fastrw), [
	fast_read/1,
	fast_write/1
   ]).
:- use_module(library(sockets), [
	socket/2,
	socket_bind/2,
	%% socket_buffering/4, [PM] 3.8.7 socket_select is fixed to work with default buffering
	socket_listen/2, 
	socket_select/5
   ]).
:- use_module(library(lists), [
	delete/3,
	member/2
   ]).

:- dynamic ts/1.
:- dynamic tuple/1.
:- dynamic waiting_list/5.
:- dynamic linda_trace/0.
:- dynamic quit/0.

linda(Hook) :-
	nonvar(Hook),
	Hook = (Host:Port)-Goal,
	create_server_socket(Socket, Host, Port),
	call(Goal),
	server1(Socket,[]).
linda :-
	linda((Addr-format(user_error,'Server address is ~q~n',[Addr]))).

server1(_Socket, []) :- quit, !,
	retractall(quit).
server1(Socket, Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	server2(ReadableStreams,Streams1,Streams2),
	server1(Socket, Streams2).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/
wait_for_arrival(Socket, Streams0, ReadableStreams, Streams) :-
	socket_select(Socket, NewStream, off, Streams0, ReadableStreams),
	new_stream_in_list(NewStream, Streams0, Streams).

new_stream_in_list(NewStream, Streams, Streams) :- 
	var(NewStream), !.
new_stream_in_list(NewStream, Streams0, [NewStream|Streams0]) :-
        %% [PM] 3.8.7 socket_select is fixed to work with buffered sockets.
	%% socket_buffering(NewStream, read, _, unbuf), % 3.8.3
	format(user_error,'---------- Opened ~q~n',[NewStream]).

create_server_socket(Socket, Host, Port) :-
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host,Port)),
	socket_listen(Socket, 5).

server2([],S,S).
server2([InStream|SSs],SS0,SS) :-
	server_one_stream(InStream,SS0,SS1),
	server2(SSs,SS1,SS).

server_one_stream(InStream,SS0,SS1) :-
	set_input(InStream),
	set_output(InStream),
	get0(Protocol),
	server_one_stream(Protocol,InStream,SS0,SS1).

server_one_stream(-1,S,SS0,SS) :- /* end of file, that is, a broken connection*/ !,
	set_input(user_input),
	set_output(user_output),
        close(S),
	format(user_error,'---------- Closed ~q~n',[S]),
	delete(SS0,S,SS).
server_one_stream(Protocol,_Stream,SS,SS) :-
	get0(Request),
	trace_linda(before,Request,Ti),
	perform_request(Request,Protocol),
	trace_linda(after,_,Ti).

%----------------------------------------
perform_request(0's,Protocol) :-  /* rd_noblock */ !,
	get_input(Protocol,Tuple),
	cond_to_client(Protocol,tuple(Tuple), Tuple).
perform_request(0'r,Protocol) :-  /* rd */ !,
	get_input(Protocol,Tuple),
	to_client_blocking(rd,Protocol,Tuple).
perform_request(0'R,Protocol) :- /* rd, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist),tuple(T) ->
	    to_client(Protocol,T)
	;   time_stamp(TimeStamp),
	    current_output(Stream),
	    assert_waiting_lists(Tlist,Protocol,TimeStamp,Stream,rd)
	).
perform_request(0'i,Protocol) :-  /* in */ !,
	get_input(Protocol,Tuple),
	to_client_blocking(in,Protocol,Tuple).
perform_request(0'I,Protocol) :- /* in, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist),tuple(T) ->
	    retract(tuple(T)),
	    to_client(Protocol,T)
	;   time_stamp(TimeStamp),
	    current_output(Stream),
	    assert_waiting_lists(Tlist,Protocol,TimeStamp,Stream,in)
	).
perform_request(0'j,Protocol) :-  /* in_noblock */ !,
	get_input(Protocol,Tuple),
	cond_to_client(Protocol,retract(tuple(Tuple)), Tuple).
perform_request(0'o,Protocol) :- /* out */ !,
	get_input(Protocol,Tuple),
	perform_out(Tuple).
perform_request(0'p,Protocol) :-  /* ping */ !,
	get_input(Protocol,In),
	ping_answer(In,Out),
	to_client(Protocol,Out).
perform_request(0'b,Protocol) :- /* bagof_rd_noblock */ !,
	get_input(Protocol,b(Template,Tuple,Bag)),
	tupleGoal(Tuple,Goal),
	(bagof(Template,Goal,Bag) ; Bag = []),
	to_client(Protocol,Bag).
perform_request(0'c,Protocol) :- /* call */ !,
	get_input(Protocol,Goal),
	cond_to_client(Protocol,call(Goal), Goal).
perform_request(0't,Protocol) :- /* trace */ !,
	get_input(Protocol,Q),
	trace_answer(Q,Repl),
	to_client(Protocol,Repl).
perform_request(0'q,Protocol) :- /* quit */ !,
	get_input(Protocol,_),
	format(user_error,'---------- Received Shutdown message. Waiting for Clients to close\n',[]),
	assert(quit).
perform_request(R, _Protocol) :-
	current_input(Stream),
	format(user_error,'*** Unknown request from ~q: ~q~n',[Stream,R]).

ping_answer(ping,pong) :- !.
ping_answer(X, illegal(X)).

%-----------------------------------------------------------------------------
perform_out(Tuple) :-
	retract(waiting_list(Tuple,Protocol,TS,WaitingStream,Op)), !,
	remove_disjunction(TS),
	to_clientS(Protocol,WaitingStream,Tuple),
	(   Op = rd ->
	    perform_out(Tuple)
	;   true
	).
perform_out(Tuple) :-
	assertz(tuple(Tuple)).

remove_disjunction(TS) :- TS>0, !,
	retractall(waiting_list(_,_,TS,_,_)).
remove_disjunction(_).	

%-----------------------------------------------------------------------------
trace_answer(Q,on) :- var(Q), linda_trace, !.
trace_answer(Q,off) :- var(Q), !.
trace_answer(on,on) :- linda_trace, !.
trace_answer(on,on) :- !, assert(linda_trace).
trace_answer(_,off) :- retractall(linda_trace).

%-----------------------------------------------------------------------------
time_stamp(T) :- retract(ts(T)), !, T1 is T+1, assert(ts(T1)).
time_stamp(1) :- assert(ts(2)).

assert_waiting_lists([],_Protocol,_TimeStamp,_Stream,_Op).
assert_waiting_lists([T|Ts],Protocol,TimeStamp,Stream,Op) :-
	assert_one_waiting(T,Protocol,TimeStamp,Stream,Op),
	assert_waiting_lists(Ts,Protocol,TimeStamp,Stream,Op).

assert_one_waiting(Tuple,Protocol,TimeStamp,Stream,Op) :-
	assertz(waiting_list(Tuple,Protocol,TimeStamp,Stream,Op)).

%-----------------------------------------------------------------------------
to_client_blocking(rd,Protocol,Tuple) :- 
	tuple(Tuple), !,
	to_client(Protocol,Tuple).
to_client_blocking(in,Protocol,Tuple) :- 
	retract(tuple(Tuple)), !,
	to_client(Protocol,Tuple).
to_client_blocking(Op,Protocol,Tuple) :- 
	current_output(Stream), 
	assert_one_waiting(Tuple,Protocol,0,Stream,Op).
%-----------------------------------------------------------------------------
tupleGoal(T,tuple(T)) :- var(T).
tupleGoal(T,G) :- tupleGoal1(T,G).
tupleGoal1(V^T,V^G) :- !, tupleGoal1(T,G).
tupleGoal1(T,linda:tuple(T)).

%-----------------------------------------------------------------------------
get_input(0'f, Data) :- fast_read(Data).
get_input(0'p, Data) :- read(Data).
	
put_output(0'f, Data):- fast_write(Data).
put_output(0'p, Data):- write_canonical(Data), write('.'), nl.

%-----------------------------------------------------------------------------
to_client(Protocol,C,Data) :- 
	put(C),
	put_output(Protocol,Data),
	flush_output.
to_client(Protocol,Data) :- 
	put_output(Protocol,Data),
	flush_output.

to_clientS(Protocol,Stream,Data) :- 
	current_output(S),
	set_output(Stream),
	put_output(Protocol,Data),
	flush_output,
	set_output(S).
%-----------------------------------------------------------------------------
cond_to_client(Protocol,Cond, Data) :-
	call(Cond), !,
	to_client(Protocol,0's, Data).
cond_to_client(_Protocol,_Cond,_Data) :-
	put(0'f),
	flush_output.

%-----------------------------------------------------------------------------
trace_linda(before,R,Ti) :-
	linda_trace, !,
	put(user_error,R),
	tab(user_error,2),
	flush_output(user_error),
	statistics(runtime,[Ti,_]).
trace_linda(after,_,Ti) :-
	linda_trace, !,
	statistics(runtime,[Tf,_]),
	T is Tf-Ti,
	format(user_error,'(~w ms)~n',[T]).
trace_linda(_,_,0).
