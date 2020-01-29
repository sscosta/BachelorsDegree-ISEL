/* Copyright (C) 1990 Swedish Institute of Computer Science */
/* Modified 2000 by Malcolm Ryan to include the 'quit' message */

:- module(linda_client,[
	linda_client/1,
	close_client/0,
	in/1,
	in/2,
	in_noblock/1,
	out/1,
	rd/1,
	rd/2,
	rd_noblock/1,
        bagof_rd_noblock/3,
	linda_timeout/2,
	shutdown_server/0
   ]).

:- use_module(library(fastrw), [
	fast_read/2,
	fast_write/2
   ]).
:- use_module(library(sockets), [
	socket/2,
	%% socket_buffering/4, [PM] 3.8.7 socket_select is fixed to work with default buffering
	socket_connect/3,
	socket_select/5
   ]).
:- use_module(library(lists), [
	member/2
   ]).

:- dynamic linda_stream/2, time_out/1.

time_out(20:0).

protocol(0'f).      /* 0'f = special format, 0'p = write_canonical */

linda_client(NewAddress) :-
	linda_stream(Stream,OldAddress),
	current_stream(_N,socket,Stream), !,
	(   NewAddress = OldAddress ->
	    true    % Seems ok, just ignore it
	;   format(user_error,
	           '{ERROR: linda_client/1: Already client to ~w}~n',
		   [OldAddress])
	).
linda_client(Address) :-
	retractall(linda_stream(_,_)),%Might be a try to reestablish a broken conn.
	open_client(Address,Stream),
	assert(linda_stream(Stream,Address)),
	ping(Answer),
	ping_answer_ok(Answer).

ping_answer_ok(pong) :- !.
ping_answer_ok(A) :-
	format(user_error,
	       '{ERROR: linda_client/1: strange answer from server: ~q}~n',
	       [A]).	

open_client(Host:Port,Stream) :-
	atom(Host),
	integer(Port), !,
	socket('AF_INET', Sock),
	socket_connect(Sock, 'AF_INET'(Host,Port), Stream),
        %% [PM] 3.8.7 socket_select is fixed to work with buffered sockets.
	%% socket_buffering(Stream, read, _, unbuf), % 3.8.3
        true.
open_client(Addr,_Stream) :-
	format(user_error,
	       '{ERROR: open_client/3: Illegal network address: ~w}~n',
	       [Addr]),
	fail.

%-----------------------------------------------------------------------------
close_client :-
	retract(linda_stream(Stream,_Address)),
	close(Stream).

ping(Answer) :-	
	to_linda(0'p, ping),
	time_out_select,
	from_linda(Answer).

out(T) :-
	to_linda(0'o, T).

in(T) :-
        to_linda(0'i, T),
	from_linda(T).

in(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'I, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

in_noblock(T) :-
        to_linda(0'j, T),
	time_out_select,
	from_linda(0's, T).
	
rd_noblock(T) :-
	to_linda(0's, T),
	time_out_select,
	from_linda(0's, T).

rd(T) :-
	to_linda(0'r, T),
	from_linda(T).

rd(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'R, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

bagof_rd_noblock(Template,Tuple,Bag) :- 
	to_linda(0'b, b(Template,Tuple,Bag)),
	time_out_select,
	from_linda(Bag).

linda_trace(OnOff) :-
	(var(OnOff) ; OnOff=on ; OnOff=off), !,
	to_linda(0't, OnOff),
	time_out_select,
	from_linda(OnOff).

linda_call(Goal) :-
	to_linda(0'c, Goal),
	from_linda(0's, Goal).

shutdown_server :-
   %% [PM] 3.8.7 was: to_linda(0'q, quit) | true.
   ( to_linda(0'q, quit) ->
      true
   ; Goal = linda_client:shutdown_server,
      %% [PM] 3.9b5 Used to silently succeed if there was no connection to the server.
      prolog:illarg(existence('connection to',  server, 0), Goal, 0)
   ).

%-----------------------------------------------------------------------------
to_linda(Code, Item) :-
        linda_stream(Stream,Address),
	(   current_stream(_N,socket,Stream) ->
	    protocol(P),
	    write_out(P,Code,Item,Stream)
        ;   
            linda_client(Address) -> % Connection broken; could reestablish it
	    to_linda(Code, Item)
        ;   format(user_error,
	           '{ERROR: the connection with linda has been shut down, can\'t reopen it!}\n',
		   []),
	    fail
        ).

%-----------------------------------------------------------------------------
write_out(0'p,Code,Item,Stream) :- 
	put(Stream,0'p),
	put(Stream,Code), 
	write_canonical(Stream,Item),
	write(Stream,'.'),
	nl(Stream),
	flush_output(Stream).
write_out(0'f,Code,Item,Stream) :-
	put(Stream,0'f), 
	put(Stream,Code), 
	fast_write(Stream,Item),
	flush_output(Stream).

read_in(0'p,Item,Stream) :- read(Stream,Item).
read_in(0'f,Item,Stream) :- fast_read(Stream,Item).
%-----------------------------------------------------------------------------
from_linda(Item) :-
	linda_stream(Stream,_Address),
	protocol(P),
	read_in(P,Item,Stream).

from_linda(Code, Item) :-
	linda_stream(Stream,_Address),
	get0(Stream,Code),
	protocol(P),
	read_in(P,Item,Stream).

%-----------------------------------------------------------------------------
time_out_select :- time_out(TimeOut), time_out_select1(TimeOut).

time_out_select1(off) :- !.
time_out_select1(TimeOut) :-
	TimeOut = _:_,
	linda_stream(Stream,_Address),
	socket_select([], _, TimeOut, [Stream], _), !.
time_out_select1(_:_) :-
	format(user_error,'{ERROR: wait for linda timed out}~n',[]),
	fail.
%-----------------------------------------------------------------------------

linda_timeout(Old, New) :-
	linda_timeout_arg(Old, New),
	time_out(Old),
	retract(time_out(Old)),
	assert(time_out(New)).

linda_timeout_arg(O, N) :- var(N), !, O==N. % ==/2, not =/2 !
linda_timeout_arg(_, S:M) :- integer(S), integer(M), S>=0, M>=0, !.
linda_timeout_arg(_, off).
