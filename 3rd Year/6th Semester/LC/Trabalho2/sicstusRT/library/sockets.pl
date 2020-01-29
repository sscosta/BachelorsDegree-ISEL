/* Copyright (C) 1995 Swedish Institute of Computer Science. */

%   File       : sockets.pl
%   Author     : Stefan Andersson
%   Updated    : 7 August 2000
%   Purpose    : Sockets interface

:- module(sockets, [
	socket/2,
	socket_bind/2,
	socket_connect/3,
	socket_listen/2,
	socket_accept/2,
	socket_accept/3,
	socket_select/5,
	socket_select/6,
	socket_close/1,
	socket_buffering/4,
	current_host/1,
	hostname_address/2
		   ]).
	
% -----------------------------------------------------------------------

socket(Domain, Socket) :-
	atom(Domain),
	socket_domain(Domain), !,
	var(Socket),
	socket1(Domain, Socket).
socket(Domain, Socket) :- 
	findall(X, socket_domain(X), L),
	prolog:illarg(domain(atom,one_of(L)), socket(Domain,Socket), 1).

socket_domain('AF_INET').
socket_domain('AF_UNIX').

socket1('AF_INET', Socket) :- '$socket_I'(Socket).
socket1('AF_UNIX', Socket) :- '$socket_U'(Socket).

% -----------------------------------------------------------------------

socket_bind(Socket, Address) :-
	integer(Socket), !,
	socket_bind2(Address, Socket).
socket_bind(Socket, Address) :-
	prolog:illarg(type(integer), socket_bind(Socket,Address), 1).

socket_bind2(Address, Socket) :-
	var(Address), !,
	prolog:illarg(var, socket_bind(Socket,Address), 2).
socket_bind2('AF_INET'(Machine,Port), Socket) :-
	(   var(Port) ->
	    '$bind_I'(Socket, 0, Port)
	;   integer(Port) ->
	    '$bind_I'(Socket, Port, _)
	),
	!,
	(   nonvar(Machine) -> true
	;   '$hostname'(Machine)
	).
socket_bind2('AF_UNIX'(Path), Socket) :-
	atom(Path), !,
	(   '$bind_U'(Socket, Path, 1) -> true
	;   prolog:illarg(domain(term,'shorter path'),
			  socket_bind(Socket,'AF_UNIX'(Path)), 2, Path)
	).
socket_bind2(Address, Socket) :-
	prolog:illarg(domain(term,socket_address),
		      socket_bind(Socket,Address), 2, Address).

% -----------------------------------------------------------------------

socket_connect(Socket, Address, Stream) :-
	integer(Socket), !,
	socket_connect2(Address, Socket, StreamCode),
	stream_code(Stream, StreamCode).
socket_connect(Socket, Address, Stream) :-
	prolog:illarg(type(integer), socket_connect(Socket,Address,Stream), 1).

socket_connect2('AF_INET'(Machine,Port), Socket, StreamCode) :-
	'$connect_I'(Socket, Machine, Port, StreamCode).
socket_connect2('AF_UNIX'(Path), Socket, StreamCode) :-
	(   '$connect_U'(Socket, Path, StreamCode, 1) -> true 
	;   prolog:illarg(domain(term,'shorter path'),
			  socket_connect(Socket,'AF_UNIX'(Path),_), 2, Path)
	).

% -----------------------------------------------------------------------

socket_listen(Socket, Length) :-
	integer(Socket), !,
	(   integer(Length), Length >= 1 ->
	    '$listen'(Socket, Length)
	;   prolog:illarg(domain(integer,>=(1)), socket_listen(Socket,Length), 2)
	).
socket_listen(Socket, Length) :-
	prolog:illarg(type(integer), socket_listen(Socket,Length), 1).

% -----------------------------------------------------------------------

socket_accept(Socket, Stream) :-
	socket_accept(Socket, _, Stream).

socket_accept(Socket, Client, Stream) :-
	integer(Socket), !,
	'$accept'(Socket, Client, StreamCode),
	stream_code(Stream, StreamCode).
socket_accept(Socket, Client, Stream) :-
	prolog:illarg(type(integer), socket_accept(Socket,Client,Stream), 1).

% -----------------------------------------------------------------------

socket_select(Sockets, NewStreams, TimeOut, Streams, ReadStreams) :-
	Goal = socket_select(Sockets,NewStreams,TimeOut,Streams,ReadStreams),
	ss_pre(Sockets, Goal, Sockets1, SSMode),
	select(Sockets1, CSockets, NewStreams1, Clients, TimeOut, 3, Streams, 4, ReadStreams, Goal),
	ss_post(SSMode, Sockets, CSockets, NewStreams1, NewStreams, Clients, _).

socket_select(Sockets, NewStreams, Clients, TimeOut, Streams, ReadStreams) :-
	Goal = socket_select(Sockets,NewStreams,Clients,TimeOut,Streams,ReadStreams),
	ss_pre(Sockets, Goal, Sockets1, SSMode),
	select(Sockets1, CSockets, NewStreams1, Clients1, TimeOut, 4, Streams, 5, ReadStreams, Goal),
	ss_post(SSMode, Sockets, CSockets, NewStreams1, NewStreams, Clients1, Clients).

select(Sockets, CSockets, NewStreams, Clients, TimeOut, TONr, Streams, SNr, ReadStreams, Goal) :-
	(   nonvar(TimeOut),
	    time_out_spec(TimeOut, Tsec, Tusec) -> true
	;   prolog:illarg(domain(term,time_out_spec), Goal, TONr)
	),
	streams_codes(Streams, StreamsCodes),
        % '$select'(+term,-term,-term,-term,+integer,+integer,+term,-term,[-integer])).
	'$select'(Sockets, CSockets0, NewStreamsCodes0, Clients0, Tsec, Tusec, StreamsCodes, ReadStreamsCodes0, RC),
        ( RC = 3 ->             % [PM] 3.9b5 EINTR happened (and nothing selected), should retry
            select(Sockets, CSockets, NewStreams, Clients, TimeOut, TONr, Streams, SNr, ReadStreams, Goal)
        ; RC = 0 ->
            CSockets = CSockets0,
            NewStreamsCodes = NewStreamsCodes0,
            Clients = Clients0,
            ReadStreamsCodes = ReadStreamsCodes0,
            codes_streams(NewStreamsCodes, NewStreams0), % make it steadfast
            NewStreams = NewStreams0,
            codes_streams(ReadStreamsCodes, ReadStreams0), % make it steadfast
            ReadStreams = ReadStreams0
        ; RC = 1 -> prolog:illarg(domain(term,sockets), Goal, 1, Sockets)
        ; % RC = 2 ->
            prolog:illarg(domain(term,stream_with_IO_descriptor), Goal, SNr, Streams)
        ).

time_out_spec(off, -1, -1).
time_out_spec(Tsec:Tusec, Tsec, Tusec) :- integer(Tsec), integer(Tusec).

streams_codes([], []).
streams_codes([S|Ss], [C|Cs]) :-
	stream_code(S, C),
	streams_codes(Ss, Cs).

codes_streams([], []).
codes_streams([C|Cs], [S|Ss]) :-
	stream_code(S, C),
	codes_streams(Cs, Ss).

ss_pre(Sockets, _, Sockets1, SSMode) :- var(Sockets), !,
	Sockets1 = [],
	SSMode = 0.
ss_pre(Sockets, _, Sockets1, SSMode) :-
	integer(Sockets), !,
	Sockets1 = [Sockets],
	SSMode = 0.
ss_pre([], _, Sockets, SSMode) :- !,
	Sockets = [], SSMode = 2.
ss_pre(Sockets, Goal, Sockets1, SSMode) :- 
	Sockets = [_-_|_], !,
	SSMode = 2,
	ss_pre2(Sockets, Goal, Sockets1).
ss_pre(Sockets, _, Sockets, 1).

ss_pre2([], _, []).
ss_pre2([Socket|Sockets], Goal, [Socket1|Sockets1]) :-
	(   Socket = _-Socket1 ->
	    true
	;   prolog:illarg(domain(term,list_of_term_socket_pairs), Goal, 1)
	),
	ss_pre2(Sockets, Goal, Sockets1).


ss_post(2, Sockets, CSockets, Streams0, Streams, Clients, _) :- !,
	ss_membs(CSockets, Clients, Streams0, Sockets, Streams).
ss_post(1, _, _, Streams0, Streams, Clients0, Clients) :- !,
	Streams = Streams0,
	Clients = Clients0.
ss_post(0, _, _, [], _, _, _) :- !.
ss_post(0, _, _, [Stream], Stream, [Client], Client).


ss_membs([], [], [], _, []).
ss_membs([CS|CSs], [C|Cs], [S0|Ss0], Sockets, [S|Ss]) :-
	S = IdTerm-connection(C,S0),
	ss_memb(Sockets, CS, IdTerm),
	ss_membs(CSs, Cs, Ss0, Sockets, Ss).

ss_memb([IdTerm-Sock|_], Sock, IdTerm) :- !.
ss_memb([_|Sockets], Sock, IdTerm) :-
	ss_memb(Sockets, Sock, IdTerm).

% -----------------------------------------------------------------------

current_host(Host) :- '$hostname'(Host).

hostname_address(Host, Address) :- atom(Host), !,
	'$hostname_address'(Host, Address),
	Address \== [].
hostname_address(Host, Address) :- var(Host), !,
	(   atom(Address) ->
	    '$address_hostname'(Address, Host),
	    Host \== []
	;   prolog:illarg(type(atom), hostname_address(Host,Address), 2)
	).
hostname_address(Host, Address) :-
	prolog:illarg(type(atom), hostname_address(Host,Address), 1).


% -----------------------------------------------------------------------

socket_buffering(Stream, ReadWrite, Old, New) :-
	stream_code(Stream, StreamCode),
	read_write_enc(ReadWrite, RWE),
	'$setbuf'(StreamCode, RWE, Old, New, Res),
        Res == 1.

read_write_enc(read, 1).
read_write_enc(write, 0).

% -----------------------------------------------------------------------

:- dynamic foreign/2, foreign_resource/2.

foreign(socket_connect_I,
	'$connect_I'(+integer,+string,+integer,[-address('SP_stream')])).
foreign(socket_connect_U,
	'$connect_U'(+integer,+string,-address('SP_stream'),[-integer])).
foreign(socket_accept, '$accept'(+integer,-term,[-address('SP_stream')])).
foreign(socket_select,
	'$select'(+term,-term,-term,-term,+integer,+integer,
	          +term,-term,[-integer])).
foreign(socket_socket_U, '$socket_U'([-integer])).
foreign(socket_socket_I, '$socket_I'([-integer])).
foreign(socket_bind_U, '$bind_U'(+integer,+string,[-integer])).
foreign(socket_bind_I, '$bind_I'(+integer,+integer,[-integer])).
foreign(socket_listen, '$listen'(+integer,+integer)).
foreign(socket_close, socket_close(+integer)).
foreign(hostname, '$hostname'([-atom])).
foreign(hostname_address, '$hostname_address'(+string,-term)).
foreign(address_hostname, '$address_hostname'(+string,-term)).
foreign(socket_setbuf, '$setbuf'(+address,+integer,+term /* [PM] 3.8.7 yes: +term ! */,+term,[-integer])).

foreign_resource(sockets, [
	init(socket_init),
	deinit(socket_deinit),
	socket_connect_I,
	socket_connect_U,
	socket_accept,
	socket_select,
	socket_socket_U,
	socket_socket_I,
	socket_bind_U,
	socket_bind_I,
	socket_listen,
	socket_close,
	hostname,
	hostname_address,
	address_hostname,
	socket_setbuf
			  ]).	

:- load_foreign_resource(library(system(sockets))).
