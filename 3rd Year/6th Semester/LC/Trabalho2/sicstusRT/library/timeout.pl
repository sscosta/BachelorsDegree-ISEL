/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : timeout.pl
%   Author     : Mats Carlsson
%   Updated    : 4 July 2000
%   Purpose    : Meta-calls with time-out

:- module(timeout, [time_out/3]).

:- public time_out/4, time_out_rec/4.

:- meta_predicate time_out(:,?,?), time_out(:,?,?,?).

time_out(Goal, Time, Flag) :-
	integer(Time),
	Time > 0, Time < 16'7fffffff, !,
	'$stop_timer'(ContTime),
	limit_min(ContTime, Time, Time1),
	Limit = [Time1],
	ContLimit = [ContTime],
				% 3.8: catch abort etc. too!
	prolog:internal_catch(timeout:time_out(Goal, Limit, ContLimit, Flag0),
			      Excp,
			      timeout:time_out_rec(Excp, Limit, ContLimit, Flag0)),
	Flag = Flag0.
time_out(Goal, Time, Flag) :-
	prolog:illarg(domain(integer,between(1,16'7ffffffe)),
	              time_out(Goal,Time,Flag), 2).


% This is like a Byrd box with two assignable variables:
% Limit - resource limit for this goal.  Update on EXIT/REDO.
% ContLimit - resource limit for ancestor goal.  Update on REDO.
time_out(Goal, Limit, ContLimit, success) :-
	prolog:'CHOICE IDIOM'(Chpt),
	(   Limit = [L1],
	    start_timer(L1),
	    Goal
	;   time_out_exit(Limit, ContLimit),
	    fail
	),
	(   time_out_exit(Limit, ContLimit)
	;   Limit = [L4],
	    '$stop_timer'(CL4),
	    limit_min(CL4, L4, L5),
	    prolog:'$smash'(L5, Limit),
	    prolog:'$smash'(CL4, ContLimit),
	    start_timer(L5),
	    fail
	),
	prolog:'$clean_up'(Chpt, 2).

time_out_rec(time_out, [L1], [CL1], Flag) :-
	limit_gt(CL1, L1), !,
	Flag = time_out,
	limit_difference(CL1, L1, L2),
	start_timer(L2).
time_out_rec(time_out, _, _, _) :- !,
	raise_exception(time_out).
time_out_rec(Excp, Limit, ContLimit, _) :-
	time_out_exit(Limit, ContLimit),
	raise_exception(Excp).

time_out_exit(Limit, [CL2]) :-
	'$stop_timer'(Avail),
	Limit = [L2],
	limit_difference(L2, Avail, Elapsed0),
	Elapsed is max(Elapsed0,1),	% ensure eventual time_out
	% uncomment the next line if time limit applies to total CPU	
% as opposed to CPU per solution!
	% prolog:'$smash'(Avail, Limit),		% REDO will use it
	limit_difference(CL2, Elapsed, CL3),
	start_timer(CL3).

start_timer(off) :- !. % no future time_outs
start_timer(0) :- !, raise_exception(time_out). % imminent anyway
start_timer(When) :-
   '$start_timer'(When,Err),
   Err == 0, !.
start_timer(_When) :-           % [PM] 3.8.7 got an error from itimer.
   %% [PM] 3.8.7 This will not happen, currently a system_error is raised from timeout.c
   raise_exception(time_out).

limit_min(off, Y, Y) :- !.
limit_min(Y, off, Y) :- !.
limit_min(X, Y, Z) :- Z is min(X,Y).

limit_gt(off, _) :- !.
limit_gt(X, Y) :- X > Y.

limit_difference(off, _, off) :- !.
limit_difference(X, off, X) :- !.
limit_difference(X, Y, Z) :- Z is X-Y.



:- dynamic foreign/2, foreign_resource/2.

% [PM] 3.8.7 will raise system_error if an error occurred (Solaris: EACCESS from setitimer)
foreign(to_start_timer, '$start_timer'(+term,[-integer])).
% [PM] 3.8.7 will silently return 'off' if an error occurred.
foreign(to_stop_timer, '$stop_timer'([-term])).

foreign_resource(timeout, [
	init(to_init),
	deinit(to_deinit),
	to_start_timer,to_stop_timer]).

:- load_foreign_resource(library(system(timeout))).

