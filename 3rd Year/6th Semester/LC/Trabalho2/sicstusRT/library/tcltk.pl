/* Copyright(C) 1995, Swedish Institute of Computer Science */

%   File       : tcltk.pl
%   Author     : Stefan Andersson
%   Updated    : 12 June 2000
%   Purpose    : Tcl/Tk interface

:- module(tcltk, [
	tcl_new/1,
	tcl_delete/1,
	tcl_eval/3,
	tcl_event/3,
	tk_new/2,
	tk_main_window/2,
	tk_destroy_window/1,
	tk_make_window_exist/1,
	tk_num_main_windows/1,
	tk_do_one_event/0,
	tk_do_one_event/1,
	tk_next_event/2,
	tk_next_event/3,
	tk_main_loop/0,
	tk_terminal/5
	       ]).

% -----------------------------------------------------------------------

call_from_tcl(StreamCode) :-
	stream_code(Stream, StreamCode),
	read_term(Stream, Goal, [variable_names(Vars)]),
	call(user:Goal),			% default module "user"
	add_result(Vars, StreamCode).

add_result([], _S).
add_result([(Var = Val)|T], S) :-
	'$tcl_add_result'(S, Var, Val),
	add_result(T, S).

% -----------------------------------------------------------------------

read_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	read(Stream, Term).

write_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write(Stream, Term).

writeq_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	writeq(Stream, Term).

write_canonical_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write_canonical(Stream, Term).

format_sc(StreamCode, Fmt, Args) :-
	stream_code(Stream, StreamCode),
	format(Stream, Fmt, Args).

% -----------------------------------------------------------------------
% Tk

tk_new(Options, Interp) :-
	tk_options(Options, '', AppName, '', Display, 0, Flags), !,
	tcl_new(Interp),
	'$tk_new'(Interp, AppName, Display, Flags, 41).
tk_new(Options, Interp) :-
	Goal = tk_new(Options,Interp),
	prolog:illarg(domain(list(callable),tk_new_options), Goal, 1).

tk_options(Option, _, _, _, _, _, _) :- var(Option), !, fail.
tk_options([], App, App, Disp, Disp, Flags, Flags).
tk_options([Option|Options], App0, App, Disp0, Disp, Flags0, Flags) :-
	nonvar(Option),
	tk_option(Option, App0, App1, Disp0, Disp1, Flags0, Flags1),
	tk_options(Options, App1, App, Disp1, Disp, Flags1, Flags).

tk_option(top_level_events, App0, App, Disp0, Disp, Flags0, Flags) :-
	App=App0, Disp=Disp0,
	Flags is Flags0 \/ 1.
tk_option(name(Name), _, App, Disp0, Disp, Flags0, Flags) :-
	App=Name, Disp=Disp0, Flags=Flags0.
tk_option(display(Display), App0, App, _, Disp, Flags0, Flags) :-
	App=App0, Disp=Display, Flags=Flags0.

% -----------------------------------------------------------------------
% Handle one event

tk_do_one_event :-
	'$tk_do_one_event'(16'3f, 1).		% all + don't wait

tk_do_one_event(Options) :-
	tk_do_one_event1(Options).

tk_do_one_event1(Options) :- integer(Options), !,
	'$tk_do_one_event'(Options, 1).
tk_do_one_event1(Options) :-
	translate_options(Options, 0, Opt),
	!,
	'$tk_do_one_event'(Opt, 1).
tk_do_one_event1(Options) :-
	prolog:illarg(domain(list(atom),tk_do_one_event_options), tk_do_one_event(Options), 1).

translate_options(Opt, _, _) :- var(Opt), !, fail.
translate_options([], Opt, Opt).
translate_options([Option|Options], Opt1, Opt) :-
	nonvar(Option),
	is_option(Option, Opt2),
	Opt3 is Opt1 \/ Opt2,
	translate_options(Options, Opt3, Opt).

is_option(tk_dont_wait,16'2).
is_option(tk_x_events,16'4).
is_option(tk_window_events,16'4).
is_option(tk_file_events,16'8).
is_option(tk_timer_events,16'10).
is_option(tk_idle_events,16'20).
is_option(tk_all_events,16'3d).


% -----------------------------------------------------------------------
% Process all X-events until there is a Prolog event 

tk_next_event(Interp, Event) :-
	'$tk_do_one_event'(Interp, 16'3d, Event0),
	tk_num_main_windows(WindowsLeft),
	tk_next_event_cont(WindowsLeft, Interp, 16'3d, Event0, Event).

tk_next_event(Options, Interp, Event) :-
	integer(Options), !,
	M is Options /\ 16'fd,			% always wait
	'$tk_do_one_event'(Interp, M, Event0),
	tk_num_main_windows(WindowsLeft),
	tk_next_event_cont(WindowsLeft, Interp, M, Event0, Event).
tk_next_event(Options, Interp, Event) :-
	translate_options(Options, 0, Opt),
	!,
	tk_next_event(Opt, Interp, Event).
tk_next_event(Options, Interp, Event) :-
	prolog:illarg(domain(list(atom),tk_next_event_options), tk_next_event(Options,Interp,Event), 1).

tk_next_event_cont(0, _, _, _, Event) :- !, Event=[].
tk_next_event_cont(_, Interp, M, Event0, Event) :-
	(   Event0 = [] ->				% no Prolog event
	    tk_next_event(M, Interp, Event)
	;   Event = Event0
	).


% -----------------------------------------------------------------------
% Defines TkMainLoop in Prolog to enable Prolog control-C
% interrupt while executing Tk

tk_main_loop :-
	tk_num_main_windows(N),
	(   N > 0 ->
	    '$tk_do_one_event'(16'3d, _),
	    tk_main_loop
        ;   true
	).


% -----------------------------------------------------------------------

/* Error printing */

:- multifile user:generate_message_hook/3.
:- dynamic   user:generate_message_hook/3.

user:generate_message_hook(tcl_error(Goal,Message)) --> !,
	generate_message_hook(Goal, Message, 'TCL').
user:generate_message_hook(tk_error(Goal,Message)) --> !,
	generate_message_hook(Goal, Message, 'TK').

% should be combinaed with a portray/1 that recognizes
% and prints "strings"
generate_message_hook(Goal, Message, Package) -->
	[Package-[], nl,
	 'goal:  '-[], write_term(Goal), nl,
	 '~s'-[Message], nl].
	
/**** Pre 3.9
:- multifile
	user:portray_message/2.

:- dynamic
	user:portray_message/2.

user:portray_message(error, tcl_error(Goal,Message)) :- !,
	perror(Goal, 'TCL', Message).
user:portray_message(error, tk_error(Goal,Message)) :- !,
	perror(Goal, 'TK', Message).

perror(Name/Arity, Package, Message) :- !,
	format(user_error,
	       '{~a ERROR: ~a/~d - ~s}~n',
	       [Package,Name,Arity,Message]).
perror(Goal, Package, Message) :-
	Goal =.. [Name|Args],
	format(user_error, '{~a ERROR: ~a(', [Package,Name]),
	numbervars(Args,0,_),
	portray_goal_args(Args),
	format(user_error, ') - ~s}~n', [Message]).

portray_goal_args([Arg]) :-
	!,
	portray_goal_arg(Arg).
portray_goal_args([Arg|Args]) :-
	portray_goal_arg(Arg),
	write(user_error,','),
	portray_goal_args(Args).

portray_goal_arg(Arg) :-
	integer_list(Arg),
	!,
	format(user_error,'"~s"',[Arg]).
portray_goal_arg(Arg) :-			% For now simple write
	write(user_error,Arg).


integer_list([]).
integer_list([H|T]) :-
	integer(H),
	integer_list(T).
****/

% -----------------------------------------------------------------------

tk_terminal(Interp, TextWidget, InStream, OutStream, ErrStream) :-
	absolute_file_name(library('tkterm.tcl'),TkTermSrc),
	'$tk_term'(Interp, TkTermSrc, TextWidget, InCode, OutCode, ErrCode),
        stream_code(InStream, InCode),
        stream_code(OutStream, OutCode),
        stream_code(ErrStream, ErrCode).


% -----------------------------------------------------------------------

:- dynamic foreign/2, foreign_resource/2.

foreign(tcl_new, tcl_new(-term)).
foreign(tcl_delete_interp, tcl_delete(+term)).
foreign(tcl_eval, tcl_eval(+term,+term,-term)).
foreign(tcl_event, tcl_event(+term,+term,-term)).
foreign(tcl_add_result, '$tcl_add_result'(+term,+term,+term)).

foreign(tk_new, '$tk_new'(+term,+string,+string,+integer,+integer)).
foreign(tk_destroy_window, tk_destroy_window(+term)).
foreign(tk_make_window_exist, tk_make_window_exist(+term)).
foreign(tk_main_window, tk_main_window(+term,-term)).
foreign(tk_do_one_event1, '$tk_do_one_event'(+integer,[-integer])).
foreign(tk_do_one_event3, '$tk_do_one_event'(+term,+integer,-term)).
foreign(tk_num_main_windows, tk_num_main_windows(-term)).
foreign(tk_term, '$tk_term'(+term,+string,+chars,-address('SP_stream'),-address('SP_stream'),-address('SP_stream'))).

foreign_resource(tcltk, [
	init(tk_initialize),
	deinit(tk_deinitialize),
	tcl_new,
	tcl_delete_interp,
	tcl_eval,
	tcl_event,
	tcl_add_result,
	tk_new,
	tk_destroy_window,
	tk_make_window_exist,
	tk_main_window,
	tk_do_one_event1,
	tk_do_one_event3,
	tk_num_main_windows,
	tk_term
			    ]).	

:- load_foreign_resource(library(system(tcltk))).
