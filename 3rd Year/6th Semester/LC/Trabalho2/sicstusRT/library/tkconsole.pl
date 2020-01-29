/* tkconsole.pl; a simple console window written in Tcl/Tk. */
%% CVS: $Id: tkconsole.pl,v 1.1 1999/09/03 14:00:37 jojo Exp $

:- module(tkconsole,[tk_console/0,tk_make_dump/1]).

:- use_module(library(tcltk)).

tk_console :-
	tk_new([name('SICStus Prolog')], Interp),
	absolute_file_name(library('tkconsole.tcl'),TkConSrc),
	tcl_eval(Interp, [chars("source"),write(TkConSrc)], _),
	tcl_eval(Interp, chars("tk_console_window"), TextWidget),
	tk_terminal(Interp, TextWidget, InStream, OutStream, ErrStream),
	prolog_flag(user_input, _, InStream),
	prolog_flag(user_output, _, OutStream),
	prolog_flag(user_error, _, ErrStream),
	set_input(InStream),
	set_output(OutStream),
	prolog_flag(version,V,V),
	write(V),nl,
	format('TkConsole beta, version $Revision: 1.1 $~n',[]).
	

tk_make_dump(Name) :-
	save_program(Name,tk_console).
