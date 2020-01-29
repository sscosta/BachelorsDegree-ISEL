:- use_module(wcx_box).

copy_file(Fin, Ein, Fout, Eout, Trace) :-
	my_open(Fin, read, Sin, Ein),
	my_open(Fout, write, Sout, Eout),
	repeat,
	get0(Sin, C),
	(   C < 0 -> true
	;   put(Sout, C), write_code(Trace, C), fail
	),
	!,
	my_close(Sin),
	my_close(Sout).

write_code(trace, C) :-
	!,
	(   C =< 32 -> put(user_error, 0'.)
	;   C > 255 -> put(user_error, 0'^)
	;   put(user_error, C)
	), 
	tab(user_error, 1),
	format(user_error, '0x~16r', C),
	tab(user_error, 1),
	(   C = 10 -> nl(user_error)
	;   true
	).
write_code(_Trace, _).

my_open(user, read, S, Enc) :-
	!, S = user_input,
	set_encoding(S, Enc).
my_open(user, write, S, Enc) :-
	!, S = user_output,
	set_encoding(S, Enc).
my_open(File, Mode, S, Enc) :-
	open(File, Mode, S, [wcx(Enc)]).

my_close(user_input) :-
	!, set_encoding(user_input, []).
my_close(user_output) :-
	!, set_encoding(user_output, []).
my_close(S) :-
	close(S).

show_file(File, FileEnc) :-
	show_file(File, FileEnc, []).

show_file(File, FileEnc, ConsEnc) :-
	copy_file(File, FileEnc, user, ConsEnc, trace).

enter_file(File, FileEnc) :-
	enter_file(File, FileEnc, []).

enter_file(File, FileEnc, ConsEnc) :-
	copy_file(user, ConsEnc, File, FileEnc, trace).
	
