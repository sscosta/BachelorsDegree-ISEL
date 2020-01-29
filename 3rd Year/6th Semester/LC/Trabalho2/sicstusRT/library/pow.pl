%% POW - Prolog Objects Windows
%% An Object Library for creating Tk GUIs.

:- use_module(library(tcltk)).
:- use_module(library(objects)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

tk_root :: {

super(utility) &

dynamic is_instance/1 &

clean :-
	:: retract(is_instance(O)),
	O :: abolish,
	:fail &
clean &

instance(O) :-
	super <: instance(O),
	:: assert(is_instance(O)) &

[] :-
	:true &

[Mess | Ms] :-
	Mess,
	Ms 
}.

%--------------------------------------------------------------------

tcl :: {

super(tk_root) &

attributes([interp([])]) &

init :-
	:tcl_new(I),
	set(interp(I)) &

init(TclO) :-
	instance(TclO),
	TclO :: init &

eval(C, R) :-
		get(interp(I)),
	:tcl_eval(I, C, R) &

eval(C) :-
	get(interp(I)),
	:tcl_eval(I, C, _R) &

event(C, R) :-
	get(interp(I)),
	:tcl_event(I, C, R) &	
	     
delete :-
	get(interp(I)),
	delete(I),
	clean &

delete([]):- ! &
delete(I) :-
	:tcl_delete(I),
	set(interp([])) 
}.


%--------------------------------------------------------------------

tk :: {

super(tcl) &

init :-
	get(interp(I)), :(I \== []), ! &
init :-
	:tk_new([], I),
	set(interp(I)),
	init_action &

init_action :- 
         tk::eval([ proc, prolog_apply, br([p, args]), 
	             br(
	                 'set a "$p\\(\'$args\'\\)" ; prolog $a '
		       )]) &

tk_get(FX) :-
	:functor(FX, F, 1),
	:arg(1, FX, X), 
	eval([set, F], Y),:name(Y1, Y), X = Y1 &

tk_set(FX) :-
	:functor(FX, F, 1),
	:arg(1, FX, X), 
	eval([set, F, br(write(X))], XS) &

tk_set(Var, Val) :-
	eval([set, write(Var), br(write(Val))], _) &

atom_term(X, F) :-
	:format_to_chars('~w . ', [X], XS),
	:read_from_chars(XS, F) &

main_window(W) :-
	get(interp(I)),
	:tk_main_window(I,W) &

show_window(W) :-
	get(interp(I)),
	:tk_make_window_exist(I,W) &

destroy_window(W) :-
	:tk_destroy_window(W) &


num_main_windows(NW) :-
	:tk_num_main_windows(NW) 
}.


/********************************************************************
widgets:

frame
options:
	-relief		{sunken, flat, groove, flat, ridge}
      -borderwidth
	-width
	-height
	-background	-bd
	-foreground	-fg
---------------------------------------------------------------------
toplevel
options:
	-screen
---------------------------------------------------------------------
label
options:
	-text
	-textvariable
	-bitmap
	-font
---------------------------------------------------------------------
button 
options:
	-command
	
checkbutton
options:
	-variable

radiobutton
options:
	-value
	-variable


*********************************************************************/


widget(Widget) :: {

% super(tk_root) &
super(tk) &

widget &

attributes([tcl('$TclInterp'(0)), path("")])  &

% winfo exists .w
% winfo children .w
% winfo class .w

w_instance(Win, Parent) :-
	init_path(Win, Parent) &

init :-
	get(path(Path)),
	class(Widget),
	tk::eval([Widget, chars(Path)]) &

init(OptionList) :- 
	get(path(Path)),
	class(Widget),
	tk:translate(OptionList, TkOpL),
	tk::eval([Widget, chars(Path)|TkOpL]) &

init_path(Win, Parent) :-
	instance(Win),
	Parent :: get(path(PP)),
	:name(Win, WinS),
	:append(PP, [0'.|WinS], Path),
	Win::set(path(Path)) &

tkInit(Win, Parent) :-
	init_path(Win, Parent),
	Win::get(path(Path)),
	tk::eval([Widget, chars(Path)]) &

tkInit(Win, Parent, OptionList) :-
	init_path(Win, Parent),
	Win::get(path(Path)),
	tk:translate(OptionList, TkOpL),
	tk::eval([Widget, chars(Path)|TkOpL]) &

tk(Command) :-
	self(S),
	S :: get(path(SPath)), :name(SP, SPath),
	:(Command =.. [CN | Args]),
	tk:translate(Args, TkArgs),
	tk :: eval([SP, CN | TkArgs]) &

tk(Command, R) :-
	self(S),
	S :: get(path(SPath)),:name(SP, SPath),
	:(Command =.. [CN | Args]),
	tk:translate(Args, TkArgs),
	tk :: eval([SP, CN | TkArgs], R) 
}.

topwidget :: {

super(widget(topwidget)) &

attributes([path([])]) &

destroy :-
	tk :: eval('destroy . ')
}.

toplevel :: {

super(widget(toplevel)) &

class(toplevel) &

tkInit(Win) :-
	init_path(Win, topwidget),
	Win::get(path(Path)),
	tk::eval([toplevel,chars(Path)])  &

tkInit(Win, OptionList) :-
	init_path(Win, topwidget),
	Win::get(path(Path)),
	tk_option_list(OptionList, TkList),
	tk::eval([toplevel,chars(Path)|TkList]) 
}.

canvas :: {

super(widget(canvas)) &

class(canvas)

}.	

label :: {

super(widget(label)) &

class(label)
}.

frame :: {

super(widget(frame)) &

class(frame)
}.

button :: {

super(widget(button)) &

class(button)
}.

checkbutton :: {

super(widget(checkbutton)) &

class(checkbutton)

}.

radiobutton :: {

super(widget(radiobutton)) &

class(radiobutton)
}.

message :: {

super(widget(message)) &

class(message)
}.

%%%%

text :: {

super(widget(text)) &

class(text)
}.

%%%%

listbox :: {

super(widget(listbox)) &

class(listbox)
}.

scrollbar :: {

super(widget(scrollbar)) &

class(scrollbar)
}.

scale :: {

super(widget(scale)) &

class(scale)
}.

menu :: {

super(widget(menu)) &

class(menu)
}.

menubutton :: {

super(widget(menubutton)) &

class(menubutton)
}.


entry :: {
super(widget(entry)) &

class(entry)
}.


%--------------------------------------------------------------------

tk(Command) :-
	Command =.. [CN | Args],
	tk:translate(Args, TkArgs),
	tk :: eval([CN | TkArgs]).
	

tk(Command, Value) :-
	Command =.. [CN | Args],
	tk:translate(Args, TkArgs),
	tk :: eval([CN | TkArgs], Value).
	
sleep(T, G, R) :-
	tk :: eval([after, T, br( [prolog, write(G)])], R).

sleep(T, G) :-
	tk :: eval([after, T, br( [prolog, write(G)])], _R).

repeat(0, _T, _G) :-
	!.
repeat(N, T, G) :-
	N1 is N-1,
	sleep(T, (G, repeat(N1, T, G))).

forall(G0, G1) :-
	\+ (G0, \+ G1).

tk_do :-
	tk_do_one_event,
	!,
	tk_do.
tk_do.


%--------------------------------------------------------------------

tk:tk_option(i(OptionN), OptionV, TkOptionN, TkOptionV) :-
	!, 
	TkOptionN = OptionN,
	tk:translate_item(OptionV,[TkOptionV],[]).
tk:tk_option(OptionN, OptionV, TkOptionN, TkOptionV) :-
%	TkOptionN = format('-~w',[OptionN]),
	TkOptionN = min(OptionN),
	tk:translate_item(OptionV,[TkOptionV],[]).

tk:translate([],[]).
tk:translate([Item | L], TkL) :-
	tk:translate_item(Item, TkL, TkL1),
	tk:translate(L, TkL1).

tk:translate_item(I, _L0, _L1) :-
	var(I), !,
	raise_exception(tk).

tk:translate_item(ON:OV , L0, L1) :-
	!,
	L0 = [TkON, TkOV | L1],
	tk:tk_option(ON, OV, TkON, TkOV).
tk:translate_item(prolog(I) , L0, L1) :-
	!,
	L0 = [br([prolog, write(I)])| L1].
tk:translate_item( { I }, L0, L1) :-
	!,
	L0 = [br([prolog, write(I)])| L1].
tk:translate_item(w(topwidget), L0, L1) :-
	!,
	L0 = ['.'|L1].
tk:translate_item(w(I), L0, L1) :-
	!,
	L0  = [chars(IP) | L1],
	I :: get(path(IP)).
tk:translate_item(i(I) , L0, L1) :-
	!,
	L0 = [I | L1].
tk:translate_item(n(I) , L0, L1) :-
	!,
	L0 = [I | L1].
tk:translate_item(s(I) , L0, L1) :-
	!,
	S = dq(chars(I)),
	L0 = [S | L1].
tk:translate_item(I, L0, L1) :-
	tk_root :: is_instance(I), !,
	L0  = [IPN | L1],
	I :: get(path(IP)), name(IPN, IP).
tk:translate_item(I , L0, L1) :-
%	atomic(I),
	L0 = [br(I) | L1].



tk:tag({}).
tk:tag(prolog).
tk:tag(w).
tk:tag(i).
tk:tag(n).
tk:tag(s).


tk:stringToName(S, N) :-
	name(N, S).

tk:stringToNumber(S, N) :-
	name(N, S).

tk:stringToNameList("",[]).
tk:stringToNameList([C | CL0], NL) :-
	CL = [C | CL0],
	tk:get_name(CL, CL1, Name, F),
	(F = false ->
	 tk:stringToNameList(CL1, NL)
	;
	    NL = [Name | NL1],
	    tk:stringToNameList(CL1, NL1)
	).

tk:get_name(CL0, CL, Name, F) :-
	tk:skip_blanks(CL0, CL1),
	( CL1 = [] ->
	  F = false,
	  CL = CL1,
	  Name = ''
	;
	    F = true,
	    tk:get_name1(CL1, CL, NameL),
	    name(Name, NameL)
	).

tk:get_name1([], [], []).
tk:get_name1([C | CL0], CL, NameL) :-
	( C =< 32 ->
	  CL = CL0,
	  NameL = []
	;
	    NameL = [C | NameL1],
	    tk:get_name1(CL0, CL, NameL1)
	).

tk:skip_blanks([X | L0], L) :-
       X =< 32,
	!,
	tk:skip_blanks(L0, L).
tk:skip_blanks(L, L).


