%%% -*- Mode: Prolog; Module: SU_messages; -*-
/* Copyright (C) 1995, Swedish Institute of Computer Science. */


:- module('SU_messages', [/*generate_message/3,
			    query_class/5,
			    query_input/3,
			    query_map/4*/]).


%------------------------------------------------------------------------
%------------------------ Message printing ------------------------------
%------------------------------------------------------------------------

:- multifile generate_message/3.
:- dynamic   generate_message/3.
generate_message(Message) --> error_msg(Message), !.
generate_message(Message) --> other_msg(Message), !.


error_msg(representation_error(_,_,bound(What,Max))) -->
	['Representation error'-[],nl,
	 'compiled representation of clause has more than ~d ~w variables'-
		   [Max,What],nl].
error_msg(system_error(Message)) --> 
	['System error'-[],nl,
	 '~q'-[Message],nl].
error_msg(instantiation_error(Goal,ArgNo)) -->
	['Instantiation error'-[]],
	head(Goal, ArgNo),
	goal(Goal).
error_msg(type_error(Goal,ArgNo,TypeName,Culprit)) -->
	['Type error'-[]],
	head(Goal, ArgNo),
	type(TypeName, Culprit),
	goal(Goal).
error_msg(domain_error(Goal,ArgNo,DomainName,Culprit)) -->
	['Domain error'-[]],
	head(Goal, ArgNo),
	domain_error_msg(DomainName, Culprit),
	goal(Goal).
error_msg(representation_error(Goal,ArgNo,ErrorType)) -->
	['Representation error'-[]],
	head(Goal, ArgNo),
	message(ErrorType),
	goal(Goal).
error_msg(existence_error(Goal,ArgNo,ObjType,Culprit,Message)) -->
	['Existence error'-[]],
	(   {Message == past_end_of_stream} ->
	    head(Goal, 0),
	    ['attempt to read past end of stream'-[],nl]
	;   head(Goal, ArgNo),
	    typename(ObjType),
	    [' '-[], write_term(Culprit),' does not exist'-[],nl]
	),
	goal(Goal).
error_msg(consistency_error(Goal,Culprit1,Culprit2,Message)) -->
	['Consistency error: '-[],
	 write_term(Culprit1),
	 ' is inconsistent with '-[],
	 write_term(Culprit2),nl],
	message(Message),
	goal(Goal).
error_msg(context_error(Goal,not(file_load),CommandType)) -->
	['Context error: '-[]],
	commandtype(CommandType),
	[' did not appear during load of file(s)'-[],nl],
	goal(Goal).
error_msg(context_error(Goal,ContextType,CommandType)) -->
	['Context error: '-[]],
	commandtype(CommandType),
	[' appeared in '-[]],
	contexttype(ContextType), [nl],
	goal(Goal).
error_msg(resource_error(Goal,Resource)) -->
	['Resource error: insufficient '-[]],
	resource(Resource), [nl],
	goal(Goal).
error_msg(permission_error(Goal,Operation,ObjType,Culprit,Message)) -->
	['Permission error: cannot '-[]],
	operation(Operation), [' '-[]],
	typename(ObjType),
	[' '-[],write_term(Culprit),nl],
	message(Message),
	goal(Goal).
error_msg(syntax_error(Goal,Line,Msg,Tokens,AfterError)) -->
	['Syntax error'-[]],
	head(Goal, 0),
	message(Msg),
	['in line ~d'-[Line],nl],
	(   {Tokens = []} -> {true}
	;   {length(Tokens, Length),
	     BeforeError is Length-AfterError},
	    tokens_to_format_commands(BeforeError, Tokens, Tokens1), [nl],
	    ['<<here>>'-[], nl],
	    tokens_to_format_commands(Tokens1), [nl]
	).
error_msg(evaluation_error(Goal,ArgNo,ErrorType,Culprit)) -->
	['Evaluation error'-[]],
	head(Goal, ArgNo),
	errortype(ErrorType),
	[' at '-[],write_term(Culprit),nl].

domain_error_msg(DomainName, Culprit) -->
	domain_error_msg(DomainName),
	[', found '-[],
	 write_term(Culprit),
	 nl].

domain_error_msg(>(I)) --> !, ['expected an integer greater than ~q'-[I]].
domain_error_msg(>=(I)) --> !, ['expected an integer not less than ~q'-[I]].
domain_error_msg(<(I)) --> !, ['expected an integer less than ~q'-[I]].
domain_error_msg(=<(I)) --> !, ['expected an integer not greater than ~q'-[I]].
domain_error_msg(=:=(I)) --> !, ['expected an integer equal to ~q'-[I]].
domain_error_msg(=\=(I)) --> !, ['expected an integer not equal to ~q'-[I]].
domain_error_msg(between(I,J)) --> {integer(I)}, !,
	['expected an integer in ~q'-[I-J]].
domain_error_msg(between(I,J)) --> !, ['expected a float in ~q'-[I-J]].
domain_error_msg(abs_ge(I)) --> !,
	['expected a float with absolute value not less than ~q'-[I]].
domain_error_msg(ge(I)) --> !, ['expected a float not less than ~q'-[I]].
domain_error_msg(gt(I)) --> !, ['expected a float greater than ~q'-[I]].
domain_error_msg(none_of(L)) --> {atom_list(L)}, !,
	['expected an atom not in ~q'-[L]].
domain_error_msg(one_of(L)) --> {atom_list(L)}, !,
	['expected an atom in ~q'-[L]].
domain_error_msg(one_of(L)) --> !, ['expected a term in ~q'-[L]].
domain_error_msg(D) --> ['expected ~q'-[D]].

atom_list([]).
atom_list([A|As]) :-
	atom(A),
	atom_list(As).

tokens_to_format_commands([]) --> [].
tokens_to_format_commands([Head|Tail]) -->
	token_to_format_commands(Head),
	[' '-[]],
	tokens_to_format_commands(Tail).

tokens_to_format_commands(0, S, S) --> !.
tokens_to_format_commands(I, [Head|Tail], S) -->
	token_to_format_commands(Head),
	[' '-[]],
	{J is I-1},
	tokens_to_format_commands(J, Tail, S).

token_to_format_commands(atom(X)) --> !, ['~q'-[X]].
token_to_format_commands(var(_,X,_)) --> !, ['~s'-[X]].
token_to_format_commands(number(X)) --> !, ['~w'-[X]].
token_to_format_commands(string(X)) --> !, {double_quotes(X, Y)}, ['"~s"'-[Y]].
token_to_format_commands(X-_) --> !, token_to_format_commands(X).
token_to_format_commands(Err:X) --> !, ['<~w:~w>'-[Err,X]].
token_to_format_commands(X) --> ['~w'-[X]].

double_quotes([], []).
double_quotes([0'"|Xs], [0'",0'"|Ys]) :- !,  %" 
	double_quotes(Xs, Ys).
double_quotes([X|Xs], [X|Ys]) :-
  	double_quotes(Xs, Ys).


head(Goal, ArgNo) -->
	head(Goal, user, ArgNo).

head(Goal, _, _) --> {var(Goal)}, !, [nl].
head((:-Directive), Module, ArgNo) --> !,
        head(Directive, Module, ArgNo).
head(Module:Goal, _, ArgNo) --> {atom(Module)}, !,
	head(Goal, Module, ArgNo).
head(Goal, Module, ArgNo) --> [X,nl],
	{goal_functor(Goal, Name, Arity)},
	{   predicate_property(Goal,built_in)
	->  Spec = Name/Arity
	;   Spec = Module:Name/Arity
	},
	{   integer(ArgNo), 0 < ArgNo, ArgNo =< Arity
	->  X = ' in argument ~d of ~q'-[ArgNo,Spec]
	;   X = ' in ~q'-[Spec]
	}.

goal_functor((_,Goal), Name, Arity) :- !,
	goal_functor(Goal, Name, Arity).
goal_functor(Goal, Name, Arity) :-
	functor(Goal, Name, Arity).

type(TypeName, Culprit) -->
	typename(TypeName),
	[' expected, but '-[],
	 write_term(Culprit),
	 ' found'-[],nl].

typename(TypeName) --> ['~q'-[TypeName]].

message(0) --> !.
message('') --> !.
message(Message) -->
	['~w'-[Message],nl].

goal(0) --> !.
goal(Goal) --> ['goal:  '-[],write_term(Goal),nl].

commandtype(CommandType) -->
	['~q'-[CommandType]].

contexttype(ContextType) -->
	['~q'-[ContextType]].

resource(0) --> !.
resource(Resource) -->
	['~q'-[Resource]].

operation(0) --> !.
operation(Operation) -->
	['~a'-[Operation]].

errortype(ErrorType) -->
	['~w'-[ErrorType]].

other_msg(statistics(
		   .(MemUsed,MemFree,Gused,Gfree,Lused,Lfree,Tused,Tfree,Cused,Cfree,
	             SStime,SSglob,SSloc,SScon,
		     GCtime,GCcount,GCacc,
		     AGCtime,AGCcount,AGCacc,
		     Run,Wall))) -->
	{Total is MemUsed+MemFree+Gused+Gfree+Lused+Lfree+Tused+Tfree+Cused+Cfree},
	['memory (total) ~t~d~28| bytes'-[Total],nl,
	 '   program space ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	        [MemUsed+MemFree,MemUsed,MemFree],nl,
	 '   global stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	        [Gused+Gfree,Gused,Gfree],nl,
	 '   local stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	        [Lused+Lfree,Lused,Lfree],nl,
	 '   trail stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	        [Tused+Tfree,Tused,Tfree],nl,
	 '   control stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	        [Cused+Cfree,Cused,Cfree],nl,
	 nl,
	 '~t~3f~12| sec. for ~d global, ~d local, and ~d control space overflows'-
	        [SStime/1000,SSglob,SSloc,SScon],nl,
	 '~t~3f~12| sec. for ~d garbage collections which collected ~d bytes'-
	        [GCtime/1000,GCcount,GCacc],nl,
	 '~t~3f~12| sec. for ~d atom garbage collections which collected ~d bytes'-
	        [AGCtime/1000,AGCcount,AGCacc],nl,
	 '~t~3f~12| sec. runtime'-[Run/1000],nl,
	 '~t~3f~12| sec. elapsed time'-[Wall/1000],nl,nl].
other_msg(help) -->
	['This is the default help message.'-[],nl,
	 'You can design your own by defining user:user_help/0.'-[],nl].
other_msg(generated(File,Time)) -->
	['~w generated, ~d msec'-[File,Time],nl].
other_msg(import(Pred,To,From,What)) -->
	['predicate ~q imported by ~w from ~w is ~a'-[Pred,To,From,What],nl].
other_msg(no_match(Doing)) -->
	['~q - no matching predicate'-[Doing],nl].
other_msg(not_module(M)) -->
	['~w is not a current module'-[M],nl].
other_msg(make_library_index(Dir)) -->
	['cannot make index file for ~w'-[Dir],nl].
other_msg(close(Stream)) -->
	['~q was closed while loading'-[Stream],nl].
other_msg(ignore_clause(Type,Pred)) -->
	['ignoring ~a clauses of ~q'-[Type,Pred],nl].
other_msg(load_context(LoadM,CompileM)) -->
	['This file, loaded into module ~w,'-[LoadM],nl,
	 '          is compiled in context of module ~w'-[CompileM],nl].
other_msg(reload(File,OldM,NewM)) -->
	['~w is previously loaded into module ~w'-[File,OldM],nl,
	 '          now it is reloaded into module ~w'-[NewM],nl].
other_msg(singletons(Vas,Pred)) -->
	['~q - singleton variables in ~q'-[Vas,Pred],nl].
other_msg(clauses_not_together(Pred)) -->
	['clauses for ~q are not together'-[Pred],nl].
other_msg(redefined(Type,Mod,[],File)) -->
	['The ~w ~q is being redefined from multifile to non-multifile.'-[Type,Mod],nl,
	 '    New file: ~w'-[File],nl,
	 'Do you really want to redefine it? '-[],nl].
other_msg(redefined(Type,Mod,OldFile,File)) -->
	['The ~w ~q is being redefined.'-[Type,Mod],nl,
	 '    Old file: ~w'-[OldFile],nl,
	 '    New file: ~w'-[File],nl,
	'Do you really want to redefine it? '-[],nl].
other_msg(redefine_procedure_help) -->
	['       y    redefine this procedure'-[],nl,
	 '       n    don\'t redefine this procedure'-[],nl,
	 '       p    redefine this procedure and don\'t ask again'-[],nl,
	 '       s    don\'t redefine this procedure and don\'t ask again'-[],nl,
	 '       a    abort'-[],nl,
	 '       b    break'-[],nl,
	 '       ?    print this information'-[],nl,nl].
other_msg(already_defined(Pred,ThisMod)) -->
	['NAME CLASH: ~w is already defined in module ~w'-
	       [Pred,ThisMod],nl].
other_msg(already_imported(Pred,ThisMod,OldMod)) -->
	['NAME CLASH: ~w is already imported into module ~w from module ~w'-
	       [Pred,ThisMod,OldMod],nl].
other_msg(override(NewMod)) -->
	['Do you really want to override this definition with the one in ~w?'-
	       [NewMod],nl,nl].
other_msg(abort(_)) -->
	['Execution aborted'-[],nl].
other_msg(halt) --> []. % can be intercepted
other_msg(break(I)) -->
	['Break level ~d'-[I],nl].
other_msg(break) -->
	['End break'-[],nl].
other_msg(prompt(G,B,M,S,A)) -->
	{prepend_item(M, user, M, [], P3),
	 prepend_item(B, 0, B, P3, P2),
	 prepend_item(S, off, source_info, P2, P1),
	 prepend_item(A, off, advice, P1, P0),
	 prepend_item(G, off, G, P0, P)},
	(   {P = []} -> []
	;   ['~q'-[P],nl]
	).
other_msg(malformed_clause(Clause)) -->
	['~q - illegal clause'-[Clause],nl].
other_msg(declaration(Decl,What)) -->
	['~q - declaration ~a'-[Decl,What],nl].
other_msg(loading(_Present,user)) --> [].
other_msg(loading(Depth,Present,AbsoluteFileName)) -->
	['~*c~a ~w...'-[Depth,0' ,Present,AbsoluteFileName],nl].
other_msg(loaded(Depth,Past,AbsoluteFileName,'$none',Msec,Bytes)) -->
 	['~*c~w ~a, ~d msec ~d bytes'-[Depth,0' ,AbsoluteFileName,Past,Msec,Bytes],nl].
other_msg(loaded(Depth,Past,AbsoluteFileName,Module,Msec,Bytes)) -->
	['~*c~w ~a in module ~w, ~d msec ~d bytes'-
	       [Depth,0' ,Past,AbsoluteFileName,Module,Msec,Bytes],nl].
other_msg(imported(Depth,Exporter,Importer)) -->
	['~*cmodule ~w imported into ~w'-[Depth,0' ,Exporter,Importer],nl].
other_msg(foreign_resource(Depth,Past,Resource,Importer)) -->
	['~*c~w foreign resource ~w in module ~w'-[Depth,0' ,Past,Resource,Importer],nl].
other_msg(created(AbsoluteFileName,T)) -->
	['~w created in ~d msec'-[AbsoluteFileName,T],nl].
other_msg(not_created(AbsoluteFileName)) -->
	['~w NOT created'-[AbsoluteFileName],nl].
other_msg(restored(AbsoluteFileName,T,S)) -->
	['~w restored in ~d msec ~d bytes'-[AbsoluteFileName,T,S],nl].
other_msg(not_loaded(Pred,What)) -->
	['~q - NOT ~a'-[Pred,What],nl].
other_msg(failed(Goal)) -->
	['~q - goal failed'-[Goal],nl].
other_msg(wrong_option) --> % not used
	['Option not applicable at this port'-[],nl].
other_msg(wrong_inv_no(Command)) -->
	['~q - wrong invocation number'-[Command],nl].
other_msg(wrong_command(Command, Port)) -->
	['~q - wrong command at ~q port'-[Command,Port],nl].
other_msg(breakpoints([])) -->
	['There are no breakpoints'-[],nl].
other_msg(breakpoints([X|Xs])) -->
	['Breakpoints:'-[],nl],
	list_breakpoints([X|Xs]).
other_msg(breakp(no,BreakPointText,Ref)) -->
	['There is no ~a ~q'-[BreakPointText,Ref],nl].
other_msg(breakp(bp(_Type,plain(MFunc),BID),add,already)) -->
	['There is already a plain spypoint on ~w, (BID=~d)'-[MFunc,BID],nl].
other_msg(breakp(bp(Type,WFunc,BID),add,ok)) --> 
	{breakpoint_type_text(Type, TypeTxt)},
	breakpoint_functor_text(WFunc, TypeTxt),
	[' added, BID=~d'-[BID],nl].
other_msg(breakp(bp(Type,WFunc,BID),What,How)) -->
	{breakpoint_type_text(Type, TypeTxt)},
	breakpoint_functor_text(WFunc, TypeTxt),
	{breakpoint_already_last_texts(How, Already, Last)},
	[', BID=~d, ~a~ad~a'-[BID,Already,What,Last],nl].
other_msg(breakp(all,Type,What)) --> 
	{breakpoint_type_text(Type, TypeTxt)},
	['All ~as ~ad'-[TypeTxt,What],nl].
other_msg(breakp(bp(_Type,WFunc,_BID),compiled_inline)) -->
	{arg(1, WFunc, MFunc)},
	['Predicate ~q compiled inline, breakable only in interpreted code'-
	       [MFunc],nl].
other_msg(trace(Goal)) -->
	write_item(Goal), [nl].
other_msg(trace_help) -->
	['Please enter a valid trace command (''h'' for help).'-[],nl].
other_msg(trace_command) -->
	['Invalid trace command'-[],nl].
other_msg(trace_command(TC)) -->
	['Incorrect trace command ~a'-[TC],nl].
other_msg(ancestors([])) -->
	['There are no ancestors'-[],nl].
other_msg(ancestors([X|Xs])) -->
	['Ancestors:'-[],nl],
	list_items([X|Xs]).
other_msg(backtrace([])) -->
	['There is no backtrace'-[],nl].
other_msg(backtrace([X|Xs])) -->
	['Backtrace:'-[],nl],
	list_items([X|Xs]).
other_msg(bgoal(G)) -->
	['Blocked goal:'-[],nl],
	list_items([bgoal(G)]).
other_msg(blocked([])) -->
	['There are no blocked goals'-[],nl].
other_msg(blocked([X|Xs])) -->
	['Blocked goals:'-[],nl],
	list_items([X|Xs]).
other_msg(debugging_options) -->
	debugging_options.
other_msg(inst_cond_spy(Module, Functor, Name)) -->
	['Placing spypoint on ~q with conditions: '-[Module:Functor/Name],nl].
other_msg(whereis(file(File),Pred)) -->
	['~q is defined in the file ~w'-[Pred,File],nl].
other_msg(whereis(built_in,Pred)) -->
	['~q is a built-in predicate'-[Pred],nl].
other_msg(whereis(dynamic,Pred)) -->
	['~q has been defined dynamically'-[Pred],nl].
other_msg(whereis(undefined,Pred)) -->
	['~q is undefined'-[Pred],nl].
other_msg(undef(Pred)) -->
	['The predicate ~q is undefined'-[Pred],nl].
other_msg(leash([])) -->
	['No leashing'-[],nl].
other_msg(leash([X|Xs])) -->
	['Using leashing stopping at ~w ports'-[[X|Xs]],nl].
other_msg(unknown(trace)) -->
	['Undefined predicates will trap to the debugger (trace)'-[],nl].
other_msg(unknown(fail)) -->
	['Undefined predicates will just fail (fail)'-[],nl].
other_msg(unknown(error)) -->
	['Undefined predicates will raise an exception (error)'-[],nl].
other_msg(debug(debug)) -->
	['The debugger will first leap -- showing spypoints (debug)'-[],nl].
other_msg(debug(trace)) -->
	['The debugger will first creep -- showing everything (trace)'-[],nl].
other_msg(debug(off)) -->
	['The debugger is switched off'-[],nl].
other_msg(debug(zip)) -->
	['The debugger will first zip -- showing spypoints (zip)'-[],nl].
other_msg(ignored(Term)) -->
	['Ignoring ~w'-[Term],nl].
other_msg(no_license_file) -->
	['License file not found'-[],nl].
other_msg(no_site_fact) -->
	['No site fact in the license file'-[],nl].
other_msg(no_product_fact(Product)) -->
	['You have no license for: ~w'-[Product],nl].
other_msg(bad_license_code(Fact)) -->
	['The code for the license is incorrect: ~q'-[Fact],nl].
other_msg(expired_license(Fact)) -->
	['The license has expired: ~q'-[Fact],nl].
other_msg(version(Version,Site)) -->
	['~w'-[Version],nl,
	 'Licensed to ~w'-[Site],nl].
other_msg(version(Version)) -->
	['~w'-[Version],nl].
other_msg(version_addon(Version)) -->
	['~w'-[Version],nl].
other_msg(blame_on(Ancestor)) -->
	goal(Ancestor).
%% ^C handling
other_msg(interrupt_options) --> [
	nl,
	'Prolog interrupt options:'-[],nl,
	'    a        abort           - cause abort'-[],nl,
	'    b        break           - cause break'-[],nl,
	'    c        continue        - do nothing'-[],nl,
	'    e        exit            - cause exit'-[],nl,
	'    d        debug           - start leaping'-[],nl,
	'    z        zip             - start zipping'-[],nl,
	'    t        trace           - start creeping'-[],nl,
	'    h        help            - get this list'-[],nl,nl].
other_msg(interruption) -->
	[nl,'Prolog interruption (h for help)? '-[],nl].
%% Emacs interface & source positions
other_msg(emacs_command(Command,Args)) -->
	display_emacs_command(Command,Args).
other_msg(source_pos(SrcInfo,Port,Cmd)) -->
	show_source_pos(SrcInfo, Port, Cmd).
other_msg(no_source_pos) -->
	no_source_pos.
%% Top-level
other_msg(solutions([])) -->
	[nl, 'true'-[],nl].
other_msg(solutions([B|Bs])) -->
	[nl],
	list_items_sep([B|Bs], ',').
other_msg(bindings_help) -->
	['Top-level options:'-[], nl,
         '   RET y    no more choices'-[],nl,
	 '     ; n    more choices'-[],nl,
	 '       b    break'-[],nl,
	 '     ? h    print this information'-[],nl,nl].
other_msg(yes) -->
	['yes'-[],nl].
other_msg(no) -->
	[nl,'no'-[],nl].
other_msg(empty) --> [].
other_msg(format(Fmt,Args)) -->
	[Fmt-Args,nl].

show_source_pos(SrcInfo, Port, Cmd) -->
	{prolog_flag(source_info, SrcFlag)},
	show_source_pos(SrcFlag, SrcInfo, Port, Cmd).

no_source_pos -->
	{prolog_flag(source_info, SrcFlag)},
	remove_source_pos(SrcFlag).

remove_source_pos(off) --> [].
remove_source_pos(on) --> [].
remove_source_pos(emacs) -->
	display_emacs_command(u, []).  % unshow

show_source_pos(on, SrcInfo, _, _) --> !,
	print_source_pos(SrcInfo).
show_source_pos(emacs, SrcInfo, Port, ask) --> !,
	emacs_source_pos(SrcInfo, Port).
show_source_pos(_, _, _, _) --> [].

print_source_pos([]) --> [].
print_source_pos(fileref(File,Line)) -->
	['in scope of a goal at line ~d in ~w'-[Line,File],nl].
print_source_pos(clauseref(File,MFunc,Nth,CallNo,_)) -->
	(   {File == []}
	->  ['in scope of body subgoal ~d of clause ~d of ~q'-
	           [CallNo,Nth,MFunc],nl]
	;   ['in scope of body subgoal ~d of clause ~d of ~q in ~w'-
	           [CallNo,Nth,MFunc,File],nl]
	).

emacs_source_pos([], _Port) --> [].
emacs_source_pos(fileref(File, Line), Port) -->
        display_emacs_command(f, [Port, Line, File]). % file_show
emacs_source_pos(clauseref(_,PredRef,_,_,Line), Port) -->
	{Buf = '*Prolog Source*'},
	display_emacs_head(c, [Buf]), [nl],	% create_buffer
	['~@'-[listing(PredRef)]],
	display_emacs_tail, [nl],
	display_emacs_head(b, [Port,Line,Buf]),	% buffer_show
	display_emacs_tail.

display_emacs_command(Command, Args) -->
	display_emacs_head(Command, Args),
	display_emacs_tail.

display_emacs_head(Command, Args) -->
	['$([{~w'-[Command]],
	display_emacs_args(Args).

display_emacs_args([]) --> [].
display_emacs_args([Arg|Args]) -->
	[',~w'-[Arg]],
	display_emacs_args(Args).

display_emacs_tail -->
	[nl,'$)]}'-[],nl].


% When Property \== Default IL is Item prepended to IL0, which is a comma
% separated list of items
prepend_item(Property, Default, _Item, IL0, IL) :-
	Property == Default, !,
	IL = IL0.
prepend_item(_, _, Item, IL0, IL) :-
	(   IL0 = [] -> IL = Item
	;   IL = (Item,IL0)
	).

list_breakpoints([]) --> [].
list_breakpoints([BPoint|BPoints]) -->
	list_breakpoint(BPoint), [nl],
	list_breakpoints(BPoints).

list_breakpoint(BPoint) -->
	{breakpoint_info(BPoint, BID, Mark, St, Func, Conds)},
	['    ~t~d~7+ ~a~a ~q'-[BID,Mark,St,Func]],
	(   {Conds = []} -> []
	;   [' if ~q'-[Conds]]
	).

breakpoint_info(bp(_Type,WFunc,BID,Conds,Status),
		BID, Mark, St, Func, Conds) :-
	functor(WFunc, Kind, _), kind_mark(Kind, Mark),
	status_mark(Status, St),
	numbervars(Conds, 0, _),
	(   WFunc = generic -> Func = generic
	;   arg(1, WFunc, Func)
	).

kind_mark(plain, '+').
kind_mark(conditional, '*').
kind_mark(generic, '#').
kind_mark(none, ' ').

status_mark(off, 'D'). % breakpoint disabled
status_mark(on, ' ').  

breakpoint_type_text(debugger, spypoint).
breakpoint_type_text(advice, 'advice point').
breakpoint_type_text(all, breakpoint).

breakpoint_functor_text(plain(MFunc), Type) -->
	['Plain ~a for ~q'-[Type,MFunc]].
breakpoint_functor_text(conditional(MFunc), Type) -->
	['Conditional ~a for ~q'-[Type,MFunc]].
breakpoint_functor_text(generic, Type) --> ['Generic ~a'-[Type]].

breakpoint_already_last_texts(already, 'already ', '').
breakpoint_already_last_texts(last, '', ' (last)').
breakpoint_already_last_texts(ok, '', '').


%--------- default printing predicates

list_items(L) -->
	list_items_sep(L, '').

list_items_sep([], _) --> [].
list_items_sep([G|Gs], Sep) -->
	write_item(G),
	(   {Gs = []} -> [nl]
	;   sep_line(Sep),
	    list_items_sep(Gs, Sep)
	).

sep_line('') --> !, [nl].
sep_line(Sep) --> ['~a'-Sep, nl].

% First two clauses for messages 'trace', 'ancestors', 'backtrace', 
% 'bgoal', 'blocked'; last two clauses for 'solutions'.
write_item(goal(Method,Kind,Inv,Depth,Port,Sel,SelGoal)) -->
	{kind_mark(Kind, Mark)},
	port_info_text(Port, Pport, Nondet),
	['~N~a~a ~t~w~9| ~t~w~16|~a'-[Nondet,Mark,Inv,Depth,Pport]],
	print_sel_list(Sel),
	write_goal(Method, SelGoal).
write_item(bgoal(I,V,G)) -->
	{prolog_flag(debugger_print_options, Options)},
	['~w (~p): '-[I,V], write_term(G,Options)].
write_item(binding(Var, Val)) -->
	['~s = '-[Var], write_term(Val)].
write_item(constraint(G)) -->
	[write_term(G)].


print_sel_list([]) --> [].
print_sel_list([S|Ss]) -->
	['^~w '-[S]],
	print_sel_list(Ss).

port_info_text(block, ' Block: ', ' ') --> []. 
port_info_text(unblock, ' Unblock: ', ' ') --> [].
port_info_text(call, ' Call: ', ' ') --> [].
port_info_text(exit(Port), Pport, Nondet) -->
	port_info_text(Port, Pport, Nondet).
port_info_text(det, ' Exit: ', ' ') --> [].
port_info_text(nondet, ' Exit: ', '?') --> [].
port_info_text(redo, ' Redo: ', ' ') --> [].
port_info_text(fail, ' Fail: ', ' ') --> [].
port_info_text(exception(_), ' Exception: ', ' ') --> [].
port_info_text(void, ' ', ' ') --> [].

write_goal(display, Goal) --> [write_term(Goal, [ignore_ops(true)])].
write_goal(print, Goal) --> [write_term(Goal, Options)],
	{prolog_flag(debugger_print_options, Options)}.
write_goal(write_term(Options), Goal) --> [write_term(Goal, Options)].
write_goal(write, Goal) --> ['~q'-[Goal]].


%-------------------------help-------------------------------------------
debugging_options --> [
	'Debugging options:'-[], nl,
	'    RET   creep            c      creep'-[],nl,
	'    l     leap             z      zip'-[],nl,
	'    s     skip             s <i>  skip i'-[],nl,
	'    o     out              o <n>  out n'-[],nl,
	'    q     q-skip           q <i>  q-skip i'-[],nl,
	'    r     retry            r <i>  retry i'-[],nl,
	'    f     fail             f <i>  fail i'-[],nl,
	'    j<p>  jump to port     j<p><i>jump to port i'-[],nl,
	'    d     display          w      write'-[],nl,
	'    p     print            p <i>  print partial'-[],nl,
	'    g     ancestors        g <n>  ancestors n'-[],nl,
	'    t     backtrace        t <n>  backtrace n'-[],nl,
	'    &     blocked goals    & <n>  nth blocked goal'-[],nl,
	'    n     nodebug          =      debugging'-[],nl,
	'    +     spy this         *      spy conditionally'-[],nl,
	'    -     nospy this       \\ <i>  remove brkpoint'-[],nl,
	'    D <i> disable brkpoint E <i>  enable brkpoint'-[],nl,
	'    a     abort            b      break'-[],nl,
	'    @     command          u      unify'-[],nl,
	'    e     raise exception  .      find this'-[],nl,
	'    <     reset printdepth < <n>  set printdepth'-[],nl,
	'    ^     reset subterm    ^ <n>  set subterm'-[],nl,
	'    ?     help             h      help'-[],nl,nl].



%------------------------------------------------------------------------
%-------------------------- User input handling -------------------------
%------------------------------------------------------------------------

/*
   There is an exact copy of the following clauses in msgs.pl for
   safety.
*/
	
:- multifile query_class/5.
:- dynamic   query_class/5.
% query_class(+QueryClass, -Prompt, -InputMethod, -MapMethod, -FailureMode)
query_class(QueryClass, Prompt, line, char(Pairs), help_query) :-
	query_abbreviation(QueryClass, Prompt, Pairs), !.
query_class(next_solution, ' ? ', line,
	    char([yes-";nN", no-[-1,0'\n,0'y,0'Y], break-"bB"]), help) :- !.
query_class(debugger, ' ? ', line, debugger, help_query) :- !.
query_class(spypoint_cond, 'conditions: ', T, M, F) :- !, def_params(T, M, F).
query_class(dbg_goal_term, 'Goal term: ',  T, M, F) :- !, def_params(T, M, F).
query_class(dbg_exception_term, 'Exception term: ',
	                                   T, M, F) :- !, def_params(T, M, F).
query_class(dbg_extend_term, '(Show,Command,Mode): ',
	                                   T, M, F) :- !, def_params(T, M, F).
query_class(dbg_command, '| :- ', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs)].
query_class(query, '| ?- ', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs)].
query_class(clause, '| ', (T-Vs/Ss)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs),singletons(Ss)].
query_class(lbp_goal, '', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(error),variable_names(Vs)].

def_params(term([consume_layout(true),syntax_errors(dec10)]), =, help_query).

:- multifile query_abbreviation/3.
:- dynamic   query_abbreviation/3.
% query_abbreviation(+QueryClass, -Prompt, -Pairs)
query_abbreviation(yes_or_no, ' (y or n) ', [yes-[-1,0'y,0'Y], no-"nN"]) :- !.
query_abbreviation(yes_no_proceed, ' (y, n, p, s, a, b, or ?) ',
		   [yes-[-1,0'y,0'Y],
		    no-"nN",
		    proceed-"pP",
		    suppress-"sS",
		    break-"bB",
		    abort-"aA"]) :- !.
query_abbreviation(interrupt, '',
		   [abort-"aA",
		    break-"bB",
		    continue-"cC",
		    exit-[-1,0'e,0'E],
		    debug(debug)-"dD",
		    debug(trace)-"tT",
		    debug(zip)-"zZ",
		    empty-[0'\n]]) :- !.


:- multifile query_input/3.
:- dynamic   query_input/3.
% query_input(+InputMethod, +Prompt, -RawInput)
query_input(line, Prompt, Line) :- !,
	write(user_error, Prompt),
	read_line(user_input, Line).
query_input(term(Opts), Prompt, Term) :- !,
	prompt(OldPrompt, Prompt),
	call_cleanup(read_term(user_input, Term, Opts),
		     prompt(_, OldPrompt)).
query_input(FullInput0^term(Term,Opts), Prompt, FullInput) :- !,
	prompt(OldPrompt, Prompt),
	call_cleanup(read_term(user_input, Term, Opts),
		     prompt(_, OldPrompt)),
	FullInput = FullInput0.


:- multifile query_map/4.
:- dynamic   query_map/4.
% query_map(+MapMethod, +RawInput, -Result, -Answer)
query_map(char(Pairs), Line, Result, Answer) :- !,
	pairs_to_answer(Pairs, Line, Result, Answer).
query_map(debugger, Line, Result, Answer) :- !,
	parse_dbg_command(Line, Result, Answer).
query_map(=, Term0, Result, Term) :- !,
	Term = Term0,
	Result = success.

/*
   The following clauses only have a simplified copy in msgs.pl.
*/

%--- Map abbreviation to answer -----------------------------------------
pairs_to_answer(Pairs, Line, Result, Answer) :-
	first_nonlayout_char(Line, C),
	member(Answer-Abrv, Pairs),
	member(C,Abrv), !,
	Result = success.
pairs_to_answer(_, _, failure, _).

first_nonlayout_char(Line, C) :-
	remove_nonlayout(Line, C, _).

remove_nonlayout([C0|Chars], First, Rest) :-
	(   white_space(C0) -> remove_nonlayout(Chars, First, Rest)
	;   First = C0,
	    Rest = Chars
	).
remove_nonlayout([], 0'\n, []).
remove_nonlayout(end_of_file, -1, []).

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).

%--- Map trace command to answer ----------------------------------------
parse_int_list([Arg|Args]) -->
	parse_int(Arg), !,
	parse_int_list(Args).
parse_int_list([]) --> all_whitespace.

parse_int(Arg) -->
	[C],
        (   {digit(C, D)} ->
	    parse_int(D, Arg)
	;   {C =:= 0'-} ->
	    parse_int(0, Arg1),
	    {Arg is -Arg1}
	;   {white_space(C)} ->
	    parse_int(Arg)
	).

parse_int(Arg0, Arg) -->
	[C],
	{digit(C, D)}, !,
	{Arg1 is Arg0*10 + D},
	parse_int(Arg1, Arg).
parse_int(Arg, Arg) --> [].

all_whitespace -->
	[C], !,
	{white_space(C)},
	all_whitespace.
all_whitespace --> [].

digit(C, D) :-
	C >= 0'0, C =< 0'9,
	D is C - 0'0.

white_space(127) :- !.  % DEL
white_space(C) :- C =< 0' .


% These are the basic_trace_commands in traceui.pl
standard_dbg_command(-1,   _, abort,                 no_arg).
standard_dbg_command(0'a,  _, abort,                 no_arg).
standard_dbg_command(0'c,  _, creep,                 no_arg).
standard_dbg_command(0'\n, _, creep,                 no_arg).
standard_dbg_command(0'l,  _, leap,                  no_arg).
standard_dbg_command(0'z,  _, zip,                   no_arg).
standard_dbg_command(0'n,  _, nodebug,               no_arg).
standard_dbg_command(0'r,  A, retry(A),              pos_arg).
standard_dbg_command(0'f,  A, fail(A),               pos_arg).
standard_dbg_command(0's,  A, skip(A),               pos_arg).
standard_dbg_command(0'q,  A, qskip(A),              pos_arg).
standard_dbg_command(0'd,  _, display,               no_arg).
standard_dbg_command(0'w,  _, write,                 no_arg).
standard_dbg_command(0'\\, A, remove_breakpoint(A),  pos_arg).
standard_dbg_command(0'D,  A, disable_breakpoint(A), pos_arg).
% These are also simple to parse, but more complex to process later
standard_dbg_command(0'\t, _, emacs_lbp,             no_arg). %!!! doesn't work now % [PM] read emacs/console Goal
standard_dbg_command(0'u,  _, unify,                 no_arg). %!!!
standard_dbg_command(0'e,  _, exception,             no_arg). %!!!
standard_dbg_command(0'x,  _, extend,                no_arg). %!!!
standard_dbg_command(0'o,  A, out(A),                int_arg).
standard_dbg_command(0'g,  A, ancestors(A),          pos_arg(-1)).
standard_dbg_command(0't,  A, backtrace(A),          pos_arg(-1)).
standard_dbg_command(0'&,  A, blocked_goals(A),      pos_arg).
standard_dbg_command(0'p,  A, print(A),              nonneg_arg(none)).
standard_dbg_command(0'+,  _, spy,                   no_arg).
standard_dbg_command(0'-,  _, nospy,                 no_arg).
standard_dbg_command(0'*,  _, conditional_spy,       no_arg).
standard_dbg_command(0'E,  A, enable_breakpoint(A),  pos_arg).
standard_dbg_command(0'.,  _, find_this,             no_arg).
standard_dbg_command(0'=,  _, debugging,             no_arg).
standard_dbg_command(0'b,  _, break,                 no_arg).
standard_dbg_command(0'@,  _, command,               no_arg). %!!!
standard_dbg_command(0'<,  A, set_printdepth(A),     nonneg_arg(10)).
standard_dbg_command(0'^,  A, set_subterm(A),        int_list).
standard_dbg_command(0'?,  _, help,                  no_arg).
standard_dbg_command(0'h,  _, help,                  no_arg).


default_or_int_with_cond([], Default, _, Default).
default_or_int_with_cond([Int], _, Cond, Arg) :-
	holds(Cond, Int), !,
	Arg = Int.
	
holds(gt(I0), I) :- I > I0.
holds(ge(I0), I) :- I >= I0.
holds(nocond, _).

type_check(no_arg, [], _).
type_check(int_list, IntL, IntL).
type_check(int_arg, IntL, Arg) :-
	default_or_int_with_cond(IntL, 1, nocond, Arg).
type_check(pos_arg, IntL, Arg) :-
	default_or_int_with_cond(IntL, none, gt(0), Arg).
type_check(pos_arg(Default), IntL, Arg) :-
	default_or_int_with_cond(IntL, Default, gt(0), Arg).
type_check(nonneg_arg(Default), IntL, Arg) :-
	default_or_int_with_cond(IntL, Default, ge(0), Arg).

parse_dbg_command(Line, Result, Answer) :-
	remove_nonlayout(Line, Char, LineEnd),
	parse_dbg_command(Char, LineEnd, Result0, Answer0),
	(   Result0 = failure(Warning)
	->  Answer = unknown([Char|LineEnd],Warning),
	    Result = success
	;   Answer = Answer0,
	    Result = Result0
	).

parse_dbg_command(Char, Line, Result, Answer) :-
	standard_dbg_command(Char, Arg, Answer, Type), !,
	parse_std_args(Line, Type, Arg, Result, Answer).
parse_dbg_command(0'j, Line, Result, Answer) :- !,
	parse_jump_command(Line, Result, Answer).
parse_dbg_command(0':, _Line, success, command) :- !. % emacs support
parse_dbg_command(_, _, failure(trace_command), _).


% parse_std_args(+Line, +Type, -Arg, -Result, +Command)
parse_std_args(Line, Type, Arg, Result, _) :-
	parse_int_list(IntL, Line, []),
	type_check(Type, IntL, Arg), !,
	Result = success.
parse_std_args(_, _, _, Result, Command) :-
	functor(Command, CommandName, _),
	Result = failure(trace_command(CommandName)).

parse_jump_command([Char|Line], Result, Answer) :-
	parse_jump_character(Char, JumpCommand), !,
	parse_std_args(Line, pos_arg, Arg, Result, jump),
	Answer = jump(JumpCommand, Arg).
parse_jump_command(_,failure(trace_command(jump)),_).

parse_jump_character(0'c, jump_retry).
parse_jump_character(0'f, jump_fail).
parse_jump_character(0'e, jump_reexit).
parse_jump_character(0'r, jump_redo).
