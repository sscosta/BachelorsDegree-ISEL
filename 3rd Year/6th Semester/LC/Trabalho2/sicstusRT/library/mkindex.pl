/* Copyright (C) 1995-96, Swedish Institute of Computer Science. */

:- module(make_index, []).

% make_library_index(Dir)
% Make an INDEX.pl in the directory Dir

:- use_module(library(system)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

%--------------------------------------------------------------------------

% Only *.pl files in the top directory given are considered. For
% backwards compatibility however subdirectories stated here are
% considered also.

go_down(src).
go_down(linda).
go_down(chr).

%--------------------------------------------------------------------------
make_library_index(Dir) :-
	file_exists(Dir, [read,write,search]),
	working_directory(OldDir, Dir),
	call_cleanup(make_library_index, working_directory(_, OldDir)).

make_library_index :-
	pl_files('.', PlFiles, []),
	open('INDEX.pl', write, IndexStream),
	make_library_index2(PlFiles, IndexStream),
	close(IndexStream).

%--------------------------------------------------------------------------
pl_files(Dir, PlFiles, PlFiles0) :-
	file_exists(Dir, [read,search]), !,
	directory_files(Dir, Files),
	pl_files1(Files, Dir, PlFiles, PlFiles0).

% Note! RelFile means the file with path relative to the library directory

%% [MC] 3.8.6: made determinate
pl_files1([], _, PlFiles, PlFiles).
pl_files1([File|Files], Dir, PlFiles, PlFiles0) :-
	concat(Dir, File, RelFile),
	file_property(RelFile, type(Type)),	
	(   Type=regular ->
	    (   file_exists(RelFile, [read]),
		is_pl_file(File)
	    ->  PlFiles = [f(RelFile,Dir,File)|PlFiles1]
	    ;   PlFiles = PlFiles1
	    )
	;   Type=directory ->
	    (   go_down(File),
		pl_files(RelFile, PlFiles, PlFiles1) -> true
	    ;   PlFiles = PlFiles1
	    )
	;   PlFiles = PlFiles1
	),
	pl_files1(Files, Dir, PlFiles1, PlFiles0).

concat('.', A2, A3) :- !, A2=A3.
concat(A1, A2, A3) :-
	atom_to_chars(A1, Ch, [0'/|Ch0]),
	atom_to_chars(A2, Ch0, []),
	name(A3, Ch).

is_pl_file('genmakefile.pl') :- !, % genmakefile.pl is a Perl script
	fail.
is_pl_file(File) :-
	name(File, Ch),
	suffix(".pl", Ch).

%--------------------------------------------------------------------------
make_library_index2([], _).
make_library_index2([f(RelFile,Dir,File)|Files], IndexStream) :-
	prolog_flag(typein_module, Module),
 	scan_module_decl(RelFile, File, Dir, Module, IndexStream),
        make_library_index2(Files, IndexStream).

scan_module_decl(RelFile, File, Dir, Module0, IndexStream) :-
	open(RelFile, read, Stream),
	absolute_file_name(Dir, AbsDir),
 	asserta(prolog:'$load context'(Module0,File,Stream,AbsDir), Ref),
 	on_exception(Excp,
	             read(Stream, First),
		     (   format(user_error,
			        'Warning: While reading ~w:~n', [RelFile]),
			 print_message(error, Excp)
		     )
		    ),
	erase(Ref),
	close(Stream),
	(   First = (:- module(Module, Public)) ->
	    add_library_index(Public, RelFile, Module, IndexStream)
	;   true
	).

add_library_index([], _, _, _).
add_library_index([Name/Arity|T], RelFile, Module, IndexStream) :-
	format(IndexStream, 'index(~q, ~d, ~q, ~q).~n',
	       [Name, Arity, Module, RelFile]), !,
	add_library_index(T, RelFile, Module, IndexStream).
