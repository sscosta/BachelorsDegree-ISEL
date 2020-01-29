/* Copyright (C) 1996, Swedish Institute of Computer Science. */

:- module(genflink, [
	prepare_foreign_resource/3,
	prepare_resource_table/2,
	generate_flinkage/1		% for backwards compatibility
		    ]).

% The module provides services for creating glue code files for
% foreign resources. It uses goal expansions to intercept calls to
% load_foreign_files/2.

:- use_module(library(system), [delete_file/2]).

:- multifile
	user:goal_expansion/3.

:- dynamic
	user:goal_expansion/3,
	generating_flinkage/1,
	prepared_resource/1.


prepare_foreign_resource(Resource, SourceFile, GlueFile) :-
	prolog:prepare_foreign_resource(Resource, SourceFile, GlueFile).

prepare_resource_table(Resources, CFile) :-
	prolog:prepare_resource_table(Resources, CFile).
	

user:goal_expansion(load_foreign_files(Files0,_), _, ExpGoal) :-
	generating_flinkage(GlueFile), !,
	prolog:get_module(Files0, Files, Module),
	ExpGoal = true,
	(   Files = [File|_] -> true
	;   Files = File
	),
	prolog:make_names(File, Resource, _),
	prepare_compatible_resource(Resource, Files, Module, GlueFile),
	assert(prepared_resource(Resource)).

prepare_compatible_resource(Resource, Files, Module, GlueFile) :-
	Goal = prepare_compatible_resource(Resource,Files,Module,GlueFile),
	prolog:collect_functions(Files, Funcs, Decls, Module, InitFun, DeInitFun, Goal),
	prolog:encode_decls(Decls, Codes, Goal),
	prolog:'$prepare_foreign_resource'(Resource, GlueFile, 1, Funcs, Codes, InitFun, DeInitFun).

generate_flinkage(Files) :-
	absolute_file_name('flinkage.c', GlueFile),
	delete_file(GlueFile, [ignore]),
	assert(generating_flinkage(GlueFile)),
	use_module(Files),
	retractall(generating_flinkage(_)),
	findall(Resource, prepared_resource(Resource), Resources),
	retractall(prepared_resource(_)),
	prolog:'$prepare_resource_table'(Resources, GlueFile, 1).
