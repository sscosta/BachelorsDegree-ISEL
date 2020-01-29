:- module(vbsp, []).

open_query(StreamCode, Goal, Vars) :-
	stream_code(Stream, StreamCode),
	read_term(Stream, Goal, [variable_names(Vars)]).

query_cut_fail(StreamCode) :-
	stream_code(Stream, StreamCode),
	read(Stream, Goal),
	call(user:Goal).

vbsp_write_term(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write(Stream, Term).

vbsp_write_term_quoted(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	writeq(Stream, Term).

vbsp_write_excp(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write_term(Stream, Term, [quoted(true),max_depth(10)]).

set_paths(VBSPPath, AppPath) :-
	assert(user:file_search_path(vbsp,VBSPPath)),
	assert(user:file_search_path(app,AppPath)).
