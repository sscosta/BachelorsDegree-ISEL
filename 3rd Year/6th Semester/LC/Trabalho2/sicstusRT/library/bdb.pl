/* Copyright (C) 1999, Swedish Institute of Computer Science. */

%   File       : bdb.pl
%   Author     : Tamas Benko
%   Updated    : 29 August 2000
%   Purpose    : An interface to Berkeley DB

:- module(bdb, [db_open_env/2, db_open_env/3,
		  db_close_env/1, db_current_env/2,
		  db_open/4, db_open/5, 
		  db_close/1, db_current/5,
		  db_store/3, db_fetch/3, db_erase/2, db_erase/3,
		  db_enumerate/3,
		  db_findall/5,
		  db_compress/2, db_compress/3,
		  db_make_iterator/2, db_make_iterator/3,
		  db_iterator_next/3, db_iterator_done/1,
		  db_current_iterator/3]).

:- use_module(library(fastrw), [
	fast_buf_read/2,
	fast_buf_write/3
	]).

:- dynamic '$db_env'/2, '$db'/5, '$db_it'/2, '$db_it'/3, '$first_error'/2.

db_open_env(EnvName, EnvRef) :-
	db_open_env(EnvName, 0, EnvRef).

db_open_env(EnvName0, CacheSize, EnvRef) :-
	ErrGoal = db_open_env(EnvName, CacheSize, EnvRef),
	db_normal_path(EnvName0, EnvName),
	(   '$db_env'(_, EnvName) ->
	    prolog:illarg(permission(open,environment,'already open'), ErrGoal, 1, EnvName0)
	;   true
	),
	c_open_env(EnvName, CacheSize, EnvAddr, R),
	db_error_chk(R, open_env),
	EnvRef = '$db_env'(EnvAddr),
	assertz('$db_env'(EnvAddr, EnvName)).

db_close_env(EnvRef) :-
	ErrGoal = db_close_env(EnvRef),
	must_be_open_env(EnvRef, EnvAddr, CRef, ErrGoal, 1),
	close_all_dbs_in_env(EnvAddr),
	c_close_env(EnvAddr, R),
	db_error_chk(R, close_env),
	erase(CRef).

close_all_dbs_in_env(EnvAddr) :-
	'$db'(DBAddr, EnvAddr, _N, _M, _S),
	on_exception(E, db_close_int(DBAddr),
		     asserta_if_ok(close_all_dbs_in_env, E)),
	fail.
close_all_dbs_in_env(_) :-
	retract('$first_error'(close_all_dbs_in_env, E)), !,
	raise_exception(E).
close_all_dbs_in_env(_).

db_current_env(EnvName, '$db_env'(EnvAddr)) :-
	'$db_env'(EnvAddr, EnvName).

db_open(DBName, Mode, SpecList, DBRef) :-
	ErrGoal = db_open(DBName, Mode, SpecList, DBRef),
	db_open_int(none, DBName, Mode, SpecList, 0, DBRef, ErrGoal).

% SpecList is not relevant if Mode = enumerate.
db_open(DBName, Mode, SpecList, Options, DBRef) :-
	ErrGoal = db_open(DBName, Mode, SpecList, Options, DBRef),
	db_open_options(Options, none, EnvRef, 0, CacheSize),
	db_open_int(EnvRef, DBName, Mode, SpecList, CacheSize, DBRef, ErrGoal).

db_open_options(EnvRef, _, EnvRef, CacheSize, CacheSize) :-
	nonvar(EnvRef),
	EnvRef = none, !. % backward compatibility
db_open_options(EnvRef, _, EnvRef, CacheSize, CacheSize) :-
	nonvar(EnvRef),
	EnvRef = '$db_env'(_), !. % backward compatibility
db_open_options(CacheSize, EnvRef, EnvRef, _, CacheSize) :-
	integer(CacheSize), !.	% convenience
db_open_options([], EnvRef, EnvRef, CacheSize, CacheSize).
db_open_options([Opt|Opts], EnvRef0, EnvRef, CacheSize0, CacheSize) :-
	db_open_option(Opt, EnvRef0, EnvRef1, CacheSize0, CacheSize1),
	db_open_options(Opts, EnvRef1, EnvRef, CacheSize1, CacheSize).

db_open_option(environment(EnvRef), _, EnvRef, CacheSize, CacheSize).
db_open_option(cache_size(CacheKey), EnvRef, EnvRef, _, CacheSize) :-
	(   CacheKey==none    -> CacheSize = -1
	;   CacheKey==off     -> CacheSize = -1
	;   CacheKey==default -> CacheSize = 0
	;   CacheSize = CacheKey
	).

db_open_int(EnvRef, DBName0, Mode, SpecList, CacheSize, DBRef, ErrGoal) :-
	(   EnvRef==none -> EnvAddr = 0 % uses NULL pointer!!!
	;   must_be_open_env(EnvRef, EnvAddr, _, ErrGoal, 4)
	),
	must_be_valid_mode(Mode, ErrGoal, 2),
	must_be_valid_spec_list(SpecList, ErrGoal, 3),
	must_be_integer(CacheSize, ErrGoal, 5),
	(   var(SpecList) -> SpecSize = 0, SpecAddr = 0	% NULL pointer!!!
	;   fast_buf_write(SpecList, SpecSize, SpecAddr)
	),
	db_normal_path(DBName0, DBName),
	c_open_db(EnvAddr, DBName, Mode, DBAddr, SpecAddr, SpecSize, CacheSize, R),
	db_error_chk(R, db_open),
	(   Mode = enumerate -> true
	;   c_read_spec(DBAddr, RealSpecAddr, R1),
	    (	R1 =:= 0 -> true
	    ;	c_close_db(DBAddr, _), db_error('read_spec', R1)
	    ),
	    fast_buf_read(SpecList1, RealSpecAddr),
	    (	SpecList = SpecList1 -> true
	    ;	c_close_db(DBAddr, _),
		prolog:illarg(consistency(SpecList1,SpecList,'inconsistent database spec'), ErrGoal, 0, 0)
	    )
	),
	DBRef = '$db'(DBAddr),
	assertz('$db'(DBAddr, EnvAddr, DBName, Mode, SpecList)).

valid_mode(-) :- !, fail.
valid_mode(read).
valid_mode(update).
valid_mode(enumerate).

valid_spec_list(SL) :- var(SL), !.
valid_spec_list(SL) :-
	SL = [_|_], valid_specs(SL).

valid_specs([]).
valid_specs([S|Ss]) :-
	valid_spec(S),
	valid_specs(Ss).

valid_spec(S) :-
	nonvar(S),
	S =.. [N|AL],
	atom(N),
	valid_argspecs(AL).

valid_argspecs([]).
valid_argspecs([A|As]) :-
	valid_argspec(A),
	valid_argspecs(As).

valid_argspec(?) :- !, fail.
valid_argspec(+).
valid_argspec(-).

db_close(DBRef) :-
	ErrGoal = db_close(DBRef),
	must_be_open_db(DBRef, DBAddr, _, _, ErrGoal, 1),
	db_close_int(DBAddr).

db_close_int(DBAddr) :-
	close_all_its_in_db(DBAddr),
	c_close_db(DBAddr, R),
	db_error_chk(R, close_env),
	retract('$db'(DBAddr, _, _, _, _)).

close_all_its_in_db(DBAddr) :-
	db_current_iterator_int(DBAddr, _T, I),
	on_exception(E, db_iterator_done_int(I),
		     asserta_if_ok(close_all_its_in_db, E)),
	fail.
close_all_its_in_db(_) :-
	retract('$first_error'(close_all_its_in_db, E)), !,
	raise_exception(E).
close_all_its_in_db(_).

db_current(DBName, Mode, Spec, EnvRef, '$db'(DBAddr)) :-
	'$db'(DBAddr, EnvAddr, DBName, Mode, Spec),
	(   EnvAddr =:= 0 -> EnvRef = none
	;   EnvRef = '$db_env'(EnvAddr)
	).


%%% storing %%%%%%%%%%%%%%%%%%%%%%%%
db_store(DBRef, Term, TermRef) :-
	ErrGoal = db_store(DBRef, Term, TermRef),
	TermRef = '$db_termref'(ITermRef),
	must_be_open_db(DBRef, DBAddr, update, SpecList, ErrGoal, 1),
	must_be_valid_term(Term, ErrGoal, 2),
	index_keys(Term, SpecList, Keys),
	illarg_on_nil(Keys, domain(term,'valid term'), ErrGoal, 2, Term),
	hash_key_set(Keys, HCs),
	c_next_termref(DBAddr, ITermRef, R),
	db_error_chk(R, next_termref),
	store_hash_codes(HCs, DBAddr, ITermRef),
	fast_buf_write(Term, TermSize, TermAddr),
	c_store_term(DBAddr, ITermRef, TermAddr, TermSize, R1),
	db_error_chk(R1, store_term).

illarg_on_nil([], Ball, ErrGoal, ArgNo, Culprit) :-
	prolog:illarg(Ball, ErrGoal, ArgNo, Culprit).
illarg_on_nil([_|_], _, _, _, _).

hash_key_set(Ks, HCs) :-
	hash_keys(Ks, HCs0),
	sort(HCs0, HCs).

hash_keys([], []).
hash_keys([K|Ks], [H|Hs]) :-
	c_term_hash(K, H),
	hash_keys(Ks, Hs).

store_hash_codes([], _, _).
store_hash_codes([H|Hs], DBAddr, ITermRef) :-
	c_store_termref(DBAddr, H, ITermRef, R),
	db_error_chk(R, store_termref),
	store_hash_codes(Hs, DBAddr, ITermRef).

%%% fetching %%%%%%%%%%%%%%%%%%%%%%%
db_fetch(DBRef, Term, TermRef) :-
	nonvar(TermRef), !,
	ErrGoal = db_fetch(DBRef, Term, TermRef),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_termref(TermRef, ITermRef, ErrGoal, 3),
	fetch_ref(DBAddr, Term, ITermRef).
db_fetch(DBRef, Term, TermRef) :-
	ErrGoal = db_fetch(DBRef, Term, TermRef),
	must_be_open_db(DBRef, DBAddr, Mode, SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	must_be_valid_term(Term, ErrGoal, 2),
	TermRef = '$db_termref'(ITermRef),
	query_keys(Term, SpecList, Keys, ErrGoal),
	hash_key_set(Keys, HCs),
	c_term_iterator(DBAddr, HCs, ItAddr, R),
	db_error_chk(R, term_iterator),
	call_cleanup(tfetch(ItAddr, Term, ITermRef), tfetch_cleanup(ItAddr)).

fetch_ref(DBAddr, Term, ITermRef) :-
	c_fetch_term(DBAddr, ITermRef, TermAddr, R),
	db_error_chk(R, fetch_term),
	fast_buf_read(Term, TermAddr).

tfetch(ItAddr, Term, ITermRef) :-
	repeat,
	  c_term_iterator_next(ItAddr, TermAddr, ITermRef, R),
	  db_error_chk(R, term_iterator_next),
	  (   TermAddr =:= 0 -> !, fail
	  ;   fast_buf_read(Term, TermAddr)
	  ).

tfetch_cleanup(ItAddr) :-
	c_term_iterator_done(ItAddr, R),
	db_error_chk(R, term_iterator_done).

%%% erasing %%%%%%%%%%%%%%%%%%%%%%%%
db_erase(DBRef, TermRef) :-
	ErrGoal = db_erase(DBRef, TermRef),
	db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef, SpecList),
	fetch_ref(DBAddr, Term, ITermRef),
	index_keys(Term, SpecList, Keys),
	db_erase_int(DBAddr, Keys, ITermRef).

db_erase(DBRef, TermRef, Term) :-
	ErrGoal = db_erase(DBRef, TermRef, Term),
	db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef, SpecList),
	index_keys(Term, SpecList, Keys),
	illarg_on_nil(Keys, domain(term,'valid term'), ErrGoal, 3, Term),
	db_erase_int(DBAddr, Keys, ITermRef).

db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef, SpecList) :-
	must_be_open_db(DBRef, DBAddr, update, SpecList, ErrGoal, 1),
	must_be_termref(TermRef, ITermRef, ErrGoal, 2).

db_erase_int(DBAddr, Keys, ITermRef) :-
	hash_key_set(Keys, HCs),
	c_delete_term(DBAddr, ITermRef, R),
	db_error_chk(R, delete_term),
	delete_hash_codes(HCs, DBAddr, ITermRef).

delete_hash_codes([], _, _).
delete_hash_codes([HC|HCs], DBAddr, ITermRef) :-
	c_delete_termref(DBAddr, HC, ITermRef, R),
	db_error_chk(R, delete_termref), % leaves dangling termrefs if it fails
	delete_hash_codes(HCs, DBAddr, ITermRef).

%%% enumerate %%%%%%%%%%%%%%%%%%%%%%
db_enumerate(DBRef, Term, TermRef) :-
	ErrGoal = db_enumerate(DBRef, Term, TermRef),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	TermRef = '$db_termref'(ITermRef),
	c_global_iterator(DBAddr, ItAddr, R),
	db_error_chk(R, global_iterator),
	call_cleanup(gfetch(ItAddr, Term, ITermRef), gfetch_cleanup(ItAddr)).

gfetch(ItAddr, Term, ITermRef) :-
	repeat,
	  c_global_iterator_next(ItAddr, TermAddr, ITermRef, R),
	  db_error_chk(R, global_iterator_next),
	  (   TermAddr =:= 0 -> !, fail
	  ;   fast_buf_read(Term, TermAddr)
	  ).

gfetch_cleanup(ItAddr) :-
	c_global_iterator_done(ItAddr, R),
	db_error_chk(R, global_iterator_done).

%%% findall %%%%%%%%%%%%%%%%%%%%%%%%
db_findall(DBRef, Template, Term, Goal, Bag) :-
	(   var(Term) -> db_make_iterator(DBRef, Iterator)
	;   db_make_iterator(DBRef, Term, Iterator)
	),
	call_cleanup(db_findall_cont(Iterator, Template, Term, Goal, Bag),
		     db_iterator_done_int(Iterator)).

db_findall_cont(Iterator, Template, Term, Goal, Bag) :-
	db_iterator_next_int(Iterator, Term1, _, R),
	(   R =\= 0 -> Bag = []	% no more terms
	;   findall(Template, unify_call(Term, Term1, Goal), Bag, BagT),
	    db_findall_cont(Iterator, Template, Term, Goal, BagT)
	).

unify_call(T, T, G) :-
	call(G).

%%% compress %%%%%%%%%%%%%%%%%%%%%%%
db_compress(DBRef, DBName) :-
	ErrGoal = db_compress(DBRef, DBName),
	must_be_open_db(DBRef, DBAddr, Mode, SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	db_compress_int(DBAddr, DBName, SpecList).

db_compress(DBRef, DBName, Spec) :-
	ErrGoal = db_compress(DBRef, DBName, Spec),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_valid_spec_list(Spec, ErrGoal, 3),
	db_compress_int(DBAddr, DBName, Spec).

db_compress_int(DBAddr, DBName, Spec) :-
	db_open(DBName, update, Spec, DBRefNew),
	db_make_iterator_int(DBAddr, Iterator),
	call_cleanup(copy_db(Iterator, DBRefNew), copy_db_cleanup(Iterator, DBRefNew)).

copy_db(Iterator, DBRef) :-
	db_iterator_next_int(Iterator, Term, _, R),
	(   R =\= 0 -> true	% no more terms
	;   db_store(DBRef, Term, _), copy_db(Iterator, DBRef)
	).

copy_db_cleanup(Iterator, DBRefNew) :-
	db_iterator_done_int(Iterator),
	db_close(DBRefNew).

%%% iterators %%%%%%%%%%%%%%%%%%%%%%
db_make_iterator(DBRef, Iterator) :-
	ErrGoal = db_make_iterator(DBRef, Iterator),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	db_make_iterator_int(DBAddr, Iterator).

db_make_iterator_int(DBAddr, Iterator) :-
	c_global_iterator(DBAddr, ItAddr, R),
	db_error_chk(R, global_iterator),
	Iterator = '$db_global_it'(ItAddr),
	assertz('$db_it'(ItAddr, DBAddr)).

db_make_iterator(DBRef, Term, Iterator) :-
	ErrGoal = db_make_iterator(DBRef, Term, Iterator),
	must_be_open_db(DBRef, DBAddr, Mode, SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	must_be_valid_term(Term, ErrGoal, 2),
	Iterator = '$db_term_it'(ItAddr),
	query_keys(Term, SpecList, Keys, ErrGoal),
	hash_key_set(Keys, HCs),
	create_term_iterator(DBAddr, HCs, Term, ItAddr).

db_iterator_next(Iterator, Term, TermRef) :-
	ErrGoal = db_iterator_next(Iterator, Term, TermRef),
	iterator_chk(Iterator, ErrGoal),
	db_iterator_next_int(Iterator, Term, TermRef, 0).

iterator_chk(Iterator, ErrGoal) :-
	(   Iterator = '$db_global_it'(ItAddr), integer(ItAddr) -> true
	;   Iterator = '$db_term_it'(ItAddr), integer(ItAddr) -> true
	;   prolog:illarg(domain(ground,'iterator'), ErrGoal, 1, Iterator)
	).

db_iterator_next_int('$db_global_it'(ItAddr), Term, '$db_termref'(ITermRef),
		     Code) :-
	c_global_iterator_next(ItAddr, TermAddr, ITermRef, R),
	db_error_chk(R, global_iterator_next),
	(   TermAddr =:= 0 -> Code = 1 % no more terms
	;   Code = 0, fast_buf_read(Term, TermAddr)
	).
db_iterator_next_int('$db_term_it'(ItAddr), Term, TermRef, Code) :-
	'$db_it'(ItAddr, DBAddr, Copy),
	term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code).

term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code) :-
	c_term_iterator_next(ItAddr, TermAddr, ITermRef, R),
	db_error_chk(R, term_iterator_next),
	(   TermAddr =:= 0 -> Code = 1 % no more terms
	;   fast_buf_read(TermR, TermAddr),
	    (	Copy = TermR ->
		Term = TermR, TermRef = '$db_termref'(ITermRef), Code = 0
	    ;	term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code)
	    )
	).

create_term_iterator(DBAddr, HCs, Term, ItAddr) :-
	c_term_iterator(DBAddr, HCs, ItAddr, R),
	db_error_chk(R, term_iterator),
	assertz('$db_it'(ItAddr, DBAddr, Term)).

db_iterator_done(Iterator) :-
	ErrGoal = db_iterator_done(Iterator),
	iterator_chk(Iterator, ErrGoal),
	db_iterator_done_int(Iterator).

db_iterator_done_int('$db_global_it'(ItAddr)) :-
	c_global_iterator_done(ItAddr, R),
	db_error_chk(R, global_iterator_done),
	retract('$db_it'(ItAddr, _DBAddr)).
db_iterator_done_int('$db_term_it'(ItAddr)) :-
	c_term_iterator_done(ItAddr, R),
	db_error_chk(R, term_iterator_done),
	retract('$db_it'(ItAddr, _DBAddr, _Copy)).

db_current_iterator('$db'(DBAddr), Term, Iterator) :-
	db_current_iterator_int(DBAddr, Term, Iterator).

db_current_iterator_int(DBAddr, Term, Iterator) :-
	(   '$db_it'(ItAddr, DBAddr), Iterator = '$db_global_it'(ItAddr)
	;   '$db_it'(ItAddr, DBAddr, Term), Iterator = '$db_term_it'(ItAddr)
	).

% Syntax of specs
% ---------------
%  
%   speclist  = [spec1, ..., specM]
%   spec      = FUNCTOR(argspec1, ..., argspecN)
%   argspec   = + | -

query_keys(Term, Spec, Keys, ErrGoal) :-
	keylist(Spec, 1, Term, -1, R, key(-1, -1, nil), key(_, I, M)),
	(   R =:= -1 ->
	    prolog:illarg(domain(term,'valid term'), ErrGoal, 2, Term)
	;   R =:= -2 ->
	    prolog:illarg(var, ErrGoal, 2, Term)
	;   true
	),
	(   M = [] -> Keys = [[I]]
	;   var_code(VAR), Keys = [[I|M],[I,VAR]]
	).

keylist([], _, _, R, R, K, K).
keylist([S|Spec], I, T, R0, R, Key0, Key) :-
	c_index_keys(S, T, K, C),
	(   C < 0 -> newcode(C, R0, R1), Key1 = Key0 % chpt!
	;   R1 = 0, better_key(Key0, key(C, I, K), Key1)
	),
	I1 is I+1,
	keylist(Spec, I1, T, R1, R, Key1, Key).

better_key(Key0, Key1, Key) :-
	Key0 = key(C0, _, _),
	Key1 = key(C1, _, _),
	(   C1 > C0 -> Key = Key1
	;   Key = Key0
	).

newcode(-1, R, R).		% not applicable
newcode(-2, R0, R) :-		% vars in `+' positions
	newcode2(R0, R).

newcode2(0, 0).
newcode2(-1, -2).
newcode2(-2, -2).

index_keys(Term, Spec, Keys) :-
	index_keys(Spec, 1, Term, Keys).

index_keys([], _, _, []).
index_keys([S|Spec], N, T, Keys) :-
	c_index_keys(S, T, K, C),
	(   C =:= -1 -> Keys = Keys1
	;   C =:= -2 -> var_code(VAR), Keys = [[N,VAR]|Keys1]
	;   Keys = [[N|K]|Keys1]
	),
	N1 is N+1,
	index_keys(Spec, N1, T, Keys1).

var_code(-16'1227F5A).


%%% aux preds %%%%%%%%%%%%%%%%%%%%%%
% decode the type of error and print it along with the context of error
db_error(Ctxt, ErrorCode) :-
	decode_error(ErrorCode, Err),
	prolog:illarg(system(berkeley_db(Ctxt,Err)), 0, 0, 0). % TODO: msgs.pl

db_error_chk(R, _) :- R =:= 0, !.
db_error_chk(R, M) :- 
	db_error(M, R).

asserta_if_ok(Id, _) :-
	'$first_error'(Id, _), !.
asserta_if_ok(Id, E) :-
	asserta('$first_error'(Id, E)).

/*** Argument checking preds. --Mats ***/

must_be_open_env(EnvRef, EnvAddr, CRef, ErrGoal, ArgNo) :-
	(   EnvRef = '$db_env'(EnvAddr), integer(EnvAddr) ->
	    (	clause('$db_env'(EnvAddr, _), true, CRef) -> true
	    ;   prolog:illarg(permission(access,environment,'not open'), ErrGoal, ArgNo, EnvRef)
	    )
	;   prolog:illarg(domain(ground,'environment reference'), ErrGoal, ArgNo, EnvRef)
	).

must_be_valid_mode(Mode, ErrGoal, ArgNo) :-
	(   valid_mode(Mode) -> true
	;   prolog:illarg(domain(atom,one_of([read,update,enumerate])),
			  ErrGoal, ArgNo, Mode)
	).

must_be_valid_spec_list(SpecList, ErrGoal, ArgNo) :-
	(   valid_spec_list(SpecList) -> true
	;   prolog:illarg(domain(list,'database spec'),
			  ErrGoal, ArgNo, SpecList)
	).

must_be_open_db(DBRef, DBAddr, Access, SpecList, ErrGoal, ArgNo) :-
	must_be_db(DBRef, DBAddr, ErrGoal, ArgNo),
	(   '$db'(DBAddr, _, _, Access1, SpecList) -> true
	;   prolog:illarg(permission(access,database,'not open'), ErrGoal, ArgNo, DBRef)
	),
	(   Access = Access1 -> true
	;   prolog:illarg(permission(store,database,read_only), ErrGoal, ArgNo, DBRef)
	).
	

must_be_db(DBRef, DBAddr, ErrGoal, ArgNo) :-
	(   DBRef = '$db'(DBAddr), integer(DBAddr) -> true
	;   prolog:illarg(domain(ground,'database reference'), ErrGoal, ArgNo, DBRef)
	).

must_be_valid_term(Term, ErrGoal, ArgNo) :-
	(   callable(Term) -> true
	;   prolog:illarg(type(callable), ErrGoal, ArgNo, Term)
	).

must_be_integer(Term, ErrGoal, ArgNo) :-
	(   integer(Term) -> true
	;   prolog:illarg(type(integer), ErrGoal, ArgNo, Term)
	).

must_be_termref(TermRef, ITermRef, ErrGoal, ArgNo) :-
	(   TermRef = '$db_termref'(ITermRef), integer(ITermRef) -> true
	;   prolog:illarg(domain(ground,'term reference'), ErrGoal, ArgNo, TermRef)
	).

must_not_be_enumerate(enumerate, ErrGoal, ArgNo, DBRef) :- !,
	prolog:illarg(permission(access,database,enumerate_only), ErrGoal, ArgNo, DBRef).
must_not_be_enumerate(_, _, _, _).


%%% foreign %%%%%%%%%%%%%%%%%%%%%%%%
foreign(open_env,
	c_open_env(+string, +integer, % cache size in KB (0 selects default)
		   -integer/*address('DB_ENV')*/, [-integer])).
foreign(close_env,
	c_close_env(+integer/*address('DB_ENV')*/, [-integer])).
foreign(open_db,
	c_open_db(+integer/*address('DB_ENV')*/, +string, +string, -address('db_struct'),
		  +integer/*address(void)*/, +integer, +integer, [-integer])).
foreign(close_db,
	c_close_db(+address('db_struct'), [-integer])).
foreign(read_spec,
	c_read_spec(+address('db_struct'), -integer/*address(void)*/, [-integer])).
foreign(next_termref,
	c_next_termref(+address('db_struct'), -integer, [-integer])).
foreign(store_termref,
	c_store_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(store_term,
	c_store_term(+address('db_struct'), +integer, +integer/*address(void)*/, +integer, [-integer])).
foreign(fetch_term,
	c_fetch_term(+address('db_struct'), +integer, -integer/*address(void)*/, [-integer])).
foreign(delete_term,
	c_delete_term(+address('db_struct'), +integer, [-integer])).
foreign(delete_termref,
	c_delete_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(global_iterator,
	c_global_iterator(+address('db_struct'), -integer/*address('DBC')*/, [-integer])).
foreign(global_iterator_next,
	c_global_iterator_next(+integer/*address('DBC')*/, -integer/*address(void)*/, -integer, [-integer])).
foreign(global_iterator_done,
	c_global_iterator_done(+integer/*address('DBC')*/, [-integer])).
foreign(term_iterator,
	c_term_iterator(+address('db_struct'), +term, -address('iterator'), [-integer])).
foreign(term_iterator_next,
	c_term_iterator_next(+address('iterator'), -integer/*address(void)*/, -integer, [-integer])).
foreign(term_iterator_done,
	c_term_iterator_done(+address('iterator'), [-integer])).
foreign(db_term_hash,
	c_term_hash(+term, [-integer])).
foreign(ixkeys, c_index_keys(+term, +term, -term, [-integer])).
%foreign(max_it, c_max_it([-integer])).
foreign(decode_error, decode_error(+integer, [-string])).
foreign(db_normal_path, db_normal_path(+string, [-string])).
%foreign(printstat, printstat).	% TODO: omit
%foreign(zerostat, zerostat).	% TODO: omit

foreign_resource(bdb,
  [
   init(db_init),
   open_env,close_env,
   open_db,close_db,
   read_spec,next_termref,
   store_termref,store_term,
   fetch_term,delete_termref,delete_term,
   global_iterator,global_iterator_next,global_iterator_done,
   term_iterator,term_iterator_next,term_iterator_done,
   db_term_hash,ixkeys,decode_error,db_normal_path
%   ,printstat,zerostat		% TODO: omit
  ]).

:- load_foreign_resource(library(system(bdb))).
