/* Copyright(C) 1989, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : random.pl                                                        %
%   Maintainer : Lena Flood                                                   %
%   Date   : 20 June 1989                                                     %
%   Purpose: To provide a random number generator                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(random, [
        getrand/1,
        setrand/1,
        random/1,
        random/3,
        randseq/3,
        randset/3
	]).

:- use_module(library(assoc), [
	empty_assoc/1,
	get_assoc/3,
	put_assoc/4
	]).


%   Adapted from shared code written by Richard A O'Keefe

%  return current state

getrand(rand(X,Y,Z)) :-		
   cgetrand(X,Y,Z).

%set a new state
setrand(rand(X,Y,Z)) :-
	Goal = setrand(rand(X,Y,Z)),
	(   integer(X), X > 0, X < 30269 -> true
	;   prolog:illarg(domain(integer,between(1,30268)), Goal, 1, X)
	),
	(   integer(Y), Y > 0, Y < 30307 -> true
	;   prolog:illarg(domain(integer,between(1,30306)), Goal, 1, Y)
	),
	(   integer(Z), Z > 0, Z < 30323 -> true
	;   prolog:illarg(domain(integer,between(1,30322)), Goal, 1, Z)
	),
	cputrand(X, Y, Z).


%   random(-R) 
%   binds R to a new random number in [0.0,1.0)

random(R) :-
        crandom(R).



%   random(+L, +U, -R) 
%   binds R to a random integer in [L,U) when L and U are integers 
%   (note that U will NEVER be generated), or to a random floating 
%   number in [L,U) otherwise.

random(L, U, R) :-
	L < U,
	random(X),
	(   integer(L), integer(U)
	->  R is L+integer((U-L)*X)
	;   R is L+((U-L)*X)
	).

	
%   randseq(K, N, L)
%   generates a random sequence of K integers in the range 1..N.
%   The result is in random order.
%   Courtesy of Christian Holzbaur.

randseq(K, N, Bag) :-
	K >= 0,
	K =< N,
	M is N+1,
        empty_assoc(A),
        randseq(K, M, A, Bag, []).

randseq(0, _, _) --> !.
randseq(N, M, A0) -->
        {random(1, M, R)},
        (   {get_assoc(R, A0, _)} ->
            randseq(N, M, A0)
        ;   [R],
            {N1 is N-1},
            {put_assoc(R, A0, R, A1)},
            randseq(N1, M, A1)
        ).


%   randset(K, N, S)
%   generates a random set of K integers in the range 1..N.
%   The result is an ordered list.
%   Courtesy of Christian Holzbaur.

randset(K, N, Set) :-
	randseq(K, N, Bag),
	sort(Bag, Set).


%   Load the c function files

:- dynamic foreign/3, foreign_resource/2.

foreign_resource(random,
      [cgetrand,
       cputrand,
       crandom,
       init(rand_init),
       deinit(rand_deinit)]).

foreign(cgetrand, c, cgetrand(-integer, -integer, -integer)).
foreign(cputrand, c, cputrand(+integer, +integer, +integer)).
foreign(crandom, c, crandom([-float])).

:- load_foreign_resource(library(system(random))).



