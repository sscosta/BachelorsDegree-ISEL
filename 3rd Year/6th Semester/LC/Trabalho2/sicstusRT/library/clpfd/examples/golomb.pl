/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Golomb Ruler
 * Author    : Mats Carlsson
 * 
 * This is the clpfd implementation of a
 * problem that was proposed as a fd benchmark by Jean-Francois Puget:
 * 
 *      ftp://ftp.cs.city.ac.uk/pub/constraints/benchmarks/golomb/
 * 
 * The relative superiority of all_distinct/1 vs. all_different/1 
 * shows for rulers with more than 8 marks (e.g. 17 vs. 22 minutes for N=9).
 * [Can't reproduce - I get 167 vs. 133 seconds for N=9. --Mats]
 * 
 * There are other people who take this problem rather seriously:
 * 
 *       ftp://ftp.ee.duke.edu/users/wrankin/golomb/Golomb.Art1.ps.Z
 * 
 *       The optimal ruler with 19 marks took the equivalent of 
 *       36200 CPU hours (Sparc Classic) --- approx. 4 years 
 *       with a specialized algorithm.
 * 
 * Conjecture: the squares of all ranges 0..n are valid Golomb ruler.
 * This can be used to obtain an upper bound on the domains.
 * 
 * Here's an idea for surrogate constraints. 
 * Consider a ruler and its differences:
 * 
 *      X0      X1      X2      X3      X4
 *      ==================================
 * 	 D01     D12     D23     D34
 * 	     D02     D13     D24
 * 		 D03    D14
 * 		     D04
 * 
 * where Xj-Xi = Dij.
 * Then for all i \= j \= k, Dik + Dkj = Dij.
 * These constraints maintain a form of path consistency over the
 * differences.  It yielded very little, if anything at all, so it's disabled.
 * No extra pruning of the Xi was detected.
 * 
 * Using two of the techniques mentioned
 * in W. Rankin's paper yielded more:
 * 
 * a) constrain Xa > Xn-Xm to suppress symmetries
 * b) place a lower bound on each difference as a function of the distance
 * 
 * Furthermore, traditional b&b search did not perform as well as first
 * labeling the end marker and then using first-fail labeling on the others,
 * until a solution is found.
 * 
 * Some timings on else.sics.se, in msec:
 * 
 * N =  8       9       10
 * ===========================
 *       500     9080    20280  all_different, labeling end then ff
 *       810    14850    33930  all_distinct,  labeling end then ff
 *       650    11500    26000  all_distinct,  labeling end then ff, improved
 *       650     9250    274620 all_different, b&b labeling
 * 
 * sinuhe.sics.se, FDZ:  labeling Xn, X1, ..., Xm.  Bisecting makes things worse.
 * 
 *       460     6650    17780  113590  all_diff
 *       600     8680    24240  151980  all_diff, PC
 *       400     5230    12540   70570  all_diff, domain-consistent plus
 *       470     6450    14530   80150  all_diff, domain-consistent plus, PC
 * 
 *       700     8060    22350  122640  all_distinct
 *       930    11710    33720  194950  all_distinct, PC
 *       660     6920    18040   89780  all_distinct, domain-consistent plus
 *       890     9830    25400  126900  all_distinct, domain-consistent plus, PC
 * 
 * N=10:   #failures       msec
 *      =========       ====
 *      943             11630   all_different, plus/3, no PC
 *      875             16430   all_different, plus/3, PC(intervals)
 *      766             21150   all_different, plus/3, PC(domains)
 *      696             30580   all_distinct, plus/3, no PC
 *      662             36620   all_distinct, plus/3, PC(intervals)
 *      618             43730   all_distinct, plus/3, PC(domains)
 * 
 */

:- module(golomb, [golomb/1, golomb/3]).
:- use_module(library(lists), [
	last/2,
	reverse/2
			      ]).
:- use_module(library(clpfd)).

golomb(N) :-
	statistics(runtime, _),
	golomb([], N, Marks), !,
	statistics(runtime, [_,T]),
	format('~q took ~d msec.\n', [golomb(N,Marks),T]).


%
% compute an optimum golomb ruler with N marks
%
golomb(Lab, N, Marks) :-
	marks(N, Marks, Last),
	indomain(Last),
	labeling(Lab, Marks).

/* 
% Holzbaur's version
golomb(N, Marks) :-
	marks(N, Marks, Last),
	labeling([minimize(Last)], Marks).
	
marks(N, L, Xn) :-
	length(L, N),
	L = [0|Xs],
	append(_, [Xn], L),
	ascending(Xs, 0),
	deltas(L, Ds, []), 
	domain(Ds, 1, 99999),
	all_different(Ds).
	% all_distinct(Ds).

deltas([]) --> [].
deltas([X|Xs]) --> delta(Xs, X), deltas(Xs).

delta([],     _) --> [].
delta([X|Xs], Y) --> {D #= X-Y}, [D], delta(Xs, Y).

ascending([],     _).
ascending([X|Xs], Y) :- Y #< X, ascending(Xs, X).
*/

% Techniques from the paper:
% put X1 #> Xn-Xm;
% provide lower bounds for the differences.
marks(N1, [0|Xs], Xn) :-
	N is N1-1,
	length(Xs, N),
	Max is N*N,
	domain(Xs, 1, Max),
	deltas(Xs, Triangle, Ds, Xs),
	min_diffs([Xs|Triangle]),
	% diffs(Triangle),
	Xs = [X1|_],
	last(Xs, Xn),
	last(Triangle, [Dmn]),
	X1 #> Dmn,
	% all_different(Ds).
	all_distinct(Ds).

deltas([_], []) --> !.
deltas([X|Xs], [Row|Triangle]) -->
	delta(Xs, X, Row),
	deltas(Xs, Triangle).

delta([],     _, []) --> [].
delta([Xj|Xs], Xi, [Dij|Ds]) --> [Dij],
	{/*Dij #= Xj-Xi*/ plus(Xi, Dij, Xj)},
	delta(Xs, Xi, Ds).

min_diffs([]).
min_diffs([Row|R]) :-
	min_diffs(Row, 0),
	min_diffs(R).

min_diffs([], _).
min_diffs([D|Row], I) :-
	J is I+1,
	d(J, N),
	D #>= N,
	min_diffs(Row, J).

% let Dij = Xj-Xi
% then form all constraints Dik = Dij+Djk
% diffs([[D12,...,D1n],...,[Dmn]]) where m = n-1

diffs([]).
diffs([DsI|R]) :-
	diffs1(R, DsI),
	diffs(R).

diffs1([], _).
diffs1([DsJ|R], [Dik|DsI]) :-
	diffs2(DsJ, Dik, DsI),
	diffs1(R, DsI).

diffs2([], _, _).
diffs2([Dkj|DsJ], Dik, [Dij|DsI]) :-
	% Dik + Dkj #= Dij,
	plus(Dik, Dkj, Dij),
	diffs2(DsJ, Dik, DsI).
	
d(1, 1).
d(2, 3).
d(3, 6).
d(4, 11).
d(5, 17).
d(6, 25).
d(7, 34).
d(8, 44).
d(9, 55).
d(10, 72).

% domain consistent addition
plus(X, Y, Z) +:
	Z in dom(X)+dom(Y) !,
	X in dom(Z)-dom(Y) !,
	Y in dom(Z)-dom(X) !.

