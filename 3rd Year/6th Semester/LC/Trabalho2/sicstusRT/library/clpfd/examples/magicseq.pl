/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : magic.pl                                               */
/* Title          : magic series                                           */
/* Original Source: W.J. Older and F. Benhamou - Programming in CLP(BNR)   */
/*                  (in Position Papers of PPCP'93)                        */
/* Adapted by     : Daniel Diaz - INRIA France                             */
/* Date           : May 1993                                               */
/*                                                                         */
/* A magic serie is a sequence x0, x1, ..., xN-1 such that each xi is the  */
/* number of occurences of i in the serie.                                 */
/*           N-1                                                           */
/*  ie  xi = Sum (xj=i)  where (xj=i) is 1 if x=y and 0 if x<>y            */
/*           i=0                                                           */
/*                                                                         */
/* two redundant constraints are used:                                     */
/*           N-1                     N-1                                   */
/*           Sum i = N          and  Sum i*xi = N                          */
/*           i=0                     i=0                                   */
/*                                                                         */
/* Note: in the Pascal's original version the length of a magic serie is   */
/* N+1 (x0, x1, ..., XN) instead of N (x0, x1, ..., xN-1). Finding such a  */
/* serie (for N) only corresponds to find a serie for N+1 in this version. */
/* Also the original version only used one redundant constraint.           */
/*                                                                         */
/* Solution:                                                               */
/* N=1,2,3 and 6 none                                                      */
/* N=4  [1,2,1,0] and [2,0,2,0]                                            */
/* N=5  [2,1,2,0,0]                                                        */
/* N=7  [3,2,1,1,0,0,0]   (for N>=7  [N-4,2,1,<N-7 0's>,1,0,0,0])          */
/*-------------------------------------------------------------------------*/

:- module(magic, [magic/3,magic_gcc/3]).
:- use_module(library(clpfd)).

magic(N, L, Lab) :-
	length(L, N),
	N1 is N-1,
	domain(L, 0, N1),
	constraints(L, L, 0, Cs),		% essential
	sum(L, #=, N),				% redundant #1
	scalar_product(Cs, L, #=, N),		% redundant #2
	labeling(Lab, L).

constraints([], _, _, []).
constraints([X|Xs], L, I, [I|S2]):-
	count(I, L, #=, X),
	I1 is I+1,
	constraints(Xs, L, I1, S2).

magic_gcc(N, L, Lab) :-
	length(L, N),
	N1 is N-1,
	domain(L, 0, N1),
	constraints_gcc(L, Cs),	% essential
	% sum(L, #=, N),		% redundant #1 does not help
	scalar_product(Cs, L, #=, N), % redundant #2 really helps
	labeling(Lab, L).

constraints_gcc(Vars, Counts) :-
	pairs(Vars, 0, Pairs, Counts),
	global_cardinality(Vars, Pairs).

pairs([], _, [], []).
pairs([X|Xs], I, [I-C|Ps], [I|Is]) :-
	X = C,			% creating coreference
	J is I+1,
	pairs(Xs, J, Ps, Is).

