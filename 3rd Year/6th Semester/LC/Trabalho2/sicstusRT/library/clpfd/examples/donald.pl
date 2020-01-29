/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : donald.pl                                              */
/* Title          : crypt-arithmetic                                       */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     :                                                        */
/* Date           : September 1992                                         */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*    D O N A L D                                                          */
/*  + G E R A L D                                                          */
/*  --------------                                                         */
/*  = R O B E R T                                                          */
/*                                                                         */
/* (resolution by line)                                                    */
/*                                                                         */
/* Solution:                                                               */
/*  [D,O,N,A,L,G,E,R,B,T]                                                  */
/*  [5,2,6,4,8,1,9,7,3,0]                                                  */
/*-------------------------------------------------------------------------*/

:- module(donald,[donald/2,donald_ix/2]).
:- use_module(library(clpfd)).

donald(Lab,LD):-
	LD=[D,O,N,A,L,G,E,R,B,T],
	domain(LD,0,9),
	domain([D,G],1,9),
	all_different(LD),
	   100000*D+10000*O+1000*N+100*A+10*L+D +
	   100000*G+10000*E+1000*R+100*A+10*L+D
	#= 100000*R+10000*O+1000*B+100*E+10*R+T,
	labeling(Lab,LD).

donald_ix(Lab,LD):-
	LD=[D,O,N,A,L,G,E,R,B,T],
	domain(LD,0,9),
	domain([D,G],1,9),
	all_different(LD),
	eq(D,O,N,A,L,G,E,R,B,T),
	labeling(Lab,LD).

eq(D,O,N,A,L,G,E,R,B,T) +:
	   100000*D+10000*O+1000*N+100*A+10*L+D +
	   100000*G+10000*E+1000*R+100*A+10*L+D
	#= 100000*R+10000*O+1000*B+100*E+10*R+T.

