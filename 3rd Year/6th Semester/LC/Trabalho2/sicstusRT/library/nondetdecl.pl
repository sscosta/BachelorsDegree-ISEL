/* ----------------------------------------------------------------------
    File:	nondetdecl.pl
    Authors:	Dave Bowen and Peter Schachte
    SCCS:	@(#)det.pl	66.1 10/14/92
    Purpose:	Support for nondeterminacy declarations

    Copyright (C) 1989 Quintus Computer Systems, Inc.  Permission is hereby
    granted for this program to be used or modified for any purpose whatsoever
    provided only that this copyright notice is retained in the source.

    The Quintus determinacy checker recognizes declarations of the form

	:- nondet foo/2.
	:- det foo/2.

    to indicate the expectation that the predicate foo/2 will be
    [non]deterministic.  To allow these declarations to appear in a source
    file without confusing the Prolog compiler, each source file that
    includes such declarations should include the line

	:- use_module(library(nondetdecl), [when(compile_time), if(changed)]).

    near the top.

    Alternatively, the file could contain the line

	:- use_module(library(detcheck), [when(compile_time), if(changed)]).

    which will cause the Prolog compiler to perform determinacy checking
    on that file each time it is compiled.

   ---------------------------------------------------------------------- */
    
:- module(nondetdecl, [nondet/1,det/1]).

:- op(1150, fx, nondet).

nondet _.

:- op(1150, fx, det).

det _.
