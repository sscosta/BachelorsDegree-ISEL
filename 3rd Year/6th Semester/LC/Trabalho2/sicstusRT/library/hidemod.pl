/* A trick for making all library modules hidden in the distribution. */

:- multifile user:term_expansion/2.
:- dynamic user:term_expansion/2.
user:term_expansion((:- module(M,E)), (:- module(M,E,[hidden(true)]))).

