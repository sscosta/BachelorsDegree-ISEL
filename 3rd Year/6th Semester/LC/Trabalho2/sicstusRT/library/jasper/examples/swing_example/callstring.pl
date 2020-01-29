:- use_module(library(charsio)).

callstring(S,X) :-
	read_from_chars(S, X), call(X).
