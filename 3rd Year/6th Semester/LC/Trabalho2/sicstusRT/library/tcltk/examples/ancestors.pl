:- use_module(library(tcltk)).

go :-
   tk_new([name('ancestors')], X),
   absolute_file_name(library('tcltk/examples/ancestors.tcl'), Abs),
   tcl_eval(X, [source,Abs], _),
   tk_main_loop,
   tcl_delete(X).

father(ann, fred).
father(fred, jim).
mother(ann, lynn).
mother(fred, lucy).
father(jim, sam).

parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

all_ancestors(X, Z) :- findall(Y, ancestor(X, Y), Z).

all_parents(X, Z) :-   findall(Y, parent(X, Y), Z).
