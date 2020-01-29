places(From, To, X) :-
	connected(From, To, Way, Way),
	(   member(X, Way)
	;   X = ''			% marks new route
	).

connected(From, From, [From], _):- !.
connected(From, To, [From| Way], Been):-
        (no_stop(From, Through)
        ;
        no_stop(Through, From)),
        not_been_before(Been, Through),
        connected(Through, To, Way, Been).

no_stop('Stockholm', 'Katrineholm').
no_stop('Stockholm', 'Vasteras').
no_stop('Katrineholm', 'Hallsberg').
no_stop('Katrineholm', 'Linkoping').
no_stop('Hallsberg', 'Kumla').
no_stop('Hallsberg', 'Goteborg').
no_stop('Orebro', 'Vasteras').
no_stop('Orebro', 'Kumla').

not_been_before(Way, _) :- var(Way),!.
not_been_before([Been| Way], Am) :- 
        Been \== Am,
        not_been_before(Way, Am).

member(E, [E|_]).
member(E, [_|L]) :- member(E, L).
