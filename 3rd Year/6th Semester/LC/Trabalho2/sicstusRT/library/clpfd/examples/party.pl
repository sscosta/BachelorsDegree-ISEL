/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Progressive Party Problem
 * Author    : Mats Carlsson
 * 
 * A set of guest boat crews are supposed to visit a set of host boats in
 * six shifts.  The host boats have finite capacity. A guest crew can't
 * visit a host twice.  Two guest crews can't meet twice.
 * 
 * See Smith, B.,Brailsford, S., Hubbard, P., and Williams, H.
 * The progressive party problem: integer linear programming and
 * constraint programming compared.  Constraints 1:119-138, 1996.
 * 
 * Constants:
 * 
 *     spare(I) : the spare capacity of host I
 *     crew(I) : the crew size of guest I
 * 
 * 
 * Variables:
 * 
 *     h(I,T) in 1..13 : the host boat that guest boat I visits at time T
 *     v(I,J,T) #<=> h(I,T) #= J
 *     m(I,J,T) #<=> h(I,T) #= h(J,T)
 *     
 * 
 * Problem constraints:
 * 
 *     all_different([h(I,1),...,h(I,6)]) % guest I can't visit a host twice
 *     scalar_product(CrewSizes, [v(1,J,T),...,v(29,J,T)], #=<, spare(J))
 *     sum([m(I,J,1)...,m(I,J,6)], #=<, 1) % crews I,J can't meet twice
 * 
 * Redundant constraints (do not seem to help):
 * 
 *     count(H, [h(1,T),...,h(29,T)], #>=, 1)
 * 
 * Asymmetry constraints:
 * 
 *     h(1,1) #< ... #< h(1,6)
 *     I<J & crew(I) #= crew(J) #=> 
 *         (h(I,1) #=< h(J,1) #/\ (h(I,1) #< h(J,1) #\/ h(I,2) #< h(J,2)))
 *     I<J & spare(I) #= spare(J) #=>
 *         v(1,I,1)+...+v(K-1,I,1) #>= v(K,J,1)
 */
:- module(party, [party/1]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3]).

% First 13 are hosts, remaining 29 are guests.
% boat(BoatNo, Capacity, CrewSize)
boat( 1,  6, 2).
boat( 2,  8, 2).
boat( 3, 12, 2).
boat( 4, 12, 2).
boat( 5, 12, 4).
boat( 6, 12, 4).
boat( 7, 12, 4).
boat( 8, 10, 1).
boat( 9, 10, 2).
boat(10, 10, 2).
boat(11, 10, 2).
boat(12, 10, 3).
boat(13,  8, 4).
boat(14,  8, 2).
boat(15,  8, 3).
boat(16, 12, 6).
boat(17,  8, 2).
boat(18,  8, 2).
boat(19,  8, 4).
boat(20,  8, 2).
boat(21,  8, 4).
boat(22,  8, 5).
boat(23,  7, 4).
boat(24,  7, 4).
boat(25,  7, 2).
boat(26,  7, 2).
boat(27,  7, 4).
boat(28,  7, 5).
boat(29,  6, 2).
boat(30,  6, 4).
boat(31,  6, 2).
boat(32,  6, 2).
boat(33,  6, 2).
boat(34,  6, 2).
boat(35,  6, 2).
boat(36,  6, 2).
boat(37,  6, 4).
boat(38,  6, 5).
boat(39,  9, 7).
boat(40,  0, 2).
boat(41,  0, 3).
boat(42,  0, 4).

% derived facts
% host(Id, SpareCap, BoatNo)
% Total spare cap = 98, guest crews = 94
host( 1, 10, 3).
host( 2, 10, 4).
host( 3,  9, 8).
host( 4,  8, 5).
host( 5,  8, 6).
host( 6,  8, 7).
host( 7,  8, 9).
host( 8,  8, 10).
host( 9,  8, 11).
host(10,  7, 12).
host(11,  6, 2).
host(12,  4, 1).
host(13,  4, 13).

% guest(Id, CrewSize, BoatNo)
guest( 1, 7, 39).
guest( 2, 6, 16).
guest( 3, 5, 22).
guest( 4, 5, 28).
guest( 5, 5, 38).
guest( 6, 4, 19).
guest( 7, 4, 21).
guest( 8, 4, 23).
guest( 9, 4, 24).
guest(10, 4, 27).
guest(11, 4, 30).
guest(12, 4, 37).
guest(13, 4, 42).
guest(14, 3, 15).
guest(15, 3, 41).
guest(16, 2, 14).
guest(17, 2, 17).
guest(18, 2, 18).
guest(19, 2, 20).
guest(20, 2, 25).
guest(21, 2, 26).
guest(22, 2, 29).
guest(23, 2, 31).
guest(24, 2, 32).
guest(25, 2, 33).
guest(26, 2, 34).
guest(27, 2, 35).
guest(28, 2, 36).
guest(29, 2, 40).

% sizes(Hosts, Guests)
sizes(13, 29).

party(Lab) :-
	party_variables(6, Vars),
	party_constraints(6, Vars),
	phases(0, 6, Lab, Vars),
	format('Guest~t~10|Hosts\n', []),
	pp_party(13, 42, Vars).

pp_party(G, G, _) :- !.
pp_party(I, G, Vars) :-
	J is I+1,
	(guest(J1, _, J) -> true),
	aget(h(J1), Vars, Row),
	host_ids(Row, Row1),
	format('~w~t~10|~w\n', [J,Row1]),
	pp_party(J, G, Vars).

host_ids([], []).
host_ids([H|Hs], [H1|H1s]) :-
	host(H, _, H1),
	host_ids(Hs, H1s).
	

phases(N, N, _, _) :- !.
phases(I, N, Lab, Vars) :-
	J is I+1,
	labelvars(J, Vars, Hs, []),
	labeling(Lab, Hs), !,
	phases(J, N, Lab, Vars).

labelvars(T, Vars) -->
	{sizes(_, Guests)},
	labelvars(0, Guests, T, Vars).

labelvars(G, G, _, _) --> !.
labelvars(I, G, T, Vars) --> [V],
	{J is I+1, aget(h(J,T), Vars, V)},
	labelvars(J, G, T, Vars).

party_variables(T, Vars) :-
	Vars = vars(H,V,M),
	sizes(Hosts, Guests),
	functor(H, h, Guests),
	functor(V, v, Guests),
	functor(M, m, Guests),
	h_array(Guests, T, Hosts, H),
	v_array(Guests, Hosts, T, Vars, V),
	m_array(Guests, T, Vars, M).

party_constraints(Times, Vars) :-
	sizes(Hosts, Guests),
	single_hosts(Guests, Vars),
	host_capacities(Hosts, Times, Vars),
	single_guests(Guests, Vars),
	first_guest_order(Vars),
	% redundant(Hosts, Guests, Times, Vars), % does not help
	asym_crews(Vars),
	asym_hosts(Vars).

single_hosts(0, _) :- !.
single_hosts(G, Vars) :-
	aget(h(G), Vars, L),
	all_different(L),
	G1 is G-1,
	single_hosts(G1, Vars).

host_capacities(0, _, _) :- !.
host_capacities(J, T, Vars) :-
	sizes(_, Guests),
	host(J, Spare, _),
	host_capacities(J, T, Spare, Guests, Vars),
	J1 is J-1,
	host_capacities(J1, T, Vars).

host_capacities(_, 0, _, _, _) :- !.
host_capacities(J, T, Spare, Guests, Vars) :-
	host_capacities(Guests, J, T, Vars, Crews, Vs),
	scalar_product(Crews, Vs, #=<, Spare),
	T1 is T-1,
	host_capacities(J, T1, Spare, Guests, Vars).

host_capacities(0, _, _, _, [], []) :- !.
host_capacities(G, J, T, Vars, [C|Crews], [V|Vs]) :-
	guest(G, C, _),
	aget(v(G,J,T), Vars, V),
	G1 is G-1,
	host_capacities(G1, J, T, Vars, Crews, Vs).

single_guests(0, _) :- !.
single_guests(I, Vars) :-
	I1 is I-1,
	single_guests(I, I1, Vars),
	single_guests(I1, Vars).

single_guests(_, 0, _) :- !.
single_guests(I, J, Vars) :-
	aget(m(I,J), Vars, L),
	sum(L, #=<, 1),
	J1 is J-1,
	single_guests(I, J1, Vars).

first_guest_order(Vars) :-
	aget(h(1), Vars, [H1|Hosts]),
	first_guest_order(Hosts, H1).

first_guest_order([], _).
first_guest_order([H2|Hs], H1) :- H1 @< H2, first_guest_order(Hs, H2).

% redundant(Hosts, Guests, Times, Vars)
redundant(_, _, 0, _) :- !.
redundant(H, G, T, Vars) :-
	redundant2(G, T, Vars, L),
	redundant(H, L),
	T1 is T-1,
	redundant(H, G, T1, Vars).

redundant2(0, _, _, []) :- !.
redundant2(G, T, Vars, [V|Vs]) :-
	aget(h(G,T), Vars, V),
	G1 is G-1,
	redundant2(G1, T, Vars, Vs).

redundant(0, _) :- !.
redundant(H, L) :-
	count(H, L, #>=, 1),
	H1 is H-1,
	redundant(H1, L).


asym_crews(Vars) :- 
	findall(Crew-[Guest], guest(Guest,Crew,_), Pairs),
	keysort(Pairs, Keysorted),
	keymerge(Keysorted, Keymerged),
	asym_crews(Keymerged, Vars).

asym_crews([], _).
asym_crews([_-Class|Cs], Vars) :-
	asym_crews1(Class, Vars),
	asym_crews(Cs, Vars).

asym_crews1([_], _) :- !.
asym_crews1([G1,G2|Gs], Vars) :-
	aget(h(G1,1), Vars, H11),
	aget(h(G1,2), Vars, H12),
	aget(h(G2,1), Vars, H21),
	aget(h(G2,2), Vars, H22),
	H11 #=< H21,
	H11 #< H21 #\/ H12 #< H22,
	asym_crews1([G2|Gs], Vars).


asym_hosts(Vars) :-
	findall(Spare-[Host], host(Host,Spare,_), Pairs),
	keysort(Pairs, Keysorted),
	keymerge(Keysorted, Keymerged),
	asym_hosts(Keymerged, Vars).

asym_hosts([], _).
asym_hosts([_-Class|Cs], Vars) :-
	sizes(_, Guests),
	asym_hosts(Class, Guests, Vars),
	asym_hosts(Cs, Vars).

asym_hosts([_], _, _) :- !.
asym_hosts([H1,H2|Hs], Guests, Vars) :-
	asym_hosts(0, Guests, H1, H2, 0, Vars),
	asym_hosts([H2|Hs], Guests, Vars).

asym_hosts(G, G, _, _, _, _) :- !.
asym_hosts(I, G, H1, H2, Sum0, Vars) :-
	J is I+1,
	aget(v(J,H2,1), Vars, Y),
	Sum0 #>= Y,
	aget(v(J,H1,1), Vars, X),
	Sum #= Sum0+X,
	asym_hosts(J, G, H1, H2, Sum, Vars).


h_array(0, _, _, _) :- !.
h_array(I, T, Hosts, H) :-
	arg(I, H, Row),
	length(L, T),
	domain(L, 1, Hosts),
	Row =.. [h|L],
	J is I-1,
	h_array(J, T, Hosts, H).

v_array(0, _, _, _, _) :- !.
v_array(I, Hosts, T, Vars, V) :-
	arg(I, V, Row),
	functor(Row, v, Hosts),
	v_array1(I, Hosts, T, Vars, Row),
	I1 is I-1,
	v_array(I1, Hosts, T, Vars, V).

v_array1(_, 0, _, _, _) :- !.
v_array1(I, J, T, Vars, V) :-
	arg(J, V, Row),
	functor(Row, v, T),
	v_array2(I, J, T, Row, Vars),
	J1 is J-1,
	v_array1(I, J1, T, Vars, V).

v_array2(_, _, 0, _, _) :- !.
v_array2(I, J, T, Row, Vars) :-
	arg(T, Row, X),
	aget(h(I,T), Vars, Y),
	Y #= J #<=> X,
	T1 is T-1,
	v_array2(I, J, T1, Row, Vars).


m_array(0, _, _, _) :- !.
m_array(I, T, Vars, M) :-
	I1 is I-1,
	arg(I, M, Row),
	functor(Row, m, I1),
	m_array1(I, I1, T, Vars, Row),
	m_array(I1, T, Vars, M).

m_array1(_, 0, _, _, _) :- !.
m_array1(I, J, T, Vars, M) :-
	arg(J, M, Row),
	functor(Row, m, T),
	m_array2(I, J, T, Row, Vars),
	J1 is J-1,
	m_array1(I, J1, T, Vars, M).

m_array2(_, _, 0, _, _) :- !.
m_array2(I, J, T, Row, Vars) :-
	arg(T, Row, X),
	aget(h(I,T), Vars, Y),
	aget(h(J,T), Vars, Z),
	Y #= Z #<=> X,
	T1 is T-1,
	m_array2(I, J, T1, Row, Vars).

aget(h(I), vars(H,_,_), L) :-
	arg(I, H, X0),
	X0 =.. [h|L].
aget(h(I,T), vars(H,_,_), X) :-
	arg(I, H, X0),
	arg(T, X0, X).
aget(v(I,J,T), vars(_,V,_), X) :-
	arg(I, V, X0),
	arg(J, X0, X1),
	arg(T, X1, X).
aget(m(I,J), vars(_,_,M), L) :-
	arg(I, M, X0),
	arg(J, X0, X1),
	X1 =.. [m|L].
aget(m(I,J,T), vars(_,_,M), X) :-
	arg(I, M, X0),
	arg(J, X0, X1),
	arg(T, X1, X).

keymerge([], []).
keymerge([K-I,K-J|L1], L2) :- !, append(I,J,IJ), keymerge([K-IJ|L1], L2).
keymerge([X|L1], [X|L2]) :- keymerge(L1, L2).

