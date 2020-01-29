/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Social Golfer Problem
 * Author    : Mats Carlsson
 *
 * We have 32 golfers, individual play.
 * We will golf for 16 weeks.
 * Set up the foursomes so that each person only golfs with the same
 * person once.
 * How many weeks can we do this before is starts to duplicate?
 */

% Best luck so far:
% | ?- golf(6,8,_R,[ff],bycol).

:- module(golf, [golf/5]).

:- use_module(library(clpfd)).

golf(NWeeks, RoundSize, Rounds, Opt, VarOrder) :-
	% variables
	NPlayers is RoundSize<<2,
	rounds(0, NWeeks, RoundSize, Rounds),
	first_round(Rounds, OtherRounds),
	% essential
	round_constraints(OtherRounds),
	rounds_meets(Rounds, NPlayers, Meets, []),
%alt 	all_different(Meets),
	disjoint2(Meets),
	% symmetries
	rounds_order(OtherRounds),
	% implied
	rounds_cols(OtherRounds, L1, L2, L3, L4),
	counts_col1(0, NPlayers, NWeeks, up, Bounds1, []),
	counts_col2(0, NPlayers, NWeeks, up, Bounds2, []),
	counts_col2(0, NPlayers, NWeeks, down, Bounds3, []),
	counts_col1(0, NPlayers, NWeeks, down, Bounds4, []),
	global_cardinality(L1, Bounds1),
	global_cardinality(L2, Bounds2),
	global_cardinality(L3, Bounds3),
	global_cardinality(L4, Bounds4),
	% search
	variables(VarOrder, OtherRounds, All),
	label_sets(All, Opt),
	display_rounds(Rounds, 0).

label_sets([], _).
label_sets([Set|Sets], Opt) :-
	labeling(Opt, Set), % !,
	label_sets(Sets, Opt).

variables(byrow, OtherRounds, All) :-
	rows_by_week(OtherRounds, All, []).
variables(bycol, OtherRounds, All) :-
	cols_by_week(OtherRounds, All, []).

rows_by_week([]) --> [].
rows_by_week([Round|Rounds]) --> [All],
	{prolog:'$term_variables'(Round, All)},
	rows_by_week(Rounds).

cols_by_week([]) --> [].
cols_by_week([Round|Rounds]) --> [L1],
	{cols_of_week(Round, L1, L2, L2, L3, L3, L4, L4, [])},
	cols_by_week(Rounds).

cols_of_week([], L1, L1, L2, L2, L3, L3, L4, L4).
cols_of_week([[A,B,C,D]|Round], [A|L1], L2, [B|L3], L4, [C|L5], L6, [D|L7], L8) :-
	cols_of_week(Round, L1, L2, L3, L4, L5, L6, L7, L8).

c(S0, S, S0, S).

display_rounds([], _).
display_rounds([Round|Rounds], V) :-
	W is V+1,
	format('Week ~d:\n', [W]),
	display_round(Round),
	display_rounds(Rounds, W).

display_round([]).
display_round([Four|Round]) :-
	format('                    ~d ~d ~d ~d\n', Four),
	display_round(Round).

rounds(N, N, _, []) :- !.
rounds(I, N, Size, [Round|Rounds]) :-
	round(0, Size, Round),
	J is I+1,
	rounds(J, N, Size, Rounds).

round(N, N, []) :- !.
round(I, N, [[_,_,_,_]|Round]) :-
	J is I+1,
	round(J, N, Round).

first_round([Round|OtherRounds], OtherRounds) :-
	prolog:'$term_variables'(Round, All),
	enum_round(All, 0).

enum_round([], _).
enum_round([I|Xs], I) :-
	J is I+1,
	enum_round(Xs, J).

round_constraints([]).
round_constraints([Round|Rounds]) :-
	prolog:'$term_variables'(Round, All),
	All = [0|Rest],
	length(Rest, N),
	domain(Rest, 1, N),
	all_distinct(Rest),
	% symmetries
	order(Round),
	round_constraints(Rounds).

order([[A,B,C,D]|Rest]) :-
	A+4#=<B, B+4#=<C, C+4#=<D,
	(   Rest==[] -> true
	;   Rest = [[E|_]|_],
	    A#<E,
	    order(Rest)
	).

rounds_order([]).
rounds_order([Round|Rounds]) :-
	Round = [[0|_],[1|_],[2|_],[3|_]|_],
	rounds_order(Rounds).

rounds_meets([], _NPlayers) --> [].
rounds_meets([Round|Rounds], NPlayers) -->
	round_meets1(Round, NPlayers),
	rounds_meets(Rounds, NPlayers).

round_meets1([], _NPlayers) --> [].
round_meets1([[A,B,C,D]|Round], NPlayers) -->
	round_meet(A, B, NPlayers),
	round_meet(A, C, NPlayers),
	round_meet(A, D, NPlayers),
	round_meet(B, C, NPlayers),
	round_meet(B, D, NPlayers),
	round_meet(C, D, NPlayers),
	round_meets1(Round, NPlayers).

%alt round_meet(A, B, NPlayers) --> [AB],
%alt 	{AB #= NPlayers*A + B}.

round_meet(A, B, _NPlayers) --> [r(A,1,B,1)].

counts_col1(I, P, _W, _) -->
	{I+12 >= P}, !.
counts_col1(I, P, W, Key) -->
	{I < 4}, !,
	{W1 is W-1},
	gcc_item(Key, P, I, W1, W1),
	{J is I+1},
	counts_col1(J, P, W, Key).
counts_col1(I, P, W, Key) -->
	{I00 is I /\ -4},
	{Min is W-I00},
	{Max is (P-I00-4)//3},
	gcc_item(Key, P, I, Min, Max),
	{J is I+1},
	counts_col1(J, P, W, Key).

counts_col2(I, P, _W, _) -->
	{I+8 >= P}, !.
counts_col2(I, P, W, Key) -->
	{I00 is I /\ -4},
	{Max is min((P-I00-4)//2,I00)},
	gcc_item(Key, P, I, 0, Max),
	{J is I+1},
	counts_col2(J, P, W, Key).

gcc_item(_, _, _Val, 0, 0) --> !.
gcc_item(up, _, Val, Min, Max) --> [Val-C],
	{C in Min..Max}.
gcc_item(down, P, Val, Min, Max) --> [Val1-C],
	{Val1 is P-1-Val},
	{C in Min..Max}.


rounds_cols([], [], [], [], []).
rounds_cols([Round|Rounds], L1, L2, L3, L4) :-
	round_cols(Round, L1, L1b, L2, L2b, L3, L3b, L4, L4b),
	rounds_cols(Rounds, L1b, L2b, L3b, L4b).

round_cols([], L1, L1, L2, L2, L3, L3, L4, L4).
round_cols([[A,B,C,D]|Round], [A|L1], L2, [B|L3], L4, [C|L5], L6, [D|L7], L8) :-
	round_cols(Round, L1, L2, L3, L4, L5, L6, L7, L8).

% % better pruning for X for:
% % X#<Y, X#<Z, Y#\=Z
% ltboth(X, Y, Z) +:
% 	X in (inf .. (max(Y)-1)) /\
% 	     (inf .. (max(Z)-1)) /\
% 	     ((inf .. (max(Y)-2)) \/ (inf .. (max(Z)-2))),
% 	Y in (min(X)+1) .. sup,
% 	Z in (min(X)+1) .. sup.

end_of_file.

Occurrences in the four columns:
W weeks, G groups, P players, foursomes.

            Column 1        Column 2            Column 3        Column 4
            ********        ********            ********        ********

 0..3          W-1              0               Rev(Column 2)   Rev(Column 1)
 4..7    W-4..(P-8)/3     0..min(4,(P-8)/2)     
 8..11   W-8..(P-12)/3    0..min(8,(P-12)/2)    
12..15  W-12..(P-16)/3    0..min(12,(P-16)/2)   
16..19  W-16..(P-20)/3    0..min(16,(P-20)/2)   
20..23        0           0..min(20,(P-24)/2)   
24..27        0                 0               
28..31        0                 0               

