%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FILE
%    mm
%  AUTHOR
%    Greger Ottosson (greger@csd.uu.se)
%    Kristina Sirhuber (kiina@csd.uu.se)
%  PUPRPOSE
%   Implements the game of Mastermind.
%  HISTORY
%    greger - 1994-10-06 : Created.
% 

/* Expected output:
| ?- mm([6,5,6,3,2,3]).

My Guess: [1,1,1,1,1,1]  (Blacks,Whites) = 0,0
My Guess: [2,2,2,2,2,2]  (Blacks,Whites) = 1,0
My Guess: [2,3,3,3,3,3]  (Blacks,Whites) = 2,1
My Guess: [4,2,3,3,4,4]  (Blacks,Whites) = 1,2
My Guess: [5,2,5,5,3,3]  (Blacks,Whites) = 1,3
My Guess: [5,3,2,3,6,6]  (Blacks,Whites) = 1,5
My Guess: [6,5,3,2,3,6]  (Blacks,Whites) = 2,4
My Guess: [6,5,6,3,2,3]  (Blacks,Whites) = 6,0

Correct guess!!!
*/

:-use_module(library(clpfd)).
:-use_module(library(lists), [
	member/2,
	select/3
			     ]).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % This is the main predicate that is called when we start the
 % program. It reads the secret code and  goes into a loop. It always
 % start guessing with a list of ones.
mm :-
	write('Mata in en kod, en lista med nummer i [1..6]: '),
	read(Code),
	mm(Code).

mm(Code) :-
	build_first_guess(Code, Guess),
	game_loop(Code, Guess, []).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % Init the guesslist.					
build_first_guess([], []).
build_first_guess([_|T], [1|T2]) :-
	build_first_guess(T, T2).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % This is the main loop, which starts with one guess, and generates i
 % new one after computing the Blacks and Whites for the old one. It has
 % reached the end when the number Blacks equals the lenght of the code.
game_loop(Code, CurGuess, OldGuesses) :-
	write('My Guess: '), write(CurGuess),
	write('  (Blacks,Whites) = '),
	count_hit(Code, CurGuess, Blacks, Whites),
	write((Blacks,Whites)), nl,
	(   length(Code,Blacks)
	->  nl, nl, write('Correct guess!!!'), nl
	;   Guesses = [guess(CurGuess, CurOccs, Blacks, Whites)|OldGuesses],
	    color_counts(CurGuess, CurOccs),
	    next_guess(Guesses, NewGuess),
	    inc_labeling(NewGuess, CurGuess)
	->  game_loop(Code, NewGuess, Guesses)
	).


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % Our own labeling that instantiates the variables of the guess such
 % that this new guess is lexicographically greater than the previous one.

 % If the new color is equal to the old in the same position, then one of
 % the following colors must be increased.
inc_labeling([New|Ns], [Old|Os]) :-
	New #= Old,
	inc_labeling(Ns, Os).
 % If the new color is greater than the old, vanilla labeling can be used
 % on the rest --- we don't care what color they have.
inc_labeling([New|Ns], [Old|_]) :-
	New #> Old,		% Strictly greather than.
	labeling([],[New|Ns]).	% leftmost labeling

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % count_hit/4 counts the Blacks and Whites in a quite primitive
 % manner. :-) Since count_whites actually counts Blacks+Whites, we
 % subtract Blacks from Whites in the end to reach the correct answer.
count_hit(Code, Guess, Blacks, Whites) :-
	count_blacks(Code, Guess, Blacks),
	count_whites(Code, Guess, AllWhites),
	Whites is AllWhites - Blacks.
 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % count_blacks/3 counts the blacks by comparing the two lists element
 % by element.
count_blacks([],[],0).
count_blacks([C|Cs], [C|Gs], Blacks) :- !,
	count_blacks(Cs, Gs, NewBlacks),
	Blacks is NewBlacks + 1.
count_blacks([_|Cs], [_|Gs], Blacks) :-
	count_blacks(Cs, Gs, Blacks).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % count_whites/3 counts the number of whites plus the number of blacks in
 % the list. It uses recursion over the two lists in the following way:
 % - If G is found somewhere in [C|Cs], that element is removed.
 %   count_whites is called with the new code, and the rest of the
 %   guess. The number of whites are increased by 1.
 % - If on the other hand the selection fails, only a new call is made
 %   to count_whites with the whole code list and the rest of the
 %   guesses. The number of whites remains the same.
 % The predicate cheats a bit, since it does not count the real whites,
 % but the sum of whites and blacks. We therefore remove Blacks from
 % Whites in the calling predicate. (count_hit/4)
count_whites(_,[],0).
count_whites([C|Cs], [G|Gs], Whites) :-
	( select(G, [C|Cs], NewCs) ->
	  count_whites(NewCs, Gs, RestWhites),
	  Whites is RestWhites + 1
	;
	    count_whites([C|Cs], Gs, Whites)).


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % next_guess/2 is the predicate that is responsible for generating the
 % new guess according to the old guesses. It takes a list of guesses
 % as its first argument. That list consists of tuples of the following
 % format:
 % guess(OldGuess, OldOccs, Blacks, Whites) 
 % ..that is every old guess is coupled to its Blacks and Whites.
 % The predicate recurses over the whole list of these old guesses, so
 % to set the constraints. 
next_guess([], _).
next_guess([guess(OldGuess, OldOccs, Blacks, Whites)|T], NewGuess) :-
	set_black_constraint(OldGuess, Bs, NewGuess),
	sum(Bs, #=, Blacks),
	set_white_constraint(OldOccs, Blacks, Whites, NewGuess),
	next_guess(T, NewGuess).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % set_black_constraint/3 is sets the following constraint:
 % Blacks should be the number of equal elements in the old guess and
 % the new one. That is we want to keep at least Blacks number of
 % elements at the same place as before.
set_black_constraint([], [], []).
set_black_constraint([O1|Old], [B|Bs], [N1|New]) :-
	colors(MaxColor),
	N1 in 1..MaxColor,
	O1 #= N1 #<=> B,
	set_black_constraint(Old, Bs, New).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % set_white_constraint/4 associates to each color the number of
 % occurences in the old guess. Then call set_sum_constraint/3.
set_white_constraint(Occs, Blacks, Whites, New) :-
	set_sum_constraint(Occs, New, Terms),
	Sum is Blacks + Whites,
	sum(Terms, #=, Sum).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 % set_sum_contraint/3 sets the following constraints:
 % Sum = sum(min(NewCount,Count)) for every element in the new guess and
 % the old guess. In effect, this makes the new guess have as many color
 % equivalences to the old guess as the old guess had to the secret code.
set_sum_constraint([], _, []).
set_sum_constraint([Color-Count|Cl], NewList, [Term|Terms]) :-
	count(Color, NewList, #=, NewCount),
	Term #= min(NewCount,Count),
	set_sum_constraint(Cl, NewList, Terms).


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % color_counts/2 produces a list with elements in the format:
 % -(<A color>,<The number of occurences>)
 % For each different element in the list Old it creates a tuple in the list
 % Cl. 
color_counts(L, Occs) :-
	keytag(L, L1),
	keysort(L1, L2),
	keyfuse(L2, Occs).

keytag([], []).
keytag([X|L1], [X-1|L2]) :- keytag(L1, L2).

keyfuse([], []).
keyfuse([K1-I,K2-J|L1], L2) :- K1==K2, !, IJ is I+J, keyfuse([K1-IJ|L1], L2).
keyfuse([X|L1], [X|L2]) :- keyfuse(L1, L2).

 % The number of colors used.
colors(6).

