%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FILE
%    photo
%  AUTHOR
%    Greger Ottosson (greger@csd.uu.se)
%  HISTORY
%    greger - 1996-05-24 : Created.
%			   (Adapted from Oz program)
% 

:- use_module(library(clpfd)).

% [min] is the best heuristic.
% all_distinct really helps a lot.
% This is the overall winner with Lab = [min] and Size = 5.
photo_bb(Lab, Size, NextTo, L, Benefit) :-
	problem(Size, NextTo, L, Benefit),
	labeling([maximize(Benefit)|Lab], L).

% This is the overall winner with Lab = [min] and Size = 7.
photo_iter(Lab, Size, NextTo, L, Benefit) :-
	problem(Size, NextTo, L, Benefit),
	labeling([down],[Benefit]),
	labeling(Lab, L), !.

problem(Size, NextTo, L, Benefit) :-
	L = [Alice,Bert|_],
	problem(Size, L, Pairs),
	domain(L, 1, Size),
	next_to(Pairs, NextTo, Bs),
	sum(Bs, #=, Benefit),
	Alice #< Bert,
        all_distinct(L).

next_to([], _, []).
next_to([[X,Y]|Ps], Param, [B|Bs]) :-
	(   Param=:=1 -> D #= X-Y, D in -1..1 #<=> B
	;   Param=:=2 -> abs(X-Y) #=< 1 #<=> B
	;   Param=:=3 -> X#=Y+1 #\ Y#=X+1 #<=> B
	),
	next_to(Ps, Param, Bs).

problem(5, [Alice,Bert,Chris,Deb,Evan], 
	  [[Alice,Chris], [Bert,Evan],
	   [Chris,Deb],   [Chris,Evan],  
	   [Deb,Alice],   [Deb,Evan],
	   [Evan,Alice],  [Evan,Bert]]).
problem(7, [Alain,Beatrice,Christian,Daniel,Eliane,Francois,Gerard],
	[[Beatrice,Gerard],
	 [Beatrice,Eliane],
	 [Beatrice,Christian],
	 [Francois,Eliane],
	 [Francois,Daniel],
	 [Francois,Alain],
	 [Alain,Daniel],
	 [Gerard,Christian]]).
