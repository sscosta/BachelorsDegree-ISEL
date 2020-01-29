:-module(resolverProf,resolver/5).

%Ponto 6
resolver(Xp,Yp,Xc,Yc,P) :- dMax(DMax), resolverLabirinto(casa(Xp,Yp),casa(Xc,Yc),DMax,Dfinal,P), asserta(dPerc(Dfinal)).

%Ponto 5 -  resolverLabirinto(+Casa1,+Casa2,+Dmax,-Percurso/-Dist)
resolverLabirinto(Origem,Destino,Dmax,Dfinal,P) :- pesqProf(estado(Origem,Destino)/Dmax/0,[estado(Origem,Destino)],P/Dfinal).

pesqProf(E,V,S):- novoEstado(E,EstNovo), podeJuntar(EstNovo,V,NovosVis), pesqProf(EstNovo,NovosVis,S).
pesqProf(E,V,S):- final(E,V,S),!.
final(Ea/Dmax/Dactual,Visitados,Sol/Dactual):- final1(Ea),transformar(Visitados,Sol).
final1(estado(Cfinal,Cfinal)).

transformar([],[]).
transformar([estado(casa(X,Y),_)|R],[e(X,Y)|R1]):-transformar(R,R1).

%Ponto 4 - novoEstadoHC(+EstadoCorrente,NovoEstado,-H)
%novoEstadoHC(estado(Cactual,casa(Xf,Yf),estado(casa(Xn,Yn),casa(Xf,Yf)),H) :- ligaBid(Cactual,casa(Xn,Yn),D), H is D + sqrt((Xf - Xn)  * (Xf - Xn) + (Yf - Yn) *(Yf - Yn)).

%Ponto 3 - novoEstado(+EstadoCorrente,NovoEstado) 
novoEstado(estado(Cactual,Cfinal)/Dmax/Dant, estado(Cnova,Cfinal)/Dmax/Dnovo) :- ligaBid(Cactual,Cnova,D1), Dnovo is Dant + D1, Dnovo =< Dmax.

%Ponto 2
podeJuntar1(E,[],[E]).
podeJuntar1(E,[C|R],[C|L]) :- E \= C, podeJuntar1(E,R,L).
podeJuntar(E/Dmax/Dnovo,Visitados,NovosVisitados) :- podeJuntar1(E, Visitados, NovosVisitados).


%Ponto 1
%connections between cells
% move up
liga(casa(X,Y),casa(X,Z)) :- saida(X,Y,c), Z is Y + 1.
liga(casa(X,Y),casa(X,Z)) :- saida(X,Z,b), Y is Z - 1.

% move right
liga(casa(X,Y),casa(Z,Y)) :- saida(X,Y,d), Z is X + 1.
liga(casa(X,Y),casa(Z,Y)) :- saida(Z,Y,e), X is Z - 1.

ligaBid(X,Y,1) :- liga(X,Y).
ligaBid(X,Y,1) :- liga(Y,X).