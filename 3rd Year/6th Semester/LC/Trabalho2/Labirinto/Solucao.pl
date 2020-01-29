
/* Labirinto:

O problema:
      Pretende-se um programa que procure caminhos num labirinto
      inscrito numa quadricula.
      
      Só existem uma casa de partida e uma casa de chegada.
      
      Cada casa pode ter um máximo de 4 saídas (bidireccionais),
      indicadas por factos do tipo saida(X,Y,Aresta), onde
      Aresta pertence ao conjunto
             {d(ireita),e(squerda),c(ima),b(aixo)}
             

Exemplo:
       +---+---+---+---+---+---+---+---+---+---+
     9 | P |   /   /   /   |   |   |   |   |   |
       +-/-+-/-+---+-/-+-/-+---+---+---+---+---+
     8 |   /   /   |   |   |   |   |   |   |   |
       +-/-+-/-+---+---+-/-+---+---+---+---+---+
     7 |   |   |   |   |   |   |   |   |   |   |
       +---+---+---+---+-/-+-/-+---+---+---+---+
     6 |   |   |   |   |   |   |   |   | C /   |
       +---+---+---+---+-/-+-/-+---+---+---+-/-+
     5 |   |   |   |   |   |   |   |   |   |   |
       +---+---+---+---+-/-+-/-+---+---+---+-/-+
     4 |   |   |   |   |   /   /   |   |   |   |
       +---+---+---+---+---+-/-+-/-+---+---+-/-+
     3 |   |   |   |   |   |   |   /   /   /   |
       +---+---+---+---+---+-/-+-/-+---+---+---+
     2 |   | P |   |   |   |   /   /   |   |   |
       +---+-/-+---+---+---+-/-+---+-/-+---+---+
     1 |   |   |   |   |   |   |   |   |   |   |
       +---+-/-+---+---+---+-/-+---+-/-+---+---+
     0 |   |   /   /   /   /   |   |   /   / C |
       +---+---+---+---+---+---+---+---+---+---+
         0   1   2   3   4   5   6   7   8   9

Uma forma sistemática de gravar o labirinto consiste em
percorrer a quadricula de cima para baixo e da esquerda 
para a direita escrevendo, por cada casa, tantos factos 
saida(x,y,aresta) quantos os nela aplicaveis.            

Ver ficheiros labirinto.pl e labirintoGrande.pl
        
        
*/


:-use_module('labirinto.pl').

%Ponto 6
resolver(Xp,Yp,Xc,Yc,P) :- dMax(DMax), resolverLabirinto(casa(Xp,Yp),casa(Xc,Yc),DMax,Dfinal,P), asserta(dPerc(Dfinal)).

dMax(5000).


melhorEstado(EstadoAct,EstadoNovo) :- findall(H-Suc, novoEstadoHC(EstadoAct, Suc,H), L), ordenarPorH(L,Lo), member(_-EstadoNovo,Lo).
ordenarPorH(L,Lo) :- keysort(L,Lo).
transformar([],[]).
transformar([estado(casa(X,Y),_)|R],[e(X,Y)|R1]):-transformar(R,R1).

%Ponto 5 -  resolverLabirinto(+Casa1,+Casa2,+Dmax,-Percurso/-Dist)
resolverLabirinto(Origem,Destino,Dmax,Dfinal,P) :- pesqProfHC(estado(Origem,Destino)/Dmax/0,[estado(Origem,Destino)],P/Dfinal).

pesqProfHC(EstActual,Visitados,Percurso) :- melhorEstado(EstActual,EstNovo), podeJuntar(EstNovo, Visitados, NovosVis), pesqProfHC(EstNovo,NovosVis,Percurso).
pesqProfHC(E,V,S):- final(E,V,S),!.
final(Ea/Dmax/Dactual,Visitados,Sol/Dactual):- final1(Ea),transformar(Visitados,Sol).
final1(estado(Cfinal,Cfinal)).

%Ponto 4 - novoEstadoHC(+EstadoCorrente,NovoEstado,-H)
novoEstadoHC(estado(Cactual,casa(Xf,Yf))/Dmax/Dant,estado(casa(Xn,Yn),casa(Xf,Yf))/Dmax/Dnovo,H) :- ligaBid(Cactual,casa(Xn,Yn),D), H is D + abs(Xf - Xn) + abs(Yf - Yn), Dnovo is Dant + D, Dnovo =< Dmax.

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


liga(casa(X,Y),casa(Z,Y)) :- saida(X,Y,d), Z is X + 1.
liga(casa(X,Y),casa(Z,Y)) :- saida(Z,Y,e), X is Z - 1.


ligaBid(X,Y,1) :- liga(X,Y).
ligaBid(X,Y,1) :- liga(Y,X).

%ligaBid(X,Y) :- liga(X,Y).
%ligaBid(X,Y) :- liga(Y,X).
