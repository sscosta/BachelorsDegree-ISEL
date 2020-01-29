
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
:-use_module('solHC.pl').
:-include('solProf.pl').

modulo(solHC).
resolver(Xp,Yp,Xc,Yc,P) :- call(M:resolver(Xp,Yp,Xc,Yc,P)).


dMax(5000).