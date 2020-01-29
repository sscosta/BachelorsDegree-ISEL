:-include('solucao.pl').

/* Labirinto:

O problema:
      Pretende-se um programa que procure caminhos num labirinto
      inscrito numa quadricula.
      
      A (UNICA) casa de partida e a (UNICA) casa de chegada s�o dadas 
      por factos dos tipos partida(X,Y) e chegada(X,Y).
      
      Cada casa pode ter um m�ximo de 4 sa�das (bidireccionais),
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

Uma forma sistem�tica de gravar o labirinto consiste em
percorrer a quadricula de cima para baixo e da esquerda 
para a direita escrevendo, por cada casa, tantos factos 
saida(x,y,aresta) quantos os nela aplicaveis.            

A solucao:
      Consideremos os seguintes aspectos importantes:
        1- N�o existem erros na defini��o do labirinto.
        2- A informa��o de estado apenas tem de conter a casa actual.
        
        Informacao de estado: e(X,Y)
        
        
        
*/

labirinto(P) :- 
                partida(Xi,Yi),
                chegada(Xc,Yc),
                resolver(Xi,Yi,Xc,Yc,P).

