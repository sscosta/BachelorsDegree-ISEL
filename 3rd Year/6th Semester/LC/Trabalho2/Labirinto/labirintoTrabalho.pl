base(4,4).
% dimensões x,y do labirinto
saida(1,3,b).
% da casa 1,3 pode transitar-se para a casa 1,2
saida(0,2,d).
% da casa 0,2 pode transitar-se para a casa 1,2
saida(0,2,b).

% da casa 0,2 pode transitar-se para a casa 0,1
saida(1,2,b).
% da casa 1,2 pode transitar-se para a casa 1,1
saida(1,2,d).
% da casa 1,2 pode transitar-se para a casa 2,2
saida(0,1,b).
% da casa 0,1 pode transitar-se para a casa 0,0
saida(1,1,d).
% da casa 1,1 pode transitar-se para a casa 2,1
saida(2,1,d).         
% da casa 2,1 pode transitar-se para a casa 3,1
saida(3,1,b).
% da casa 3,1 pode transitar-se para a casa 3,0
saida(0,0,d).          
% da casa 0,0 pode transitar-se para a casa 1,0
saida(1,0,d).            
% da casa 1,0 pode transitar-se para a casa 2,0
saida(2,0,d).            
% da casa 2,0 pode transitar-se para a casa 3,0 