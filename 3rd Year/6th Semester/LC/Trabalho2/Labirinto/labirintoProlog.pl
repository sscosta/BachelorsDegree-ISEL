:-use_module(library(lists)).
% :-use_module(library(jasper)).


:-reconsult('resLab.pl').

:-use_module(graficos).

:-dynamic graphicsComp/1, java_vm/1, base/2, escala/1, saida/3, segundaCasa/0, 
          partida/2, chegada/2.



:-assert(base(10,10)).

                

labirintoNovoMundo(File) :- 
                grClear,
                abolish(escala/1),
                abolish(base/2),
                abolish(saida/3),
                reconsult(File), 
                base(Dx,Dy), 
                grGetPreferredWidth(MaxX),
                grGetPreferredHeight(MaxY), 
                calculaEscala(Dx,Dy,MaxX,MaxY,Esc), 
                assert(escala(Esc)),
                desenharLab,
                grRepaint.



calculaEscala(X,Y,MaxX,MaxY,Esc) :-
            (MaxX/X < MaxY/Y,!, Esc is MaxX/X; Esc is MaxY/Y).

desenharLab :-  
                base(Dx,Dy),
                escala(Esc),
                desenharQuadr(Dx,Dy,Esc), 
                findall(X/Y/D, saida(X,Y,D),L),
                grColor(0,0,255),
                desenharLigacoes(Esc,L),
                grColor(0,0,0).

desenharQuadr(Dx,Dy,Esc) :- DimL is round(Dx*Esc),
                            DimC is round(Dy*Esc), 
                            desenharLinhas(0,Dy,DimL,Esc), 
                            desenharColunas(0,Dx,DimC,Esc).

desenharLinhas(D,Dy,_,_) :- D > Dy,!.
desenharLinhas(N,Dy,Dl,Esc) :- 
                               Y is N*Esc,
                               grLine(0,Y,Dl,Y),
                               N1 is N + 1,
                               desenharLinhas(N1,Dy,Dl,Esc).

desenharColunas(D,Dx,_,_) :- D > Dx,!.
desenharColunas(N,Dx,Dc,Esc) :- X is N*Esc,
                                grLine(X,0,X,Dc),
                                N1 is N + 1,
                                desenharColunas(N1,Dx,Dc,Esc).
                               

desenharLigacoes(_,[]).
desenharLigacoes(Esc,[C|R]) :-
                 desenharTrans(Esc,C),
                 desenharLigacoes(Esc,R).
                 
desenharTrans(Esc,X/Y/D) :- novaCasa(X,Y,D,Xn,Yn),
                            desenharTrans1(Esc,X,Y,Xn,Yn).

desenharTrans1(Esc,X1,Y1,X2,Y2) :- 
                                  linha(Esc,X1,Y1,X2,Y2).


novaCasa(X,Y,b,X,Y1) :- Y1 is Y-1.
novaCasa(X,Y,c,X,Y1) :- Y1 is Y+1.
novaCasa(X,Y,d,X1,Y) :- X1 is X+1.
novaCasa(X,Y,e,X1,Y) :- X1 is X-1.
                                  

linha(Esc,X,Y1,X,Y2) :- !,
                    Xs is X - 1/Esc, Xi is X + 1/Esc,
                    linha1(Esc,Xs,Y1,Xs,Y2),
                    linha1(Esc,X,Y1,X,Y2),
                    linha1(Esc,Xi,Y1,Xi,Y2).

linha(Esc,X1,Y,X2,Y) :- !,
                    Ys is Y - 1/Esc, Yi is Y + 1/Esc,
                    linha1(Esc,X1,Ys,X2,Ys),
                    linha1(Esc,X1,Y,X2,Y),
                    linha1(Esc,X1,Yi,X2,Yi).


linha1(Esc,X1,Y1,X2,Y2) :-
                           X1e is round((X1+0.5)*Esc),
                           Y1e is round((Y1+0.5)*Esc),
                           X2e is round((X2+0.5)*Esc),
                           Y2e is round((Y2+0.5)*Esc),
                           grLine(X1e,Y1e,X2e,Y2e).

                                  

labirintoNovoPonto(Xp, Yp) :- % chamada nos eventos do rato
                   base(Dx,Dy),
                   escala(Esc),
                   X1 is integer(Xp/Esc),
                   Y1 is integer(Yp/Esc),
                   X1 < Dx,
                   Y1 < Dy,
                   (segundaCasa,!,retract(segundaCasa),abolish(chegada/2),
                    assert(chegada(X1,Y1)),cruz(X1,Y1),labirintoResolver;
                    assert(segundaCasa),abolish(partida/2),assert(partida(X1,Y1)),
                    desenharLab,
                    cruz(X1,Y1)).

cruz(X,Y) :- escala(Esc), 
             Xm is round((X+0.5)*Esc),
             Ym is round((Y+0.5)*Esc),
             Y1 is Ym-5,
             Y2 is Ym+5,
             X1 is Xm-5,
             X2 is Xm+5,
             grColor(255,0,0),
             grLine(X1,Ym,X2,Ym),
             grLine(Xm,Y1,Xm,Y2),
             grColor(0,0,0),
             grRepaint.
                    
                   


labirintoResolver :- 
                (labirinto(Solucao),!; Solucao=[]),
                grClear,
                desenharLab,
                escala(Esc),
                grColor(255,0,0),                
                desenharPercurso(Esc,Solucao),
                grColor(0,0,0),!.
                
desenharPercurso(Esc,[]).
desenharPercurso(Esc,[_]).
desenharPercurso(Esc,[e(X1,Y1),e(X2,Y2)|R]) :- desenharTrans1(Esc,X1,Y1,X2,Y2),
                                               desenharPercurso(Esc,[e(X2,Y2)|R]).
                                               