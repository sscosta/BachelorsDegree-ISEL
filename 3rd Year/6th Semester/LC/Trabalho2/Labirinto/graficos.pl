:- module(graficos, [myPrologInit/1,grRepaint/0,grLine/5,grLine/4,grOval/4,grFillOval/4,
                     grRect/4,grFillRect/4,grString/3, desenhar/6, getGraphics/1,
                     grColor/3, grXORMode/3, grGetWidth/1, grGetHeight/1, grGetPreferredWidth/1, grGetPreferredHeight/1,
                     grRoundRect/6, grFillRoundRect/6,
                     grArc/6, grPolygon/3,grFillPolygon/3,grPolyline/3, grClear/0]).

:-use_module(library(jasper)).
:-dynamic graphicsComp/1, java_vm/1, graphics/1.



                     
sicstusCanvasInit(GraphicsComp) :-
           assert(graphicsComp(GraphicsComp)),
           jasper_initialize(JVM),
           assert(java_vm(JVM)).
           %grRepaint.               


grRepaint :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'pintar', [instance]),
                      pintarJava(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas')),
                      pintarJava(Gc)
                     ).

grClear :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('',    % para [instance] pode ser assim
                              'clear', [instance]),
                      clr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas')),
                      clr(Gc)
                     ).

grGetWidth(W) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'getWidth', [instance]),
                      getW(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),[-integer]),
                      getW(Gc,W)
                     ).


grGetPreferredWidth(W) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'getPreferredWidth', [instance]),
                      getW(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),[-integer]),
                      getW(Gc,W)
                     ).



grGetHeight(H) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'getHeight', [instance]),
                      getH(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),[-integer]),
                      getH(Gc,H)
                     ).
                     
                     
grGetPreferredHeight(H) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'getPreferredHeight', [instance]),
                      getH(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),[-integer]),
                      getH(Gc,H)
                     ).                     

grColor(R,G,B) :- %Graphics nao e' usado nesta versao
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'setColor', [instance]),
                      setColor(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer),
                      setColor(Gc,R,G,B)
                     ).

grXORMode(R,G,B) :- %Graphics nao e' usado nesta versao
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'setXORMode', [instance]),
                      xMode(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer),
                      xMode(Gc,R,G,B)
                     ).

grLine(X1,Y1,X2,Y2) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawLine', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer),
                      linha(Gc,X1,Y1,X2,Y2)
                     ).
grOval(X,Y,W,H) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawOval', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer),
                      linha(Gc,X,Y,W,H)
                     ). 


grFillOval(X,Y,W,H) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'fillOval', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer),
                      linha(Gc,X,Y,W,H)
                     ). 

grRect(X,Y,W,H) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawRect', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer),
                      linha(Gc,X,Y,W,H)
                     ).

grFillRect(X,Y,W,H) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'fillRect', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer),
                      linha(Gc,X,Y,W,H)
                     ).

grRoundRect(X,Y,W,H, ArcWidth, ArcHeight) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawRoundRect', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer,+integer,+integer),
                      rr(Gc,X,Y,W,H, ArcWidth, ArcHeight)
                     ).

grFillRoundRect(X,Y,W,H, ArcWidth, ArcHeight) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'fillRoundRect', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer,+integer,+integer),
                      rr(Gc,X,Y,W,H, ArcWidth, ArcHeight)
                     ).


grArc(X,Y,W,H, StartAngle, ArcAngle) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawArc', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+integer,+integer,+integer,+integer,+integer,+integer),
                      rr(Gc,X,Y,W,H, StartAngle, ArcAngle)
                     ).

grPolyline(Lx,Ly,Np) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawPolyline', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+term,+term,+integer),
                      rr(Gc,Lx,Ly,Np)
                     ).

grPolygon(Lx,Ly,Np) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawPolygon', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+term,+term,+integer),
                      rr(Gc,Lx,Ly,Np)
                     ).

grFillPolygon(Lx,Ly,Np) :-
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'fillPolygon', [instance]),
                      rr(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+term,+term,+integer),
                      rr(Gc,Lx,Ly,Np)
                     ).
     

grString(Str,X,Y) :- 
          java_vm(JVM),
          graphicsComp(Gc),
          jasper_call(JVM,
                      method('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas',
                              'drawString', [instance]),
                      linha(+object('wv.isel.ssic.lc.sicstus.graficos.SICStusCanvas'),+string,+integer,+integer),
                      linha(Gc,Str,X,Y)
                     ).



