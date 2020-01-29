package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawOval implements GraphicsCmd {
     private int x,y,w,h;
     
     public DrawOval(int x, int y, int w, int h) {
     	this.x = x;
     	this.y = y;
     	this.w = w;
     	this.h = h;
     }

     public void executar(Graphics g) {
     	g.drawOval(x,y,w,h);
     }
}
