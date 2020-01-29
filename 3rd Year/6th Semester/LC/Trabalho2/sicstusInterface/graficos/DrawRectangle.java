package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawRectangle implements GraphicsCmd {
     private int x,y,w,h;
     
     public DrawRectangle(int x, int y, int w, int h) {
     	this.x = x;
     	this.y = y;
     	this.w = w;
     	this.h = h;
     }

     public void executar(Graphics g) {
     	g.drawRect(x,y,w,h);
     }
}
