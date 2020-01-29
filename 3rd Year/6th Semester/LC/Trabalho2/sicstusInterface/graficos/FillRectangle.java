package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class FillRectangle implements GraphicsCmd {
     private int x,y,w,h;
     
     public FillRectangle(int x, int y, int w, int h) {
     	this.x = x;
     	this.y = y;
     	this.w = w;
     	this.h = h;
     }

     public void executar(Graphics g) {
     	g.fillRect(x,y,w,h);
     }
}
