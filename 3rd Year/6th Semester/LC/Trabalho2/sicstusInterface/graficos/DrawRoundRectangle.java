package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawRoundRectangle implements GraphicsCmd {
     private int x,y,w,h,aw,ah;
     
     public DrawRoundRectangle(int x, int y, int w, int h, int arcWidth, int arcHeight) {
     	this.x = x;
     	this.y = y;
     	this.w = w;
     	this.h = h;
     	this.aw = arcWidth;
     	this.ah = arcHeight;
     }

     public void executar(Graphics g) {
     	g.drawRoundRect(x,y,w,h,aw,ah);
     }
}
