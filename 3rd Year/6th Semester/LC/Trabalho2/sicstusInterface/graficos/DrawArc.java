package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawArc implements GraphicsCmd {
     private int x, y, w, h, startAngle, arcAngle;
     
     public DrawArc(int x, int y, int w, int h, int startAngle, int arcAngle ) {
     	this.x = x;
     	this.y = y;
     	this.w = w;
     	this.h = h;
     	this.startAngle = startAngle;
     	this.arcAngle = arcAngle;
     	
     }

     public void executar(Graphics g) {
     	g.drawArc(x,y,w,h,startAngle,arcAngle);
     }
}
