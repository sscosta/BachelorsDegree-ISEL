package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawPolygon implements GraphicsCmd {
     private int[] px;
     private int[] py;
     private int np;
     
     public DrawPolygon(int[] px, int[] py, int np ) {
     	this.px = px;
     	this.py = py;
     	this.np = np;
     	
     }

     public void executar(Graphics g) {
     	g.drawPolygon(px,py,np);
     }
}
