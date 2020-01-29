package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class DrawString implements GraphicsCmd {
     private String str;
     private int x,y;
     
     public DrawString(String str, int x, int y) {
     	this.str = str;
     	this.x = x;
     	this.y = y;
     	
     }

     public void executar(Graphics g) {
     	g.drawString(str,x,y);
     }
}
