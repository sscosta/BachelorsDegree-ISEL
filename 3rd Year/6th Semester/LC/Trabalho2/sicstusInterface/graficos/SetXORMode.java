package wv.isel.ssic.lc.sicstus.graficos;

import java.awt.*;

public class SetXORMode implements GraphicsCmd {
     private Color c;
     
     public SetXORMode(int r, int g, int b ) {
     	// r, g, b no intervalo [0,255]
     	 c = new Color(r,g,b);
     	
     }

     public void executar(Graphics g) {
     	g.setXORMode(c);
     }
}
