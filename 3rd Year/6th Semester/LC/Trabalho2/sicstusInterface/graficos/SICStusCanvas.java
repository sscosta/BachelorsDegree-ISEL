package wv.isel.ssic.lc.sicstus.graficos;


import se.sics.jasper.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;
import java.io.Serializable;



public class SICStusCanvas extends Canvas {

        private Vector comandosGraficos = new Vector();
        protected SICStus sp = null;
        protected Prolog pi = null;
	
	private Graphics grContext = null;
	
	

	public SICStusCanvas(Prolog pi,SICStus sp) {
                this.pi = pi;
                this.sp = sp;

       }


       public void initPrologGraphics() {
		try {
                   SPTerm term = new SPTerm(sp);
                   term.putObject(this);
                   HashMap varMap = new HashMap();
	           varMap.put("GraphicsComp",term);
	           if(sp.query("graficos:sicstusCanvasInit(GraphicsComp).",varMap)) {
                   }
                   else {
                   }
         
	        } catch ( Exception e ) {
	           e.printStackTrace();
	        }

        }

	
        public void pintar() {
             repaint();
             // paint(getGraphics());
        }        
        
        
        public void setColor(int r, int g, int b) {
        	comandosGraficos.add(new SetColor(r,g,b));
        }
        
        public void clear() {
        	comandosGraficos.clear();
        	repaint();
        }

        public void setXORMode(int r, int g, int b) {
        	comandosGraficos.add(new SetXORMode(r,g,b));
        }
        
        
        public void drawLine(int x1, int y1, int x2, int y2) {
        	comandosGraficos.add(new DrawLine(x1,y1,x2,y2));
        }
        
        public void drawOval(int x, int y, int w, int h) {
        	comandosGraficos.add(new DrawOval(x,y,w,h));
        }

        public void fillOval(int x, int y, int w, int h) {
        	comandosGraficos.add(new FillOval(x,y,w,h));
        }
        
	public void drawRect(int x, int y, int w, int h) {
        	comandosGraficos.add(new DrawRectangle(x,y,w,h));
        }

	public void fillRect(int x, int y, int w, int h) {
        	comandosGraficos.add(new FillRectangle(x,y,w,h));
        }

	public void drawRoundRect(int x, int y, int w, int h, int aw, int ah) {
        	comandosGraficos.add(new DrawRoundRectangle(x,y,w,h,aw,ah));
        }
        
	public void fillRoundRect(int x, int y, int w, int h, int aw, int ah) {
        	comandosGraficos.add(new FillRoundRectangle(x,y,w,h,aw,ah));
        }

	public void drawArc(int x, int y, int w, int h, int sa, int aa) {

       	        comandosGraficos.add(new DrawArc(x,y,w,h,sa,aa));
        }

	public void drawString(String s, int x, int y) {
        	comandosGraficos.add(new DrawString(s,x,y)); //paint(getGraphics());
 System.out.println("DrawString");
        }

	public void drawPolygon(Term tx, Term ty, int np) {
	        int[] px = new int[np];
		int[] py = new int[np];
	        try {
		    Term[] tax = tx.toPrologTermArray();
		    Term[] tay = ty.toPrologTermArray();
		
		    for(int i = 0; i < np; ++i) {
			px[i] = (int) tax[i].getInteger();
			py[i] = (int) tay[i].getInteger();
		    }
                } catch(Exception e) {
                     e.printStackTrace();
                }
		
       	        comandosGraficos.add(new DrawPolygon(px,py,np));
        }

	public void fillPolygon(Term tx, Term ty, int np) {
	        int[] px = new int[np];
		int[] py = new int[np];
	        try {
		    Term[] tax = tx.toPrologTermArray();
		    Term[] tay = ty.toPrologTermArray();
		
		    for(int i = 0; i < np; ++i) {
			px[i] = (int) tax[i].getInteger();
			py[i] = (int) tay[i].getInteger();
		    }
                } catch(Exception e) {
                     e.printStackTrace();
                }
       	        comandosGraficos.add(new FillPolygon(px,py,np));
        }
        
        public void drawPolyline(Term tx, Term ty, int np) {
	        int[] px = new int[np];
		int[] py = new int[np];
	        try {
		    Term[] tax = tx.toPrologTermArray();
		    Term[] tay = ty.toPrologTermArray();
		
		    for(int i = 0; i < np; ++i) {
			px[i] = (int) tax[i].getInteger();
			py[i] = (int) tay[i].getInteger();
		    }
                } catch(Exception e) {
                     e.printStackTrace();
                }
       	        comandosGraficos.add(new DrawPolyline(px,py,np));
        }


        public int getPreferredWidth() {
        	return (int) getPreferredSize().getWidth();
        }


        public int getPreferredHeight() {
        	return (int) getPreferredSize().getHeight();
        }

        public void sicstusCanvasPaint(Graphics g) {
           Vector cmds = (Vector) comandosGraficos.clone();
           ListIterator li = cmds.listIterator();
           while(li.hasNext()) {
        	((GraphicsCmd)li.next()).executar(g);
           }
        }
        
       public Dimension  getPreferredSize() {return new Dimension(800,600);}

        public void paint( Graphics g ) {
  System.out.println("Em paint");
           sicstusCanvasPaint(g);
        }
        
               
  
}


  