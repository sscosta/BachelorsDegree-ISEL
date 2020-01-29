package wv.isel.ssic.lc.sicstus;

import se.sics.jasper.*;


public class SICStusFrontEnd { //extends Thread {
           private SICStus sp = null;    
           private Prolog pi=null;
           
           public SICStusFrontEnd() {
             try {
 	        if (null == (sp = SICStus.getInitializedSICStus())) {
		    sp = new SICStus();
                }
                pi = sp.newProlog();
	      } catch ( Exception e ) {
	         e.printStackTrace();
	      }

          }
        
          public void load(String pfile) {
             try {
 	        if (sp != null) 
	            sp.load(pfile);  // para ficheiros .pl
	        // sp.restore("layout.po"); // para ficheiros .po ou .sav
             }catch ( Exception e ) {
	         e.printStackTrace();
	     }
          }
        

          public void restore(String pfile) {
             try {
 	        if (sp != null) 
	            sp.restore(pfile); // para ficheiros .po ou .sav
             }catch ( Exception e ) {
	         e.printStackTrace();
	     }
          }
          
          public Prolog getPrologInterface() { 
          	return pi;
          }
          
          public SICStus getPrologEngine() { 
          	return sp;
          }
          
          public void start() {
   	    try {
   	    	sp.startServer();
   	    }
   	    catch(Exception e) {
   	    	e.printStackTrace();
   	    }
          }
          
          public void stop() {
   	    try {
   	    	sp.stopServer();
   	    }
   	    catch(Exception e) {
   	    	e.printStackTrace();
   	    }
          }

}
