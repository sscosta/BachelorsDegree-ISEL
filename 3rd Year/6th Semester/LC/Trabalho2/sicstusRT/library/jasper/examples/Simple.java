/*
 * Simple.java: a couple of simple Prolog-Java examples
 */

import se.sics.jasper.*;
import java.util.HashMap;

public class Simple
{
    private String a;
    private int x;
	
    public Simple(String a, int b) {
	this.a = a + " " + b;
    }

    public static int square(int x) {
	return x*x;
    }

    public String get() {
	return a;
    }

    public void set(String b) {
	a = b;
    }

    public void append(String b) {
	a = new String(a + b);
    }

    public static SPTerm train(String filename)
    {
	SICStus sp;
	/* Necessary for old style, see below
	SPTerm from, to, way = null;
	*/
	HashMap varMap = new HashMap();
	SPQuery query;
	int i;
	
	try {
	    if (null == (sp = SICStus.getInitializedSICStus())) {
		sp = new SICStus();
            }

	    if (filename != null) {
                // You probably want to use sp.restore() instead
                sp.load(filename);
            }
	    
	    /* Old style
	    to = new SPTerm(sp, "Orebro");
	    from = new SPTerm(sp, "Stockholm");
	    way = new SPTerm(sp).putVariable();	    
            sp.query("user", "connected", new SPTerm[] { from, to, way, way });
	    */
	    /* New style */
	    sp.query("connected('Stockholm','Orebro',Way,Way).", varMap);
	} catch ( Exception e ) {
	    e.printStackTrace();
	}
	/* Old style
	return way;
	*/
	/* New style */
	return ((SPTerm)varMap.get("Way"));
    }
    // [PM] 3.8.5 train when called from prolog and thus the prolog code is already loaded.
    public static SPTerm train()
    {
        return train(null);
    }

    
    public static void main(String argv[])
    {
	SPTerm result = train("simple");
	System.out.println(result.toString());
    }
}
