/*
/src/sicstus/install/src/JDK/linux/jdk1.2.2_FCS/bin/java -native  \
	-Dsicstus.path=/home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib/sicstus-3.8.5beta1 \
	-classpath /home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib/sicstus-3.8.5beta1/bin/jasper.jar:. \
	-Djava.library.path=/home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib \
	Queens
*/
import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.io.*;
import java.net.*;

import se.sics.jasper.*;

class QueensCanvas extends Canvas
{
  public int boardSize, cellSize;
  private final int SPACING = 0;
  private final int OFFSET = 10;
  public boolean queens[][];
  private Image queen = null;

  public QueensCanvas(int boardSize, int cellSize)
  {
    int canvasSize = 
      boardSize*cellSize + 
      (boardSize-1)*this.SPACING +
      this.OFFSET*2;
    
    this.setSize(canvasSize, canvasSize + this.OFFSET*2);

    this.boardSize = boardSize;
    this.cellSize = cellSize;
      
    queens = new boolean[boardSize][boardSize];
  }

  private Color getCellColor(int i, int j)
  {
    if ((i+j) % 2 == 0)
      return Color.black;
    else
      return Color.white;
  }

  private Color getInvCellColor(int i, int j)
  {
    if (getCellColor(i,j) == Color.white)
      return Color.black;
    else
      return Color.white;
  }

  public void paint(Graphics g)
  {
    repaintBoard(g);
  }

  public void update(Graphics g)
  {
    repaintBoard(g);
  }

  public void repaintBoard(Graphics g)
  {
    int i, j;

    for (i = 0; i < boardSize; i++)
      {
	for (j = 0; j < boardSize; j++)
	  {
	    if (queens[i][j])
	      {
		drawQueen(g,i,j);
	      }
	    else
	      {
		g.setColor(getCellColor(i,j));
		g.fillRect
		  (OFFSET + i*(cellSize + SPACING),
		   OFFSET + j*(cellSize + SPACING),
		   cellSize,
		   cellSize);
	      }
	  }
      }

    /*
      for (i = 0; i < boardSize; i++)
      {
      for (j = 0; j < boardSize; j++)
      {
      drawAttacks(g,i,j);
      }
      }
    */
  }
  
  public void drawAttacks(Graphics g, int x, int y)
  {
    if (queens[x][y])
      {
	g.setColor(Color.red);
	
	g.drawLine(OFFSET,
		   OFFSET + y*cellSize + cellSize/2,
		   OFFSET + (boardSize*cellSize),
		   OFFSET + y*cellSize + cellSize/2);
	
	g.drawLine(OFFSET + x*cellSize + cellSize/2,
		   OFFSET,
		   OFFSET + x*cellSize + cellSize/2,
		   OFFSET + (boardSize*cellSize)
		   );
	
      }
  }

  public void drawQueen(Graphics g, int i, int j)
  {
    g.setColor(getCellColor(i,j));
    g.fillRect
      (OFFSET + i*(cellSize + SPACING),
       OFFSET + j*(cellSize + SPACING),
       cellSize,
       cellSize);
      
    g.setColor(Color.blue);
    g.fillOval
      (OFFSET + i*(cellSize + SPACING) + 3,
       OFFSET + j*(cellSize + SPACING) + 3,
       cellSize - 6,
       cellSize - 6);		
  }

  public void place(int x, int y, boolean hasQueen)
  {
    queens[x][y] = hasQueen;

    this.repaint();
  }

  public void removeAllQueens()
  {
    int i,j;

    for (i = 0; i < boardSize; i++)
      for (j = 0; j < boardSize; j++)
	queens[i][j] = false;
  }
}

class QueensFrame extends Frame
{
  MenuBar menubar;
  Menu filemenu;
  MenuItem itemQuit, itemRestart, itemNextSol, itemSep;
  QueensCanvas board;
  // SPPredicate jqueens;
  //  SPQuery jquery = null;
  Query jquery = null;          // 3.9
  // SPTerm size, res, pofile;
  Term size, res, pofile;       // 3.9
  // SPTerm res_array[];
  Term res_array[];             // 3.9
  // SICStus sp;
  Prolog sp;                    // 3.9
  Font font;

  public QueensFrame(int boardSize, int cellSize)
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.
  {
/* pre 3.9 way
    sp = new SICStus();
    size = new SPTerm(sp, boardSize);
    res = new SPTerm(sp).putVariable();
*/
    // Using thread safe interface in 3.9
    {
      sp = Jasper.newProlog();
      size = sp.newTerm(boardSize);
      res = sp.newVariable();
    }

    font = new Font("Helvetica",Font.PLAIN,12);

    // load jqueens.po with
    // load_files(library('jasper/examples/jqueens')).

    /* pre 3.8.5 way using (deprecated) SPPredicate
       sp.query(new SPPredicate(sp,"load_files",1,""), 
       new SPTerm[]
       { new SPTerm(sp).consFunctor("library", new SPTerm[]
       { new SPTerm(sp).putString("jasper/examples/jqueens")})});
    */
    // One of several new ways in 3.8.5
    {
      java.util.HashMap map = new java.util.HashMap();
//      map.put("Path", new SPTerm(sp).putString("jasper/examples/jqueens"));
      map.put("Path", sp.newTerm("jasper/examples/jqueens")); // 3.9
      sp.queryCutFail("load_files(library(Path)).", map);
    }
    /* An alternative way closer to the pre 3.8.5 code above:
       sp.query("user", "load_files",
                new SPTerm[] { 
                    new SPTerm(sp).consFunctor("library", new SPTerm[] {
                        new SPTerm(sp).putString("jasper/examples/jqueens")})});
    */
    
    // SPPredicate was deprecated in 3.8.5 
    // jqueens = new SPPredicate(sp, "jqueens", 2, "jqueens");

    menubar = new MenuBar();
    filemenu = new Menu("File");
    itemQuit = new MenuItem("Quit", new MenuShortcut(KeyEvent.VK_Q));
    itemRestart = new MenuItem("Restart", new MenuShortcut(KeyEvent.VK_R));
    itemNextSol = new MenuItem("Next Solution", new MenuShortcut(KeyEvent.VK_N));
    itemSep = new MenuItem("-");
    
    board = new QueensCanvas(boardSize, cellSize);
    this.add(board);
    
    itemQuit.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) { System.exit(0); }});
    
    itemRestart.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) 
          { 
            try { actionRestart(); }
//            catch ( SPException spe ) {}
            catch ( Exception spe ) { // 3.9 **** Note: Exceptionhandling may
                                     // change to throwing more specific
                                    // exceptions in future versions of Jasper.
              System.err.println("itemRestart failed to call actionRestart:");
              spe.printStackTrace(System.err);
            }
          }});
    
    itemNextSol.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) 
          { 
            try { actionNextSol(); }
//            catch ( SPException spe ) {}
            catch ( Exception spe ) { // 3.9 **** Note: Exceptionhandling may
                                     // change to throwing more specific
                                    // exceptions in future versions of Jasper.
              System.err.println("itemNextSol failed to call actionNextSol:");
              spe.printStackTrace(System.err);
            }
          }});
    
    this.addWindowListener
      (new WindowAdapter()
        { public void windowClosing(WindowEvent e) { System.exit(0); }});
    
    this.setMenuBar(menubar);
    menubar.add(filemenu);
    filemenu.add(itemRestart);
    filemenu.add(itemNextSol);
    filemenu.add(itemSep);
    filemenu.add(itemQuit);
    
    this.setTitle("JQueens");

    this.pack();

    this.setResizable(false);

    this.show();    

    actionRestart();
  }

  public void placeQueen(int x, int y, boolean onoff)
  {
    board.place(x, y, onoff);
  }
  
//  public void placeQueens(SPTerm qargs[])
  public void placeQueens(Term qargs[]) // 3.9
  {
    int i, x, y;
    
    board.removeAllQueens();

    for (i = 0; i < res_array.length; i++)
      {
	x = i;
	y = Integer.valueOf(res_array[i].toString()).intValue() - 1;
	
	placeQueen(x,y,true);
      }
  }

  public void actionRestart()
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    int i,j;

    if (jquery != null)
      jquery.close();
    /*
     * Note: Ideally openQuery(), all corresponding nextSolution(),
     * and the final close() or cut() should be enclosed in a
     * "synchronized (sp) {}" block. Failure to do so may not only
     * invalidate SPTerm objects created by other threads but may also
     * cause such SPTerm objects to refer to non-existing terms (this
     * may cause hard crashes in the prolog run-time system).  In this
     * example, however, after initialization the only SPTerm objects
     * are those created when nextSolution() is called in
     * actionNextSol().
     */
    // jquery = sp.openQuery(jqueens, new SPTerm[] { size, res } );
//    jquery = sp.openQuery("jqueens"/*module*/, "jqueens"/*predicate*/, new SPTerm[] { size, res } );
    // 3.9
    {
      java.util.HashMap argmap = new java.util.HashMap();
      argmap.put("Size", size);
      argmap.put("Res", res);
      jquery = sp.openPrologQuery("jqueens:jqueens(Size, Res).", argmap);
    }
    actionNextSol();
  }

  public void actionNextSol()
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    if (jquery.nextSolution())
      {
//	res_array = ((SPTerm)res).toTermArray();
	res_array = res.toPrologTermArray(); // 3.9
    
	if (res_array == null) {
	  System.out.println("res is not a list");
        } else {
	  placeQueens(res_array);
        }
      }
    else
      {
        System.out.println("no more solutions");
      }
  }
}

public class Queens
{
  QueensFrame qf;
    
  public static void main(String argv[])
  {
    play();
  }
    
  public static void play() {
    try {
      new Queens().startGame(8);
//    } catch (SPException spe) {
      } catch (Exception spe) { // 3.9 **** Note: Exceptionhandling may
                               // change to throwing more specific
                              // exceptions in future versions of Jasper.
        System.err.println("Failed to create a Queens frame:");
        spe.printStackTrace(System.err);
      }
  }
    
    
  public void startGame(int boardSize)
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    qf = new QueensFrame(boardSize, 20);
  }
}




/** [PM] Keep the original indentation style
 *  Local variables:
 *      c-basic-offset: 2
 *      indent-tabs-mode: nil
 *  end:
 **/

