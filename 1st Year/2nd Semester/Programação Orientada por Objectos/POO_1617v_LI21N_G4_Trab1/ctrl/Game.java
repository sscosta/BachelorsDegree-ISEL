package ctrl;

import java.awt.event.KeyEvent;
import java.io.*;
import java.util.*;

import isel.leic.pg.Console;

import isel.leic.pg.MouseEvent;

import model.Circuit;
import model.Loader;
import model.Position;
import view.Panel;
/*

* @startuml
*

package "Control" #DDDDDD {
 class Game
}

package "Model" {
 Game *-- Circuit : -model
 enum Dir
 Circuit ..> Dir
 Circuit ..> Position: «use»
 Circuit <.. Loader : «create»
 Circuit *-- Cell : -grid
 Cell <|-- LineCell : "*"
 Cell <|-- FreeCell : "*"
 Cell <|-- TerminalCell : "*"
 Game ..> Loader : «use»
}

package "View" #DDDDAA {
 Panel --> Circuit : -model
 Game *-- Panel : -view
 Abstract CellView
 Panel *-- CellView
 Cell<-- CellView :  " #cell"
 CellView <|-- LineView :"*"
 CellView <|-- FreeView : "*"
 CellView <|-- TerminalView : "*"
}

* @enduml
*/

/**
 * Main class in console mode for the "Circuit" game.
 * Performs interaction with the user.<br/>
 * Implements the Controller in the MVC model,
 * making the connection between the model and the viewer specific to the console mode.
 */
class Game {
    public static void main(String[] args) {
        Game game = new Game();
        game.run();
    }

    private static final String LEVELS_FILE = "Demo.txt"; // Name of levels file
    private Circuit model;              // Model of current level
    private Panel view = new Panel();   // View for the current level

    /**
     * Main game loop.
     * Returns when there are no more levels or the player gives up.
     */
    private void run() {
        int num = 0;    // Current level number
        while (loadLevel(++num))
            if (!playLevel() || !view.question("Next")) {
                exit("Bye.");
                return;
            }
        exit("No more Levels");
    }

    /**
     * Displays the message and closes the console window.
     * @param msg the message to show
     */
    private void exit(String msg) {
        view.message(msg);
        view.close();
    }

    /**
     * Main loop of each level.
     * @return true - the level has been completed. false - the player has given up.
     */
    private boolean playLevel() {
        // Opens the console window with dimensions appropriate to the current level.
        // Starts the viewer for each model cell.
        // Shows the initial state of all cells in the model.
        view.open(model);

        while ( play() ) {      // Process one input event (mouse or keyboard)
            if ( winGame() )    // Verify win conditions
                return true;    // The level is completed
            view.repaintTime(); // Updates the elapsed time
        }
        return false;           // Finished without complete
    }

    /**
     * Verify win conditions and print winner message.
     * @return true - if level is completed
     */
    private boolean winGame() {
        if (model.isOver()) {
            view.message("Winner");
            return true;
        }
        return false;
    }

    /**
     * Load the model of the indicated level from the LEVELS_FILE file
     * @param n The level to load (1..MAX)
     * @return true if the level is loaded
     */
    private boolean loadLevel(int n) {
        Scanner in = null;
        try {
            in = new Scanner(new FileInputStream(LEVELS_FILE)); // Scanner to read the file
            model = new Loader(in).load(n);                     // Load level from scanner
            return true;
        } catch (FileNotFoundException | InputMismatchException e) {
            System.out.println("Error loading file \""+LEVELS_FILE+"\":\n"+e.getMessage());
            return false;
        } catch (Loader.LevelFormatException e) {
            System.out.println(e.getMessage()+" in file \""+LEVELS_FILE+"\"");
            System.out.println(" "+e.getLineNumber()+": "+e.getLine());
            return false;
        } finally {
            if (in!=null) in.close();   // Close the file
        }
    }

    /**
     * Makes the move corresponding to the mouse event.
     * @return false - If user game aborted (escape key)
     */
    private boolean play() {
        int key = Console.waitKeyPressed(250); // Wait for mouse event or a key
        if (key == KeyEvent.VK_ESCAPE)         // Escape key -> abort game
            return false;
        if (key == Console.MOUSE_EVENT) {      // Is a mouse event
            MouseEvent me = Console.getMouseEvent();
            Position pos = view.getModelPosition(me.line,me.col); // Convert mouse position to cell coordinates

            if (pos!=null)                     // Is a valid position ?
                processMouseEvent(me.type, pos); // Makes the move.
        }
        return true;
    }

    /**
     * Stores last move positions
     */
    private Position from, first;

    /**
     * Make one move.
     * @param eventType - The type of mouse event (DOWN, DRAG, UP)
     * @param pos - The current cell coordinates
     * @return true if the move is relevant to the game
     */
    private boolean processMouseEvent(int eventType, Position pos) {
        switch (eventType) {
            case MouseEvent.DOWN:
                view.paint(from = first = pos,true); // Show cell highlighted
                break;
            case MouseEvent.DRAG:
                if (!pos.equals(from)) {        // Is a different coordinate ?
                    view.paint(from,false); // Shows last cell without highlighting
                    view.paint(pos,true);   // Show current cell highlighted
                    Position f = from;
                    from = pos;
                    if (model.drag(f,pos)) {     // Try make the move in model
                        view.paint(f,false);  // Repaint from cell of the drag
                        view.paint(pos,false);// Repaint to cell of the drag
                        return true;
                    }
                }
                break;
            case MouseEvent.UP:
                view.paint(pos,false);         // Shows cell without highlighting
                // If is a CLICK on one cell try unlink the cell.
                if (pos.equals(first) && model.unlink(pos))
                    view.repaint();            // Repaint all board if is unlinked.
                break;
        }
        return false;
    }
}
