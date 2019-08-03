package view;

import isel.leic.pg.Console;
import javafx.scene.control.*;
import model.Circuit;
import model.Position;
import model.cell.Cell;
import view.cell.*;

public class Panel {
    private Circuit model;
    private CellView [][] grid;

    private int panelWidth;
    private int panelHeight;

    private final long beggining = System.currentTimeMillis();

    /** Show a message in the last line of the Console.
     * @param msg is the message to be written.
     **/
    public void message(String msg){
        Console.cursor(panelWidth, 0);
        Console.color(Console.BLACK,Console.GRAY);
        Console.print(msg);
    }

    /** Close the Console.
     **/
    public void close(){
        Console.close();
    }

    public boolean question (String question){
        message(question + "? Y/N");
        while (Console.getKeyPressed()!='Y'){
            if(Console.getKeyPressed()=='N')
                return false;
        }
        return true;
    }

    /**
     *  Open the console window with dimensions appropriate to the current level.
     *  Start the viewer for each model cell.
     *  Show the initial state of all cells in the model.
     */
    public void open(Circuit model) {
        this.model=model;
        this.panelWidth =model.getWidth()* CellView.CELL_SIDE;
        this.panelHeight =model.getHeight()* CellView.CELL_SIDE;

        Console.open("Circuit",this.panelHeight +1, this.panelWidth);
        Console.exit(true);
        startViewer(model);
        Console.enableMouseEvents(true);
    }

    /** Initiate a grid of references to CellView, transposing the grid in model.
     *  Paint the current view of the panel.
     * @param model the model to be transposed.
     * */
    private void startViewer(Circuit model) {
        this.grid = new CellView [model.getHeight()][model.getWidth()];
        for ( int l= 0; l<model.getHeight(); l++) {
            for (int col = 0; col < model.getWidth(); col++) {
                Cell cellToCopy= model.grid[l][col];
                CellView cellview = CellView.newInstance(cellToCopy);
                this.grid [l][col] = cellview;
            }
        }
        repaint();
    }


    /** Update the elapsed time since beggining of the game.
     * */
    public void repaintTime() {
        long timeElapsed = (System.currentTimeMillis() - beggining)/1000; //gets time in seconds since beggining of the game.

        Console.cursor(model.getHeight()*CellView.CELL_SIDE,0);
        Console.color(Console.BLACK,Console.GRAY);

        String sec = timeElapsed%60<10? "0" : "" ; //adds zero to seconds if less than 10
        sec+= (timeElapsed%60);
        String min = (timeElapsed)%3600<600? "0" : "" ; //adds zero to hours if less than 10
        min+=(timeElapsed/60);

        Console.cursor(model.getHeight()*CellView.CELL_SIDE,0);
        String str = min + ":" + sec;
        Console.print(String.valueOf(str));
        for (int j = panelWidth -str.length(); j>0; j--){
            Console.print(' ');
        }
    }

    /** Convert mouse position to cell coordinates
    */
    public Position getModelPosition(int line, int col) {
        if (line>0 && line<this.panelHeight && col>0 && col<this.panelWidth)//alternative: add this verification to control
            return new Position(line/CellView.CELL_SIDE, col/CellView.CELL_SIDE);
        return null;
    }

    /** Show cell highlighted.
     * @param position the Position of the CellView in the grid.
     * @param b it is highlighted.
    */
    public void paint(Position position, boolean b) {
        getCellFromPosition(position).paint(position, b);
    }

    /** Repaint all board if is unlinked.
    */
    public void repaint() {
        for ( int l = 0; l<model.getHeight(); l++ ){
            for ( int c = 0; c<model.getWidth(); c++){
                grid[l][c].paint(new Position(l,c),false);
            }
        }
    }

    /** Return a reference to an object CellView located in position p of the grid.
     * @param p the position in array.
     * @return a reference to CellView in position p of the grid.
     */
    private CellView getCellFromPosition(Position p) {
        return this.grid[p.getLine()][p.getCol()];
    }
}
