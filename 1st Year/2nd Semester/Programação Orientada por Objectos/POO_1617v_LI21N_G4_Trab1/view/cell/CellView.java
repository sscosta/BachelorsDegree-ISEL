package view.cell;

import isel.leic.pg.Console;
import model.cell.*;
import model.Position;

/**
 * Base class hierarchy of cell viewers
 */
public abstract class CellView {

    public static final int CELL_SIDE = 3;  // Each side of the cell occupies 3 characters

    // Matches between the color number in the model and the color displayed on the console
    private static final int[] COLORS = {
        Console.RED, Console.GREEN, Console.YELLOW,
        Console.BLUE, Console.MAGENTA, Console.ORANGE
    };
    static final int EMPTY_COLOR = Console.GRAY;
    final Cell cell; // Reference to the model cell

    CellView(Cell cell) {
        this.cell= cell;
    }



    /**
     * The color used to show the cell
     * @return The console color to use in that cell
     */
    int getColor() {
        if (cell.getFilled()!= 0)
            return COLORS[ cell.getFilled()- 'A' ];
        else
            return EMPTY_COLOR;
    }

    /**
     * Creates the appropriate view of the cell type of the model
     * @param cell The model cell
     * @return The view for the cell
     */
    public static CellView newInstance(Cell cell) {
        if (cell instanceof TerminalCell) return new TerminalView(cell);
        else if (cell instanceof LineCell) return new LineView(cell);
        else if (cell instanceof FreeCell) return new FreeView(cell);
        else return null;
    }

    /**
     * Helper method to write a char at the current position of the console with a background color
     * @param v Char to write
     * @param bc Background color to use
     */
    static void write(char v, int bc) {
        Console.setBackground(bc);
        Console.print(v);
    }

    /**
     * Paint the cell of a coordinate (of the model) with or without highLight.
     * @param pos Line in model
     * @param highLight Is to present highlighted
     */
    abstract public void paint(Position pos, boolean highLight);

    /**
     * Paint background of the cell of a coordinate (of the model) with or without highLight.
     * @param pos Line in model
     * @param highLight Is to present highlighted
     */
    void paintBackground(Position pos, boolean highLight){
        int bk = highLight ? Console.LIGHT_GRAY : Console.BLACK;
        paintBackground(pos, bk);
     }

    /**
     * Paint background of cell in coordinate (of the model) with bk color.
     * @param pos Line in model
     * @param bk color to paint
     */
    void paintBackground(Position pos, int bk) {
        int line = pos.getLine()*CELL_SIDE;
        int column = pos.getCol()*CELL_SIDE;

        for (int i=line; i<line+CELL_SIDE; i++ ){
            for ( int j=column; j<column+CELL_SIDE;j++){
                Console.cursor(i,j);write(' ',bk);
            }
        }
    }

}
