package view.cell;

import isel.leic.pg.Console;
import model.Position;
import model.cell.Cell;

public class TerminalView extends CellView {
    TerminalView(Cell cell) {super(cell);}

    public void paint(Position pos, boolean b){
        int bk = this.getColor();
        this.paintBackground(pos,bk);
        Console.cursor(pos.getLine()*CELL_SIDE+1, pos.getCol()*CELL_SIDE+1);
        Console.color(Console.BLACK, bk);
        Console.print('O');
    }
}
