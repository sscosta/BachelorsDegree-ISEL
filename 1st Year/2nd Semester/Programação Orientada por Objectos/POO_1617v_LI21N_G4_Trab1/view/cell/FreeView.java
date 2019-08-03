package view.cell;

import isel.leic.pg.Console;
import model.Dir;
import model.Position;
import model.cell.Cell;

public class FreeView extends CellView {
    FreeView(Cell cell) {
        super(cell);
    }

    public void paint(Position pos, boolean highLight) {
        super.paintBackground(pos, highLight);
        int line = pos.getLine()*CELL_SIDE+1;
        int column =  pos.getCol()*CELL_SIDE+1;
        int bk = this.getColor();
        Console.cursor(line, column);
        super.write(' ',bk);
        for (Dir d : Dir.values()){
            if(this.cell.dirConnected.contains(d)){
                Console.cursor(line+d.deltaLin(), column+d.deltaCol());
                super.write(' ',bk);
            }
        }
    }
}
