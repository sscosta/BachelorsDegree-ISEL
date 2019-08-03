package pt.isel.leic.poo.app.view.cell;

import android.graphics.Canvas;
import pt.isel.leic.poo.app.model.cell.Cell;

public class TerminalView extends CellView {
    TerminalView(Cell cell) {
        super(cell);
    }

    @Override
    public void draw(Canvas canvas, int side) {
        super.draw(canvas,side);
        super.drawDirections(canvas,side);
        super.drawTerminal(canvas,side);
    }
}
