package pt.isel.leic.poo.app.view.cell;

import android.graphics.Canvas;
import pt.isel.leic.poo.app.model.cell.Cell;

public class FreeView extends CellView {
    FreeView(Cell cell) {
        super(cell);
    }

    @Override
    public void draw(Canvas canvas, int side) {
        super.draw(canvas,side);
        super.drawCenter(canvas,side);
        super.drawDirections(canvas,side);
    }

}
