package pt.isel.leic.poo.app.view.cell;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.RectF;

import pt.isel.leic.poo.app.model.Dir;
import pt.isel.leic.poo.app.model.cell.*;

import pt.isel.poo.tile.Tile;

/**
 * Base class hierarchy of cell viewers
 */
public abstract class CellView implements Tile {
    private final static Paint backGroundPaint =getPaintFromColor(Color.BLACK);
    private final static Paint backGroundPaintSelected =getPaintFromColor(Color.LTGRAY);
    private final static Paint cellEmptyPaint =getPaintFromColor(Color.GRAY);

    // Matches between the color number in the modelGame and the color displayed on the console
    private final static Paint[] cellPaints = {
            getPaintFromColor(Color.RED),
            getPaintFromColor(Color.GREEN),
            getPaintFromColor(Color.YELLOW),
            getPaintFromColor(Color.BLUE),
            getPaintFromColor(Color.MAGENTA),
            getPaintFromColor(Color.CYAN )};

    private final static float relativeRadiusOfTerminal=0.8f;          // set relative size
    private final static float relativeRadiusOfTerminalInternal=0.2f;  // set relative size
    private final static float relativeWithOfLine=0.3f;                // set relative size

    private final Cell cell;             // Reference to the modelGame cell
    private boolean isSelected=false;

    CellView(Cell cell) {
        this.cell= cell;
    }

    /**
     * Creates the appropriate view of the cell type of the modelGame
     * @param cell The modelGame cell
     * @return The view for the cell
     */
    public static CellView newInstance(Cell cell) {
        if (cell instanceof TerminalCell) return new TerminalView(cell);
        else if (cell instanceof LineCell) return new LineView(cell);
        else if (cell instanceof FreeCell) return new FreeView(cell);
        else if (cell instanceof BlockCell) return new BlockView(cell);
        else if (cell instanceof CurveCell) return new CurveView(cell);
        else return null;

    }


    @Override
    public void draw(Canvas canvas, int side) {
        canvas.drawRect(0,0,side,side,(this.isSelected?backGroundPaintSelected:backGroundPaint));
    }

    @Override
    public boolean setSelect(boolean selected) {
        if (this.isSelected==selected)
            return false;
        else {
            this.isSelected=selected;
            return true;
        }

    }

    /**
     * The paint used to show the cell
     * @return The Paint to use in that cell
     */
    private Paint getPaint() {
        if (cell.getFilled()!= 0)
            return cellPaints[cell.getFilled() - 'A'];
        else
            return cellEmptyPaint;
    }

    /**
     * helper to draw center of tile
     * @param canvas of tile to draw
     * @param side of tile to draw
     */

    protected void drawCenter(Canvas canvas,int side){
        float center=side/2;
        canvas.drawCircle(center,center,side*relativeWithOfLine/2, this.getPaint());
    }

    /**
     * helper to draw terminal
     * @param canvas of tile to draw
     * @param side of tile to draw
     */
    protected void drawTerminal(Canvas canvas,int side){
        float center=side/2;
        canvas.drawCircle(center,center, (center*relativeRadiusOfTerminal), this.getPaint());
        canvas.drawCircle(center,center, (center*relativeRadiusOfTerminalInternal), backGroundPaint);
    }

    /**
     * helper to draw restriction directions
     * @param canvas of tile to draw
     * @param side of tile to draw
     */
    protected void drawLine(Canvas canvas ,int side) {
        RectF rect;
        for (Dir d:Dir.values()) {
            if ( this.cell.acceptsDirectionFromType(d)) {
                rect = this.getRectFromDirection(d,side);
                if (rect != null)
                    canvas.drawRect(rect,this.getPaint());
            }

        }
    }

    /**
     * helper to draw filled directions of tile
     * @param canvas of tile to draw
     * @param side of tile to draw
     */
    protected void drawDirections(Canvas canvas ,int side) {
        RectF rect;
        for (Dir d:this.cell.dirConnected) {
            rect = this.getRectFromDirection(d,side);
            if (rect != null)
                canvas.drawRect(rect,this.getPaint());
        }
    }

    private RectF getRectFromDirection(Dir d, int side) {
        float center=side/2;
        float halfLine= (side*relativeWithOfLine/2);
        switch (d){
            case DOWN:
                return new RectF(center-halfLine,side-center,center+halfLine,side);
            case UP:
                return new RectF(center-halfLine,0,center+halfLine,center);
            case LEFT:
                return new RectF(0,center-halfLine,center,center+halfLine);
            case RIGHT:
                return new RectF(center,center-halfLine,side,center+halfLine);
        }
        return null;
    }

    /**
     * helper to get paint from color
     * @param color to paint
     * @return new paint
     */
    private static Paint getPaintFromColor (int color){
        Paint paint=new Paint();
        paint.setColor(color);
        return paint;
    }
}
