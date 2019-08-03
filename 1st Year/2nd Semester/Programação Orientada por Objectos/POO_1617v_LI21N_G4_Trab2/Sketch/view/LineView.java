package app.Sketch.view;

import android.graphics.Canvas;

import app.Sketch.model.Figure;
import app.Sketch.model.Line;

class LineView extends FigureView {
    LineView (Figure f){
        super(f);
    }

    void draw(Canvas c){
        Line l = (Line) this.getFigure();
        c.drawLine(l.getStart().getX(),l.getStart().getY(),l.getEnd().getX(),l.getEnd().getY(),super.paint);
    }
}
