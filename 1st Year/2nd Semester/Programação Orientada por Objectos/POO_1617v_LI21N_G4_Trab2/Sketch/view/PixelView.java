package app.Sketch.view;

import android.graphics.Canvas;

import app.Sketch.model.Figure;
import app.Sketch.model.Pixel;

class PixelView extends FigureView {
    PixelView(Figure f) {
        super(f);
    }

    void draw(Canvas c){
        Pixel p = (Pixel) this.getFigure();
        c.drawPoint(p.getStart().getX(),p.getStart().getY(), super.paint);
    }
}
