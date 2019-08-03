package app.Sketch.view;

import android.graphics.Canvas;

import app.Sketch.model.Figure;
import app.Sketch.model.Circle;

class CircleView extends FigureView {

    CircleView(Figure f) {
        super(f);
    }

    void draw(Canvas c){
        Circle circle = (Circle) this.getFigure();
        c.drawCircle(circle.getStart().getX(), circle.getStart().getY(), circle.getRadius(),super.paint);
    }
}
