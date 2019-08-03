package app.Sketch.view;

import android.graphics.Canvas;
import android.util.Log;

import app.Sketch.model.Figure;
import app.Sketch.model.Rect;
import app.Sketch.model.Point;

class RectView extends FigureView {
    RectView(Figure f) {
        super(f);
    }
    void draw(Canvas c){
        Rect r = (Rect) this.getFigure();
        Point start = r.getStart();
        Point end = r.getEnd();
        if (start.getY()==end.getY()||start.getX()==end.getX()){
            c.drawLine(start.getX(),start.getY(),end.getX(),end.getY(),super.paint);
            return;
        }
        int left = start.getX()<end.getX()?start.getX():end.getX();
        int right= start.getX()<end.getX()?end.getX():start.getX();
        int top=start.getY()>end.getY()?end.getY():start.getY();
        int bottom = start.getY()>end.getY()?start.getY():end.getY();
        c.drawRect(left,top,right,bottom,super.paint);
    }
}
