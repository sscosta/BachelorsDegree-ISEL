package app.Sketch.view;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;

import app.Sketch.model.Figure;

abstract class FigureView {
    private Figure figure;
    protected Paint paint;

    FigureView(Figure f){
        this.figure = f;
        this.paint = new Paint();
        paint.setStyle(Paint.Style.STROKE);
        paint.setColor(Color.BLACK);
        paint.setStrokeWidth(5);
    }

    public Figure getFigure(){
        return this.figure;
    }
    abstract void draw(Canvas c);
}
