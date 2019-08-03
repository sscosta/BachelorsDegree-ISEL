package app.Sketch.view;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;

import java.util.Iterator;
import java.util.LinkedList;

import app.Sketch.SketchController;
import app.Sketch.model.Sketch;
import app.Sketch.model.Figure;

public class SketchView extends View{
    private LinkedList<FigureView> views;
    private Sketch model;
    private Paint paint;

    public SketchView(SketchController ctrl){
        super(ctrl);
        this.views = new LinkedList<>();
        this.model = ctrl.model;
        this.paint = new Paint();
    }

    private static FigureView createView(Figure f){
        switch (f.getLetter()){
            case 'L': return new LineView(f);
            case 'R': return new RectView(f);
            case 'C': return new CircleView(f);
            case 'P': return new PixelView(f);
            default: return null;
        }
    }

    public boolean OnTouchEvent(MotionEvent evt){
            Log.i("POO", evt.toString());
            if (evt.getAction() == MotionEvent.ACTION_DOWN){
                this.views.add(createView(model.getLastCreated()));
            }
            this.invalidate();
            return true;
    }

    protected void onDraw(Canvas canvas){
        int h = canvas.getHeight();
        int w = canvas.getWidth();
        this.paint.setStyle(Paint.Style.STROKE);
        this.paint.setColor(Color.RED);
        this.paint.setStrokeWidth(5);
        canvas.drawRect(0,0,w,h,this.paint);

        for(FigureView v : views)
            v.draw(canvas);
    }

    public void reload(Sketch model){
        clear();
        Iterator<Figure> it = model.iterator();
        while(it.hasNext()){
            views.add(createView(it.next()));
        }
        this.invalidate();
    }

    public void clear(){
        views.clear();
        this.invalidate();
    }

}
