package app.Sketch.model;

public class Rect extends Line {
    public Rect(){
        this(0,0);
    }

    public Rect(int x, int y){
        super('R',x,y);
    }
}
