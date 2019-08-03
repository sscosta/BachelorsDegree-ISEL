package app.Sketch.model;


public class Pixel extends Figure{

    public Pixel(){
        this(0,0);
    }

    public Pixel(int x,int y){
        super('P',x,y);
    }

    public void setEnd(int x, int y){
        //this.pixelEnd = new Point (x,y);
    }

    protected int fromString(String str, int idx){
        Point p =new Point();
        idx=p.fromString(str,idx);
        this.startPoint.set(p.getX(),p.getY());
        return idx;
    }

}
