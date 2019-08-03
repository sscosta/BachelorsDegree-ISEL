package app.Sketch.model;


public class Line extends Figure {
    private Point endPoint;

    public Line(){
        this(0,0);
    }

    public Line(int x, int y){
        super('L',x,y);
        this.endPoint =new Point(x,y);
    }

    protected Line(char letter,int x, int y){
        super(letter,x,y);
        this.endPoint =new Point(x,y);
    }

    public void setEnd(int x,int y){
        this.endPoint = new Point (x,y);
    }

    public String toString(){
        return super.toString()+ " " + this.endPoint.toString();
    }

    public int fromString(String str, int idx){
        Point p =new Point();
        idx=p.fromString(str,idx);
        this.startPoint.set(p.getX(),p.getY());
        idx=p.fromString(str,idx);
        this.endPoint =p;
        return idx;
    }


    public Point getEnd(){
        return this.endPoint;
    }
}
