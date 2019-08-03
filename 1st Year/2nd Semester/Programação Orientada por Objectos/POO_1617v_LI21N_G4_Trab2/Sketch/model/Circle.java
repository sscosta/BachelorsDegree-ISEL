package app.Sketch.model;

public class Circle extends Figure {
    private int radius;

    public Circle(){
        this(0,0);
    }

    public Circle(int x, int y){
        super('C',x,y);
        this.radius=1;
    }

    public void setEnd(int x, int y){
        this.radius = calcDist(this.getStart(),new Point(x,y));
        if (this.radius<1) this.radius=1;
    }

    public int calcDist(Point begin, Point end) {
        return (int)Math.sqrt(
                (Math.pow(begin.getX()-end.getX(),2)+
                 Math.pow(begin.getY()-end.getY(),2)
                ));
    }

    public String toString (){
        return super.toString() + " |" + this.getRadius()+ "|";
    }

    public int fromString(String str, int idx){
        Point p =new Point();
        idx=p.fromString(str,idx);
        this.startPoint.set(p.getX(),p.getY());

        int idxBegin = str.indexOf('|',idx);
        int idxEnd = str.indexOf('|',idxBegin+1);
        this.radius= Integer.parseInt(str.substring(idxBegin+1,idxEnd));
        return idxEnd+1;
    }

    public int getRadius(){
        return this.radius;
    }
}