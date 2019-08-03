package app.Sketch.model;

public class Point {
    private int x,y;

    protected Point (){}

    protected Point (int x, int y){
        this.x=x;
        this.y=y;
    }

    protected void set (int x,int y){
        this.x=x;
        this.y=y;
    }

    public int getX(){
        return this.x;
    }
    public int getY(){
        return this.y;
    }

    @Override
    public String toString(){
        return "("+this.getX()+","+this.getY()+")";
    }

    public int fromString(String str, int idx){
        int idxBegin = str.indexOf('(',idx) ;
        int idxEnd = str.indexOf(')',idxBegin+1) ;
        String[] parts = str.substring(idxBegin+1, idxEnd).split(",");
        this.set(Integer.parseInt(parts[0]),Integer.parseInt(parts[1]));
        return idxEnd+1;
    }
}