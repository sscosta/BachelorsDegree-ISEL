package app.Sketch.model;

public abstract class Figure {
    private char LETTER;

    Point startPoint;

    public Figure(char letter){
        this(letter,0,0);
    }

    public Figure(char letter,int x, int y){
        this.LETTER=letter;
        this.startPoint = new Point(x,y);
    }

    public char getLetter(){
        return this.LETTER;
    }

    public Point getStart(){
        return startPoint;
    }

    public abstract void setEnd(int x, int y);

    public String toString(){
        return this.getLetter() + " "+ this.startPoint.toString();
    }

    protected abstract int fromString(String str, int idx);

    static Figure GetFigureFromString(String str) {
        if (str.length() > 0) {
             char letter = str.charAt(0);
             switch (letter) {
                 case 'L':
                     Line l =new Line();
                     l.fromString(str,1);
                     return l;
                 case 'C':
                     Circle c =new Circle();
                     c.fromString(str,1);
                     return c;
                 case 'P':
                     Pixel p  =new Pixel();
                     p.fromString(str,1);
                     return p;
                 case 'R':
                     Rect r =new Rect();
                     r.fromString(str,1);
                     return r;
             }
         }
         return null;
    }

    public static Figure GetFigureFromLetter(char letter,int x,int y){
        switch (letter) {
            case 'L': return new Line(x,y);
            case 'R': return new Rect(x,y);
            case 'P': return new Pixel(x,y);
            case 'C': return new Circle(x,y);
            default: return null;
        }
    }


}
