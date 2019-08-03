package model;

public class Position  {
    private int line ;
    private int col ;

    public Position(int line,int col) {
        this.line=line;
        this.col=col;
    }

    /**Get line from position. */
    public int getLine(){ return this.line;}

    /**Get Column from position. */
    public int getCol(){ return this.col;}

    /**Returns true if 'to' has the same parameters line and col as the position it is called from.
    */
    public boolean equals(Position to) {
        return (to != null && this.line==to.getLine() && this.col==to.getCol());
    }

    /**
     * Return a position from direction.
     * @param dir Direction in relation to which the new position is to be specified.
     * @return new position.
     */
    public Position newPositionFromDirection(Dir dir) {
        return new Position(this.line+ dir.deltaLin(), this.col + dir.deltaCol());
    }

}
