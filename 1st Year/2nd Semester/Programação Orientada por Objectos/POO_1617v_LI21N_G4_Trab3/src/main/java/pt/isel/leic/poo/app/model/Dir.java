package pt.isel.leic.poo.app.model;

public enum Dir {
    EMPTY,
    DOWN ,
    UP   ,
    LEFT ,
    RIGHT;

    /** Is the difference in Lines in relation to the center of the cell. The center and EMPTY are both taken as the origin.
     * @return an int of the difference of lines that direction introduces.
     */
    public int deltaLin(){
        switch (this){
            case DOWN: return 1;
            case UP: return -1;
            default: return 0;
        }
    }

    /**Is the difference in Columns in relation to the center of the cell. The center and EMPTY are both taken as the origin.
    */
    public int deltaCol(){
        switch (this){
            case LEFT: return -1;
            case RIGHT: return 1;
            default: return 0;
        }
    }

    /**Returns the opposite direction.
    */
    public Dir complement(){
        switch (this){
            case LEFT: return RIGHT;
            case RIGHT: return LEFT;
            case UP: return DOWN;
            case DOWN: return UP;
            default: return EMPTY;
        }
    }


}