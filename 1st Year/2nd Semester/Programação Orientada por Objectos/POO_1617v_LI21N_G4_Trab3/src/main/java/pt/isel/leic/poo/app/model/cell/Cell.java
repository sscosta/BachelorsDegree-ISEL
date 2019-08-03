package pt.isel.leic.poo.app.model.cell;

import pt.isel.leic.poo.app.model.Dir;

import java.util.EnumSet;

public abstract class Cell {
    private final char type;
    char filled;
    public final EnumSet<Dir> dirConnected;


    Cell(char type) {
        this.type = type;
        this.dirConnected = EnumSet.of(Dir.EMPTY);
    }

    /** Static method that returns a new instance of one of derivate types of Cell based on type char.
     *  Returns a reference to Cell for purposes of compatibility.
     * @param type the type of the cell to be instanced.
     * @return Cell to be put on grid.
     */
    public static Cell newInstance(char type) {
        if (type == '.')
            return new FreeCell();
        else if (type == '-' || type == '|' )
            return new LineCell(type);
        else if ( type == '7' || type == '9' || type == '1' || type == '3' )
            return new CurveCell(type);
        else if ( type == '*')
            return new BlockCell(type);
        else if (type >= 45 && type <= 90)
            return new TerminalCell(type);
        return null;
    }

    /** Returns the type of the Cell. '.' - Free; '|' or '-' - Line; 'A' to 'Z' - Terminal.
     */
    char getType(){return this.type;}

    /** If filled, returns a character that represents the type of the terminal it is linked to.*/
    public char getFilled(){return this.filled;}

    public boolean fromString(String word) {
        return (this.type == word.charAt(0));
    }

    /**Abstract methods to be implemented in the derivate classes.*/
    public abstract boolean isFull(); /*is inverted in derivate classes*/
    public abstract boolean canConnectFrom(char terminal, Dir direction);
    public abstract boolean canDisconnectFrom(char terminal, Dir direction);
    public abstract void connectFrom(char terminal, Dir direction);
    public abstract void disconnectFrom( Dir direction);
    public abstract boolean acceptsDirectionFromType(Dir direction);
    /**
     * fill cell from saved data
     * @param saved data of cell
     */
    public void fromSaved(char saved){
       if (saved>0){
           int s =(int) saved;
           this.filled= (char) ((s &0b1111)+64);
           s=(saved & 0b11110000);
           if (s>0) {
               if ((s & 0b00010000) > 0) dirConnected.add(Dir.DOWN);
               if ((s & 0b00100000) > 0) dirConnected.add(Dir.UP);
               if ((s & 0b01000000) > 0) dirConnected.add(Dir.LEFT);
               if ((s & 0b10000000) > 0) dirConnected.add(Dir.RIGHT);
           }
       }
    }

    /**
     * save data of cell
     * @return char with data of cell
     */
    public char toSave (){
        int tosave=0;
        if (this.filled>0) {
            tosave=this.filled-64;
            if (this.dirConnected.size() >1) {
                for (Dir d : this.dirConnected) {
                    switch (d) {
                        case DOWN:
                            tosave = (tosave | 0b00010000);
                            break;
                        case UP:
                            tosave = (tosave | 0b00100000);
                            break;
                        case LEFT:
                            tosave = (tosave | 0b01000000);
                            break;
                        case RIGHT:
                            tosave = (tosave | 0b10000000);
                            break;
                    }
                }
            }
        }
        return (char) tosave;
    }


}