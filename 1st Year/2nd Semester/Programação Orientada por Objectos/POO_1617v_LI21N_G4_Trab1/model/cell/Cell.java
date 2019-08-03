package model.cell;

import model.Dir;

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
        else if (type == '-')
            return new LineCell(type);
        else if ( type == '|')
            return new LineCell(type);
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
    public abstract boolean canConnectFrom(char terminal, model.Dir direction);
    public abstract boolean canDisconnectFrom(char terminal, model.Dir direction);
    public abstract void connectFrom(char terminal, model.Dir direction);
    public abstract void disconnectFrom(char terminal, model.Dir direction);

}