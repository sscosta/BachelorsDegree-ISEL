package model.cell;

import model.Dir;

public class FreeCell extends Cell {
    FreeCell() {
        super('.');
    }

    /** It is assumed that a FreeCell is full when it has 2 connected directions.
     * Because EMPTY is also a enum value, a Free Cell is full when it has 3 connected directions.
     * Returns true when the cell is Full.
     */
    @Override
    public boolean isFull() {
        return this.dirConnected.size()==3;
    }

    /** Returns true if the cell is not full and it can connect in the specified direction
     * @param terminal filled character of connecting cell.
     * @param direction direction to be connected.
     */
    @Override
    public boolean canConnectFrom(char terminal, Dir direction) {
        return ! this.isFull() && (this.filled ==0 || this.filled==terminal) && !this.dirConnected.contains(direction);
    }
    @Override
    public boolean canDisconnectFrom(char terminal, Dir direction) {
        return this.filled==terminal && this.dirConnected.contains(direction);
    }

    @Override
    public void connectFrom(char terminal, Dir direction) {
        this.filled=terminal;
        this.dirConnected.add(direction);
    }

    /**Disconnects the cell from the specified terminal in the specified direction
     * @param terminal char filled
     * @param direction direction to disconnect in destination cell.
     */
    @Override
    public void disconnectFrom(char terminal, Dir direction) {
        this.dirConnected.remove(direction);
        if (this.dirConnected.size()==1) {this.filled=0;}
    }
}
