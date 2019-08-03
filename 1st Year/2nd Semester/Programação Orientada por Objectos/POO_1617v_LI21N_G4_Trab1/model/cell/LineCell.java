package model.cell;

import model.Dir;

public class LineCell extends Cell {
    LineCell(char type) {
        super(type);
    }

    @Override
    public boolean isFull() {
        return super.dirConnected.size()==3;
    }

    @Override
    public boolean canConnectFrom(char terminal, Dir direction) {
        return  !this.isFull() && (this.filled ==0 || this.filled==terminal) && this.isDirectionFromType(direction) && !this.dirConnected.contains(direction);
    }

    @Override
    public boolean canDisconnectFrom(char terminal, Dir direction) {
        return this.filled==terminal  && this.isDirectionFromType(direction) && this.dirConnected.contains(direction);
    }

    @Override
    public void connectFrom(char terminal, Dir direction) {
        this.filled=terminal;
        this.dirConnected.add(direction);
    }

    @Override
    public void disconnectFrom(char terminal, Dir direction) {
        this.dirConnected.remove(direction);
        if (this.dirConnected.size()==1) {this.filled=0;}
    }

    /**Returns true if the type admits connection in the specified direction.
     * @param direction to be evalued.
     * @return true if the cell can connect in that direction.
     */
    public boolean isDirectionFromType( Dir direction) {
        return this.getType() == '-' && (direction == Dir.LEFT || direction == Dir.RIGHT) || this.getType() == '|' && (direction == Dir.UP || direction == Dir.DOWN);
    }
}
