package model.cell;

import model.Dir;

public class TerminalCell extends Cell {
    private char type;
    TerminalCell(char type) {
        super(type);
        super.filled=type;
        this.type=type;
    }

    @Override
    public boolean isFull() {
        return super.dirConnected.size()==2;
    }

    @Override
    public boolean canConnectFrom(char terminal, Dir direction) {
        return (! this.isFull() &&  this.filled==terminal && !this.dirConnected.contains(direction));
    }

    @Override
    public boolean canDisconnectFrom(char terminal, Dir direction) {
        return this.filled==terminal && this.dirConnected.contains(direction);
    }

    @Override
    public void connectFrom(char terminal, Dir direction) {
        this.dirConnected.add(direction);
    }

    @Override
    public void disconnectFrom(char terminal, Dir direction) {
        this.dirConnected.remove(direction);
    }
}
