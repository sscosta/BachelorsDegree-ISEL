package pt.isel.leic.poo.app.model.cell;

import pt.isel.leic.poo.app.model.Dir;

public class BlockCell extends Cell {
    BlockCell(char type) {
        super(type);
    }

    @Override
    public boolean isFull() {
        return true;
    }

    @Override
    public boolean canConnectFrom(char terminal, Dir direction) {
        return  false;
    }

    @Override
    public boolean canDisconnectFrom(char terminal, Dir direction) {
        return false;
    }

    @Override
    public void connectFrom(char terminal, Dir direction) {
    }

    @Override
    public void disconnectFrom( Dir direction) {
    }

    @Override
    public boolean acceptsDirectionFromType(Dir direction) {
        return false;
    }
}
