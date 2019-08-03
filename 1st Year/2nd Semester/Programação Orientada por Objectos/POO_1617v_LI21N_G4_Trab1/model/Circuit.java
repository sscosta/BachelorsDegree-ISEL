package model;

import model.cell.*;

public class Circuit /*implements Position*/ {
    private int height, width;
    public Cell[][] grid;

    Circuit(int height, int width) {
        this.height = height;
        this.width = width;
        this.grid= new Cell[height][width];
    }

    public int getHeight() {
        return this.height;
    }

    public int getWidth() {
        return this.width;
    }

    /**It is assumed that a game is over when every cell of the grid is full.
     * Returns false when one of the cells of the grid is not full.
     */

    public boolean isOver() {
        for (int l =0;l<this.height;l++) {
            for (int c =0;c<this.height;c++) {
                if (!this.grid[l][c].isFull()) return false;
            }
        }
        return true;
    }

    /**Try to connect 2 cell in position 'from' and 'to'.
     * Returns true if the action is permitted and is done.
    */

    public boolean drag(Position from, Position to) {
        Cell cellFrom=this.getCellFromPosition(from);
        Cell cellTo=this.getCellFromPosition(to);
        char fillFrom =cellFrom.getFilled();

        if (fillFrom>0 && !cellFrom.isFull()) {
            int dif=0;
            Dir dirFrom = null;
            if (from.getLine()==to.getLine()) {
                dif=(from.getCol()-to.getCol());
                if (dif==1) dirFrom = Dir.LEFT ;
                else if (dif==-1) dirFrom = Dir.RIGHT;
                else dif=0;
            } else if (from.getCol()==from.getCol()){
                dif=(from.getLine()-to.getLine());
                if (dif==-1) dirFrom=Dir.DOWN;
                else if (dif==1) dirFrom=Dir.UP;
                else dif=0;
            }

            if (dif!=0) {
                if (!cellTo.isFull())
                    return this.connectCells(fillFrom,cellFrom,cellTo,dirFrom,dirFrom.complement());
                else
                    return this.disconnectCells(fillFrom,cellFrom,cellTo,dirFrom,dirFrom.complement());
            }
        }
        return false;
    }

    /** Returns true if two cells can connect, connecting them both in the directions passed as parameter.
     */
    private boolean connectCells(char fill, Cell cellFrom, Cell cellTo,Dir dirFrom, Dir dirTo) {
        if (cellFrom.canConnectFrom(fill,dirFrom) & cellTo.canConnectFrom(fill,dirTo)) {
            cellFrom.connectFrom(fill,dirFrom );
            cellTo.connectFrom(fill,dirTo );
            return true;
        }
        return false;
    }

    /**returns true if it disconnected two cells that could be disconnected
     *
     * @param fill character filled
     * @param cellFrom origin cell of connection
     * @param cellTo destination cell of connection
     * @param dirFrom direction be disconnected in origin cell
     * @param dirTo direction to be disconnected in destination cell
     * @return true if the disconnection was successful.
     */
    private boolean disconnectCells(char fill, Cell cellFrom, Cell cellTo,Dir dirFrom, Dir dirTo) {
        if (cellFrom.canDisconnectFrom(fill,dirFrom) & cellTo.canDisconnectFrom(fill,dirTo)) {
            cellFrom.disconnectFrom(fill,dirFrom );
            cellTo.disconnectFrom(fill,dirTo );
            return true;
        }
        return false;
    }

    /** Un-links Position pos of the grid.
     *  For every direction that the cell is connected, disconnects adjacent cell on opposite direction.
     *  Then, it disconnects selected cell on the direction it is connected.
     *  Finally, it will determine recursively whether the cell on the end of the connection is a terminal.
     * @param pos position of cell in grid to be unlinked
     * @return true if it is unlinked.
     */
    public boolean unlink(Position pos) {
        Cell cellFrom=this.getCellFromPosition(pos);
        char fillFrom = cellFrom.getFilled();
        boolean unlinked =false;

        if (fillFrom>0) {
            for (Dir d : cellFrom.dirConnected) {
                if (d != Dir.EMPTY ) {
                    Position newPos = pos.newPositionFromDirection(d);
                    this.getCellFromPosition(newPos).disconnectFrom(fillFrom,d.complement());
                    cellFrom.disconnectFrom(fillFrom, d);
                    unlinked=true;
                    if (this.disconnectIfLastNotTerminal(fillFrom,newPos,d.complement()))
                        break;
                }
            }
        }
        return unlinked;
    }

    /**Returns true if the end of the connection referenced in the direction dirFrom of Position pos of the array is not a terminal.
     * Recursively, disconnects all the connections in the cells of that connection.
     * @param fillFrom character filled in origin cell.
     * @param pos Position of reference cell in grid
     * @param dirFrom origin direction of disconnection to be evalued.
     * @return true if the end of the connection is not a terminal.
     */
    private boolean disconnectIfLastNotTerminal(char fillFrom,Position pos,Dir dirFrom){
        Cell cellFrom=this.getCellFromPosition(pos);

        if (!(cellFrom instanceof TerminalCell)) {
            for (Dir d : cellFrom.dirConnected) {
                if (d != Dir.EMPTY && d != dirFrom) {
                    Position newPos = pos.newPositionFromDirection(d);
                    if (this.disconnectIfLastNotTerminal(fillFrom,newPos,d.complement())) {
                        this.getCellFromPosition(newPos).disconnectFrom(fillFrom,d.complement());
                        cellFrom.disconnectFrom(fillFrom,d);
                        return true;
                    }
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**puts a reference in position l,i of the grid to the cell passed as parameter.
     *
     * @param l line in grid.
     * @param i column in grid.
     * @param cell cell to be put.
     */
    void putCell(int l, int i, Cell cell) {
        this.grid[l][i]=cell;
    }

    /**returns the cell located in position p of the grid
     * @param p position in the grid
     * @return cell from position
     */
    private Cell getCellFromPosition(Position p) {
        return this.grid[p.getLine()][p.getCol()];
    }
}