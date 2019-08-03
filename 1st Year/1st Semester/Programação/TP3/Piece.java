import isel.leic.pg.Console;

public class Piece {
	public static final int	    // Piece dimensions (3x3 ; 2x2 ; 3x2 ... )
            HEIGHT = 3, WIDTH = 3;
	public static final int     // Colors of pieces for each player
            PLAYER1_COLOR = Console.BLUE, PLAYER2_COLOR = Console.RED;
    private static final int FALL_STEP = 75; // 75 milliseconds second by step

	// The piece state
    public final boolean player;     // Current player (false-1 , true-2) [can not be changed]
	int line;                 // Line in Board coordinates (-1 above the board)
	int column;               // Column in Board coordinates

  public static Piece last;                       // Reference to the last piece placed in board

    // Only current piece may be falling
    private static boolean falling;                 // If is moving in the column
    private static long nextStepTime;               // Time to next step in piece falling

    /**
     * Build a piece for a player in the indicated position.
     * @param lin   The line of start position (0 .. Board.DIM_LINES-1) or -1 if is in the top
     * @param col   The column of start position (0 .. Board.DIM_COLS-1)
     * @param player The player (false-1, true-2)
     */
    public Piece(int lin, int col, boolean player) {
        this.player = player;
        line = lin;
        column = col;
    }

    /**
     * Build a piece for a player in the top of the board aligned with the indicated column.
     * @param player The player (false-1, true-2)
     * @param col The column of start position (0 .. Board.DIM_COLS-1)
     */
    public Piece(boolean player, int col) {
        this(-1,col,player);
        falling = false;         // not falling
        show();                  // show the piece in the top of board
    }

    /**
     * Returns the current line.
     * @return current line (0 .. Board.DIM_LINES-1)
     */
    public int getLine()         { return line; }
    /**
     * Returns the current column.
     * @return current column (0 .. Board.DIM_COLS-1)
     */
    public int getColumn()       { return column; }
    /**
     * Returns true if it is falling
     * @return
     */
    public boolean isFalling()   { return Piece.falling; }

    /**
     * Shows the piece in its current position.<br/>
     * The interior is filled with 'V' if the piece is on the top or with '*' if it was last placed on the grill.
     */
    public Piece previous;

    public void show() { // Show the piece in the current position
        char inside = (line==-1) ? 'V' : (this==last) ? '*' : ' ';
        int viewL = Board.toViewLin(line);
        int viewC = Board.toViewCol(column);
        Console.color(Console.BLACK, player ? PLAYER2_COLOR : PLAYER1_COLOR);
        for (int l = 0; l < HEIGHT ; l++, viewL++) {
            Console.cursor(viewL, viewC);
            for (int c = 0; c < WIDTH; c++) Console.print(inside);
        }
    }


    /**
     * Hide the piece in its current position.
     */
    public void hide() {  // Hide the piece in the current position
        int viewL = Board.toViewLin(line);
        int viewC = Board.toViewCol(column);
        Console.setBackground(Console.BLACK);
        for (int l = 0; l < HEIGHT ; l++, viewL++) {
            Console.cursor(viewL, viewC);
            for (int c = 0; c < WIDTH; c++) Console.print(' ');
        }
    }
    /**
     * Move the piece to the left column, if possible
     * Hides it in the previous position and shows it in the new position
     */
    public void jumpLeftColumn()  {
        if (!falling && column>0) move(0,-1);
    }
    /**
     * Move the piece to the right column, if possible
     * Hides it in the previous position and shows it in the new position
     */
	public void jumpRightColumn() {
        if (!falling && column<Board.DIM_COLS-1) move(0,+1);
    }
    /**
     * Mark the piece as falling.<br/>
     * NOTE: Method stepFall() must be called repeatedly during fall.
     * @see #stepFall()
     */
    public void startFall() {
        if(InLine.availablePosition(column)!=-1) {
            if (falling) return;
            falling = true;
            nextStepTime = System.currentTimeMillis() + FALL_STEP;  // Sets the next move down time
        }
    }
    /**
     * Make a step of the movement of the fall, if the right time is reached.<br/>
     * Returns false if the current piece is falling but can not fall any more.
     * @return true if the method should continue to be called.
     */
    public boolean stepFall() {
        if (!falling) return true;          // Prevents a possible call without the piece to be falling.
        if (line>=Board.DIM_LINES-1-InLine.contadorLinhasPreenchidas())        // Condition to stop fall  [TO CHANGE]
            return false;
        if (System.currentTimeMillis()>=nextStepTime) { // The right time?
            move(1, 0);                     // Move to next line
            nextStepTime += FALL_STEP;      // Set next time to move the piece
        }
        return true;
    }



    /**
     * Stops the falling movement and shows the piece as the last one placed on the grid.
     */
    public void fix() {
        falling = false;    // Mark as stop.
        Piece previous = last;              // The previous last piece
        last = this;                        // The actual last piece
        /*InLine.undo [0] = InLine.undo [1];
        InLine.undo [1] = InLine.undo[2];*/
        if (InLine.winningPlayer == player && InLine.registoPieces[InLine.playernum(InLine.winningPlayer)-1] == 0 && InLine.registoPieces[InLine.playernum(!InLine.winningPlayer)-1] == 0)
            last.show();         //do not show previous when a new game begins
        else if (previous!=null) {
            previous.show();    // Show previous as normal piece
            last.show();        // Show current as the last
        }
    }

    private void move(int dLin, int dCol) {
		hide();
        column += dCol;
        line += dLin;
		show();
	}
}
