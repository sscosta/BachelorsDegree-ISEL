import isel.leic.pg.Console;

public class Board {
	public static final int  // Board dimensions (N=4 -> 6x7 ; N=3 -> 5x5 ; ...)
		    DIM_LINES = InLine.N*2-InLine.N/2, DIM_COLS = InLine.N*2-1;
	public static final int  // Visual dimensions in console coordinates
            VIEW_LINES = DIM_LINES * (Piece.HEIGHT+1) +1,
            VIEW_COLS = DIM_COLS * (Piece.WIDTH+1) +1+21;
	private static final int  // Visual positions in console coordinates
			VIEW_TOP = Piece.HEIGHT, VIEW_LEFT = 1;
	private static final int
			GRID_COLOR = Console.BROWN;

	/**
	 * Draws the entire grid without pieces
	 */
	public static void drawGrid() {
		Console.cursor(VIEW_TOP,VIEW_LEFT);
		drawDivHorizontal();					// First horizontal division. Top of the first row of holes.
		for (int l = 0; l < DIM_LINES; l++) {
			int h;
			for (h = 0; h < Piece.HEIGHT; h++) {// Each line of holes
				Console.cursor(VIEW_TOP+l*(Piece.HEIGHT+1)+h+1, VIEW_LEFT);
				drawDivVertical();
			}
			Console.cursor(VIEW_TOP+l*(Piece.HEIGHT+1)+h+1, VIEW_LEFT);
			drawDivHorizontal();				// Base of each row of holes
		}
	}

	private static void drawDivHorizontal() {
		Console.color(Console.WHITE,GRID_COLOR);
		Console.print('o');
		for (int c = 0; c < DIM_COLS; c++) {
			for (int w = 0; w < Piece.WIDTH; w++) Console.print('-');
			Console.print('o');
		}
	}

	private static void drawDivVertical() {
		Console.color(Console.WHITE,GRID_COLOR);
		Console.print('|');
		for (int c = 0; c < DIM_COLS; c++) {
			Console.color(Console.WHITE,Console.BLACK);
			for (int w = 0; w < Piece.WIDTH; w++) Console.print(' ');
			Console.color(Console.WHITE,GRID_COLOR);
			Console.print('|');
		}
	}

	/**
	 * Converts the column number in the grid to coordinates in the console where the column begins.
	 * @param col The column number (0..DIM_COLS-1)
	 * @return The Console column.
	 */
    public static int toViewCol(int col) {
        return VIEW_LEFT+col*(Piece.WIDTH+1)+1;
    }

	/**
	 * Converts the line number in the grid to coordinates in the console where the line begins.
	 * @param lin The line number (0..DIM_LINES-1)
	 * @return The Console line.
	 */
	public static int toViewLin(int lin) {
        return VIEW_TOP+lin*(Piece.HEIGHT+1)+1;
    }
}
