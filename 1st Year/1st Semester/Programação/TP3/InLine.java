import isel.leic.pg.Console;
import java.awt.event.KeyEvent;

public class InLine {
    public static final int N = 4;           // Number of pieces on a winning line. (3,4,5...)
    static final int                 // Console dimensions
            VIEW_LINES = Board.VIEW_LINES + Piece.HEIGHT + 1, VIEW_COLS = Board.VIEW_COLS + 2;
    // The game state
    public static boolean player = false;    // Current player (false- 1 , true- 2)
    public static Piece piece;               // Current piece (not placed on the board)
    public static int[][] arrBoard = new int[2 * N - N / 2][2 * N - 1]; //saves the positions of the pieces played by each player
    public static int[] registoPieces = {0, 0}; //saves the number of moves by each player
    public static int[] registoVitorias = {0, 0}; //saves victories - position 0 player 1, position 1 player 2
    public static boolean winningPlayer = false;         //player that wins previous game begins the following

    /**
     * Start method of game
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        init();
        run();        // Loop of the game
    }

    private static void init() {
        Console.open("PG In Line (" + N + ')', VIEW_LINES, VIEW_COLS);
        Console.exit(true);				 // Uncomment to enable exit closing the console
        startGame();
    }

    private static void startGame() {
        Board.drawGrid();                    // Draw initial board
        piece = new Piece(player, Board.DIM_COLS / 2);  // Create first piece
        drawCommands();
    }
    //draws a piece and victory registry table for both of the players
    public static void drawRecord() {
        Console.cursor(2, VIEW_COLS + 7 - 21);
        Console.color(Console.WHITE, Console.BLACK);
        Console.print("Player");                  //Table's header

        //player 1
        Console.cursor(3, VIEW_COLS + 7 - 21);
        Console.color(Console.WHITE, Console.BLUE);
        Console.print(" 1 ");
        drawRegistoJogador1();

        // Player 2
        Console.cursor(3, VIEW_COLS + 10 - 21);
        Console.color(Console.WHITE, Console.RED);
        Console.print(" 2 ");
        drawRegistoJogador2();

        //table entries
        Console.cursor(4, VIEW_COLS + 1 - 21);
        Console.color(Console.WHITE, Console.BLACK);
        Console.print("Games:");

        Console.cursor(5, VIEW_COLS - 21);
        Console.color(Console.WHITE, Console.BLACK);
        Console.print("Pieces:");
    }

    public static void drawRegistoJogador1() {

        Console.cursor(4, VIEW_COLS + 7 - 21);
        Console.color(Console.YELLOW, Console.DARK_GRAY);
        if (registoVitorias[0] < 10)
            Console.print(" " + registoVitorias[0] + " ");
        else Console.print(" " + registoVitorias[0]);

        Console.cursor(5, VIEW_COLS + 7 - 21);
        Console.color(Console.YELLOW, Console.DARK_GRAY);
        if (registoPieces[0] < 10)
            Console.print(" " + registoPieces[0] + " ");
        else Console.print(" " + registoPieces[0]);
    }

    public static void drawRegistoJogador2() {

        Console.cursor(4, VIEW_COLS + 10 - 21);
        Console.color(Console.YELLOW, Console.DARK_GRAY);
        if (registoVitorias[1] < 10)
            Console.print(" " + registoVitorias[1] + " ");
        else Console.print(" " + registoVitorias[1]);

        Console.cursor(5, VIEW_COLS + 10 - 21);
        Console.color(Console.YELLOW, Console.DARK_GRAY);
        if (registoPieces[1] < 10)
            Console.print(" " + registoPieces[1] + " ");
        else Console.print(" " + registoPieces[1]);
    }
    //writes a table with the commands of the game to the right of the window.
    public static void drawCommands() {
        drawIndividualCommand(VIEW_LINES - 8, " Left- Move to left ");
        drawIndividualCommand(VIEW_LINES - 7, "Right- Move to right");
        drawIndividualCommand(VIEW_LINES - 6, " Down- Drop piece   ");
        drawIndividualCommand(VIEW_LINES - 5, "Space- Drop piece   ");
        drawIndividualCommand(VIEW_LINES - 4, "  Esc- Terminate    ");
        drawIndividualCommand(VIEW_LINES - 3, "   Up- Undo last    ");
        drawIndividualCommand(VIEW_LINES - 2, "   F2- Auto 2 level ");
    }
    //given a line and a string of text, writes the string on the line in white over dark gray background
    public static void drawIndividualCommand(int line, String Command) {
        Console.cursor(line, VIEW_COLS - 21);
        Console.color(Console.WHITE, Console.DARK_GRAY);
        Console.print(Command);
    }

    private static final String GAME_OVER_TXT = "GAME OVER";
    //draws a game over message to the center of the display. closes the window
    private static void terminate() {
        Console.cursor(VIEW_LINES / 2, (VIEW_COLS - GAME_OVER_TXT.length()) / 2);
        Console.color(Console.RED, Console.YELLOW);
        Console.print(GAME_OVER_TXT);        // Message GAME OVER
        while (Console.isKeyPressed()) ;    // Wait if any key is pressed
        Console.waitKeyPressed(5000);        // Wait 5 seconds for any key
        Console.close();                    // Close Console window
    }
/*    public static Piece [] undo = new Piece [2];
    public static void writePiece(){
        int size=0
    }*/
    private static void run() {
        int key;           // Current key
        do {
            /*writePiece();*/
            drawRecord();
            key = Console.getKeyPressed();  // Read a key
            if (endOfGame()) {             //true if the game ends, false otherwise
                askToRestart(key);                                //displays end of game event, winner and option to start a new game
            }
            if (!piece.stepFall() && availablePosition(piece.column) != -1 && !endOfGame()) {          // Try move the piece down(modified for the case of a column being totally filled)
                stopFall();                 // Fix the piece
                /*System.out.print("Vitorias player 1 "+ vitoriasplayer1 + " , p2 :" + vitoriasplayer2);*/
            }
            if (key != Console.NO_KEY && !endOfGame())        // A key was pressed ?
                action(key);                // Do action for the key(modified for the case of a column being totally filled)
        } while (key != KeyEvent.VK_ESCAPE);  // Key to abort game.
        /*terminate();
        System.exit(0);*/
    }

    //scans a column of the array from the bottom-up. returns the index of the first available line.
    //if totally filled, returns -1
    public static int availablePosition(int column) {
        int i = (2 * N - N / 2) - 1;
        do {
            if (arrBoard[i][column] == 0)
                return i;
            if (i > 0)
                i--;
            else break;
        } while (arrBoard[i][column] != 0 || i >= 0);
        return -1;
    }
    private static void stopFall() {
        piece.fix();                        // Fix current piece
        ++registoPieces[playernum(player) - 1]; //adds to the number of played pieces
        /*for (int i =0; i<=registoPieces.length -1; i++){      //print registoPieces
            if (i==0)
                System.out.print("{ "+ registoPieces[i] + " ,");
            else if ( i == registoPieces.length -1)
                System.out.println(" " + registoPieces[i] + " }");
            else System.out.print(" " + registoPieces[i] + " ,");
        }*/
        writeArray();                       // write the position of the piece in the array
        if (endOfGame()) {
            winningPlayer = player;          //saves the player who won the game
            if (!winningPlayer && !draw())    //adds one to vitorias p1
                ++registoVitorias[0];
            else if (winningPlayer && !draw()) //adds one to vitórias p2
                ++registoVitorias[1];
            return;
        }
        player = !player;                   // Swap player
        piece = new Piece(player, piece.getColumn()); // Create other piece
    }

    //returns a bi-dimensional array, totally filled with zeros (used in the beginning of a new game
    // to keep a record of the positions of the pieces during the game)
    public static int[][] resetArray(int[][] a, int lines, int columns) {
        for (int i = 0; i <= lines - 1; i++) {
            for (int j = 0; j <= columns - 1; j++) {
                a[i][j] = 0;
            }
        }
        return a;
    }
    //returns a one-dimensional array filled with zeros (piece registry)
    public static int[] resetArray(int[] a, int dim) {
        for (int i = 0; i <= dim - 1; i++) {
            a[i] = 0;
        }
        return a;
    }

    //shows a message with the type of event that caused the end of game. if ESC is pressed, aborts the game, if ENTER is pressed
    //it erases the message and a new game is started.
    public static void askToRestart(int key) {
        drawEndOfGameMessage();
        if ( key==KeyEvent.VK_ESCAPE){
            terminate();
            System.exit(0);
        }
        else if (key == KeyEvent.VK_ENTER) {
            eraseEndOfGameMessage();
            resetArray(arrBoard, 2 * N - N / 2, 2 * N - 1);        //reset position tracker
            resetArray(registoPieces, 2);                   //reset counter of moves
            startGame();                                      //new game
            run();                                  //
        }

    }
    public static void eraseEndOfGameMessage(){
        for (int i = VIEW_LINES - 11; i <= VIEW_LINES - 10; i++) {
            for (int j = VIEW_COLS-21; j <= VIEW_COLS-1; j++) {
                Console.cursor(i, j);
                Console.color(Console.BLACK, Console.BLACK);
                Console.print(" ");
            }
        }
    }
    //shows the type of event that caused the end of the game, the winner, and an option to restart the game.
    public static void drawEndOfGameMessage() {
        String typeOfEnd = "";
        if (draw())
            typeOfEnd += "Tied Game";
        else if (winHor(playernum(winningPlayer)))
            typeOfEnd += "Horizontally";
        else if (winVert(playernum(winningPlayer)))
            typeOfEnd += "Vertically";
        else if (winDiag(playernum(winningPlayer)))
            typeOfEnd += "Diagonally";
        Console.cursor(VIEW_LINES - 11, VIEW_COLS-21);
        Console.color(Console.WHITE, Console.BLACK);
        if (draw())
            Console.print(typeOfEnd);
        else Console.print("P" + playernum(winningPlayer) + " won " + typeOfEnd);

        String restart = "Press ENTER";
        Console.cursor(VIEW_LINES - 10, VIEW_COLS-21);
        Console.color(Console.WHITE, Console.BLACK);
        Console.print(restart);        // Message new game?
    }

    //writes the number of the player in the position played.
    public static void writeArray() {
        int avLine = availablePosition(piece.column);
        if (avLine != -1) {
            if (player)
                arrBoard[avLine][piece.column] = 2;
            else
                arrBoard[avLine][piece.column] = 1;
            printArray(arrBoard);
        } else return;
    }
    //returns an integer. scans the column in which the last play was made from the bottom-up,
    //counting filled cells in that column.
    public static int contadorLinhasPreenchidas() {
        int contador = 0;
        for (int i = (2 * N - N / 2) - 1; i >= 0; i--) {
            if (arrBoard[i][piece.column] == 0)
                break;
            else if (arrBoard[i][piece.column] != 0)
                contador++;
        }
        return contador;
    }
    //associates keyboard commands to actions - see drawCommand method
    private static void action(int key) {
        switch (key) {
            case KeyEvent.VK_LEFT:
                piece.jumpLeftColumn();
                break;
            case KeyEvent.VK_RIGHT:
                piece.jumpRightColumn();
                break;
            case KeyEvent.VK_SPACE:
            case KeyEvent.VK_DOWN: {
                piece.startFall();
                break;
            }
            /*case KeyEvent.VK_F2:
                undolast();*/
            case KeyEvent.VK_ESCAPE:{
                terminate();
                System.exit(0);
            }
        }
        while (Console.isKeyPressed(key)) ;  // Wait to release key
    }
    /*public static void undolast(){
    }*/
    //passing a bi-dimensional array as a parameter, shows the elements in the array as a table
    private static void printArray(int matriz[][]) {
        for (int[] aMatriz : matriz) {
            for (int anAMatriz : aMatriz) {
                System.out.print(anAMatriz + " ");
            }
            System.out.println();
        }
    }
    //returns number of player - entry parameter boolean player
    public static int playernum(boolean p) {
        if (p)
            return 2;
        else
            return 1;
    }

//the followin methods have return type boolean and evaluate the end of the game. true- the game has ended.
//The game can either end as a draw or a victory of one of the players.
// Victory can be Horizontal, vertical or diagonal.

    // returns true if the game has ended, ie there was victory or draw
    private static boolean endOfGame() {
        return !(piece.getLine() < 0 || piece.getLine() > (2 * N - N / 2) - 1) && (win(playernum(player)) || draw());
    }
    //returns true if there was victory: vertical, horizontal, diagonal
    private static boolean win(int p) {
        return winVert(p) || winHor(p) || winDiag(p);
    }

    //returns true if one of the players has a sequence of N pieces in one of the columns.
    private static boolean winVert(int p) {
        int contadorwinvert = 0;
        if (contadorLinhasPreenchidas() >= N) { //vitória vertical
            for (int i = (2 * N - N / 2) - 1; i >= 0; i--) {
                if (arrBoard[i][piece.column] == p) {
                    contadorwinvert++;
                    if (contadorwinvert == N)
                        return true;
                } else if (arrBoard[i][piece.column] == 0)
                    return false;
                else if (arrBoard[i][piece.column] == 2 && p == 1 || arrBoard[i][piece.column] == 1 && p == 2)
                    contadorwinvert = 0;
            }
        }
        return false;
    }
    //returns true if one of the players has a sequence of N pieces in one of the lines.
    private static boolean winHor(int p) {
        int contadorwinhor = 0;
        for (int i = 0; i < (2 * N) - 1; i++) {
            if (arrBoard[piece.getLine()][i] == p) {
                ++contadorwinhor;
                /*System.out.println("Contador WinHOR qd está a contar = " + contadorwinhor);*/
                if (contadorwinhor == N) {
                   /* System.out.println("Contador WinHOR qd já contou = " + contadorwinhor);*/
                    return true;
                }
            } else if (arrBoard[piece.getLine()][i] == 0) {
                contadorwinhor = 0;
                /*System.out.println("Contador WinHOR qd é 0 = " + contadorwinhor);*/
            } else if (arrBoard[piece.getLine()][i] == 1 && p == 2 || arrBoard[piece.getLine()][i] == 2 && p == 1) {
                contadorwinhor = 0;
                /*System.out.println("Contador WinHOR qd é outro player = " + contadorwinhor);*/
            }
        }
        return false;
    }

    //returns true if one of the players has a sequence of N pieces in one of the diagonals(ascending, descending)
    private static boolean winDiag(int p) {
        return winDiagAsc(playernum(player)) || winDiagDesc(playernum(player));
    }

    //returns true if one of the players has a sequence of N pieces in an ascending diagonal of the board.
    private static boolean winDiagAsc(int player) {
        int row, col;
        for (row=2*N-N/2-1; row>=N-1; row--) {    //starts in rows 0, 1, or 2 (case N=4)
            for (col=0; col<=N-1; col++) {              //starts in columns 0, 1, 2, or 3 (case N=4)
                if (arrBoard[row][col] == player) {
                    int contadorWin = 1;
                    for (int i=row-1, j=col+1; i>=row-N-1 && j<=col+N-1 && i>=0 && j<=2*N-1; i--,j++ ) {
                        if (arrBoard[i][j] == player) contadorWin += 1;
                        if (contadorWin >= N) return true; // winner
                    }
                }
            }
        }
        return false;
    }
    //returns true if one of the players has a sequence of N pieces in an descending diagonal of the board.
    private static boolean winDiagDesc(int player){
        int row, col;
        for (row=0; row<=N-N/2; row++) {  //starts in rows 0, 1, or 2 (case N=4)
            for (col=0; col<=N-1; col++) {//starts in columns 0, 1, 2, or 3 (case N=4)
                if (arrBoard[row][col] == player) {
                    int contadorWin = 1;
                    for (int i=row+1, j=col+1; i<=row+N-1 && j<=col+N-1 && i<=2*N-N/2 && j<=2*N-1; i++,j++ ) {
                        if (arrBoard[i][j] == player) contadorWin += 1;
                        if (contadorWin >= N) return true; // winner
                    }
                }
            }
        }
        return false;
    }
    //returns true if the game ends in a tie.
    private static boolean draw(){
        if (!win (playernum(player))) {
            for (int i = 0; i < 2 * N - 1; i++) {
                if (arrBoard[0][i] == 0)
                    return false;
            }
            return true;
        }
        else return false;
    }
}