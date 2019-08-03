class TUI_Game {

    private static final String TXTSCORE ="Score:";
    private static final String TXTLEVEL ="NIV:";
    private static final String TXTGAMEOVER = "*** Game Over ***";

    public static void printHumanShip(){
        TUI.writeAtPosition(TUI_Special_Char.HUMANSHIP.getChar(),0,1);
    }

    public static void printMissileToShot(char missile){
        TUI.writeAtPosition(missile,0,0);
    }

    public static void printMissileShoted(int col){
        TUI.writeAtPosition(TUI_Special_Char.MISSILE.getChar(),0,col);
        TUI.writeAtPosition(TUI_Special_Char.VAZIO.getChar(),0,col);
    }

    public static void printAliens(char[] alienTrain){
        TUI.writeAtPosition(new String(alienTrain),0,2);
    }

    public static void printScoreText(){
        TUI.writeAtPosition(TXTSCORE,1,0);
    }

    public static void printScore(int score){
        TUI.writeAtPosition(""+score,1,TXTSCORE.length());
    }

    public static void printGameOver() {
        TUI.writeAtPosition(TXTGAMEOVER,0,0);
    }

    public static void printLevelText(){
        TUI.writeAtPosition(TXTLEVEL,1,TUI.getLCDcols()- TXTLEVEL.length()-1);
    }

    public static void printLevel(int level){
        TUI.writeAtPosition(""+level,1, TUI.getLCDcols()-1);
    }
}
