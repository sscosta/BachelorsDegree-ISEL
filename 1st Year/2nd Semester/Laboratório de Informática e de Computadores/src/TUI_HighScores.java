class TUI_HighScores {
    private static final String TXTSCORE ="Score:";
    private static final String TXTNAME ="Name:";

    public static void printHighScore(int score){
        TUI.writeAtPosition(TXTSCORE,1,0);
        TUI.writeAtPosition(""+score,1,TXTSCORE.length());
    }

    public static String getName(){
        TUI.writeAtPosition(TXTNAME,0,0);
        return TUI.getString(5,0,TXTNAME.length());
    }

}
