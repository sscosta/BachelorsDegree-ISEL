class TUI_SpaceInvaders {
    private static final String TXTGAMETITLE = " Space Invaders ";
    private static final String TXTGAME = "GAME "+ TUI_Special_Char.HUMANSHIP.getChar()+" "+ TUI_Special_Char.ALIANSHIP.getChar()+ " "+ TUI_Special_Char.ALIANSHIP.getChar()+" ";

    /*******Metodos usados pelo SpaceInvaders*********/

    public static void printGameTitle(){
        TUI.writeAtPosition(TXTGAMETITLE,0,0);
    }

    public static void printGameCredits(int c){
        TUI.writeAtPosition(String.format("%1$-10s $%2$ 3d",TXTGAME,c),1,0);
    }

    public static void printHighScore(int ord, Score sc){
        TUI.writeAtPosition(String.format("%1$02d-%2$-8s%3$5s",ord,sc.getName(),sc.getScore()),1,0);
    }
}
