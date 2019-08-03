import isel.leic.utils.Time;

class SpaceInvaders {
    public static void main(String[] args) {
        init();
        startMenu();

    }
    private final static char START_KEY='*';
    private final static int COIN_VALUE=2;
    private final static int timeBetweenHighScore=1500;

    private static final long DELAY_MENU = 10000;
    private static final long DELAY_SUBMENU = 10000;
    private static final long TIME_TO_SHOW_GOODBYE_MESSAGE = 2000;

    private static final char ASTERISK = '*';
    private static final char CARDINAL = '#';
    private static final char YES_CMD = '5';

    private static int CREDITS;

    private static long timeNextHighScore;
    private static char keyPressed ;
    private static int ordinalScore;

    // initiates the model
    private static void init(){
        TUI.init();
        SoundGenerator.init();
        Statistics.init();
        Scores.init();
        CREDITS=0;
    }

    // Este metodo gere o ecra de inicio: a aplicacao inicia com o título do jogo mostrado na
    // linha 0 do ecrã LCD, bem como os créditos do jogador na linha 1.
    // As pontuacoes maximas sao mostradas por ordem decrescente, cada uma durante um segundo e meio
    // ou sempre que e pressionada uma tecla que nao seja a tecla de inicio de jogo '*'.

    private static void startMenu() {
        while (true) {
            TUI.setCursorBlink(false);
            TUI_SpaceInvaders.printGameTitle();
            TUI_SpaceInvaders.printGameCredits(CREDITS);

            timeNextHighScore = System.currentTimeMillis() + 2*timeBetweenHighScore;

            ordinalScore = 0;
            boolean resetMenu =false;

            while (!resetMenu) {

                resetMenu = evaluateMaintenance();

                if (!resetMenu) {
                    evaluateNewCoins();

                    keyPressed = TUI.getKey();

                    showOtherHigthScore();

                    resetMenu = evaluateNewGame();
                }

            }
        }
    }

    private static void evaluateNewCoins(){
        if (CoinAcceptor.checkForInsertedCoin()) {
            Statistics.addCoins(COIN_VALUE);
            CREDITS += COIN_VALUE;
            TUI_SpaceInvaders.printGameTitle();
            TUI_SpaceInvaders.printGameCredits(CREDITS);
            timeNextHighScore = System.currentTimeMillis() + timeBetweenHighScore;
        }
    }

    private static void showOtherHigthScore() {
        if ((keyPressed != 0 && keyPressed != START_KEY) || (System.currentTimeMillis() > timeNextHighScore)) {
            if (ordinalScore < Scores.getScoresCount() - 1) {
                ordinalScore++;
                TUI_SpaceInvaders.printHighScore(ordinalScore, Scores.getScoreByOrdinal(ordinalScore));
            } else {
                ordinalScore = 0;
            }
            timeNextHighScore = System.currentTimeMillis() + timeBetweenHighScore;
        }
    }

    private static boolean evaluateNewGame(){
        if (CREDITS > 0 && keyPressed == START_KEY) {
            CREDITS -= 1;
            Statistics.addGame();
            evaluateNewScore(Game.generateGame());
            return true;
        }
        return false;
    }

    private static void evaluateNewScore(int score){
        if (Scores.isNewScoreHighScore(score)){
            TUI.clearLine(0);
            TUI_HighScores.printHighScore(score);
            String name = TUI_HighScores.getName();
            if (name.length()>0)
                Scores.addScore(name,score);
        }
    }


    /***********MAINTENANCE******************/
    private static boolean evaluateMaintenance(){
        if (M.checkIsInMaintenance()) {
            char currKey;
            SoundGenerator.stop();
            boolean exitMaintenance=false;
            while (!exitMaintenance) {
                TUI_M.printMaintenanceMenu();
                currKey = TUI.waitKey(DELAY_MENU);
                if (currKey == CARDINAL)
                    countersMenu();
                if (currKey == ASTERISK)
                    if (shutDownMenu())
                        exit();
                    else
                        exitMaintenance=true;
                if (currKey != 0 && currKey != CARDINAL && currKey != ASTERISK)
                    if (wannaPlay())
                        Game.generateGame();
            }
            return true;
        }
        return false;
    }

    private static boolean wannaPlay() {
        TUI_M.printWannaPlayMeny();
        char cmd = TUI.waitKey(DELAY_SUBMENU);
        return (cmd==YES_CMD);
    }


    private static boolean shutDownMenu() {
        TUI_M.printShutDownMenu();
        char cmd = TUI.waitKey(DELAY_SUBMENU);
        return (cmd==YES_CMD);
    }

    //saves changes to file and prints a goodbye message
    private static void exit() {
        Statistics.save();
        TUI_M.printGoodByeMessage();
        Time.sleep(TIME_TO_SHOW_GOODBYE_MESSAGE);
        System.exit(0);
    }

    private static void countersMenu() {
        TUI_M.printStatistics(Statistics.getGames(),Statistics.getCoins());
        char cmd = TUI.waitKey(DELAY_SUBMENU);
        if (cmd ==ASTERISK)
            if (clearCountersMenu())
                Statistics.clear();
    }

    private static boolean clearCountersMenu() {
        TUI_M.printClearCountersMenu();
        char cmd = TUI.waitKey(DELAY_SUBMENU);
        return  (cmd == YES_CMD) ;
    }
}
