import isel.leic.utils.Time;

class Game {
    public static void main(String[] args) {
        TUI.init();
        SoundGenerator.init();
        Game.generateGame();
    }


    private final static char SHOT_KEY = '*';
    private final static char QUIT_KEY = '#';
    private final static int BASE_ASCII = 48;
    private final static int upper_alien_number = 9;
    private final static int lower_alien_number = 0;
    private final static int baseSpeed = 1500;
    private final static int baseScoreToLevelUP = 100;
    private final static int timeBetweenExplosions=100;
    private final static long GAME_OVER_DELAY=2000;
    private final static char [] alienTrainEmpty = {0,0,0,0,0,0,0,0,0,0,0,0,0,generateAlienNumber()};

    private static char [] alienTrain ;

    private static char missileToShot = '1';
    private static int speed;
    private static int level;
    private static int scoreToLevelUP;
    private static int score ;

    private static long timeNextAlien;
    private static long timeNextExplosion;
    private static long timeStopSoundShot;

    public static int generateGame()
    {
        boolean invalidate ;

        speed=baseSpeed;
        level=1;
        scoreToLevelUP =baseScoreToLevelUP;
        score=0;

        timeNextAlien = System.currentTimeMillis()+speed;
        timeNextExplosion =0;
        alienTrain=alienTrainEmpty.clone();

        TUI.setCursorBlink(false);
        TUI.clearScreen();
        SoundGenerator.stop();
        SoundGenerator.play(SoundGenerator.SOUND.GAME);

        TUI_Game.printMissileToShot(missileToShot);
        TUI_Game.printHumanShip();
        TUI_Game.printAliens(alienTrain);
        TUI_Game.printScoreText();
        TUI_Game.printScore(score);
        TUI_Game.printLevelText();
        TUI_Game.printLevel(level);

        while(alienTrain[0] <10) {                       // condiçao sera ate caracter chegar à nave
            invalidate = evaluateKey(TUI.getKey());
            if (evaluateExplosion()) invalidate=true;
            if (evaluateAlien())  invalidate=true;

            if(invalidate)
                TUI_Game.printAliens(alienTrain);

            evaluateLevel();

            if ( timeStopSoundShot>0 && System.currentTimeMillis()> timeStopSoundShot) {
                timeStopSoundShot=0;
                SoundGenerator.playIfDiferent(SoundGenerator.SOUND.GAME);
            }
        }

        SoundGenerator.playIfDiferent(SoundGenerator.SOUND.GAMEOVER );
        TUI_Game.printGameOver();
        Time.sleep(GAME_OVER_DELAY);
        SoundGenerator.stop();
        return score;
    }

    private static boolean evaluateKey(char key){
        if(key != 0) {
            switch (key) {
                case (SHOT_KEY):
                    return evaluateShoot();
                case (QUIT_KEY):
                    alienTrain[0] = 11;
                    break;
                default:
                    updateMissileToShot(key);
            }
        }
        return false;
    }


    private static Boolean evaluateShoot() {
        boolean invalidate=false;
        SoundGenerator.playIfDiferent(SoundGenerator.SOUND.SHOT);
        for (int i = 2; i <alienTrain.length ; i++) {
            TUI_Game.printMissileShoted(i);
            if(alienTrain[i] >10) {
                if (missileToShot == alienTrain[i]) {
                    alienTrain[i] = TUI_Special_Char.EXPLOSION1.getChar() ;
                    timeNextExplosion = System.currentTimeMillis()+timeBetweenExplosions;
                    evaluateScore(missileToShot);
                    invalidate= true;
                }
                break;
            }
        }

         if (invalidate && timeNextExplosion>0)
            SoundGenerator.playIfDiferent(SoundGenerator.SOUND.NEWGAME);
         else
            SoundGenerator.playIfDiferent(SoundGenerator.SOUND.GAME);

        return invalidate;
    }


    private static void evaluateScore(char alien) {
        score += ((int)alien)-BASE_ASCII+1;
        TUI_Game.printScore(score);
    }

    private static void updateMissileToShot(char key) {
        missileToShot =key;
        TUI_Game.printMissileToShot(missileToShot);
    }

    private static boolean evaluateAlien(){
        if(System.currentTimeMillis()> timeNextAlien ) {
            timeNextAlien = System.currentTimeMillis()+speed;
            addAlienToArray(alienTrain);
            return true;
        }
        return false;
    }

    private static boolean evaluateExplosion(){
        boolean invalidate =false;
        if(timeNextExplosion >0 && System.currentTimeMillis()>= timeNextExplosion ) {
            for (int i = 0; i <alienTrain.length ; i++) {
                if (alienTrain[i]>10) break;

                if (alienTrain[i] == TUI_Special_Char.EXPLOSION2.getChar()) {
                    alienTrain[i] = TUI_Special_Char.VAZIO.getChar();
                    timeNextExplosion = 0;
                    invalidate = true;
                } else if (alienTrain[i] == TUI_Special_Char.EXPLOSION1.getChar()) {
                    alienTrain[i] = TUI_Special_Char.EXPLOSION2.getChar();
                    timeNextExplosion = System.currentTimeMillis() + timeBetweenExplosions;
                    invalidate = true;
                }
            }
        }
        if (timeNextExplosion>0) {
            SoundGenerator.playIfDiferent(SoundGenerator.SOUND.NEWGAME);
        }
        else {
            SoundGenerator.playIfDiferent(SoundGenerator.SOUND.GAME);
        }
        return invalidate;
    }

    private static void evaluateLevel(){
        if(score >= scoreToLevelUP) {
            level++;
            speed -= 100;
            scoreToLevelUP +=50;
            TUI_Game.printLevel(level);
        }
    }

    private static void addAlienToArray(char[] train) {
        for (int i = 1; i <train.length; i++) {
                    train[i-1] = train[i];
        }
        train[train.length-1] = generateAlienNumber();
    }

    private static  char generateAlienNumber() {
        return (char)((Math.random() * (upper_alien_number- lower_alien_number)) + lower_alien_number+BASE_ASCII);
    }

}
