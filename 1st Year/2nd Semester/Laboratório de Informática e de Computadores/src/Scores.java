import java.util.ArrayList;

class Scores {//implements Iterable<Score> {
    public static void main(String[] args) {
        TUI.init();
        Scores.init();
        addScore("PEDRO",100);

        for (Score hs: highscores) {
            System.out.println(hs.toString());
        }

    }

    private final static String HIGHSCORESFILENAME ="SIG_scores.txt";
    private final static int MAXHIGHSCORES=20;
    private final static ArrayList<Score> highscores=new ArrayList<>();

    public static void init(){
        load();
    }

    //Adiciona uma pontuacao a lista, dado um nome e uma pontuacao.
    public static void addScore(String name, int score){
        while (highscores.size() >=MAXHIGHSCORES) {
            highscores.remove(MAXHIGHSCORES-1);
        }
        highscores.add(new Score(name,score));
        sortScores();
        save();
    }

    // Ordena as pontuacoes introduzidas por ordem decrescente
    private static void sortScores(){
        highscores.sort((hs2, hs1) -> Integer.compare(hs1.getScore(), hs2.getScore()));
    }

    //Carrega pontuacoes a partir de um ficheiro
    private static void load(){
        highscores.clear();

        for (String s: FileAccess.load(HIGHSCORESFILENAME,MAXHIGHSCORES))
            highscores.add(Score.fromText(s));

        sortScores();
    }

    // Grava as alteracoes feitas para o ficheiro de pontuacoes
    private static void save(){
        ArrayList<String> SL=new ArrayList<>(MAXHIGHSCORES);
        for (Score hs: highscores) {
            SL.add(hs.toString());
        }
        FileAccess.save(HIGHSCORESFILENAME,SL);

    }

    public static  boolean isNewScoreHighScore(int score) {
        return score > 0 && highscores.size() > 0 && score >= highscores.get(highscores.size() - 1).getScore();
    }

    public static int getScoresCount(){
        return highscores.size();
    }

    public static Score getScoreByOrdinal(int ordinal){
        return highscores.get(ordinal);
    }
}
