import java.util.ArrayList;

class Statistics {
    public static void main(String[] args) {
        init();
        addCoins(5);
        addGame();
    }

    private final static String STATISTICSFILENAME ="statistics.txt";
    private static int games;
    private static int coins;

    public static void init(){
        load();
    }

    public static void addGame(){
        games++;
        save();
    }

    public static void addCoins(int c){
        coins+=c;
        save();
    }
    public static int getGames(){return games;}
    public static int getCoins(){return coins;}

    public static void clear(){
        coins=0;
        games=0;
    }



    //Carrega estatisticas a partir de um ficheiro
    private static void load(){
        clear();

        ArrayList<String> SL=FileAccess.load(STATISTICSFILENAME,2);

        if (SL.size()>=2) {
            games =Integer.parseInt(SL.get(0) );
            coins =Integer.parseInt(SL.get(1) );
        }
    }

    // Grava as estatisticas
    public static void save(){
        ArrayList<String> SL=new ArrayList<>(2);
        SL.add(""+games);
        SL.add(""+coins);
        FileAccess.save(STATISTICSFILENAME,SL);
    }
}
