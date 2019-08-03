public class Score  {

    private final String name;
    private final int score;
    private static final int MAX_NAME_LENGTH = 7;

    public  Score(String name,int score){
        if (name.length()>MAX_NAME_LENGTH) name=name.substring(0,MAX_NAME_LENGTH);
        this.name=name;
        this.score=score;
    }


    public String getName(){
        return this.name;
    }

    public int getScore(){
        return this.score;
    }

    public static Score fromText(String text){
        if (!text.isEmpty() ) {
            String[] splitedText = text.split(";");
            if (splitedText.length == 2) {
                return new Score(splitedText[1], Integer.parseInt(splitedText[0]));
            }
        }
        return null;
    }
    public String toString (){
        return "" + this.score + ";" + this.name;
    }
}
