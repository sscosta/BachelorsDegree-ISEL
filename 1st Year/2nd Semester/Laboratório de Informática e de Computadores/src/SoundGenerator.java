
import isel.leic.utils.Time;

public class SoundGenerator {
    public static void main(String[] args) {
        HAL.init();
        SerialEmitter.init();
        //init();
        setVolume(1);
        do {
            play(SOUND.NEWGAME);
            Time.sleep(2000);
            stop();
            Time.sleep(2000);
            play(SOUND.SHOT);
            Time.sleep(2000);
            stop();
            Time.sleep(2000);
            play(SOUND.GAME);
            Time.sleep(2000);
            stop();
            Time.sleep(2000);
            play(SOUND.GAMEOVER );
            Time.sleep(2000);
            stop();
            Time.sleep(2000);

        }while(true);

    }

    private static final int CMD_STOP_MASK =0;//0b00;
    private static final int CMD_PLAY_MASK =1;//0b10;
    private static final int CMD_SETSOUND_MASK =2;//0b01;
    private static final int CMD_SETVOL_MASK =3;//0b11;

    enum SOUND {GAMEOVER,SHOT, GAME,NEWGAME}

    private static int lastSOUND=-1;

    // Inicia a classe, estabelecendo os valores iniciais.
    public static void init() {
        setVolume(2);
        play(SOUND.NEWGAME);
        lastSOUND=-1;
    }

    // Envia comando para reproduzir um som, com a identificação deste
    public static void play(SOUND sound) {
        lastSOUND=sound.ordinal();
        send(CMD_PLAY_MASK,0);                 // para o simulador fazer primeiro play depois setsound
        send(CMD_SETSOUND_MASK,sound.ordinal());     // para o hardware fazer primeiro setsound depois play
    }

    public static void playIfDiferent(SOUND sound) {
        if (sound.ordinal()!=lastSOUND) {
            stop();
            play(sound);
            lastSOUND=sound.ordinal();
        }
    }

    //    // Envia comando para parar o som
    public static void stop() {
        lastSOUND=-1;
        send(CMD_STOP_MASK,0);
    }

    //    // Envia comando para definir o volume do som
    private static void setVolume(int volume) {
        send(CMD_SETVOL_MASK,volume);
    }

    private static void send (int command, int data){
        data<<=2;
        data=data+command;
        SerialEmitter.send(SerialEmitter.Destination.SSC,4, data);
    }
}
