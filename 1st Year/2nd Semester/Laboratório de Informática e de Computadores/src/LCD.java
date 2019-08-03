import isel.leic.utils.Time;

class LCD { // Escreve no LCD usando a interface a 8 bits.
    public static void main(String[] args) {

        HAL.init();
        SerialEmitter.init();
        init();
        //write('A');
        write("0123456789ABCDEF");
        setcursorPosition(1,0);
        write("teste");
    }

    public static final int COLS = 16; // Dimensão do display.

    // Envia a sequência de iniciação para comunicação a 8 bits.
    public static void init() {
        //Function set
        writeCMD(0b00110000);
        //waits 5 milisec
        Time.sleep(5);
        writeCMD(0b00110000);
        //Display on/off control
        writeCMD(0b00110000);
        Time.sleep(1);
        //Entry mode set
        //writeCMD(0b00110000);
        //character font and number of lines
        writeCMD(0b00111000);
        //Sets entire display off, cursor on, and blinking of cursor position character (B)
        writeCMD(0b00001000);
        //Clears entire display and sets DDRAM address 0 in address counter
        writeCMD(0b00000001);
        //Sets cursor move direction and specifies display shift. Performed during data write and read
        writeCMD(0b00000110);
        //Sets display to on
        writeCMD(0b00001111);


    }

    // Escreve um comando/dados no LCD
    private static void writeByte(boolean rs, int data){
        data<<=1;
        int RS = !rs ?0:1;
        data+=RS;
        SerialEmitter.send(SerialEmitter.Destination.SLCD,9,data);
        Time.sleep(10);
    }

    // Escreve um comando no LCD
    private static void writeCMD(int data) {
        writeByte(false,data);
    }

    // Escreve um dado no LCD
    private static void writeDATA(int data) {
        writeByte(true,data);
    }


    // Escreve um caráter na posição corrente.
    public static void write(char c) {
        writeDATA((int) c);
    }

    // Escreve uma string na posição corrente.
    public static void write(String txt) {
        for (int i = 0; i < txt.length(); i++) {
            write(txt.charAt(i));
        }
    }

    // Envia comando para posicionar cursor (‘lin’:0..LINES-1 , ‘col’:0..COLS-1)
    public static void setcursorPosition(int lin, int col) {
        writeCMD(0x80+ (lin==1?0x40:0x0) +col );//0x80 - set cursor cmd; line 0= 0x0; line 1 =0x40;
    }

    public static void setCursorBlink(boolean blink) {
        writeCMD(blink?0xF:0xC);
    }


    public static void sendSpecialChar(int ordinal, int[] data){
        writeCMD( 64 | ordinal * 8);
        for(int i = 0; i < 8; ++i) {
            writeDATA(data[i]);
        }
    }

}
