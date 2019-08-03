import isel.leic.utils.Time;

class TUI {
    public static void main(String[] args) {
        HAL.init();
        SerialEmitter.init();
        LCD.init();
        KBD.init();
        createSpecialChars();

        // LCD.ClearLCD();
        /******** TESTE GETSTRING********/
        //String s = TUI.getString(5,0,6);
        //System.out.println(s);

        /******** TESTE SPECIAL CHARS********/
        writeAtPosition(""+TUI_Special_Char.VAZIO.getChar() + " " +
                            TUI_Special_Char.GUNSHIP.getChar() + " " +
                            TUI_Special_Char.HUMANSHIP.getChar()+" " +
                            TUI_Special_Char.ALIANSHIP.getChar()+" " +
                            TUI_Special_Char.ALIANSHIP.getChar(),0,5);
        //SetCursorBlink(false);
        while (true){
            writeAtPosition("M",1,2);
            Time.sleep(1000);
            writeAtPosition(TUI_Special_Char.EXPLOSION1.getChar(),1,2);
            Time.sleep(150);
            writeAtPosition(TUI_Special_Char.EXPLOSION2.getChar(),1,2);
            Time.sleep(150);
            writeAtPosition(" ",1,2);
            Time.sleep(1000);
        }

    }

    private static final char ENTER_KEY = '5';
    private static final char LEFT_ARROW = '4';
    private static final char RIGHT_ARROW = '6';
    private static final char UP_ARROW = '2';
    private static final char DOWN_ARROW = '8';
    private static final char DELETE_KEY = '*';

    private static final String TXTCLEAR="                ";

    public static void init(){
        HAL.init();
        SerialEmitter.init();
        KBD.init();
        LCD.init();
        LCD.setcursorPosition(0,0);
        LCD.setCursorBlink(false);
        createSpecialChars();
    }

    public static int getLCDcols(){
        return LCD.COLS;
    }

    //Permite obter uma string de comprimento size, a ser escrita na linha e coluna especificada
    public static String getString(int size,int linStart, int colStart) {
        char[] sarry = new char[size];
        LCD.setCursorBlink(true);
        int pos = 0;
        LCD.setcursorPosition(linStart, colStart + pos);

        sarry[0] = 'A';
        writeSetFinalPos(sarry[0], linStart, colStart + pos);

        char keyPressed = 0;

        while (keyPressed != ENTER_KEY) {
            keyPressed = KBD.getKey();

            switch (keyPressed) {
                case 0:
                    break;
                case RIGHT_ARROW:
                    if (pos < size - 1) {
                        pos++;
                        LCD.setcursorPosition(linStart, colStart + pos);
                    }
                    if (sarry[pos] < 'A' || sarry[pos] > 'Z') {
                        sarry[pos] = 'A';
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    }
                    break;

                case LEFT_ARROW:
                    if (pos > 0) {
                        pos--;
                        LCD.setcursorPosition(linStart, colStart + pos);
                    }
                    break;

                case UP_ARROW:
                    if (sarry[pos] < 'A' || sarry[pos] >= 'Z') {
                        sarry[pos] = 'A';
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    } else if (sarry[pos] < 'Z') {
                        sarry[pos]++;
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    }
                    break;

                case DOWN_ARROW:
                    if (sarry[pos] < 'A' || sarry[pos] > 'Z') {
                        sarry[pos] = 'A';
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    } else if (sarry[pos] == 'A') {
                        sarry[pos] = 'Z';
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    } else if (sarry[pos] > 'A') {
                        sarry[pos]--;
                        writeSetFinalPos(sarry[pos], linStart, colStart + pos);
                    }
                    break;

                case DELETE_KEY:
                    if (pos == size - 1 || (pos > 0 && sarry[pos + 1] == 0)) {
                        sarry[pos] = 0;
                        writeSetFinalPos(sarry[pos], linStart, colStart + (--pos));
                    }
                    break;
            }
        }

        return String.valueOf(sarry).trim();
    }

    // Escreve um caráter na posição corrente e define posição final.
    private static void writeSetFinalPos(char c, int linFinal, int colFinal) {
        LCD.setCursorBlink(false);
        LCD.write(c);
        LCD.setcursorPosition(linFinal,colFinal);
        LCD.setCursorBlink(true);
    }


    public static void setCursorBlink(boolean blink){
        LCD.setCursorBlink(blink);
    }

    public static void clearScreen(){
        clearLine(0);
        clearLine(1);
    }

    public static void clearLine(int line){
        LCD.setcursorPosition(line,0);
        LCD.write(TXTCLEAR);
    }

    public static void writeAtPosition(char a,int line, int column){
        LCD.setcursorPosition(line,column);
        LCD.write(a);
    }

    public static void writeAtPosition(String a,int line, int column){
        LCD.setcursorPosition(line,column);
        LCD.write(a);
    }


    /********SPECIAL CHAR*******/

    private static void createSpecialChars(){
        LCD.sendSpecialChar(TUI_Special_Char.VAZIO.ordinal(),TUI_Special_Char.VAZIO.getData());
        LCD.sendSpecialChar(TUI_Special_Char.GUNSHIP.ordinal(),TUI_Special_Char.GUNSHIP.getData());
        LCD.sendSpecialChar(TUI_Special_Char.HUMANSHIP.ordinal(),TUI_Special_Char.HUMANSHIP.getData());
        LCD.sendSpecialChar(TUI_Special_Char.ALIANSHIP.ordinal(),TUI_Special_Char.ALIANSHIP.getData());
        LCD.sendSpecialChar(TUI_Special_Char.EXPLOSION1.ordinal(),TUI_Special_Char.EXPLOSION1.getData());
        LCD.sendSpecialChar(TUI_Special_Char.EXPLOSION2.ordinal(),TUI_Special_Char.EXPLOSION2.getData());
        LCD.sendSpecialChar(TUI_Special_Char.MISSILE.ordinal(),TUI_Special_Char.MISSILE.getData());
    }

    /********KBD*******/
    public static char getKey(){
        return KBD.getKey();
    }

    public static char waitKey(long timeout){
        return KBD.waitKey(timeout);
    }
}
