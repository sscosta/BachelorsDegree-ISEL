/**
 *  virtualiza o acesso ao sistema usbport*/
import isel.leic.UsbPort;

class HAL {
    public static void main(String[] args) {
        init();
        readBits(8);

        //LCD.write("hello world");

        //HAL.init();
        //HAL.SerialEmitter.init();
        //HAL.SerialEmitter.send(HAL.SerialEmitter.Destination.SLCD, 9, 0x155);
        //HAL.SerialEmitter.send(HAL.SerialEmitter.Destination.SLCD, 9,0xFF);

        //KBD.init();
        /*while(true) {
            char key=0;
            key=KBD.getKey();
            if (key!=0)
            System.out.println(key);
        }*/

    }

    public static final boolean simul=false;

    private static int lastval;


    public static void init(){
        lastval=0x0;
        out(lastval);
    }

    // Retorna os valores dos bits representados por mask presentes no UsbPort
    public static int readBits(int mask) {
        return in() & mask;
    }

    // Retorna true se o bit tiver o valor lógico ‘1’
    public static boolean isBit(int mask) {
        return readBits(mask)!=0;
    }

    // Coloca os bits representados por mask no valor lógico ‘1’
    public static void setBits(int mask) {
        lastval|=  mask;
        out(lastval);
    }

    // Coloca os bits representados por mask no valor lógico ‘0’
    public static void clrBits(int mask){
        lastval = lastval & ~mask;
        out(lastval);
    }

    // Escreve nos bits representados por mask o valor de value
    public static void writeBits(int mask, int value) {
        lastval = (mask & value) | (~mask & lastval);
        out(lastval);
    }

    private static int in (){
        return simul?~UsbPort.in() :UsbPort.in();
    }

    private static void out(int val){
        UsbPort.out(simul?~val:val);
    }


}


