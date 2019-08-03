public  class SerialEmitter { // Envia tramas para os diferentes módulos Serial Receiver.
    public static void main (String [] args){
    }

    public  enum Destination {SLCD,SSC}
    private final static int SCLK_MASK = 0x2;
    private final static int SDX_MASK = 0x1;
    private final static int DESTINATION_MASKS[] = {0x8, 0x4};

    // Inicia a classe
    public static void init() {
        for ( int m : DESTINATION_MASKS){
            HAL.setBits(m);
        }
    }

    // Envia uma trama para o SerialReceiver identificado por addr, com a dimensão de size e os bits de ‘data’.
    public static void send(Destination addr, int size, int data) {
        int sdx;
        int parity = 0;
        HAL.clrBits(DESTINATION_MASKS[addr.ordinal()]);
        HAL.clrBits(SCLK_MASK|SDX_MASK);
        while (size > 0) {
            sdx = data % 2;
            data /= 2;
            parity ^= sdx;
            sendData(sdx);
            clock();
            size--;
        }
        sendData(parity);
        clock();
        HAL.setBits(DESTINATION_MASKS[addr.ordinal()]);
    }

    // Ativa e desativa o bit SerialClock, dando suporte a transição ascendente e descendente do relogio na interacao com os componentes Hardware_Interface_Layer.
    private static void clock (){
        HAL.setBits(SCLK_MASK);
        HAL.clrBits(SCLK_MASK);
    }

    // Da suporte ao envio de inforamacao em serie para os perifericos. Permite envio de um bit de cada vez.
    private static void sendData (int data){
        HAL.writeBits(SDX_MASK,data);
    }
}
