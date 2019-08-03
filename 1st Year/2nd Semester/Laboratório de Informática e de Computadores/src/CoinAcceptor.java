class CoinAcceptor {

    public static void main(String[] args) {
        HAL.init();
        int c=0;
        while (true){
            if ( checkForInsertedCoin()) {
                c++;
                System.out.println("Coin accepted " + c);
            }
        }
    }


    private final static int COIN_MASK=0x80;
    private final static int COIN_ACCEPT_MASK=0x40;

    public static boolean checkForInsertedCoin(){
        if(HAL.isBit(COIN_MASK)) {
            HAL.setBits(COIN_ACCEPT_MASK);
            //HAL.clrBits(COIN_ACCEPT_MASK);
            while (HAL.isBit(COIN_MASK));

            HAL.clrBits(COIN_ACCEPT_MASK);
            return true;
        }
        return false;
    }
}
