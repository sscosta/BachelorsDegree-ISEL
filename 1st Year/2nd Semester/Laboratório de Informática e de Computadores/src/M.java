class M {
    public static void main(String[] args) {
        HAL.init();
        int m=0;
        while (true){
            if ( checkIsInMaintenance()) {
                m++;
                System.out.println("Maintenance " + m);
            }
        }
    }


    private final static int MAINTENANCE_MASK=0x40;

    public static boolean checkIsInMaintenance(){
        return HAL.isBit(MAINTENANCE_MASK);
    }

}
