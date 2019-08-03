class TUI_M {

    private static final String MAINTENANCE_TITLE =" On Maintenance ";
    private static final String MAINTENANCE_MAIN_OPTIONS = "#-Count *-ShutD ";
    private static final String SHUTDOWN_TITLE = "    SHUTDOWN    ";
    private static final String OPTIONS = "5-Yes  other-No ";
    private static final String CLEAR_COUNTERS = " Clear Counters ";
    private static final String WANNA_PLAY = "  Want to test   ";
    private static final String GOODBYE = " Thank you for  ";
    private static final String SEE_YOU_NEXT_TIME = "    Playing     ";
    private static final String TXTGAMES ="Games:";
    private static final String TXTCOINS ="Coins:";

    private static void printMaintenanceText(String str0, String str1){
        TUI.writeAtPosition(str0,0,0);
        TUI.writeAtPosition(str1,1,0);
    }

    public static void printMaintenanceMenu(){
        printMaintenanceText(MAINTENANCE_TITLE,MAINTENANCE_MAIN_OPTIONS);
    }

    public static void printShutDownMenu(){
        printMaintenanceText(SHUTDOWN_TITLE,OPTIONS);
    }

    public static void printStatistics(int gm,int cn){
        printMaintenanceText(TXTGAMES+gm+ "         ",TXTCOINS+cn+"         ");
    }

    public static void printClearCountersMenu(){
        printMaintenanceText(CLEAR_COUNTERS,OPTIONS);
    }

    public static void printWannaPlayMeny() {
        printMaintenanceText(WANNA_PLAY, OPTIONS);
    }

    public static void printGoodByeMessage() {
        printMaintenanceText(GOODBYE,SEE_YOU_NEXT_TIME);
    }

}
