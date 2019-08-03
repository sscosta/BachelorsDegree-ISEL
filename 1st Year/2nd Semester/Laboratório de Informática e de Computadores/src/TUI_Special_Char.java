public enum TUI_Special_Char {
    VAZIO(0), GUNSHIP(1), HUMANSHIP(2), ALIANSHIP(3), EXPLOSION1(4),EXPLOSION2(5),MISSILE(6);
    private final int value;

    TUI_Special_Char(int value) {
        this.value = value;
    }

    public char getChar(){
        return (char)this.value;
    }

    public int[] getData(){
        switch (this.value){
            case (0):
                return new int[]{
                        0b00000000, //7
                        0b00000000, //8
                        0b00000000, //30
                        0b00000000, //8
                        0b00000000, //30
                        0b00000000, //8
                        0b00000000, //7
                        0b00000000  //0
                };
            case (1):
                return new int[]{
                        0b00000111, //7
                        0b00001000, //8
                        0b00011110, //30
                        0b00001000, //8
                        0b00011110, //30
                        0b00001000, //8
                        0b00000111, //7
                        0b00000000  //0
                };
            case(2):
                return new int[]{
                        0b00011110, //30
                        0b00011000, //24
                        0b00011100, //28
                        0b00011111, //31
                        0b00011100, //28
                        0b00011000, //24
                        0b00011110, //30
                        0b00000000  //0
                };
            case(3):
                return new int[]{
                        0b00011111, //31
                        0b00011111, //31
                        0b00010101, //21
                        0b00011111, //31
                        0b00011111, //31
                        0b00010001, //17
                        0b00010001, //17
                        0b00000000  //0
                };
            case(4):
                return new int[]{
                        0b00000000, //0
                        0b00001010, //5
                        0b00001010, //5
                        0b00000100, //4
                        0b00001010, //5
                        0b00001010, //5
                        0b00000000, //0
                        0b00000000  //0
                };
            case(5):
                return new int[]{
                        0b00010101, //21
                        0b00010101, //21
                        0b00001010, //10
                        0b00000000, //0
                        0b00001010, //10
                        0b00010101, //21
                        0b00010101, //21
                        0b00000000  //0
                };
            case(6):
                return new int[]{
                        0b00000000, //21
                        0b00000000, //21
                        0b00011100, //10
                        0b00011111, //0
                        0b00011100, //10
                        0b00000000, //21
                        0b00000000, //21
                        0b00000000  //0
                };


        }
        return null;
    }

}
