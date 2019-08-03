package pt.isel.ls.PresentationLayer.Console;

import java.util.List;

public class Print {
    public static void message(String txt) {
        System.out.println(txt);
    }
    //for debug purposes
    public static <E> void printResult(List<E> allEs) {
        allEs.stream().forEach((aGeneric) -> System.out.println(aGeneric));
    }

}


