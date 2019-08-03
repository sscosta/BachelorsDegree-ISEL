package series.serie3;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

public class AlternateEvenOddTest {

    @Test
    public void alternateEvenOdd_OnAnEmptyIterable(){
        ArrayList<Integer> noElem = new ArrayList<>();
        ArrayList<Integer> empty = new ArrayList<>();
        assertIterableEquals(empty,Iterables.alternateEvenOdd(noElem));
    }
    @Test
    public void alternateEvenOddExample(){
        ArrayList<Integer> source  = new ArrayList<>(asList(2,8,3,5,4,6,7));
        ArrayList<Integer> evenOdd = new ArrayList<>(asList(3,4,7));
        assertIterableEquals(evenOdd,Iterables.alternateEvenOdd(source));
    }
    @Test
    public void alternateEvenOdd_OnOddList(){
        ArrayList<Integer> source  = new ArrayList<>(asList(3,5,7,9,11,13));
        ArrayList<Integer> evenOdd  = new ArrayList<>(asList(3));
        assertIterableEquals(evenOdd,Iterables.alternateEvenOdd(source));
    }
    @Test
    public void alternateEvenOdd_OnEvenList(){
        ArrayList<Integer> source  = new ArrayList<>(asList(2,4,6,8,10,12));
        ArrayList<Integer> evenOdd  = new ArrayList<>();
        assertIterableEquals(evenOdd,Iterables.alternateEvenOdd(source));
    }
    @Test
    public void alternateEvenOdd_OnIntercalatedListEvenBeg(){
        ArrayList<Integer> source  = new ArrayList<>(asList(2,3,4,5,7,9));
        ArrayList<Integer> evenOdd  = new ArrayList<>(asList(3,4,5));
        assertIterableEquals(evenOdd,Iterables.alternateEvenOdd(source));
    }
    @Test
    public void alternateEvenOdd_OnIntercalatedListOddBeg(){
        ArrayList<Integer> source  = new ArrayList<>(asList(3,5,6,7,9,11,13,15,17));
        ArrayList<Integer> evenOdd  = new ArrayList<>(asList(3,6,7));
        assertIterableEquals(evenOdd,Iterables.alternateEvenOdd(source));
    }
}
