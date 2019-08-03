package series.serie3;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

public class GetTheIncreasingSequenceTest {

    static final Comparator<Integer> CMP_REVERSE_ORDER = new Comparator<Integer>() {
        public int compare(Integer i1, Integer i2) {
            return i2.compareTo(i1);
        }
    };

    static final Comparator<Integer> CMP_NATURAL_ORDER= new Comparator<Integer>() {
        public int compare(Integer i1, Integer i2) {
            return i1.compareTo(i2);
        }
    };
    @Test
    public void getIncreasingSequenceExample_Natural(){
        ArrayList<Integer> randomSeq = new ArrayList<>(asList(2,1,1,2,5,6,6,0,0,4,4,2,2,6,6,6,6,8,8));
        ArrayList<Integer> incSec    =  new ArrayList<>(asList(2,5,6,8));
        assertIterableEquals(incSec,Iterables.getTheIncreasingSequence(randomSeq,CMP_NATURAL_ORDER));
    }
    @Test
    public void getIncreasingSequenceExample_Reverse(){
        ArrayList<Integer> randomSeq = new ArrayList<>(asList(8,8,6,6,6,6,2,2,4,4,0,0,6,6,5,2,1,1,2));
        ArrayList<Integer> incSec    =  new ArrayList<>(asList(8,6,2,0));
        assertIterableEquals(incSec,Iterables.getTheIncreasingSequence(randomSeq,CMP_REVERSE_ORDER));
    }
    @Test
    public void getIncreasingSequenceExample_Empty(){
        ArrayList<Integer> emptySequence = new ArrayList<>();
        ArrayList<Integer> res = new ArrayList<>();
        assertIterableEquals(res, Iterables.getTheIncreasingSequence(emptySequence,CMP_NATURAL_ORDER));
    }
    @Test
    public void getIncreasingSequenceExample_One_Element(){
        ArrayList<Integer> list = new ArrayList<>(Arrays.asList(7));
        ArrayList<Integer> res = new ArrayList<>(Arrays.asList(7));
        assertIterableEquals(res,Iterables.getTheIncreasingSequence(list,CMP_NATURAL_ORDER));
    }
    @Test
    public void getIncreasingSequenceExample_Equals(){
        ArrayList<Integer> list = new ArrayList<>(Arrays.asList(4,4,4,4,4,4,4,4,4,4,4,4,4,4));
        ArrayList<Integer> res = new ArrayList<>(Arrays.asList(4));
        assertIterableEquals(res, Iterables.getTheIncreasingSequence(list,CMP_NATURAL_ORDER));
    }

}
