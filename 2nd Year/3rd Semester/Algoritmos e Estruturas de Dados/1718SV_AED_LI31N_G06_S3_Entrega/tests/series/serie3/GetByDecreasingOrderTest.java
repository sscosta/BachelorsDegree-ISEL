package series.serie3;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static series.serie3.TreeUtils.getByDecreasingOrder;
import static series.serie3.TreeUtils.insert;

public class GetByDecreasingOrderTest {

    @Test
    public void getByDecreasingOrder_empty(){
        Iterable<Integer> expected = new ArrayList<>();
        assertIterableEquals(expected,getByDecreasingOrder(null));
    }

    @Test
    public void getByDecreasingOrder_singleton(){
        Node<Integer> actual = new Node<Integer>(1);
        Iterable<Integer> expected = new ArrayList<>(Arrays.asList(1));
        assertIterableEquals(expected,getByDecreasingOrder(actual));
    }

    @Test
    public void getByDecreasingOrder_example(){
        Node<Integer> actual = new Node<Integer>(20);
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(5));
        insert(actual,new Node<Integer>(30));
        insert(actual,new Node<Integer>(15));
        insert(actual,new Node<Integer>(25));
        insert(actual,new Node<Integer>(40));
        Iterable<Integer> expected = new ArrayList<>(Arrays.asList(40,30,25,20,15,10,5));
        assertIterableEquals(expected,getByDecreasingOrder(actual));
    }
    @Test
    public void getByDecreasingOrder_notBalanced(){
        Node<Integer> actual = new Node<Integer>(20);
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(5));
        insert(actual,new Node<Integer>(30));
        insert(actual,new Node<Integer>(40));
        insert(actual,new Node<Integer>(1));
        insert(actual,new Node<Integer>(null));
        Iterable<Integer> expected = new ArrayList<>(Arrays.asList(40,30,20,10,5,1));
        assertIterableEquals(expected,getByDecreasingOrder(actual));
    }
    @Test
    public void getByDecreasingOrder_same(){
        Node<Integer> actual = new Node<Integer>(20);
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(10));
        insert(actual,new Node<Integer>(10));
        Iterable<Integer> expected = new ArrayList<>(Arrays.asList(20,10,10,10,10,10,10));
        assertIterableEquals(expected,getByDecreasingOrder(actual));
    }
}
