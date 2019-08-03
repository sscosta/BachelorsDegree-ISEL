package series.serie3;

import org.junit.jupiter.api.Test;

import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SumIfTest {
    Predicate<Integer> predicateOdd = new Predicate<Integer>() {
        @Override
        public boolean test(Integer num) {
            return num % 2 != 0;
        }
    };

    Predicate<Integer> predicateMultipleOfFive = new Predicate<Integer>() {
        @Override
        public boolean test(Integer num) {
            return num % 5 == 0;
        }
    };
    @Test
    public void sumIfTest_Odds(){
        Node<Integer> root = new Node<>(10);
        TreeUtils.insert(root,new Node<>(11));
        TreeUtils.insert(root,new Node<>(14));
        TreeUtils.insert(root,new Node<>(12));
        TreeUtils.insert(root,new Node<>(19));
        Integer res = 30;
        assertEquals (res,TreeUtils.sumIf(root,predicateOdd));
    }
    @Test
    public void sumIfTest_MultipleOfFive() {
        Node<Integer> root = new Node<>(10);
        TreeUtils.insert(root,new Node<>(11));
        TreeUtils.insert(root,new Node<>(13));
        TreeUtils.insert(root,new Node<>(15));
        TreeUtils.insert(root,new Node<>(25));
        Integer res = 50;
        assertEquals (res,TreeUtils.sumIf(root,predicateMultipleOfFive));
    }
    @Test
    public void sumIfTest_Empty(){
        Node<Integer> root = new Node<>();
        Integer res = 0;
        assertEquals (res,TreeUtils.sumIf(root,predicateMultipleOfFive));
    }
    @Test
    public void sumIfTest_NotBalancedOdd(){
        Node<Integer> root = new Node<>(30);
        TreeUtils.insert(root,new Node<>(20));
        TreeUtils.insert(root,new Node<>(40));
        TreeUtils.insert(root,new Node<>(10));
        TreeUtils.insert(root,new Node<>(22));
        TreeUtils.insert(root,new Node<>(21));
        TreeUtils.insert(root,new Node<>(23));
        TreeUtils.insert(root,new Node<>(35));
        TreeUtils.insert(root,new Node<>(32));
        Integer res = 79;
        assertEquals (res,TreeUtils.sumIf(root,predicateOdd));
    }
    @Test
    public void sumIfTest_DegeneratedInListMultipleOfFive() {
        Node<Integer> root = new Node<>(70);
        TreeUtils.insert(root, new Node<>(60));
        TreeUtils.insert(root, new Node<>(51));
        TreeUtils.insert(root, new Node<>(40));
        TreeUtils.insert(root, new Node<>(31));
        TreeUtils.insert(root, new Node<>(20));
        TreeUtils.insert(root, new Node<>(10));
        Integer res = 200;
        assertEquals (res,TreeUtils.sumIf(root,predicateMultipleOfFive));
    }
}
