package series.serie3;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static series.serie3.TreeUtils.insert;

public class IsBalancedTest {


    @Test
    public void isBalancedTest_empty(){
        assertEquals(true,TreeUtils.isBalanced(null));
    }

    @Test
    public void isBalancedTest_single() {
        Node<Integer> root = new Node<>(20);
        assertEquals(true,TreeUtils.isBalanced(root));
    }
    @Test
    public void isBalancedTest_balanced(){
        int [] elements = {10,30,5,15,25,40};
        Node<Integer> elem;
        Node<Integer> root = new Node<>(20);
        for(int i : elements){
            elem = new Node<Integer>();
            elem.item = i;
            insert(root,elem);
        }
        assertEquals(true,TreeUtils.isBalanced(root));
    }

    @Test
    public void isBalancedTest_NotBalanced(){
        int [] elements = {40,30,50,10,15,5,3,1};
        Node<Integer> elem;
        Node<Integer> root = new Node<>(20);
        for(int i : elements){
            elem = new Node<Integer>();
            elem.item = i;
            insert(root,elem);
        }
        assertEquals(false,TreeUtils.isBalanced(root));
    }

}
