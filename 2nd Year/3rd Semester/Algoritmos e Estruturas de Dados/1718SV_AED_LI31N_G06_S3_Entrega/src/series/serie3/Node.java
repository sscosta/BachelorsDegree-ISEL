package series.serie3;

public class Node<E> {
    E item;
    Node<E> left;
     Node<E> right;
    Node<E> parent;
    public Node(E value){
        item=value;
    }
    public Node(){}
}
