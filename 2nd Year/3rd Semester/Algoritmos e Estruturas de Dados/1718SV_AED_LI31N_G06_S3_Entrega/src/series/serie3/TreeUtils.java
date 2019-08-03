package series.serie3;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

public class TreeUtils{

  public static Integer sumIf(Node<Integer> root, Predicate<Integer> predicate){
  	if(root==null||root.item==null)return 0;
     return (predicate.test(root.item)==true?root.item:0) + sumIf(root.left,predicate) + sumIf(root.right,predicate);
  }

	public static Iterable<Integer> getByDecreasingOrder(Node<Integer> root){
		return new Iterable<Integer>() {
			@Override
			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {
					Node<Integer> curr = findMax(root);
					Node<Integer> aux;

					@Override
					public boolean hasNext() {
						if(curr!=null) return true;
						return ((curr=predecessor(aux))!=null);
					}

					@Override
					public Integer next() {
						if(curr==null)throw new NoSuchElementException();
						aux = curr;
						curr = null;
						return aux.item;
					}
				};
			}
		};
	}


  /*pseudo-código
	IsHeightBalanced(tree)
    return (tree is empty) or 
           (IsHeightBalanced(tree.left) and
            IsHeightBalanced(tree.right) and
            abs(Height(tree.left) - Height(tree.right)) <= 1)
	*/
  public static <E> boolean isBalanced(Node<E> root){
	return root==null ||
			(
			isBalanced(root.left) 
					&& isBalanced(root.right)
					&& Math.abs(height(root.left)-height(root.right))<=1
			);
	}


	/* SERVICE METHODS*/
	private static  <E> int height(Node <E> node){
    if (node == null)
        return 0;
	return 1 + Math.max(height(node.left), height(node.right));
	}
	protected static void insert(Node<Integer> root, Node<Integer> newNode){
		if(root==null)
			root = newNode;
		if(newNode==null||newNode.item==null)return;
		if(root.item >= newNode.item && root.left!=null) insert(root.left,newNode);
		if(root.item >= newNode.item && root.left==null){
			root.left = newNode;
			newNode.parent = root;
		}
		if(root.item < newNode.item && root.right!=null) insert(root.right,newNode);
		if(root.item < newNode.item && root.right==null){
			root.right = newNode;
			newNode.parent = root;
		}
	}

	public static Node<Integer> predecessor(Node<Integer> node) {
		if (node == null)
			return null;
		if (node.left != null)
			return findMax(node.left);
		Node<Integer> parent = node.parent;

		Node<Integer> y = parent;
		Node<Integer> x = node;
		while (y != null && x == y.left){
			x = y;
			y = y.parent;
		}
		return y;
	}

	private static Node<Integer> findMax(Node<Integer> root) {
		if(root==null)return null;
		Node<Integer> max = root;
		Node<Integer> aux;
		if(root.left != null) {
			if(max.item<(aux = findMax(root.left)).item)
				max = aux;
		}
		if(root.right != null) {
			if(max.item<(aux= findMax(root.right)).item)
				max = aux;
		}
		return max;
	}


}