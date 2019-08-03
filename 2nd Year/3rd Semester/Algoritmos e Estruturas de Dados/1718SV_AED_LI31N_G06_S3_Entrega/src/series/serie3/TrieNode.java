package series.serie3;

public class TrieNode {
    boolean isFragment;
    int fragmentCount;
    /*
    * 0 - a
    * 1 - c
    * 2 - t
    * 3 - g
    * */
    public TrieNode children[];
    public TrieNode(){
        isFragment=false;
        children = new TrieNode[4];
        fragmentCount=0;

    }
}
