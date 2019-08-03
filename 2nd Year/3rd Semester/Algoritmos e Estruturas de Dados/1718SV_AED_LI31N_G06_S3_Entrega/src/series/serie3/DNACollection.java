package series.serie3;

import java.util.NoSuchElementException;

public class DNACollection {

	private static TrieNode root = new TrieNode();

	public static void main(String[] args) {
		add("act");
		add("actg");
		add("accc");
		add("a");
		System.out.println("Expected: 0 - " + prefixCount("acccc"));
		System.out.println("Expected: 3 - " +prefixCount("ac"));
		System.out.println("Expected: 1 - " +prefixCount("actg"));
		System.out.println("Expected: 0 - " +prefixCount("actga"));
		System.out.println("Expected 4 - " + prefixCount("a"));
		System.out.println("Expected 2 - " + prefixCount("act"));
		//check if duplicate doesnt increase fragmentcount
		add("a");
		System.out.println("Expected 4 - " + prefixCount("a"));
		add("tggg");
		add("tatg");
		add("tgac");
		add("ttag");
		add("tgga");
		System.out.println("Expected: 5 - " + prefixCount("t"));
		System.out.println("Expected: 3 - " + prefixCount("tg"));
		System.out.println("Expected: 1 - " + prefixCount("ta"));
		System.out.println("Expected: 1 - " + prefixCount("tt"));
		System.out.println("Expected: 0 - " + prefixCount("tc"));
	}
	public static void add(String fragment) {
		TrieNode n = root;
		while(fragment.length()!=0){			//while fragment isnt processed
			int idx = index(fragment.charAt(0));	//get index of first character
			if( n.children[idx]==null)n.children[idx]= new TrieNode();	//create corresponding node
			fragment = fragment.substring(1,fragment.length());		//adjust string
			n.fragmentCount++;						//increase fragment count
			n=n.children[idx];						//access child node
		}
		if(!n.isFragment)n.fragmentCount++;		//to prevent overcounting of duplicate inserted fragments
		n.isFragment = true;					//is valid fragment
	}
	//access fragmentcount of final node in string p
	public static int prefixCount(String p){
		TrieNode n = root;
		while(p.length()!=0){
			int idx = index(p.charAt(0));
			if(n.children[idx]!=null){
				n=n.children[idx];
				p = p.substring(1,p.length());
			}
			else return 0; // fragment isn't in trie
		}
		return n.fragmentCount;
	}
	//returns index in TrieNode.children
	// 4 possible, by convention
	private static int index(char c){
		switch (c){
			case 'a': return 0;	//adenine
			case 'c': return 1;	//cytosine
			case 't': return 2;	//thymine
			case 'g': return 3;  //guanine
		}
		throw new NoSuchElementException();
	}
}