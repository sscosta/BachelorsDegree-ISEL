package series.serie3;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

public class GetWordsThatContainsTest {
    @Test
    public void getWordsThatContains_SimpleLetters(){
        ArrayList<String> first = new ArrayList<>(asList("a","b","c","d"));
        ArrayList<String> second = new ArrayList<>(asList("e","f","g","h"));
        ArrayList<String> third = new ArrayList<>(asList("i","j","k","l"));
        Iterable<Iterable<String>> list = new ArrayList<>(asList(first, second,third));
        Iterable<String> res = new ArrayList<>(asList("d"));
        assertIterableEquals(res, Iterables.getWordsThatContains(list,"d"));
    }
    @Test
    public void getWordsThatContains_Example(){
        ArrayList<String> first = new ArrayList<>(asList("O","rato","roeu","a","rolha","da","garrafa","do","rei","da","Rússia"));
        ArrayList<String> second = new ArrayList<>(asList("O","original","se","desoriginou","nem?","nunca","se","desoriginará"));
        ArrayList<String> third = new ArrayList<>(asList("Três","pratos","de","trigo","para","três","tristes","tigres"));
        Iterable<Iterable<String>> list = new ArrayList<>(asList(first, second,third));
        Iterable<String> res = new ArrayList<>(asList("rato","roeu","rolha","garrafa","rei","Rússia","original","desoriginou","desoriginará","Três","pratos","trigo","para","três","tristes","tigres"));
        assertIterableEquals(res, Iterables.getWordsThatContains(list,"r"));
    }
    @Test
    public void getWordsThatContains_NotFound(){
        ArrayList<String> first = new ArrayList<>(asList("O","rato","roeu","a","rolha","da","garrafa","do","rei","da","Rússia"));
        ArrayList<String> second = new ArrayList<>(asList("O","original","se","desoriginou","nem?","nunca","se","desoriginará"));
        ArrayList<String> third = new ArrayList<>(asList("Três","pratos","de","trigo","para","três","tristes","tigres"));
        Iterable<Iterable<String>> list = new ArrayList<>(asList(first, second,third));
        Iterable<String> res = new ArrayList<>();
        assertIterableEquals(res, Iterables.getWordsThatContains(list,"y"));
    }

    @Test
    public void getWordsThatContains_Null(){
        ArrayList<String> first = new ArrayList<>();
        ArrayList<String> second = new ArrayList<>();
        ArrayList<String> third= new ArrayList<>();
        Iterable<Iterable<String>> list = new ArrayList<>(asList(first, second,third));
        Iterable<String> res = new ArrayList<>();
        assertIterableEquals(res, Iterables.getWordsThatContains(list,"árvore"));
    }
    @Test
    public void getWordsThatContains_singleton(){
        ArrayList<String> first = new ArrayList<>(asList("a"));
        ArrayList<String> second = new ArrayList<>(asList("b"));
        ArrayList<String> third= new ArrayList<>(asList("c"));
        Iterable<Iterable<String>> list = new ArrayList<>(asList(first, second,third));
        assertIterableEquals(first, Iterables.getWordsThatContains(list,"a"));
    }
}
