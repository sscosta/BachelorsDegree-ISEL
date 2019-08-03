package series.serie3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;

public class Iterables{

	public static Iterable<Integer> alternateEvenOdd(Iterable<Integer> src){
		return new Iterable<Integer>() {
			@Override
			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {
					Iterator<Integer> it = src.iterator();
					Integer curr;
					boolean nextEven = false;
					@Override
					public boolean hasNext() {
						while(it.hasNext()){
							curr=it.next();
							if((curr % 2 == 0) == nextEven){
								nextEven = !nextEven;
								return true;
							}
						}
						return false;
					}

					@Override
					public Integer next() {
						Integer res = curr;
						curr = null;
						return res;
					}
				};
			}
		};
	}

	// ultimo retornado
	// se prox for maior e ordem for natural retornar
	// se prox for menor e ordem for inversa retornar
	public static <E> Iterable<E> getTheIncreasingSequence(Iterable<E> sequence, Comparator<E> cmp){
		return new Iterable<E>() {
			@Override
			public Iterator<E> iterator() {
				return new Iterator<E>() {
					Iterator<E> it = sequence.iterator();
					E curr;
					E prev;
					@Override
					public boolean hasNext() {
						if(curr!=null)return true;
						while(it.hasNext()){
							curr = it.next();
							if(prev==null)return true;
							if(prev!=null && cmp.compare(prev,curr)<0){
								return true;
							}
						}
						return false;
					}

					@Override
					public E next() {
						prev = curr;
						curr = null;
						return prev;
					}
				}; }};}

	public static Iterable<String> getWordsThatContains(Iterable<Iterable<String>> src, String subStr){
		return new Iterable<String>() {
			@Override
			public Iterator<String> iterator() {
				return new Iterator<String>() {
					Iterator<Iterable<String>> it = src.iterator();
					Iterator<String> subIt = it.next().iterator();;
					String str;
					@Override
					public boolean hasNext() {
						if (str != null)return true;
						//while (it.hasNext()){
						while (it.hasNext()||subIt!=null){
							while (subIt.hasNext()){
								str = subIt.next();
								if (str.toLowerCase().contains(subStr)){
									return true;
								}
							}
							if(it.hasNext())
								subIt=it.next().iterator();
							else subIt=null;
						}
						return false;
					}

					@Override
					public String next() {
						String aux = str;
						str = null;
						return  aux;

					}
				};
			}
		};
	}

	//if contains(str)
	//passar para result
	//avançar no iterável quando terminar mudar de iteravel
	/*public static Iterable<String> getWordsThatContains(Iterable<Iterable<String>> src, String subStr){return new Iterable<E>() {
		return new Iterable<String>() {
		@Override
		public Iterator<String> iterator() {
			return new Iterator<String>() {
				Iterator<Iterable<String>> itOuter = src.iterator();
				Iterator <String> itInner = src.iterator();
				String curr;
				Iterable<String> currIt;
				@Override
				public boolean hasNext() {
					if(curr!=null)return true;
					while(itOuter.hasNext()){
						currIt= itOuter.next();
						while(itInner.hasNext()){
							curr = itInner.next();
							if(curr.contains(subStr))
								return true;
						}
					}
					return false;
				}

				@Override
				public String next() {
					String res = curr;
					curr = null;
					return res;
				}
			}; }};}*/

}