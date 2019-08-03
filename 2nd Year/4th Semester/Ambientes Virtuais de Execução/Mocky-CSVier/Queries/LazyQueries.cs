using Queries.Enumerables;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Queries
{
    public delegate R Function<T, R>(T item);
    public class LazyQueries
    {
        //LAZY Implementations

        public static IEnumerable<T> RemoveEvenIndexes<T>(IEnumerable<T> src)
        {
            return new RemoveEvenIndexesEnumerable<T>(src);
        }
        public static IEnumerable<T> RemoveOddIndexes<T>(IEnumerable<T> src)
        {
            return new RemoveOddIndexesEnumerable<T>(src);
        }
        public static IEnumerable<T> Skip<T>(IEnumerable<T> lines, int count)
        {
            return new Skip<T>(lines, count);
        }

        public static IEnumerable<T> Filter<T>(IEnumerable<T> lines, Func<T, bool> p)
        {
            return new FilterEnumerable<T>(lines, p);
        }
        public static IEnumerable<string> Split(string src, params string[] seps)
        {
            return new SplitEnumerable(src, seps);
        }
        public static IEnumerable<string> Split(string src, char separator)
        {
            return new SplitEnumerable(src, separator);
        }
        public static int Count<T>(IEnumerable<T> lines)
        {
            IEnumerator<T> iter = lines.GetEnumerator();
            int cnt = 0;
            while (iter.MoveNext())
            {
                cnt++;
            }
            return cnt;
        }
        public static IEnumerable<R> Convert<T, R>(IEnumerable<T> src, Function<T, R> mapper)
        {
            return new ConvertEnumerable<T, R>(src, mapper);
        }
    }
}
