using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Queries
{
    static class IEnumerableExtensions
    {
        public static T[] ToArray<T>(this IEnumerable<T> source)
        {
            int length = LazyQueries.Count<T>(source);
            T[] newArray = new T[length];
            int i = 0;
            foreach (T item in source)
            {
                newArray[i] = item;
            }
            return newArray;
        }
    }
}
