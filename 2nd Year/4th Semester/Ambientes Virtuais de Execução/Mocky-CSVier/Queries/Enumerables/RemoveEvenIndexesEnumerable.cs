using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    public class RemoveEvenIndexesEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> src;

        public RemoveEvenIndexesEnumerable(IEnumerable<T> src)
        {
            this.src = src;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new RemoveEvenIndexesEnumerator<T>(src.GetEnumerator());
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}