using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class RemoveOddIndexesEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> src;

        public RemoveOddIndexesEnumerable(IEnumerable<T> src)
        {
            this.src = src;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new RemoveOddIndexesEnumerator<T>(src.GetEnumerator());
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}