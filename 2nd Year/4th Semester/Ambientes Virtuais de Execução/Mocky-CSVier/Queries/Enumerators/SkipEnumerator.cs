using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class SkipEnumerator<T> : IEnumerator<T>
    {
        private IEnumerator<T> srcIter;
        private int count;

        public SkipEnumerator(IEnumerator<T> enumerator, int count)
        {
            this.srcIter = enumerator;
            this.count = count;
        }

        public T Current => srcIter.Current;

        object IEnumerator.Current => this.Current;

        public void Dispose()
        {
            srcIter.Dispose();
        }

        public bool MoveNext()
        {
            while (count-- > 0) srcIter.MoveNext();
            return srcIter.MoveNext();
        }

        public void Reset()
        {
            srcIter.Reset();
        }
    }
}