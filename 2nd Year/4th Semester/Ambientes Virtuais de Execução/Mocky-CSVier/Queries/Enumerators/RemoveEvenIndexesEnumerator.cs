using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class RemoveEvenIndexesEnumerator<T> : IEnumerator<T>
    {
        private IEnumerator<T> srcIter;
        public T Current => srcIter.Current;
        object IEnumerator.Current => this.Current;
        int idx;
        public RemoveEvenIndexesEnumerator(IEnumerator<T> srcIter)
        {
            this.srcIter = srcIter;
            this.idx = 0;
        }

        public void Dispose()
        {
            srcIter.Dispose();
        }

        public bool MoveNext()
        {
            while (idx++ % 2 == 0)
            {
                srcIter.MoveNext();
            }
            return srcIter.MoveNext();
        }

        public void Reset()
        {
            srcIter.Reset();
        }
    }
}