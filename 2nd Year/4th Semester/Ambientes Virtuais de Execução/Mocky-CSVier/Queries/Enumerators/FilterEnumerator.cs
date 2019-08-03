using System;
using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class FilterEnumerator<T> : IEnumerator<T>
    {
        private IEnumerator<T> srcIter;
        private readonly Func<T, bool> p;

        public FilterEnumerator(IEnumerator<T> srcIter, Func<T, bool> p)
        {
            this.srcIter = srcIter;
            this.p = p;

        }

        public T Current => srcIter.Current;

        object IEnumerator.Current => this.Current;

        public void Dispose()
        {
            srcIter.Dispose();
        }

        public bool MoveNext()
        {
            while (srcIter.MoveNext())
            {
                if (p(srcIter.Current))
                    return true;
            }
            return false;
        }

        public void Reset()
        {
            srcIter.Reset();
        }
    }
}