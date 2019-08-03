using System;
using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class FilterEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> lines;
        private Func<T, bool> p;

        public FilterEnumerable(IEnumerable<T> lines, Func<T, bool> p)
        {
            this.lines = lines;
            this.p = p;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new FilterEnumerator<T>(lines.GetEnumerator(), p);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}