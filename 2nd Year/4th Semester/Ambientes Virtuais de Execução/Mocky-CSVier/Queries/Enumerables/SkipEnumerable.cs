using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class Skip<T> : IEnumerable<T>
    {
        private IEnumerable<T> lines;
        private int count;

        public Skip(IEnumerable<T> lines, int count)
        {
            this.lines = lines;
            this.count = count;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new SkipEnumerator<T>(lines.GetEnumerator(), count);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}