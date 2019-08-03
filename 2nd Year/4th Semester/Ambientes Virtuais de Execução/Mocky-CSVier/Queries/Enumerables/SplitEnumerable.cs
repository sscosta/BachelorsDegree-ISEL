using System.Collections;
using System.Collections.Generic;

namespace Queries
{
    internal class SplitEnumerable : IEnumerable<string>
    {
        private string src;
        private string[] seps;

        public SplitEnumerable(string src, string[] seps)
        {
            this.src = src;
            this.seps = seps;
        }

        public SplitEnumerable(string src, char separator)
        {
            this.src = src;
            this.seps = new string[1] { separator.ToString() };
        }

        public IEnumerator<string> GetEnumerator()
        {
            return new SplitEnumerator(src.GetEnumerator(), seps);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
           return this.GetEnumerator();
        }
    }
}