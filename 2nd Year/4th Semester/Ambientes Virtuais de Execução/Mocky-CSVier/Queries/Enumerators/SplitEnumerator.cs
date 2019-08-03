using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
namespace Queries
{
    internal class SplitEnumerator : IEnumerator<string>
    {
        private CharEnumerator charEnumerator;
        private readonly string[] seps;
        private string curr;

        public SplitEnumerator(CharEnumerator charEnumerator, string[] seps)
        {
            this.charEnumerator = charEnumerator;
            this.seps = seps;
        }

        public string Current => this.curr;

        object IEnumerator.Current => this.Current;

        public bool MoveNext()
        {
            StringBuilder builder = new StringBuilder();
            bool advanced;
            while (advanced = charEnumerator.MoveNext())
            {
                if (Array.IndexOf(seps, charEnumerator.Current.ToString()) > -1)
                {
                    if (builder.ToString().Equals("") && Current!=null)//for carriage return on end of line /r/n
                        continue;
                    else
                        break;
                }
                    
                else
                    builder.Append(charEnumerator.Current);
            }
            curr = builder.ToString();
            return curr == "" ? advanced : true;
        }

        public void Dispose()
        {
            charEnumerator.Dispose();
        }

        public void Reset()
        {
            charEnumerator.Reset();
        }
    }
}