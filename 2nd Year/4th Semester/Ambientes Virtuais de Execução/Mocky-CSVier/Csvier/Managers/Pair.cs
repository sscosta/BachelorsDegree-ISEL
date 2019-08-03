using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Csvier.Managers
{
    public class Pair<T1, T2>
    {
        public int ColBeg { get; set; }
        public int ColEnd { get; set; }

        public Pair(int colBeg, int colEnd)
        {
            this.ColBeg = colBeg;
            this.ColEnd = colEnd;
        }
    }
}
