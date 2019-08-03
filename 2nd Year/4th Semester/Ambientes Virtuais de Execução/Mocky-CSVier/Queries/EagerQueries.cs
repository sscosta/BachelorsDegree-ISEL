using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Queries
{
    class EagerQueries
    {

        //EAGER Implementations
        public static List<string> Remove(List<string> lines, int count)
        {
            List<string> ret = new List<string>();
            int i = 0;
            foreach (string s in lines)
            {
                if (i >= count)
                {
                    ret.Add(s);
                }
                i++;
            }
            return ret;
        }

        public static List<string> RemoveEmpties(List<string> lines)
        {
            List<string> ret = new List<string>();
            foreach (string s in lines)
            {
                if (!s.Equals(""))
                {
                    ret.Add(s);
                }
            }
            return ret;
        }

        public static List<string> RemoveWith(List<string> lines, string word)
        {
            List<string> ret = new List<string>();
            foreach (string line in lines)
            {

                if (line.Equals("") || !word.Equals(line.Substring(0, word.Length)))
                {
                    ret.Add(line);
                }
            }
            return ret;
        }
        public static List<string> RemoveEvenIndexes(List<string> lines)
        {
            List<string> ret = new List<string>();
            int i = 0;
            foreach (string line in lines)
            {
                if (i % 2 != 0)
                {
                    ret.Add(line);
                }
                i++;
            }
            return ret;
        }
        public static List<string> RemoveOddIndexes(List<string> lines)
        {
            List<string> ret = new List<string>();
            int i = 0;
            foreach (string line in lines)
            {
                if (i % 2 == 0)
                {
                    ret.Add(line);
                }
                i++;
            }
            return ret;
        }
    }
}
