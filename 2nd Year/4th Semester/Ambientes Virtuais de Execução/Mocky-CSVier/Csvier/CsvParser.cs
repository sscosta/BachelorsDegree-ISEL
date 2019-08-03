using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using ColumnSelector;
using Configurator;
using Csvier.Managers;
using Queries;
namespace Csvier
{
    public class CsvParser<T>
    {
        private readonly char separator;
        private readonly Type klass;

        private CtorManager<T> ctorManager;
        private MemberManager<T> memberManager;

        private IEnumerable<string> lines;
        private IEnumerable<string> currentLine;

        public CsvParser(char separator)
        {
            this.separator = separator;
            this.klass = typeof(T);
            ctorManager = new CtorManager<T>();
            memberManager = new MemberManager<T>();
        }
        public CsvParser() : this(',')
        {
        }

        public CsvParser<T> CtorArg(string arg, int col)
        {
            ctorManager.AddArg(arg, col);
            return this;
        }

        public CsvParser<T> CtorArg(string arg, int colBeg, int colEnd)
        {
            ctorManager.AddArrayArg(arg, colBeg, colEnd);
            return this;
        }
        
        public CsvParser<T> PropArg(string arg, int col)
        {
            memberManager.AddProperty(arg, col);
            return this;
        }

        public CsvParser<T> FieldArg(string arg, int col)
        {
            memberManager.AddField(arg, col);
            return this;
        }

        public CsvParser<T> Load(string src)
        {
            lines = LazyQueries.Split(src,"\r\n", "\r", "\n"); 
            return this;
        }

        public CsvParser<T> Remove(int count)
        {
            lines = lines.Skip<string>(count);
            return this;
        }
        public CsvParser<T> RemoveEmpties()
        {
            lines = lines.Where<string>(line => !line.Equals(""));
            return this;
        }

        public CsvParser<T> RemoveWith(string word)
        {
            lines = lines.Where<string>(line => !line.Contains(word));
            return this;
        }

        public CsvParser<T> RemoveEvenIndexes()
        {
            int idx = 0;
            lines = lines.Where<string>(line => idx++ % 2 != 0);
            return this;
        }
        public CsvParser<T> RemoveOddIndexes()
        {
            int idx = 0;
            lines = lines.Where<string>(line => idx++ % 2 == 0);
            return this;
        }

        public T[] Parse()
        {
            T[] arr = (T[])Array.CreateInstance(klass, lines.Count());
            int i = 0;
            foreach (string line in lines)
            {
                currentLine = LazyQueries.Split(line, separator);
                arr[i++] = ParseLine();
            }
            return arr;
        }

        public T[] Parse(Func<string, T> parser)
        {
            T[] arr = (T[])Array.CreateInstance(klass, lines.Count());
            int i = 0;
            foreach (string line in lines)
            {
                arr[i++] = parser(line);
            }
            return arr;
        }
        public IEnumerable<T> ToEnumerable(Func<string, T> parser)
        {
            foreach (string line in lines)
            {
                yield return parser(line);
            }
        }

        private T ParseLine()
        {
            T target = ctorManager.InvokeCtor(this);
            memberManager.SetValues(target, this);
            return target;
        }

        internal IEnumerable<string> CurrentLine()
        {
            return currentLine;
        }

        internal object GetParsedValue(Type t, String v)
        {
            if (t == typeof(string))
            {
                return v;
            }
            MethodInfo mi = t.GetMethod("Parse", new Type[] { typeof(string) });
            return mi.Invoke(null, new object[] { v });
        }

        internal object GetParsedArrayValue(Type t, IEnumerable<string> currentLine, Pair<int, int> pair)
        {
            int length = pair.ColEnd - pair.ColBeg + 1 ;
            Type elementType = t.GetElementType();
            object arr = Activator.CreateInstance(t, new object[] { length });
            Array value = (Array)arr;
            for (int i = 0; i < length; i++)
            {
                value.SetValue(GetParsedValue(elementType, currentLine.ElementAt(pair.ColBeg + i)), i);
            }
            return arr;
        }
    }
}
