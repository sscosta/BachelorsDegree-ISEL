using System.Collections;
using System.Collections.Generic;

namespace Queries.Enumerables
{
    internal class ConvertEnumerator<T, R> : IEnumerator<R>
    {
        IEnumerator<T> srcIter;
        Function<T, R> mapper;
        public ConvertEnumerator(IEnumerable<T> src, Function<T, R> mapper)
        {
            this.srcIter = src.GetEnumerator();
            this.mapper = mapper;
        }
        public bool MoveNext()
        {
            return srcIter.MoveNext();
        }
        object IEnumerator.Current
        {
            get { return this.Current; }
        }

        public R Current
        {
            get { return mapper(srcIter.Current); }
        }
        
        public void Reset()
        {
            srcIter.Reset();
        }
        public void Dispose()
        {
            srcIter.Dispose();
        }
    }
    }