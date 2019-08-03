using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Queries.Enumerables
{
    class ConvertEnumerable<T, R> : IEnumerable<R>
    {
        IEnumerable<T> src;
        Function<T, R> mapper;
        public ConvertEnumerable(IEnumerable<T> src, Function<T, R> mapper)
        {
            this.src = src;
            this.mapper = mapper;
        }
        public IEnumerator<R> GetEnumerator()
        {
            return new ConvertEnumerator<T, R>(src, mapper);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
