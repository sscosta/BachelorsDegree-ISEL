using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALAbstraction
{
    public interface IMapper<T, Tid1,Tid2> where T : class
    {
        void Create(T entity);
        T Read(Tid1 id,Tid2 id2);
        void Update(T entity);
        void Delete(T entity);

    }
}
