using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALFranqueadoInterfaces
{
    public interface IMapperFranqueado : IMapper<Franqueado, int>
    {
        void Delete(int franqId);

        IEnumerable<Franqueado> GetAll();
    }
}
