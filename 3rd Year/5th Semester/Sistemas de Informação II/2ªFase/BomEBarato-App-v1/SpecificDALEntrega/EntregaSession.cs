using BomEBarato.DALAbstraction;
using BomEBarato.DALEntregaInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALEntrega
{
    public class EntregaSession : AbstractSession, ISessionEntrega
    {
        public IMapperEntrega CreateMapperEntrega()
        {
            return new MapperEntrega(this);
        }
    }
}
