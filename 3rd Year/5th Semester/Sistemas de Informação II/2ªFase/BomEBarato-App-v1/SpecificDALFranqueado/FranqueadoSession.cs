using BomEBarato.DALAbstraction;
using BomEBarato.DALFranqueadoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFranqueado
{
    public class FranqueadoSession : AbstractSession, ISessionFranqueado
    {
        public IMapperFranqueado CreateMapperFranqueado()
        {
            return new MapperFranqueado(this);
        }
    }
}
