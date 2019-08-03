using BomEBarato.DALAbstraction;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALProdVendidoPorFranqueado
{
    public class ProdVendidoPorFranqueadoSession : AbstractSession, ISessionProdVendidoPorFranqueado
    {
        public IMapperProdVendidoPorFranqueado CreateMapperProdVendidoPorFranqueado()
        {
            return new MapperProdVendidoPorFranqueado(this);
        }
    }
}
