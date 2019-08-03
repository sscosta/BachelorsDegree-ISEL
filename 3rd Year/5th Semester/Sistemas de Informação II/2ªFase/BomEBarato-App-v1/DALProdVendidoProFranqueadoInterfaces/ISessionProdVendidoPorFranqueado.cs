using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALProdVendidoProFranqueadoInterfaces
{
    public interface ISessionProdVendidoPorFranqueado : ISession
    {
        IMapperProdVendidoPorFranqueado CreateMapperProdVendidoPorFranqueado();
    }
}
