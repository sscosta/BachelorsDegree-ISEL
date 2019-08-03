using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALTipoProdutoInterfaces
{
    public interface ISessionTipoProduto : ISession
    {
        IMapperTipoProduto CreateMapperTipoProduto();
    }
}
