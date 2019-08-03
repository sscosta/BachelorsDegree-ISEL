using BomEBarato.DALAbstraction;
using BomEBarato.DALTipoProdutoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALTipoProduto
{
    public class TipoProdutoSession : AbstractSession, ISessionTipoProduto
    {
        public IMapperTipoProduto CreateMapperTipoProduto()
        {
            return new MapperTipoProduto(this);
        }
    }
}
