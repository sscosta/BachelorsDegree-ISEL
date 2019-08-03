using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALProduto
{
    public class ProdutoSession : AbstractSession, ISessionProduto
    {
        public IMapperProduto CreateMapperProduto()
        {
            return new MapperProduto(this);
        }
    }
}
