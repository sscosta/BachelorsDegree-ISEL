using BomEBarato.DALAbstraction;
using BomEBarato.DALFornecedorProdutoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFornecedorProduto
{
    public class FornecedorProdutoSession : AbstractSession, ISessionFornecedorProduto
    {
        public IMapperFornecedorProduto CreateMapperFornecedorProduto()
        {
            return new MapperFornecedorProduto(this);
        }
    }
}
