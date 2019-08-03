using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALFornecedorProdutoInterfaces
{
    public interface IMapperFornecedorProduto : IMapper<FornecedorProduto,int,int>
    {
        void DeleteAllWithProdId(int productId);
        IEnumerable<Fornecedor> GetAllFornecedoresForProduto(int productId);
    }
}
