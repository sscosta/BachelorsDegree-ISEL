using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.DALFornecedorProdutoInterfaces
{
    public interface IMapperFornecedorProduto : IMapper<FornecedorProduto,int,int>
    {
        List<Fornecedor> GetAllFornecedoresForProduto(int prodId);
        void DeleteAllWithProdId(int prodId);
    }
}
