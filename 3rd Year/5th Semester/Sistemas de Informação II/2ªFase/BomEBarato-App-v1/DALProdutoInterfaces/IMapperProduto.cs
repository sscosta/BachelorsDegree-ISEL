using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALProdutoInterfaces
{
    public interface IMapperProduto : IMapper<Produto,int>
    {
        void Delete(int productId);
        void FulfillOrders(List<Proposta> ordersToSuppliers);
        IEnumerable<Produto> GetAll();
    }
}
