using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALEntregaInterfaces
{
    public interface IMapperEntrega : IMapper<Entrega,int,int>
    {
        void OrderOutOfStock(IEnumerable<Entrega> productsOutOfStock);
        void DeleteAllWithProdId(int productId);
        void DeleteAllWithFranqId(int franqId);
    }
}
