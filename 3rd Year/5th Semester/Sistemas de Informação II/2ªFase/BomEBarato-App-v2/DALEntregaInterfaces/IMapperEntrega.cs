using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.DALEntregaInterfaces
{
    public interface IMapperEntrega : IMapper<Entrega,int,int>
    {
        void DeleteAllWithFranqId(int franqId);
        void DeleteAllWithProdId(int prodId);
        void OrderOutOfStock(List<Entrega> prodsEmRutura);
    }
}
