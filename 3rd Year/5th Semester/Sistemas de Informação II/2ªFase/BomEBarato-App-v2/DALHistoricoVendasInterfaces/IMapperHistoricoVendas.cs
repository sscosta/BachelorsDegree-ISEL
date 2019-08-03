using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.DALHistoricoVendasInterfaces
{
    public interface IMapperHistoricoVendas : IMapper<HistoricoVendas, int, int>
    {
        void DeleteAllWithFranqId(int franqId);
        void DeleteAllWithProdId(int prodId);
    }
}
