using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALHistoricoVendasInterfaces
{
    public interface IMapperHistoricoVendas : IMapper<HistoricoVendas,int,int>
    {
        void DeleteAllWithProdId(int productId);
        void DeleteAllWithFranqId(int franqId);
    }
}
