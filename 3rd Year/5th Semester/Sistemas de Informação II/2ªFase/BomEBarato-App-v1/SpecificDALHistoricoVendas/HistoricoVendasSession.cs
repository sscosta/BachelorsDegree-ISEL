using BomEBarato.DALAbstraction;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.SpecificDALHistoricoVendas;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBaratoSpecificDALHistoricoVendas
{
    public class HistoricoVendasSession : AbstractSession, ISessionHistoricoVendas
    {
        public IMapperHistoricoVendas CreateMapperHistoricoVendas()
        {
            return new MapperHistoricoVendas(this);
        }
    }
}
