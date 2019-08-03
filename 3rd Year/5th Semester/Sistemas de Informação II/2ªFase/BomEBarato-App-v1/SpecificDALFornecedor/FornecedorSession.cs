using BomEBarato.DALAbstraction;
using BomEBarato.DALFornecedorInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFornecedor
{
    public class FornecedorSession : AbstractSession, ISessionFornecedor
    {
        public IMapperFornecedor CreateMapperFornecedor()
        {
            return new MapperFornecedor(this);
        }
    }
}
