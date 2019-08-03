using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALTipoProdutoInterfaces
{
    public interface IMapperTipoProduto : IMapper<TipoProduto,string>
    {
        IEnumerable<TipoProduto> ReadAll();
    }
}
