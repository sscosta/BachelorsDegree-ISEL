using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBaratoDto
{
    public class ProdutoEmRutura
    {
        int FranqId { get; set; }
        int ProdId { get; set; }
        int ValorAPedir { get; set; }
        public ProdutoEmRutura(int franqId, int prodId,int valorAPedir)
        {
            this.FranqId = franqId;
            this.ProdId = prodId;
            this.ValorAPedir = valorAPedir;
        }
    }
}
