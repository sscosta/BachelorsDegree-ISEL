using System;
using System.Collections.Generic;
using System.Text;

namespace BomEBarato.Entidades
{
    public class HistoricoVendas
    {
        public int FranqId { get; private set; }
        public int ProdId { get; private set; }
        public long Hist3Anos { get; set; }

        public HistoricoVendas(int franqId, int prodId, long hist3Anos)
        {
            this.FranqId = franqId;
            this.ProdId = prodId;
            this.Hist3Anos = hist3Anos;
        }
    }
}
