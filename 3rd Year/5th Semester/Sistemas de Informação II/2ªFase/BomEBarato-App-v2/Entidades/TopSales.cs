using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.Dto
{
    public class TopSales
    {
        public int FranqId { get; set; }
        public decimal VendasAnoCorrente { get; set; }

        public TopSales(int franqId, decimal vendasAnoCorrente)
        {
            this.FranqId = franqId;
            this.VendasAnoCorrente = vendasAnoCorrente;
        }
        public TopSales()
        {

        }
    }
}
