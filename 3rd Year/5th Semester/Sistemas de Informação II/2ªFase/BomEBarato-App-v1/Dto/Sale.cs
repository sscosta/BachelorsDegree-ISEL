using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.Dto
{
    public class Sale
    {
        public int ProdId { get; set; }
        public int Qty { get; set; }
        public Sale(int prodId, int quantity)
        {
            this.ProdId = prodId;
            this.Qty = quantity;
        }
    }
}
