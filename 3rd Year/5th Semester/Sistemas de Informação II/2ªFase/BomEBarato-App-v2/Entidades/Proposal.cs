using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.Entidades
{
    public class Proposta
    {
        public int ProdId { get; set; }
        public int FornId { get; set; }
        public int Qtd { get; set; }
        public decimal Preco { get; set; }
        public Proposta(int prodId,int fornId,int qtd, decimal price)
        {
            this.ProdId = prodId;
            this.FornId = fornId;
            this.Qtd = qtd;
            this.Preco = price;
        }
        public Proposta()
        {

        }
    }
}
