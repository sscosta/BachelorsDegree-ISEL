using System;
using System.Collections.Generic;
using System.Text;

namespace BomEBarato.Entidades
{
    public class Entrega
    {
        public int FranqId { get; private set; }
        public int ProdId { get; private set; }
        public int ValorPed { get; private set; }
        public int ValorForn { get; private set; }

        public Entrega(int franqId, int prodId, int valorPed, int valorForn)
        {
            this.FranqId = franqId;
            this.ProdId = prodId;
            this.ValorPed = valorPed;
            this.ValorForn = valorForn;
        }
        
    }
}
