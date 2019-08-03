using System;
using System.Collections.Generic;
using System.Text;

namespace BomEBarato.Entidades
{
    public class TipoProduto
    {
        public string Descrição { get; set; }

        public TipoProduto(string descrição)
        {
            this.Descrição = descrição;
        }
    }
}
