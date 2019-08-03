using System;
using System.Collections.Generic;
using System.Text;

namespace BomEBarato.Entidades
{
    public class FornecedorProduto
    {
        public int FornId { get; private set; }
        public int ProdId { get; private set; }
        public bool Dummy { get; private set; }

        public FornecedorProduto(int fornId, int prodId, bool dummy) {
            this.FornId = fornId;
            this.ProdId = prodId;
            this.Dummy = dummy;
        }
    }
}
