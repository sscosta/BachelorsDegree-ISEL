
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.Dto
{
    public class ProdutoViewInStore
    {
        public int ProdId { get; set; }
        public string Descricao { get; set; }
        public int Quantidade { get; set; }
        public decimal PrecoUnitario { get; set; }

        public ProdutoViewInStore(int prodId, string descricao, int stockTotal, decimal precoUnitario)
        {
            this.ProdId = prodId;
            this.Descricao = descricao;
            this.Quantidade = stockTotal;
            this.PrecoUnitario = precoUnitario;
        }


        public static ProdutoViewInStore Parse(ViewController.Produto p, ViewController.ProdVendidoPorFranqueado pvpf)
        {
            return new ProdutoViewInStore(pvpf.prod_id, p.descricao, pvpf.stock_total ?? default(int), pvpf.preco_unitario);
        }
    }
}
