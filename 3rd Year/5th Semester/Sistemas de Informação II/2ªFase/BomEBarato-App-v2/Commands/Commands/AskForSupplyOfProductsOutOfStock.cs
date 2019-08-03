using BomEBarato.DALAbstraction;
using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using System.Linq;
using ViewController;

namespace Commands
{
    public class AskForSupplyOfProductsOutOfStock : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("What percentage relative to maxStock should be considered as running out of stock? (0-100)");
            double percentagemRutura = Double.Parse(Console.ReadLine());
            Console.WriteLine("Which franchisee is making the order?");
            int fid = int.Parse(Console.ReadLine());

            List<Entrega> produtosEmRutura;


            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                List<ProdVendidoPorFranqueado> lpvpf = map.GetOutOfStock(percentagemRutura, fid);
                produtosEmRutura = lpvpf.Select(pvpf => ToEntrega(pvpf)).ToList();
                das.Commit();
            }

            using (var das = new DataAccessScope(true))
            {
                IMapperEntrega map = new MapperEntrega();
                map.OrderOutOfStock(produtosEmRutura);
                das.Commit();
            }
        }
        private Entrega ToEntrega(ProdVendidoPorFranqueado p)
        {
            return new Entrega() {
                franq_id = p.franq_id,
                prod_id = p.prod_id,
                valor_ped = (int) p.stock_max - (int) p.stock_total,
                valor_forn = 0
            };
        }
    }
}