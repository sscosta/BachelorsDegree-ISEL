using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using BomEBaratoDto;
using System;
using System.Collections.Generic;
using System.Linq;

namespace ViewController
{
    internal class AskForSupplyOfProductsOutOfStock : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("What percentage relative to maxStock should be considered as running out of stock? (0-100)");
            double percentagemRutura = Double.Parse(Console.ReadLine());
            Console.WriteLine("Which franchisee is making the order?");
            int fid;
            while (!int.TryParse(Console.ReadLine(), out fid))
                Console.WriteLine("Invalid number, please try again!");

            List<Entrega> produtosEmRutura;

            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();

                    IEnumerable<ProdVendidoPorFranqueado> lpvpf = map.GetOutOfStock(percentagemRutura,fid);
                    produtosEmRutura = lpvpf.Select(pvpf => ToEntrega(pvpf)).ToList();
                    das.Commit();
                }
            }
           
           using (EntregaSession s = new EntregaSession())
           {
              using(var das = s.CreateDataAccessScope(true))
               {
                   IMapperEntrega map = s.CreateMapperEntrega();
                    map.OrderOutOfStock(produtosEmRutura);
                    das.Commit();
               }
           }
        }

        private Entrega ToEntrega(ProdVendidoPorFranqueado p)
        {
            return new Entrega(p.FranqId, p.ProdId, p.StockMax - p.StockTotal,0);
        }
    }
}