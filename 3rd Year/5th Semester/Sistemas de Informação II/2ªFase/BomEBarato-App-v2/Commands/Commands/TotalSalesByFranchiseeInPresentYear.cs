using BomEBarato.DALAbstraction;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using System.Linq;
using ViewController;

namespace Commands
{
    public class TotalSalesByFranchiseeInPresentYear : ICommand
    {
        public void Execute()
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                List<TopSales> q = map.SalesByFranchiseeThisYear();
                foreach (var a in q)
                {
                    Console.WriteLine("Franchisee nº {0} sold {1} EUR this year!", a.FranqId, a.VendasAnoCorrente);
                }
            }
        }
    }
}