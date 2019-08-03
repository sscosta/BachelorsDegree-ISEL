using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;

namespace ViewController
{
    internal class TotalSalesByFranchiseeInPresentYear : ICommand
    {
        public void Execute()
        {
            using(ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    IEnumerable<TopSales> totalByFranchisee = map.SalesByFranchiseeThisYear();

                    foreach(TopSales ts in totalByFranchisee){
                        Console.WriteLine("Franchisee nº {0} sold {1} EUR this year!", ts.FranqId, ts.VendasAnoCorrente);
                    }
                }
            }
        }
    }
}