using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using System.Linq;
namespace ViewController
{
    internal class AverageOfSalesOfAProductInPresentYear : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("Choose Prod Id to obtain Average");
            int prodId;
            while (!int.TryParse(Console.ReadLine(), out prodId))
                Console.WriteLine("Invalid number, please try again!");


            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    IEnumerable<AvgSale> avgSales = map.AvgSalesInPresentYear(prodId);
                    if (avgSales.Count() == 0)
                        Console.WriteLine("Product with id = {0} did not sell any units this year", prodId);
                    foreach(AvgSale avg in avgSales)
                    {
                        Console.WriteLine("Product {0} sold on average {1} units during this year",avg.ProdId, avg.Avg);
                    }
                    Console.WriteLine("Press any key to continue");
                    Console.ReadKey();
                }
            }
        }
    }
}