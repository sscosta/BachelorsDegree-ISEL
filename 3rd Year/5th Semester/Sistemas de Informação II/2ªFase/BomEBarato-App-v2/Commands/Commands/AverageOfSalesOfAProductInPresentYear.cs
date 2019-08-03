using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using ViewController;

namespace Commands
{
    public class AverageOfSalesOfAProductInPresentYear : ICommand
    {
        public void Execute()
        {
            ShowAllProducts();
            ObtainAverage();
        }

        private void ShowAllProducts()
        {
            using(var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                List<ViewController.Produto> ps = map.GetAll();
                foreach(var p in ps)
                {
                    Console.WriteLine("Id = {0}, Cod = {1}, Descrição = {2}", p.id, p.cod, p.descricao);
                }
            }
        }

        private void ObtainAverage()
        {
            Console.WriteLine("Choose Prod Id to obtain Average");
            int prodId;

            while (!int.TryParse(Console.ReadLine(), out prodId))
                Console.WriteLine("Invalid number, please try again!");

            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                List<AvgSale> avgSales = map.AvgSalesInPresentYear(prodId);
                if (avgSales.Count == 0)
                    Console.WriteLine("Product with id = {0} did not sell any units this year", prodId);
                foreach (AvgSale avg in avgSales)
                {
                    Console.WriteLine("Product {0} sold on average {1} units during this year", avg.ProdId, avg.Avg);
                }
                Console.WriteLine("Press any key to continue");
                Console.ReadKey();
            }
        }
    }
}