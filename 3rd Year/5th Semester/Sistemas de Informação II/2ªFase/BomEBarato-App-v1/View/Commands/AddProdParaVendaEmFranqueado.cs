using System;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using BomEBarato.SpecificDALTipoProduto;

using BomEBarato.DALTipoProdutoInterfaces;
using System.Collections.Generic;

namespace ViewController
{
    internal class AddProdParaVendaEmFranqueado : ICommand
    {
        public void Execute()
        {
            Produto p = PromptUserToBuildProduct();

            using(ProdutoSession s = new ProdutoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Create(p);
                    das.Commit();
                }
            }
            ProdVendidoPorFranqueado pvpf = PromptUserToBuildProdVendidoPorFranqueado();
            using(ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.Create(pvpf);
                    das.Commit();
                }
            }

        }

        private ProdVendidoPorFranqueado PromptUserToBuildProdVendidoPorFranqueado()
        {
            
            Console.WriteLine("Insert info of a product for sale in a franchisee");
            Console.Write("Franchisee id : ");
            int franqId = (int)GetInput(typeof(int));

            Console.Write("Product id : ");
            int prodId = (int)GetInput(typeof(int));

            Console.Write("unit price : ");
            decimal precoUnitario = (decimal)GetInput(typeof(decimal));

            Console.Write("Total stock : ");
            int stockTotal = (int)GetInput(typeof(int));

            Console.Write("Min stock : ");
            int stockMin = (int)GetInput(typeof(int));
            
            Console.Write("Max stock : ");
            int stockMax = (int)GetInput(typeof(int));

            Console.Write("Date of Last Sale yyyy-mm-dd : ");
            DateTime dt = (DateTime)GetInput(typeof(DateTime));

            Console.Write("Amount of sales : ");
            int qtdVendas = (int)GetInput(typeof(int));

            return new ProdVendidoPorFranqueado(franqId, prodId, precoUnitario, stockTotal, stockMin, stockMax, dt, qtdVendas);
        }

        private Produto PromptUserToBuildProduct()
        {
            Console.WriteLine("Insert info of a product");
            Console.Write("cod - 10 digits: ");
            string cod = (String)GetInput(typeof(String));

            Console.Write("Product description: ");
            string desc = (String)GetInput(typeof(String));

            Console.Write("Maximum stock (number): ");
            int smax = (int)GetInput(typeof(int));

            Console.Write("Minimum stock (number): ");
            int smin = (int)GetInput(typeof(int));

            Console.Write("Total stock (number): ");
            int st = (int)GetInput(typeof(int));

            Console.Write("Type of Product ");
            ShowAllProductTypes();
            Console.Write(" : ");
            String t;

            bool isValidType = false;
            do
            {
                t = (String)GetInput(typeof(String));
                isValidType = ShowAllProductTypes_isValidType(t);
                if (!isValidType) Console.WriteLine("Invalid input, please try again");
            }
            while (!isValidType);


            Produto p = new Produto();
            p.Cod = cod;
            p.Descrição = desc;
            p.StockMax = smax;
            p.StockMin = smin;
            p.StockTotal = st;
            p.Tipo = t;
            return p;
        }



        private void ShowAllProductTypes()
        {
            using (TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    IEnumerable<TipoProduto> pts = map.ReadAll();
                    foreach (TipoProduto pt in pts)
                    {
                        Console.Write(" {0},", pt.Descrição);
                    }
                }
            }
        }


        private bool ShowAllProductTypes_isValidType(String input)
        {

            using (TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    IEnumerable<TipoProduto> pts = map.ReadAll();

                    foreach (TipoProduto pt in pts)
                    {
                        if (Equals(pt.Descrição, input)) return true;

                    }
                    return false;
                }
            }
        }



        private static Object GetInput(Type type)
        {
            // Expecting String type
            if (type == typeof(String))
            {
                string Result = "";
                do
                {
                    Result = Console.ReadLine().Trim();
                    if (string.IsNullOrEmpty(Result))
                    {
                        Console.WriteLine("Empty input, please try again!");
                    }
                } while (string.IsNullOrEmpty(Result));
                return Result;

            }
            else if(type == typeof(decimal))
            {
                decimal v = 0;
                    while (!decimal.TryParse(Console.ReadLine(), out v) || v < 0)
                        Console.WriteLine("Invalid number, please try again!");
                
                return v;
            }
            else if(type == typeof(DateTime))
            {
                DateTime dt;
                while (!DateTime.TryParse(Console.ReadLine(), out dt) || string.IsNullOrEmpty((dt.ToString())))
                    Console.WriteLine("Invalid Date, please try again!");
                return dt;
            }
            else {
                // Expecting Int type
                int value = 0;
                if (type == typeof(int))
                {
                    while (!int.TryParse(Console.ReadLine(), out value) || value < 0)
                        Console.WriteLine("Invalid number, please try again!");
                }
                return value;
            }


        }

    }



}