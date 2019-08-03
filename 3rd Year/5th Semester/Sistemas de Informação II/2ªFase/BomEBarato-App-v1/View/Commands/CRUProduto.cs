using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALTipoProdutoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALTipoProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBaratoSpecificDALHistoricoVendas;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.DALEntregaInterfaces;

namespace ViewController
{
    internal class CRUProduct : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("Current products in the database:");
            ShowProductList();

            Console.WriteLine("Do you want to Insert(i), Remove(r) or update(u) the information of a Product?");
            char c = Console.ReadKey().KeyChar;
            if (c == 'i')
                InsertProduto();
            else if (c == 'r')
                RemoveProduto();
            else if (c == 'u')
                UpdateProduto();
        }



        // Display product info.
        private void ShowProductList()
        {
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    IEnumerable<Produto> pts = map.GetAll();
                    foreach (Produto pt in pts)
                    {
                        Console.WriteLine("\tID:{0} | Cod:{1} | Description:{2}", pt.Id, pt.Cod, pt.Descrição);
                    }
                }
            }
        }



        private void InsertProduto()
        {
            Produto p = PromptUserForProductInfo();
            using(ProdutoSession s = new ProdutoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Create(p);
                    das.Commit();
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
            else {
                // Expecting Int type
                int value=0;
                if (type == typeof(int))
                {
                    while (!int.TryParse(Console.ReadLine(), out value) || value<0)
                        Console.WriteLine("Invalid number, please try again!");
                }
                return value;
            }
            

        }

        private Produto PromptUserForProductInfo()
        {
            Produto p = new Produto();
            Console.WriteLine("Insert info of the Product");
            Console.Write("Cod (10 digits) : ");
            p.Cod = (String)GetInput(typeof(String));
            Console.Write("Type of Product ");
            ShowAllProductTypes();
            Console.Write(" : ");
            
            bool isValidType = false;
            do
            {
                p.Tipo = (String)GetInput(typeof(String));
                isValidType = ShowAllProductTypes_isValidType(p.Tipo);
                if(!isValidType) Console.WriteLine("Invalid input, please try again");
            }
            while (!isValidType);
          
        
            
            Console.Write("Description :");
            p.Descrição = (String)GetInput(typeof(String)); // not null column
            Console.Write("Max Stock :");
            p.StockMax = (int)GetInput(typeof(int));
            Console.Write("Min Stock :");
            p.StockMin = (int)GetInput(typeof(int));
            Console.Write("Total Stock :");
            p.StockTotal = (int)GetInput(typeof(int));

            return p;
        }

        private void ShowAllProductTypes()
        {
            using(TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    IEnumerable<TipoProduto> pts = map.ReadAll();
                    foreach(TipoProduto pt in pts)
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
        private void RemoveProduto()
        {
            Console.WriteLine("Insert id of Product to Remove");
            int key = 0;

            while (!int.TryParse(Console.ReadLine(), out key))
            {
                Console.WriteLine("Error: Product id must be an Integer!");
            }

            RemoveProdutoVendFranqueado(key);
            RemoveProdFromHistoricoVendas(key);
            RemoveProdFromEntrega(key);
            RemoveProduto(key);
        }

        private void RemoveProdutoVendFranqueado(int key)
        {
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProdVendidoPorFranqueado prodMap = s.CreateMapperProdVendidoPorFranqueado();
                    prodMap.DeleteAllWithProdId(key);
                }
            }
        }

        private void RemoveProdFromHistoricoVendas(int key)
        {
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperHistoricoVendas prodMap = s.CreateMapperHistoricoVendas();
                    prodMap.DeleteAllWithProdId(key);
                }
            }
        }

        private void RemoveProdFromEntrega(int key)
        {
            using (EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperEntrega prodMap = s.CreateMapperEntrega();
                    prodMap.DeleteAllWithProdId(key);
                }
            }
        }

        private void RemoveProduto(int key)
        {
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProduto prodMap = s.CreateMapperProduto();
                    Produto p = prodMap.Read(key);
                    prodMap.Delete(p);
                    das.Commit();
                }
            }
        }

        private void UpdateProduto()
        {
            Console.WriteLine("Insert id of Product to Update");
            int key = 0;

            while (!int.TryParse(Console.ReadLine(), out key))
            {
                Console.WriteLine("Error: Product id must be an Integer!");
            }
            
                
            Produto p = PromptUserForProductInfo();
            p.Id = key;
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Update(p);
                    das.Commit();
                }
            }
        }
    }
}