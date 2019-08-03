using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFranqueado;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using System.Linq;
namespace ViewController
{
    internal class InteractionWithCostumerInCashRegister : ICommand
    {
        List<ProdutoViewInStore> productsCache = new List<ProdutoViewInStore>();
        int franqId;

        public void Execute()
        {
            franqId = WelcomeAndChooseStore();

            if (productsCache.Count == 0)
                RefreshProductsCache();

            bool se = false; //sale ended

            List<ProdutoViewInStore> sale = new List<ProdutoViewInStore>();

            while (!se)
            {
                ShowExistingProducts();

                //Scan product
                Console.WriteLine("Introduza o id do produto que pretende levar");
                int prodId;
                while (!int.TryParse(Console.ReadLine(), out prodId))
                    Console.WriteLine("Invalid number, please try again!");

                Console.WriteLine("Introduza a quantidade que pretende levar");
                int qty;
                while (!int.TryParse(Console.ReadLine(), out qty))
                    Console.WriteLine("Invalid number, please try again!");

                ProdutoViewInStore productInStore = productsCache.Find(p => p.ProdId == prodId);

                if (productInStore == null)
                {
                    Console.WriteLine("Product not found!");
                    se = true;
                }
                else {
                    //update cache
                    productInStore.Quantidade -= qty;

                    //add scanned product to sale list
                    ProdutoViewInStore boughtProduct = new ProdutoViewInStore(
                        productInStore.ProdId,
                        productInStore.Descricao,
                        qty,
                        productInStore.PrecoUnitario);

                    sale.Add(boughtProduct);

                    Console.WriteLine("Do you want to continue scanning items? (y/n)");

                    if (Console.ReadKey().KeyChar.Equals('n'))
                        se = true;
                }
                UpdateStock(sale);

            }
            //Calc value to be payed
            decimal total = 0;
            sale.ForEach(item => total += item.PrecoUnitario * item.Quantidade);
            Console.WriteLine("Your total is {0} euros ", total);
            //Emission of receipt with shopping list
            Console.WriteLine("Item     Unit Price      Value to Pay");
            sale.ForEach(item => Console.WriteLine("{0}      {1}eur/unit     {2}€", item.Descricao, item.PrecoUnitario, item.PrecoUnitario * item.Quantidade));

            Console.WriteLine("See you next time! Press any key to continue");
            Console.ReadKey();
        }

        private void UpdateStock(List<ProdutoViewInStore> sale)
        {
            using(ProdVendidoPorFranqueadoSession s =  new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.UpdateInBulk(franqId,sale);
                    das.Commit();
                }
            }
        }

        private void ShowExistingProducts()
        {
            productsCache.ForEach(
                pvpf => Console.WriteLine("Há {0} unidades do produto {1} - {2} com o preco {3} ", 
                                pvpf.Quantidade, pvpf.ProdId,pvpf.Descricao,pvpf.PrecoUnitario));
        }

        private void RefreshProductsCache()
        {
            List<ProdVendidoPorFranqueado> prodsVendidosPorFranqueado;
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();

                    prodsVendidosPorFranqueado = map.GetAllInFranchisee(franqId).ToList();
                }
            }
            using(ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    productsCache = map
                        .GetAll()
                        .Zip(prodsVendidosPorFranqueado, (p, pvpf) => ProdutoViewInStore.Parse(p, pvpf))
                        .ToList();
                }
            }
        }

        private int WelcomeAndChooseStore()
        {
            Console.WriteLine("Welcome to Bom & Barato");
            Console.WriteLine("Which store are you going to shop at? Enter the Id of the desired store");


            using (FranqueadoSession s = new FranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    IEnumerable<Franqueado> franchisees = map.GetAll();


                    if(franchisees.ToList().Count()>0)
                    {
                        franchisees.ToList().ForEach(x => Console.WriteLine("{0}   {1}     {2}", x.Id, x.Nome, x.Morada));
                        int f_id;
                        while (!int.TryParse(Console.ReadLine(), out f_id) || f_id < 0 || franchisees.ToList().Find(id=>id.Id==f_id)==null)
                            Console.WriteLine("Invalid Franchisee ID!");
                        return f_id;
                    }

                }
            }
            return -1;
        }
    }
}