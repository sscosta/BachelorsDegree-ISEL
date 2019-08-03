using BomEBarato.DALAbstraction;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.SpecificDALFranqueado;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using System.Linq;
using ViewController;

namespace Commands
{
    public class InteractionWithCostumerInCashRegister : ICommand
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
                int prodId = int.Parse(Console.ReadLine());
                Console.WriteLine("Introduza a quantidade que pretende levar");
                int qty = int.Parse(Console.ReadLine());

                ProdutoViewInStore productInStore = productsCache.Find(p => p.ProdId == prodId);
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

            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                map.UpdateInBulk(franqId, sale);
                das.Commit();
            }
        }

        private void ShowExistingProducts()
        {
            productsCache.ForEach(
                pvpf => Console.WriteLine("Há {0} unidades do produto {1} - {2} com o preco {3} ",
                                pvpf.Quantidade, pvpf.ProdId, pvpf.Descricao, pvpf.PrecoUnitario));
        }

        private void RefreshProductsCache()
        {
            List<ProdVendidoPorFranqueado> prodsVendidosPorFranqueado;

            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();

                prodsVendidosPorFranqueado = map.GetAllInFranchisee(franqId);
            }
            using (var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                productsCache = map
                    .GetAll()
                    .Zip(prodsVendidosPorFranqueado, (p, pvpf) => ProdutoViewInStore.Parse(p, pvpf))
                    .ToList();
            }
        }


        private int WelcomeAndChooseStore()
        {
            Console.WriteLine("Welcome to Bom & Barato");
            Console.WriteLine("Which store are you going to shop at? Enter the Id of the desired store");


            using (var das = new DataAccessScope(false))
            {
                IMapperFranqueado map = new MapperFranqueado();
                List<Franqueado> franchisees = map.GetAll();

                franchisees.ForEach(x => Console.WriteLine("{0}   {1}     {2}", x.id, x.nome, x.morada));
            }
            return int.Parse(Console.ReadLine());
        }
    }
}