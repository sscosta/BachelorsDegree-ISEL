using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using ViewController;

namespace Commands
{
    public class AddProdParaVendaEmFranqueado : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("Add a Prod");

            Produto p = PromptUserToBuildProduct();

            using (var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                map.Create(p);
                das.Commit();
            }
            Console.WriteLine("Product created with id = {0}", p.id);

            ProdVendidoPorFranqueado pvpf = PromptUserToBuildProdVendidoPorFranqueado();
            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                map.Create(pvpf);
                das.Commit();
            }
        }


        private ProdVendidoPorFranqueado PromptUserToBuildProdVendidoPorFranqueado()
        {

            Console.WriteLine("Insert info of a product for sale in a franchisee");
            Console.Write("Franchisee id : ");
            int franqId = int.Parse(Console.ReadLine());

            Console.Write("Product id : ");
            int prodId = int.Parse(Console.ReadLine());

            Console.Write("unit price : ");
            decimal precoUnitario = decimal.Parse(Console.ReadLine());

            Console.Write("Total stock : ");
            int stockTotal = int.Parse(Console.ReadLine());

            Console.Write("Min stock : ");
            int stockMin = int.Parse(Console.ReadLine());

            Console.Write("Max stock : ");
            int stockMax = int.Parse(Console.ReadLine());

            Console.Write("Date of Last Sale yyyy-mm-dd : ");
            DateTime dt = DateTime.Parse(Console.ReadLine());

            Console.Write("Amount of sales : ");
            int qtdVendas = int.Parse(Console.ReadLine());

            ProdVendidoPorFranqueado pvpf = new ProdVendidoPorFranqueado();

            pvpf.franq_id = franqId;
            pvpf.prod_id = prodId;
            pvpf.preco_unitario = precoUnitario;
            pvpf.stock_total = stockTotal;
            pvpf.stock_min = stockMin;
            pvpf.stock_max = stockMax;
            pvpf.dt_ultima_venda = dt;
            pvpf.qtd_vendas = qtdVendas;

            return pvpf;
        }

        private Produto PromptUserToBuildProduct()
        {
            Console.WriteLine("Insert info of a product");
            Console.Write("cod - 10 digits: ");
            string cod = Console.ReadLine();

            Console.Write("Product description: ");
            string desc = Console.ReadLine();

            Console.Write("Maximum stock (number): ");
            int smax = int.Parse(Console.ReadLine());

            Console.Write("Minimum stock (number): ");
            int smin = int.Parse(Console.ReadLine());

            Console.Write("Total stock (number): ");
            int st = int.Parse(Console.ReadLine());

            Console.Write("Type - Alimentar, Casa, Higiene Pessoal : ");
            string t = Console.ReadLine();

            Produto p = new Produto();
            p.cod = cod;
            p.descricao = desc;
            p.stock_max = smax;
            p.stock_min = smin;
            p.stock_total = st;
            p.tipo = t;
            return p;
        }
    }
}