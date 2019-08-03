using BomEBarato.DALAbstraction;
using BomEBarato.DALFornecedorProdutoInterfaces;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFornecedorProduto;
using BomEBarato.SpecificDALProduto;
using System;
using System.Collections.Generic;
using System.Linq;
using ViewController;
using Fornecedor = ViewController.Fornecedor;
using Produto = ViewController.Produto;

namespace Commands
{
    public class OrderToSuppliers : ICommand
    {
        public void Execute()
        {
            List<Proposta> props = new List<Proposta>();

            // First phase
            Console.WriteLine("What Product do you want to order");
            int prodId = int.Parse(Console.ReadLine());
            List<Fornecedor> fs = GetAllSuppliersForProduct(prodId);

            bool se = false; //survey ended
            while (!se)
            {
                Proposta p = new Proposta();
                p.ProdId = prodId;

                Console.WriteLine("Which Supplier do you want to query?");
                fs.ForEach(f => Console.WriteLine("{0} {1} {2}", f.id, f.nif, f.nome));
                p.FornId = int.Parse(Console.ReadLine());

                Console.WriteLine("How much can the supplier get?");
                p.Qtd = int.Parse(Console.ReadLine());

                Console.WriteLine("What is the unit price?");
                p.Preco = decimal.Parse(Console.ReadLine());

                //Second Phase
                props.Add(p);

                Console.WriteLine("Continue surveying?(y/n)");
                if (Console.ReadKey().KeyChar == 'n')
                    se = true;
            }

            // Third Phase


            //eliminate proposals that distance themselves more than 30% of the avg
            decimal avgPrice = props.Average(p => p.Preco);
            IEnumerable<Proposta> filtered = props.Where(p => (p.Preco >= 0.7m * avgPrice && p.Preco <= 1.3m * avgPrice));


            IEnumerable<Proposta> propsOrderedByPrice = filtered.OrderBy(t => t.Preco);
            ApplySecondRule(propsOrderedByPrice, GetQuantityToSupply(prodId));

            Console.WriteLine("Press Any key to continue");
            Console.ReadKey();
        }

        private void ApplySecondRule(IEnumerable<Proposta> ps, int qty)
        {
            Proposta[] props = ps.ToArray();
            List<Proposta> encs = new List<Proposta>();
            for (int i = 0; i < props.Length && qty > 0; i++)
            {
                Proposta curr = props[i];
                if (curr.Qtd > qty)
                {
                    curr.Qtd = qty;

                }
                qty -= curr.Qtd;
                encs.Add(curr);
            }
            Order(encs);
        }

        private void Order(List<Proposta> encs)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                map.FulfillOrders(encs);
                das.Commit();
            }
        }

        private int GetQuantityToSupply(int prodId)
        {
            int qtd;
            using (var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                Produto p = map.Read(prodId);
                qtd = p.stock_max - p.stock_total;
                das.Commit();
            }
            return qtd;
        }


        private List<Fornecedor> GetAllSuppliersForProduct(int prodId)
        {
            List<Fornecedor> res = new List<Fornecedor>();
            using (var das = new DataAccessScope(true))
            {
                IMapperFornecedorProduto map = new MapperFornecedorProduto();
                res = map.GetAllFornecedoresForProduto(prodId);
            }
            return res;
        }
    }
}