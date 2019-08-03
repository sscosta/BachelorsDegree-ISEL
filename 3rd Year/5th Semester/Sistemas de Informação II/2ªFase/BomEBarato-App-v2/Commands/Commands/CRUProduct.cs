using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.SpecificDALProduto;
using System;
using System.Collections.Generic;
using System.Linq;
using ViewController;

namespace Commands
{
    public class CRUProduct : ICommand
    {
        List<TipoProduto> product_types_list;

        public void Execute()
        {
            Console.WriteLine("Current products in the database:");
            ShowProductList();

            LoadAllProductTypes();

            Console.WriteLine("Do you want to Insert(i), Remove(r) or update(u) the information of a Product?");
            char c = Console.ReadKey().KeyChar;
            if (c == 'i')
                InsertProduto();
            else if (c == 'r')
                RemoveProduto();
            else if (c == 'u')
                UpdateProduto();

        }









        private void InsertProduto()
        {
            Produto p = PromptUserForProductInfo();

            using (var das = new DataAccessScope(true))
            {
                IMapperProduto prod = new MapperProduto();
                prod.Create(p);
                das.Commit();
            }

        }

        private Produto PromptUserForProductInfo()
        {
            Produto p = new Produto();
            Console.WriteLine("Insert info of the Product");
            Console.Write("Cod (10 digits) : ");
            p.cod = (String)GetInput(typeof(String));
            Console.Write("Type of Product ");
            ShowAllProductTypes();
            Console.Write(" : ");

            bool isValidType = false;
            do
            {
                p.tipo = (String)GetInput(typeof(String));
                isValidType = ShowAllProductTypes_isValidType(p.tipo);
                if (!isValidType) Console.WriteLine("Invalid input, please try again");
            }
            while (!isValidType);

            Console.Write("Description :");
            p.descricao = (String)GetInput(typeof(String));
            Console.Write("Max Stock :");
            p.stock_max = (int)GetInput(typeof(int));
            Console.Write("Min Stock :");
            p.stock_min = (int)GetInput(typeof(int));
            Console.Write("Total Stock :");
            p.stock_total = (int)GetInput(typeof(int));

            return p;
        }



        // Use the same list for displaying & validating user input.
        private void LoadAllProductTypes()
        {
            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                product_types_list = ctx.TipoProduto.ToList();
            }
        }

        private void ShowAllProductTypes()
        {
            foreach (TipoProduto pt in product_types_list)
            {
                Console.Write(" {0},", pt.descricao);
            }
        }

        private bool ShowAllProductTypes_isValidType(String input)
        {
            foreach (TipoProduto pt in product_types_list)
            {
                if (Equals(pt.descricao, input)) return true;
            }
            return false;
        }

        private void ShowProductList()
        {
            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                List<Produto> pts = ctx.Produto.ToList();

                foreach (Produto pt in pts)
                    Console.WriteLine("\tID:{0} | Cod:{1} | Description:{2}", pt.id, pt.cod, pt.descricao);
            }
        }

        private void RemoveProduto()
        {
            Console.WriteLine("Insert id of Product to Remove");
            int key = (int)GetInput(typeof(int));

            using(var das = new DataAccessScope(true))
            {
                IMapperProduto prod = new MapperProduto();
                prod.Delete(key);
                das.Commit();
            }

        }

        private void UpdateProduto()
        {
            Console.WriteLine("Insert id of Product to Update");
            int key = (int)GetInput(typeof(int));

            
            Produto p = PromptUserForProductInfo();
            p.id = key;

 
            using (var das = new DataAccessScope(true))
            {
                IMapperProduto prod = new MapperProduto();
                prod.Update(p);
                das.Commit();
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
            else
            {
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