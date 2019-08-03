using Commands;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato_App_v2
{
    public class App
    {
        private static Dictionary<char, ICommand> dict = new Dictionary<char, ICommand>();
        private static Dictionary<char, string> opt = new Dictionary<char, string>();
        static App()
        {
            opt.Add('a', "List total of sales by franchisee in current year");
            dict.Add('a', new TotalSalesByFranchiseeInPresentYear());
            opt.Add('b', "Delete a franchisee including all related information");
            dict.Add('b', new DeleteFranchisee()); 
            opt.Add('c', "Insert, remove and update all information of a franchisee");
            dict.Add('c', new CRUFranqueado());
            opt.Add('d', "Insert, remove and update all info of a product");
            dict.Add('d', new CRUProduct());
            opt.Add('e', "Add new Product for sale in a franchisee");
            dict.Add('e', new AddProdParaVendaEmFranqueado());
            opt.Add('f', "Remove all information of a product");
            dict.Add('f', new RemoveAllOfProduct());
            opt.Add('g', "Interact with a consumer at the cash register");
            dict.Add('g', new InteractionWithCostumerInCashRegister());
            opt.Add('h', "In a franchisee, ask for the supply of products running out of stock");
            dict.Add('h', new AskForSupplyOfProductsOutOfStock());
            opt.Add('i', "3 phases of the process of orders to suppliers");
            dict.Add('i', new OrderToSuppliers()); 
            opt.Add('j', "Calc Avg of sales of a product in all franchisees in current year");
            dict.Add('j', new AverageOfSalesOfAProductInPresentYear());

        }

        public static void Menu()
        {
            PrintOptions();
            Route();
        }
        private static void PrintOptions()
        {
            Console.WriteLine("-----------------OPTIONS------------------");
            foreach (KeyValuePair<char, string> p in opt)
            {
                Console.WriteLine("Press '" + p.Key + "' to " + p.Value);
            }
            Console.WriteLine();
        }
        private static void Route()
        {
            ConsoleKeyInfo keyInfo = Console.ReadKey();
            char c = keyInfo.KeyChar;
            Console.WriteLine();
            if (!dict.ContainsKey(c))
                return;
            dict[c].Execute();
        }
    }
}
