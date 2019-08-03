using System;
using ViewController;
using System.Linq;
using System.Collections.Generic;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.SpecificDALFranqueado;

namespace Commands
{
    public class CRUFranqueado : ICommand
    {
        public void Execute()
        {

            Console.WriteLine("Current franchisees in the database:");
            ShowFranchiseesList();

            Console.WriteLine("Do you want to Insert(i), Remove(r) or update(u) the information of a Franchisee?");
            char c = Console.ReadKey().KeyChar;
            if (c == 'i')
                InsertFranqueado();
            else if (c == 'r')
                RemoveFranqueado();
            else if (c == 'u')
                UpdateFranqueado();
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

        private void ShowFranchiseesList()
        {
            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                List<Franqueado> fcs = ctx.Franqueado.ToList();
                foreach (Franqueado f in fcs)
                    Console.WriteLine("\tID:{0} | NIF:{1} | Name:{2}", f.id, f.nif, f.nome);
            }
        }


        private Franqueado PromptUserForFranchiseeInfo()
        {
            Franqueado f = new Franqueado();
            Console.WriteLine("Insert info of the franchisee");
            Console.Write("Nif (9 digits) : ");
            f.nif = (int)GetInput(typeof(int));

            Console.Write("Name of Franchisee : ");
            f.nome = (String)GetInput(typeof(String));

            Console.Write("Address :");
            f.morada = (String)GetInput(typeof(String));

            return f;
        }


        private void InsertFranqueado()
        {
            Franqueado f = PromptUserForFranchiseeInfo();

            using (var das = new DataAccessScope(true))
            {
                IMapperFranqueado franq = new MapperFranqueado();
                franq.Create(f);
                das.Commit();
                Console.WriteLine("Franchisee inserted with id: {0}.", f.id);

            }
        }
        
        private void RemoveFranqueado()
        {
            Console.WriteLine("Insert id of franchisee");
            int key = (int)GetInput(typeof(int));


            using (var das = new DataAccessScope(true))
            {
                IMapperFranqueado f = new MapperFranqueado();
                f.Delete(key);
                das.Commit();
            }
            

        }

        private void UpdateFranqueado()
        {
            Console.WriteLine("Insert id of franchisee to update");
            int key = (int)GetInput(typeof(int));

            Franqueado f = PromptUserForFranchiseeInfo();
            f.id = key;


            using (var das = new DataAccessScope(true))
            {
                IMapperFranqueado franq = new MapperFranqueado();
                franq.Update(f);
                das.Commit();
            }




        }
    }
}