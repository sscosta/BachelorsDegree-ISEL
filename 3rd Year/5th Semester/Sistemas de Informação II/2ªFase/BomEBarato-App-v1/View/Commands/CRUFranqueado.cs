using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALFranqueado;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using BomEBaratoSpecificDALHistoricoVendas;
using System;
using System.Collections.Generic;

namespace ViewController
{
    internal class CRUFranqueado : ICommand
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

        private void ShowFranchiseesList()
        {
            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    IEnumerable<Franqueado> franqueados = map.GetAll();
                    foreach (Franqueado f in franqueados)
                    {
                        Console.WriteLine("\tID:{0} | NIF:{1} | Name:{2}", f.Id, f.Nif, f.Nome);
                    }
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
                int value = 0;
                if (type == typeof(int))
                {
                    while (!int.TryParse(Console.ReadLine(), out value) || value < 0)
                        Console.WriteLine("Invalid number, please try again!");
                }
                return value;
            }


        }


        private void InsertFranqueado()
        {
            Franqueado f = PromptUserForFranchiseeInfo();
            using (FranqueadoSession s = new FranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    map.Create(f);
                    das.Commit();
                }
            }
        }
        private Franqueado PromptUserForFranchiseeInfo()
        {
            Franqueado f = new Franqueado();
            Console.WriteLine("Insert info of the franchisee");
            Console.Write("Nif (9 digits) : ");
            f.Nif = (int)GetInput(typeof(int));

            Console.Write("Name of Franchisee : ");
            f.Nome = (String)GetInput(typeof(String));

            Console.Write("Address :");
            f.Morada = (String)GetInput(typeof(String));

            return f;
        }

        private void RemoveFranqueado()
        {
            Console.WriteLine("Insert id of franchisee");
            int key = (int)GetInput(typeof(int));

            RemoveFranqueadoFromHistoricoVendas(key);
            RemoveFranqueadoFromEntrega(key);
            RemoveFranqueadoFromProdVendFranqueado(key);
            RemoveFranqueado(key);
        }


        private void RemoveFranqueadoFromProdVendFranqueado(int key)
        {
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProdVendidoPorFranqueado prodMap = s.CreateMapperProdVendidoPorFranqueado();
                    prodMap.DeleteAllWithFranqId(key);
                }
            }
        }

        private void RemoveFranqueadoFromHistoricoVendas(int key)
        {
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperHistoricoVendas prodMap = s.CreateMapperHistoricoVendas();
                    prodMap.DeleteAllWithFranqId(key);
                }
            }
        }

        private void RemoveFranqueadoFromEntrega(int key)
        {
            using (EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperEntrega prodMap = s.CreateMapperEntrega();
                    prodMap.DeleteAllWithFranqId(key);
                }
            }
        }




        private void RemoveFranqueado(int key)
        {
            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    Franqueado f = map.Read(key);
                    map.Delete(f);
                    das.Commit();
                }
            }
        }
        private void UpdateFranqueado()
        {
            Console.WriteLine("Insert id of franchisee to update");
            int key = (int)GetInput(typeof(int));

            Franqueado f = PromptUserForFranchiseeInfo();
            f.Id = key;

            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    map.Update(f);
                    das.Commit();
                }
            }

        }
    }
}