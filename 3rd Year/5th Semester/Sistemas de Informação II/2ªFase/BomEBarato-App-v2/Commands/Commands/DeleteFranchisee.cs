using BomEBarato.DALAbstraction;
using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALFranqueado;
using BomEBarato.SpecificDALHistoricoVendas;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using System.Collections.Generic;
using ViewController;

namespace Commands
{
    public class DeleteFranchisee : ICommand
    {
        public void Execute()
        {
            ShowAllFranqueados();
            Console.WriteLine("Insert Id of the franchisee you wish to delete");
            int franqId;

            while (!int.TryParse(Console.ReadLine(), out franqId))
                Console.WriteLine("Invalid number, please try again!");
            using (var das = new DataAccessScope(true))
            {
                RemoveFranchiseeFromHistoricoVendas(franqId);
                RemoveFranchiseeFromProdVendidoPorFranqueado(franqId);
                RemoveFranchiseeFromEntrega(franqId);
                RemoveFranchiseeFromFranqueado(franqId);
                das.Commit();
            }
    }

        private void ShowAllFranqueados()
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperFranqueado map = new MapperFranqueado();
                List<Franqueado> fs = map.GetAll();

                foreach(Franqueado f in fs)
                {
                    Console.WriteLine("Franqueado : {0}, Nif: {1} , Nome: {2}, Morada: {3}", f.id, f.nif, f.nome, f.morada);
                }
            }
        }

        private void RemoveFranchiseeFromFranqueado(int franqId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperFranqueado map = new MapperFranqueado();
                map.Delete(franqId);
                das.Commit();
            }
        }

        private void RemoveFranchiseeFromEntrega(int franqId)
        {
            using(var das = new DataAccessScope(true))
            {
                IMapperEntrega map = new MapperEntrega();
                map.DeleteAllWithFranqId(franqId);
                das.Commit();
            }
        }

        private void RemoveFranchiseeFromProdVendidoPorFranqueado(int franqId)
        {
            using(var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                map.DeleteAllWithFranqId(franqId);
                das.Commit();
            }
        }

        private void RemoveFranchiseeFromHistoricoVendas(int franqId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperHistoricoVendas map = new MapperHistoricoVendas();
                map.DeleteAllWithFranqId(franqId);
                das.Commit();
            }
        }
    }
}