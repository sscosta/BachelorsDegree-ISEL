using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALFranqueado;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using BomEBaratoSpecificDALHistoricoVendas;
using System;

namespace ViewController
{
    internal class DeleteFranchisee : ICommand
    {
        public IMapperProdVendidoPorFranqueado IMapperProdVendidoPorFranqueado { get; private set; }

        public void Execute()
        {
            Console.WriteLine("Insert Id of the franchisee you wish to delete");
            int input = 0;
            while (!int.TryParse(Console.ReadLine(), out input) || input < 0)
            {
                Console.WriteLine("Invalid number\n");
            }                
            RemoveFranchiseeFromHistoricoVendas(input);
            RemoveFranchiseeFromProdVendidoPorFranqueado(input);
            RemoveFranchiseeFromEntrega(input);
            RemoveFranchiseeFromFranqueado(input);
        }

        private void RemoveFranchiseeFromFranqueado(int franqId)
        {
            using(FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    map.Delete(franqId);
                }
            }
        }

        private void RemoveFranchiseeFromEntrega(int franqId)
        {
            using(EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    map.DeleteAllWithFranqId(franqId);
                }
            }
        }

        private void RemoveFranchiseeFromProdVendidoPorFranqueado(int franqId)
        {
            using(ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.DeleteAllWithFranqId(franqId);
                }
            }
        }

        private void RemoveFranchiseeFromHistoricoVendas(int franqId)
        {
            using(HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.DeleteAllWithFranqId(franqId);
                }
            }
        }
    }
}