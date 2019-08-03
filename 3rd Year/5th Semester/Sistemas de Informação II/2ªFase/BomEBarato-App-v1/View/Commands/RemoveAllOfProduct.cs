using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALFornecedorProdutoInterfaces;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALFornecedorProduto;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using BomEBaratoSpecificDALHistoricoVendas;
using System;

namespace ViewController
{
    internal class RemoveAllOfProduct : ICommand
    {
        public void Execute()
        {
            Console.WriteLine("Insert Id of product to remove");
            //int prodId = int.Parse(Console.ReadLine());
            int prodId;

            while (!int.TryParse(Console.ReadLine(), out prodId) )
                Console.WriteLine("Invalid number, please try again!");

            RemoveProductFromFornecedorProduto(prodId);
            RemoveFromHistoricoVendas(prodId);
            RemoveFromProdVendidoPorFranqueado(prodId);
            RemoveFromEntrega(prodId);
            RemoveFromProduto(prodId);
        }

        private void RemoveFromProduto(int prodId)
        {
            using(ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Delete(prodId);

                }
            }
        }

        private void RemoveFromEntrega(int prodId)
        {
            using(EntregaSession s = new EntregaSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    map.DeleteAllWithProdId(prodId);
                }
            }
        }

        private void RemoveFromProdVendidoPorFranqueado(int prodId)
        {
            using(ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using(var das = s.CreateDataAccessScope(false))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.DeleteAllWithProdId(prodId);
                }
            }
        }

        private void RemoveFromHistoricoVendas(int prodId)
        {
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.DeleteAllWithProdId(prodId);
                }
            }
        }

        private void RemoveProductFromFornecedorProduto(int prodId)
        {
            using (FornecedorProdutoSession s = new FornecedorProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(false))
                {
                    IMapperFornecedorProduto map = s.CreateMapperFornecedorProduto();
                    map.DeleteAllWithProdId(prodId);
                }
            }
        }

    }
}