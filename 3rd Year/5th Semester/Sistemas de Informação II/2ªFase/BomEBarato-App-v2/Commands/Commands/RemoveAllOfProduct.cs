using BomEBarato.DALAbstraction;
using BomEBarato.DALEntregaInterfaces;
using BomEBarato.DALFornecedorProdutoInterfaces;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.SpecificDALEntrega;
using BomEBarato.SpecificDALFornecedorProduto;
using BomEBarato.SpecificDALHistoricoVendas;
using BomEBarato.SpecificDALProduto;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using System;
using ViewController;

namespace Commands
{
    public class RemoveAllOfProduct : ICommand
    {
        public void Execute()
        {
            ShowProductList();
            Console.WriteLine("Insert Id of product to remove");
            int prodId;

            while (!int.TryParse(Console.ReadLine(), out prodId))
                Console.WriteLine("Invalid number, please try again!");

            RemoveProductFromFornecedorProduto(prodId);
            RemoveFromHistoricoVendas(prodId);
            RemoveFromProdVendidoPorFranqueado(prodId);
            RemoveFromEntrega(prodId);
            RemoveFromProduto(prodId);
        }
        private void ShowProductList()
        {
            foreach (Produto pt in new MapperProduto().GetAll())
            {
                Console.WriteLine("\tID:{0} | Cod:{1} | Description:{2}", pt.id, pt.cod, pt.descricao);

            }
        }

        private void RemoveFromProduto(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperProduto map = new MapperProduto();
                map.Delete(prodId);
                das.Commit();
            }
        }

        private void RemoveFromEntrega(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperEntrega map = new MapperEntrega();
                map.DeleteAllWithProdId(prodId);
                das.Commit();
            }
        }

        private void RemoveFromProdVendidoPorFranqueado(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperProdVendidoPorFranqueado map = new MapperProdVendidoPorFranqueado();
                map.DeleteAllWithProdId(prodId);
                das.Commit();
            }
        }

        private void RemoveFromHistoricoVendas(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperHistoricoVendas map = new MapperHistoricoVendas();
                map.DeleteAllWithProdId(prodId);
                das.Commit();
            }
        }

        private void RemoveProductFromFornecedorProduto(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                IMapperFornecedorProduto map = new MapperFornecedorProduto();
                map.DeleteAllWithProdId(prodId);
                das.Commit();
            }
        }
    }
}