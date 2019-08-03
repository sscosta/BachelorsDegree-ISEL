using System;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFornecedor;
using BomEBarato.SpecificDALProduto;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDProdutoTest
    {
        [TestMethod]
        public void ShouldTestCreate()
        {
            Produto p = new Produto();
            p.Descrição = "Batatas";
            p.Cod = "1231231239";
            p.StockMax = 100;
            p.StockMin = 12;
            p.StockTotal = 80;
            p.Tipo = "Alimentar";

            using(ProdutoSession s = new ProdutoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Create(p);

                    Assert.IsNotNull(p.Id);

                    //Rollback
                }
            }
        }
        [TestMethod]
        public void ShouldTestRead()
        {
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    Produto p = map.Read(1);

                    Assert.IsNotNull(p.Descrição);
                }
            }
        }
        [TestMethod]
        public void ShouldTestUpdate()
        {
            Produto p = new Produto();
            p.Cod = "1111111111";
            p.Descrição = "Maçãs Royal Gala";
            p.Id = 1;
            p.StockMax = 200;
            p.StockMin = 15;
            p.StockTotal = 150;
            p.Tipo = "Alimentar";
           
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    Produto old = map.Read(1);
                    map.Update(p);

                    Produto newVal = map.Read(1);

                    Assert.AreEqual(p.Descrição, newVal.Descrição);
                    Assert.AreNotEqual(old.Descrição, newVal.Descrição);
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Produto com o id especificado")]
        public void ShouldTestDelete()
        {
            Produto p = new Produto();
            p.Cod = "5353535353";
            p.Descrição = "Dummy Description";
            p.Tipo = "Alimentar";
            p.StockMax = 10;
            p.StockMin = 2;
            p.StockTotal = 10;
            using (ProdutoSession s = new ProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProduto map = s.CreateMapperProduto();
                    map.Create(p);
                    map.Delete(p);
                    Produto p1 = map.Read(p.Id);
                    das.Commit();
                }
            }
        }
    }
}
