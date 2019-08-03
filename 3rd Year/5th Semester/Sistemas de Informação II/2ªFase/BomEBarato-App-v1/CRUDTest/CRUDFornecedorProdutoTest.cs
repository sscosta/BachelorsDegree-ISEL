using System;
using BomEBarato.DALFornecedorProdutoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFornecedorProduto;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDFornecedorProdutoTest
    {
        [TestMethod]
        public void TestShouldCreate()
        {
            FornecedorProduto fp = new FornecedorProduto(1,10,true);
            using (FornecedorProdutoSession s = new FornecedorProdutoSession())
            {
                using(var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedorProduto map = s.CreateMapperFornecedorProduto();
                    map.Create(fp);

                    FornecedorProduto actual = map.Read(fp.FornId, fp.ProdId);

                    Assert.AreEqual(fp.FornId, actual.FornId);
                    Assert.AreEqual(fp.ProdId, actual.ProdId);

                    //Rollback
                }
            }
        }
        [TestMethod]
        public void TestShouldRead()
        {
            FornecedorProduto fp = new FornecedorProduto(1, 10, true);
            using (FornecedorProdutoSession s = new FornecedorProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedorProduto map = s.CreateMapperFornecedorProduto();
                    map.Create(fp);

                    FornecedorProduto actual = map.Read(fp.FornId, fp.ProdId);

                    Assert.AreEqual(fp.FornId, actual.FornId);
                    Assert.AreEqual(fp.ProdId, actual.ProdId);

                    //Rollback
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(NotSupportedException), "Operação não faz sentido neste contexto")]
        public void TestShouldUpdate()
        {
            throw new NotSupportedException("Operação não faz sentido neste contexto");
        }

        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Produto com o id especificado")]
        public void TestShouldDelete()
        {
            FornecedorProduto fp = new FornecedorProduto(1, 10, true);
            using (FornecedorProdutoSession s = new FornecedorProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedorProduto map = s.CreateMapperFornecedorProduto();
                    map.Create(fp);
                    map.Delete(fp);
                    FornecedorProduto p1 = map.Read(fp.FornId,fp.ProdId);
                    das.Commit();
                }
            }
        }
    }
}