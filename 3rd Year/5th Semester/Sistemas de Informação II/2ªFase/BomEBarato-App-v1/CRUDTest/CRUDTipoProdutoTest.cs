using System;
using BomEBarato.DALTipoProdutoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALTipoProduto;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDTipoProdutoTest
    {
        [TestMethod]
        public void ShouldTestCreate()
        {
            TipoProduto t = new TipoProduto("teste");
            using (TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    map.Create(t);
                    TipoProduto read = map.Read(t.Descrição);

                    Assert.AreEqual(t.Descrição, read.Descrição);

                }
            }
        }
        [TestMethod]
        public void ShouldTestRead()
        {
            using (TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    TipoProduto read = map.Read("Alimentar");
                    Assert.AreEqual("Alimentar", read.Descrição);
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(NotImplementedException),
        "Não faz sentido implementar esta funcionalidade, pois a chave de TipoProduto é a unica propriedade do objeto")]
        public void ShouldTestUpdate()
        {
            throw new NotImplementedException("Não faz sentido implementar esta funcionalidade, pois a chave de TipoProduto é a unica propriedade do objeto");
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Produto com o id especificado")]
        public void ShouldTestDelete()
        {
            TipoProduto t = new TipoProduto("Teste");
            using (TipoProdutoSession s = new TipoProdutoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperTipoProduto map = s.CreateMapperTipoProduto();
                    map.Create(t);
                    map.Delete(t);
                    map.Read(t.Descrição);
                }
            }
        }
    }
}
