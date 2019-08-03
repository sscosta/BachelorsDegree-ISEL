using System;
using BomEBarato.DALFornecedorInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFornecedor;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDFornecedorTest
    {
        [TestMethod]
        public void ShouldTestCreate()
        {
            Fornecedor f = new Fornecedor();
            f.Nif = 111111112;
            f.Nome = "Dummy Fornecedor";

            using (FornecedorSession s = new FornecedorSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedor map = s.CreateMapperFornecedor();
                    map.Create(f);
                    Assert.IsTrue(f.Id > 0);

                    //Rollback
                }
            }
        }
        [TestMethod]
        public void ShouldTestRead()
        {
            using (FornecedorSession s = new FornecedorSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedor map = s.CreateMapperFornecedor();
                    Fornecedor f = map.Read(1);

                    Assert.IsTrue(f.Id == 1);
                    Assert.IsTrue(f.Nif > 0);

                    das.Commit();
                }
            }
        }
        [TestMethod]
        public void ShouldTestUpdate()
        {
            Fornecedor f = new Fornecedor();
            f.Nome = "Delta Cafés : O café da Sua Vida";
            f.Id = 1;
            f.Nif = 1325468455;
            using (FornecedorSession s = new FornecedorSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedor map = s.CreateMapperFornecedor();
                    Fornecedor old = map.Read(1);
                    map.Update(f);

                    Fornecedor newF = map.Read(1);

                    Assert.AreNotEqual(old.Nome, newF.Nome);
                    Assert.AreNotEqual(old.Nif, newF.Nif);
                    Assert.AreEqual(f.Nome, newF.Nome);
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Franqueado com o id especificado")]
        public void ShouldTestDelete()
        {
            Fornecedor f = new Fornecedor();
            f.Nif = 123456777;
            f.Nome = "Nome Teste";
            using (FornecedorSession s = new FornecedorSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFornecedor map = s.CreateMapperFornecedor();
                    map.Create(f);
                    map.Delete(f);

                    map.Read(f.Id);
                }
            }
        }
    }
}
