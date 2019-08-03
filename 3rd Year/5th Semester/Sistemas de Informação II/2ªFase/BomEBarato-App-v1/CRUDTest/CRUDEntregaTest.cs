using System;
using BomEBarato.DALEntregaInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALEntrega;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDEntregaTest
    {
        [TestMethod]
        public void ShouldTestCreate()
        {
            Entrega e = new Entrega(1,10,30,30);
            using(EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    map.Create(e);
                    Entrega actual = map.Read(e.FranqId, e.ProdId);

                    Assert.AreEqual(e.ValorForn, actual.ValorForn);
                    Assert.AreEqual(e.ValorPed, actual.ValorPed);
                    Assert.AreEqual(e.ProdId, actual.ProdId);
                    Assert.AreEqual(e.FranqId, actual.FranqId);

                }
            }
        }
        [TestMethod]
        public void ShouldTestRead()
        {
            Entrega e = new Entrega(1, 10, 30, 30);
            using (EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    map.Create(e);
                    Entrega e1 = map.Read(1, 10);

                    Assert.AreEqual(e.ValorPed, e.ValorPed);
                    Assert.AreEqual(e.ValorForn, e.ValorForn);
                }
            }
        }
        [TestMethod]
        public void ShouldTestUpdate()
        {
            Entrega e = new Entrega(1, 3, 80, 70);
            using (EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    Entrega old = map.Read(e.FranqId,e.ProdId);
                    map.Update(e);

                    Entrega newE = map.Read(e.FranqId, e.ProdId);

                    Assert.AreNotEqual(old.ValorPed, newE.ValorPed);
                    Assert.AreNotEqual(old.ValorForn, newE.ValorForn);
                    Assert.AreEqual(e.FranqId, newE.FranqId);
                    Assert.AreEqual(e.ProdId, newE.ProdId);
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Franqueado com o id especificado")]
        public void ShouldTestDelete()
        {
            Entrega e = new Entrega(1, 10, 30, 30);
            using (EntregaSession s = new EntregaSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperEntrega map = s.CreateMapperEntrega();
                    map.Create(e);
                    map.Delete(e);

                    map.Read(e.FranqId,e.ProdId);
                }
            }
        }
    }
}
