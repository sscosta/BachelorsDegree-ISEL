using System;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALProdVendidoPorFranqueado;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDProdVendidoPorFranqueadoTest
    {
        [TestMethod]
        public void TestShouldCreate()
        {
            TestCreateAndRead();
        }

        [TestMethod]
        public void TestShouldRead()
        {
            TestCreateAndRead();
        }

        private void TestCreateAndRead()
        {
            ProdVendidoPorFranqueado pvpf = new ProdVendidoPorFranqueado(1, 10, 1.20m, 20, 10, 30, DateTime.Parse("2019-05-26"), 30);
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.Create(pvpf);
                    ProdVendidoPorFranqueado actual = map.Read(pvpf.FranqId, pvpf.ProdId);

                    Assert.IsTrue(AreEqual(pvpf, actual));

                }
            }
        }

        [TestMethod]
        public void TestShouldUpdate()
        {
            ProdVendidoPorFranqueado pvpf = new ProdVendidoPorFranqueado(1, 10, 1.20m, 20, 10, 30, DateTime.Parse("2019-05-26"), 30);
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.Create(pvpf);
                    pvpf.QtdVendas = 35;
                    map.Update(pvpf);

                    ProdVendidoPorFranqueado actual = map.Read(pvpf.FranqId, pvpf.ProdId);

                    Assert.AreEqual(pvpf.QtdVendas, actual.QtdVendas);
                }
            }
        }

        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Franqueado com o id especificado")]
        public void TestShouldDelete()
        {
            ProdVendidoPorFranqueado pvpf = new ProdVendidoPorFranqueado(1, 10, 1.20m, 20, 10, 30, DateTime.Parse("2019-05-26"), 30);
            using (ProdVendidoPorFranqueadoSession s = new ProdVendidoPorFranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperProdVendidoPorFranqueado map = s.CreateMapperProdVendidoPorFranqueado();
                    map.Create(pvpf);
                    map.Delete(pvpf);

                    map.Read(pvpf.FranqId, pvpf.ProdId);
                }
            }
        }
        private bool AreEqual(ProdVendidoPorFranqueado pvpf, ProdVendidoPorFranqueado actual)
        {
            Assert.AreEqual(pvpf.FranqId, actual.FranqId);
            Assert.AreEqual(pvpf.ProdId, actual.ProdId);
            Assert.AreEqual(pvpf.PrecoUnitario, actual.PrecoUnitario);
            Assert.AreEqual(pvpf.StockMax, actual.StockMax);
            Assert.AreEqual(pvpf.StockMin, actual.StockMin);
            Assert.AreEqual(pvpf.StockTotal, actual.StockTotal);
            Assert.AreEqual(pvpf.DataUltimaVenda, actual.DataUltimaVenda);
            Assert.AreEqual(pvpf.QtdVendas, actual.QtdVendas);
            return true;
        }
    }
}
