using System;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.Entidades;
using BomEBaratoSpecificDALHistoricoVendas;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDHistoricoVendasTest
    {
        [TestMethod]
        public void TestShouldCreate()
        {
            HistoricoVendas hv = new HistoricoVendas(1,10,20);
            using(HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.Create(hv);
                    HistoricoVendas actual = map.Read(hv.FranqId, hv.ProdId);

                    Assert.AreEqual(hv.Hist3Anos, actual.Hist3Anos);
                }
            }
        }
        [TestMethod]
        public void TestShouldRead()
        {
            HistoricoVendas hv = new HistoricoVendas(1, 10, 20);
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.Create(hv);
                    HistoricoVendas actual = map.Read(hv.FranqId, hv.ProdId);

                    Assert.AreEqual(hv.Hist3Anos, actual.Hist3Anos);
                }
            }
        }
        [TestMethod]
        public void TestShouldUpdate()
        {
            HistoricoVendas hv = new HistoricoVendas(1, 10, 20);
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.Create(hv);
                    hv.Hist3Anos = 40;
                    map.Update(hv);

                    HistoricoVendas actual = map.Read(hv.FranqId, hv.ProdId);
                    Assert.AreEqual(hv.Hist3Anos, actual.Hist3Anos);
                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Franqueado com o id especificado")]
        public void TestShouldDelete()
        {
            HistoricoVendas hv = new HistoricoVendas(1, 10, 20);
            using (HistoricoVendasSession s = new HistoricoVendasSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperHistoricoVendas map = s.CreateMapperHistoricoVendas();
                    map.Create(hv);
                    map.Delete(hv);

                    map.Read(hv.FranqId, hv.ProdId);
                }
            }
        }
    }
}
