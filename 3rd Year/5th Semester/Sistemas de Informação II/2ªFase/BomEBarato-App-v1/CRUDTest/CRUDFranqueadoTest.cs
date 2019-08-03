using System;
using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.Entidades;
using BomEBarato.SpecificDALFranqueado;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CRUDTest
{
    [TestClass]
    public class CRUDFranqueadoTest
    {
        [TestMethod]
        public void ShouldTestCreate()
        {
            Franqueado f = new Franqueado();
            f.Nif = 123456798;
            f.Nome = "B&B Oriente";
            f.Morada = "Rua Conselheiro Lopo Vaz AB 1ºD";

            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    map.Create(f);
                    Assert.IsTrue(f.Id > 0);

                    //Rollback
                }
            }
        }

        [TestMethod]
        public void ShouldTestRead()
        {
            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    Franqueado f = map.Read(1);

                    Assert.IsTrue(f.Id == 1);
                    Assert.IsTrue(f.Nif > 0);

                    das.Commit();
                }
            }
        }
        [TestMethod]
        public void ShouldTestUpdate()
        {
            Franqueado f = new Franqueado();
            f.Nome = "Meu Super Moscavide";
            f.Id = 1;
            f.Nif = 123456781;
            f.Morada = "R. Laureano de Oliveira 19 B, 1885-051 Lisboa";

            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    Franqueado old = map.Read(1);
                    map.Update(f);

                    Franqueado newF = map.Read(1);

                    Assert.AreNotEqual(old.Morada, newF.Morada);
                    Assert.AreNotEqual(old.Nif, newF.Nif);
                    Assert.AreEqual(f.Morada, newF.Morada);

                }
            }
        }
        [TestMethod]
        [ExpectedException(typeof(Exception),
        "Não existe Franqueado com o id especificado")]
        public void ShouldTestDelete()
        {
            Franqueado f = new Franqueado();
            f.Morada = "Morada Teste";
            f.Nif = 123456777;
            f.Nome = "Nome Teste";
            using (FranqueadoSession s = new FranqueadoSession())
            {
                using (var das = s.CreateDataAccessScope(true))
                {
                    IMapperFranqueado map = s.CreateMapperFranqueado();
                    map.Create(f);
                    map.Delete(f);

                    map.Read(f.Id);
                }
            }
        }
    }
}
