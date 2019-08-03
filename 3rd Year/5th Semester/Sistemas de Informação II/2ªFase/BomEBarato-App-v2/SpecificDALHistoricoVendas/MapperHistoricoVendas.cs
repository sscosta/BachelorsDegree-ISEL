using BomEBarato.DALAbstraction;
using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ViewController;
using HistoricoVendas = ViewController.HistoricoVendas;

namespace BomEBarato.SpecificDALHistoricoVendas
{
    public class MapperHistoricoVendas : IMapperHistoricoVendas
    {
        private ISession MySession;

        public MapperHistoricoVendas()
        {
            MySession = (ISession)System.Threading.Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }

        public void Create(HistoricoVendas entity)
        {
            throw new NotImplementedException();
        }

        public void Delete(HistoricoVendas entity)
        {
            using(var das = new DataAccessScope(true))
            {
                using (var ctx  = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.HistoricoVendas.Attach(entity);
                    ctx.HistoricoVendas.Remove(entity);
                    ctx.SaveChanges();

                }
                das.Commit();
            }
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    var histToDelete = ctx.HistoricoVendas.Where(h => h.franq_id == franqId).ToList();
                    foreach(var deleteHist in histToDelete)
                    {
                        ctx.HistoricoVendas.Remove(deleteHist);
                    }
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public void DeleteAllWithProdId(int prodId)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.HistoricoVendas.RemoveRange(ctx.HistoricoVendas.Where(hv => hv.prod_id == prodId));
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public HistoricoVendas Read(int id, int id2)
        {
            throw new NotImplementedException();
        }

        public void Update(HistoricoVendas entity)
        {
            throw new NotImplementedException();
        }
    }
}
