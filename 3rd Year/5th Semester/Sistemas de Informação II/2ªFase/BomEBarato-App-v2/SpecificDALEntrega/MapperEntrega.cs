using BomEBarato.DALAbstraction;
using BomEBarato.DALEntregaInterfaces;
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.SpecificDALEntrega
{
    public class MapperEntrega : IMapperEntrega
    {
        private ISession MySession;

        public MapperEntrega()
        {
            MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }
        public void Create(Entrega entity)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false,out isMytr, out isMyConn);

            using(var ctx = MySession.GetDBCtx())
            {
                ctx.Entrega.Add(entity);
                ctx.SaveChanges();
            }
            
            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
        }
        public Entrega Read(int franqId, int prodId)
        {
            Entrega ent;
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = MySession.GetDBCtx())
            {
                ent = ctx.Entrega.Where(e => e.franq_id == franqId && e.prod_id == prodId).FirstOrDefault();

            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
            return ent;
        }

        public void Update(Entrega entity)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = MySession.GetDBCtx())
            {
                var ex = ctx.Entrega.Where(e => e.franq_id == entity.franq_id && e.prod_id == entity.prod_id).FirstOrDefault();
                if(ex != null)
                {
                    ctx.Entry(ex).CurrentValues.SetValues(entity);
                }
                ctx.SaveChanges();
            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
        }
        public void Delete(Entrega entity)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = MySession.GetDBCtx())
            {
                ctx.Entrega.Remove(entity);
                ctx.SaveChanges();
            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                var entToDelete = ctx.Entrega.Where(e => e.franq_id == franqId).ToList();
                foreach (var deleteEnt in entToDelete)
                {
                    ctx.Entrega.Remove(deleteEnt);
                    ctx.SaveChanges();
                }
            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
        }

        public void OrderOutOfStock(List<Entrega> prodsEmRutura)
        {
            foreach (Entrega e in prodsEmRutura)
            {
                OrderSingle(e);
            }
        }

        private void OrderSingle(Entrega e)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {

                if (ctx.Entrega.Where(ent => ent.prod_id == e.prod_id && ent.franq_id == e.franq_id).Any())
                {
                    Entrega existing = ctx.Entrega.Where(ent => ent.prod_id == e.prod_id && ent.franq_id == e.franq_id).FirstOrDefault();
                    existing.valor_ped += e.valor_ped;
                }
                else
                {
                    ctx.Entrega.Add(e);
                }
                ctx.SaveChanges();
            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
        }

        public void DeleteAllWithProdId(int prodId)
        {
            bool isMytr, isMyConn;
            MySession.CreateScope(false, out isMytr, out isMyConn);

            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                ctx.Entrega.RemoveRange(ctx.Entrega.Where(e => e.prod_id == prodId));
                ctx.SaveChanges();
            }

            MySession.EndTransaction(true, isMytr, isMyConn);
            MySession.CloseConnection(isMytr);
            
        }
    }
}
