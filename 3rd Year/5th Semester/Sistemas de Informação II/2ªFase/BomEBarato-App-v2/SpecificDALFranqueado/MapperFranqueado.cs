using BomEBarato.DALAbstraction;
using BomEBarato.DALFranqueadoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.SpecificDALFranqueado
{
    public class MapperFranqueado : IMapperFranqueado
    {
        private ISession MySession;

        public MapperFranqueado()
        {
            MySession = (ISession)System.Threading.Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }
        public void Create(Franqueado entity)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.Franqueado.Add(entity);
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        
        public void Delete(int franqId)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {


                    var f = ctx.Franqueado.Find(franqId);
                    if (f != null)
                    {
                        ctx.HistoricoVendas.RemoveRange(ctx.HistoricoVendas.Where(x => x.franq_id == franqId));
                        ctx.Entrega.RemoveRange(ctx.Entrega.Where(x => x.franq_id == franqId));
                        ctx.ProdVendidoPorFranqueado.RemoveRange(ctx.ProdVendidoPorFranqueado.Where(x => x.franq_id == franqId));
                        ctx.Franqueado.Remove(f);
                        ctx.SaveChanges();
                        Console.WriteLine("Franchisee with ID {0} removed.", franqId);
                    }
                    else Console.WriteLine("Error removing Franchisee {0}", franqId);
                }
                das.Commit();
            }
            
        }

        
        public void Delete(Franqueado key)
        {
            using (var ctx = new SI2_Bom_e_BaratoEntities())
            {
                var f = ctx.Franqueado.Find(key);
                if (f != null)
                {
                    try
                    {
                        ctx.HistoricoVendas.RemoveRange(ctx.HistoricoVendas.Where(x => x.franq_id == f.id));
                        ctx.Entrega.RemoveRange(ctx.Entrega.Where(x => x.franq_id == f.id));
                        ctx.ProdVendidoPorFranqueado.RemoveRange(ctx.ProdVendidoPorFranqueado.Where(x => x.franq_id == f.id));
                        ctx.Franqueado.Remove(f);
                        ctx.SaveChanges();
                        Console.WriteLine("Franchisee with ID {0} removed.", key);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("SQL Error removing Franchisee: {0}", e.GetBaseException());

                    }
                }
                else Console.WriteLine("Error removing Franchisee {0}", key);

            }
        }
        

        public List<Franqueado> GetAll()
        {
            List<Franqueado> fs;
            using(var das = new DataAccessScope(true))
            {
                using(var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    fs = ctx.Franqueado.ToList();
                }
            }
            return fs;
        }

        public Franqueado Read(int id)
        {
            Franqueado f;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    f = ctx.Franqueado.Where(a => a.id==id).FirstOrDefault();
                }
            }
            return f;
        }

        public void Update(Franqueado entity)
        {

           using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    var franq_found = ctx.Franqueado.Find(entity.id);
                    if (franq_found != null)
                    {
                        var f = (from a in ctx.Franqueado where a.id == entity.id select a).SingleOrDefault();

                        f.id = entity.id;
                        f.nif = entity.nif;
                        f.morada = entity.morada;
                        f.nome = entity.nome;

                            try
                            {
                                ctx.SaveChanges();
                                Console.WriteLine("Franchisee {0} updated.", entity.id);
                            }
                            catch (Exception e)
                            {
                                Console.WriteLine(e.GetBaseException());
                            }
                        }
                        else Console.WriteLine("Error updating Franchisee {0}", entity.id);
                    }
                das.Commit();
            }
        }
    }
}
