using BomEBarato.DALAbstraction;
using BomEBarato.DALProdutoInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using ViewController;
using Produto = ViewController.Produto;

namespace BomEBarato.SpecificDALProduto
{
    public class MapperProduto : IMapperProduto
    {
        private ISession MySession;

        public MapperProduto()
        {
            MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }

        public void Create(Produto entity)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.Produto.Add(entity);
                    ctx.SaveChanges();
                    Console.WriteLine("Product inserted with id: {0}.", entity.id);
                }
                das.Commit();
            }
        }

        public void Delete(int ProdId)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {

                    var prd_found = ctx.Produto.Find(ProdId);

                    if (prd_found != null)
                    {
                        ctx.ProdVendidoPorFranqueado.RemoveRange(ctx.ProdVendidoPorFranqueado.Where(x => x.prod_id == prd_found.id));
                        ctx.HistoricoVendas.RemoveRange(ctx.HistoricoVendas.Where(x => x.prod_id == prd_found.id));
                        ctx.Entrega.RemoveRange(ctx.Entrega.Where(x => x.prod_id == prd_found.id));
                        ctx.Produto.Remove(prd_found);
                        ctx.SaveChanges();
                        Console.WriteLine("Product with ID {0} removed.", ProdId);
                    }
                    else Console.WriteLine("Error removing Product {0}", ProdId);

                }
                das.Commit();
            }

        }

        public void Delete(Produto entity)
        {
            Delete(entity.id);
        }

        public void FulfillOrders(List<Proposta> encs)
        {
            List<int> pIds = encs.Select(e => e.ProdId).ToList();
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    var query = (from p in ctx.Produto where pIds.Contains(p.id) select p).ToList();
                    foreach (var q in query)
                    {
                        q.stock_total += encs.Where(e => e.ProdId == q.id).First().Qtd;
                    }
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public List<Produto> GetAll()
        {
            List<Produto> ps;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ps = ctx.Produto.ToList();
                }
            }
            return ps;
        }

        public Produto Read(int id)
        {
            Produto p;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    p = ctx.Produto.Where(prod => prod.id == id).First();
                }
            }
            return p;
        }

        public void Update(Produto entity)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    var prd_found = ctx.Produto.Find(entity.id);
                    if (prd_found != null)
                    {
                        var pu = (from a in ctx.Produto where a.id == entity.id select a).SingleOrDefault();

                        pu.cod = entity.cod;
                        pu.tipo = entity.tipo;
                        pu.descricao = entity.descricao;
                        pu.stock_max = entity.stock_max;
                        pu.stock_min = entity.stock_min;
                        pu.stock_total = entity.stock_total;

                        try
                        {
                            ctx.SaveChanges();
                            Console.WriteLine("Product {0} updated.", entity.id);
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine(e.GetBaseException());
                        }
                    }
                    else Console.WriteLine("Error updating Product {0}", entity.id);


                }
                das.Commit();
            }
        }
    }
}
