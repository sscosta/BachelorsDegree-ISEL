using BomEBarato.DALAbstraction;
using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading;
using ViewController;
using ProdVendidoPorFranqueado = ViewController.ProdVendidoPorFranqueado;

namespace BomEBarato.SpecificDALProdVendidoPorFranqueado
{
    public class MapperProdVendidoPorFranqueado : IMapperProdVendidoPorFranqueado
    {
        private ISession MySession;

        public MapperProdVendidoPorFranqueado()
        {
            MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }

        public void Create(ProdVendidoPorFranqueado entity)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.ProdVendidoPorFranqueado.Add(entity);

                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public ProdVendidoPorFranqueado Read(int franqId, int prodId)
        {
            ProdVendidoPorFranqueado pvpf = new ProdVendidoPorFranqueado();
            using (var das = new DataAccessScope(true))
            {
                using(var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ViewController.ProdVendidoPorFranqueado pv = (from p in ctx.ProdVendidoPorFranqueado where p.franq_id == franqId && p.prod_id == prodId select p).FirstOrDefault();
                    pvpf.franq_id = Decimal.ToInt32(pv.franq_id);
                    pvpf.prod_id = pv.prod_id;
                    pvpf.preco_unitario = pv.preco_unitario;
                    pvpf.qtd_vendas = pv.qtd_vendas.GetValueOrDefault();
                    pvpf.stock_max = pv.stock_max.GetValueOrDefault();
                    pvpf.stock_min = pv.stock_min.GetValueOrDefault() ;
                    pvpf.stock_total = pv.stock_total.GetValueOrDefault();
                }
            }
            return pvpf;
        }

        public List<TopSales> SalesByFranchiseeThisYear()
        {
            List<TopSales> salesByFranchisee;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {

                    var query = from p in ctx.ProdVendidoPorFranqueado
                                group p by p.franq_id
                                into res
                                select new TopSales()
                                {
                                    FranqId = (int)res.Key,
                                    VendasAnoCorrente = res.Sum(p => p.preco_unitario * p.qtd_vendas).Value
                                };
                    salesByFranchisee = query.ToList();
                }
            }
            return salesByFranchisee;
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    var pvpfToDelete = ctx.ProdVendidoPorFranqueado.Where(p => p.franq_id == franqId).ToList();
                    foreach (var deletepvpf in pvpfToDelete)
                    {
                        ctx.ProdVendidoPorFranqueado.Remove(deletepvpf);
                    }
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public List<AvgSale> AvgSalesInPresentYear(int prodId)
        {
            List<AvgSale> avgSales;
            using(var das = new DataAccessScope(true))
            {
                using(var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    avgSales = ctx.ProdVendidoPorFranqueado
                        .Where(p => p.prod_id == prodId)
                        .GroupBy(p => p.prod_id, r => r.qtd_vendas)
                        .Select(g => new AvgSale() {
                            ProdId = g.Key,
                            Avg = g.Average() ?? 0.0
                        }).ToList(); ;
                }
            }
            return avgSales;
        }

        public List<ProdVendidoPorFranqueado> GetOutOfStock(double percentagemRutura, int franqId)
        {
            List<ProdVendidoPorFranqueado> ps;
            using(var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ps = ctx.ProdVendidoPorFranqueado.Where(pvpf => pvpf.franq_id == franqId && pvpf.stock_total < pvpf.stock_max * percentagemRutura).ToList();
                }
            }
            return ps;
        }

        public void Update(ProdVendidoPorFranqueado entity)
        {
            throw new NotImplementedException();
        }

        public void Delete(ProdVendidoPorFranqueado entity)
        {
            throw new NotImplementedException();
        }

       public List<ProdVendidoPorFranqueado> GetAllInFranchisee(int franqId)
        {
            List<ProdVendidoPorFranqueado> ps;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ps = ctx.ProdVendidoPorFranqueado.Where(pvpf => pvpf.franq_id==franqId).ToList();
                }
            }
            return ps;
        }

        private void UpdateDueToSale(int franqId, int prodId, int quantidade)
        {
            using(var ctx = new SI2_Bom_e_BaratoEntities())
            {
                var p = ctx.ProdVendidoPorFranqueado.Where(pvpf => pvpf.franq_id == franqId && pvpf.prod_id == prodId).SingleOrDefault();
                p.stock_total -= quantidade;
                p.qtd_vendas += quantidade;
                ctx.SaveChanges();
            }
        }

            public void UpdateInBulk(int franqId, List<ProdutoViewInStore> sale)
        {
            using (var das = new DataAccessScope(false))
            {
                foreach (ProdutoViewInStore p in sale)
                {
                    UpdateDueToSale(franqId, p.ProdId, p.Quantidade);
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
                    ctx.ProdVendidoPorFranqueado.RemoveRange(ctx.ProdVendidoPorFranqueado.Where(pvpf => pvpf.prod_id == prodId));
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }
    }
}
