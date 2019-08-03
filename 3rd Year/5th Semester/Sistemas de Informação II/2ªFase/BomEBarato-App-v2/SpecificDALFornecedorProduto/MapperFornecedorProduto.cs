using BomEBarato.DALAbstraction;
using BomEBarato.DALFornecedorProdutoInterfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.SpecificDALFornecedorProduto
{
    public class MapperFornecedorProduto : IMapperFornecedorProduto
    {
        private ISession MySession;

        public MapperFornecedorProduto()
        {
            MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
        }

        public void Create(FornecedorProduto entity)
        {
            throw new NotImplementedException();
        }

        public void Delete(FornecedorProduto entity)
        {
            throw new NotImplementedException();
        }

        public void DeleteAllWithProdId(int prodId)
        {
            using(var das = new DataAccessScope(true))
            {
                using(var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    ctx.FornecedorProduto.RemoveRange(ctx.FornecedorProduto.Where(fp => fp.prod_id == prodId));
                    ctx.SaveChanges();
                }
                das.Commit();
            }
        }

        public List<Fornecedor> GetAllFornecedoresForProduto(int prodId)
        {
            List<Fornecedor> fs;
            using(var das = new DataAccessScope(true))
            {
                using(var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    fs = ctx.FornecedorProduto.Where(fp => fp.prod_id == prodId).Select(fp => fp.Fornecedor).ToList();
                }
            }
            return fs;
        }

        public FornecedorProduto Read(int fornId, int prodId)
        {
            FornecedorProduto fp;
            using (var das = new DataAccessScope(true))
            {
                using (var ctx = new SI2_Bom_e_BaratoEntities())
                {
                    fp = ctx.FornecedorProduto.Where(f => f.forn_id == fornId && f.prod_id == prodId).FirstOrDefault();
                }
            }
            return fp;
        }

        public void Update(FornecedorProduto entity)
        {
            throw new NotImplementedException();
        }
    }
}
