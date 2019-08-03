using BomEBarato.DALAbstraction;
using BomEBarato.Dto;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALProdVendidoProFranqueadoInterfaces
{
    public interface IMapperProdVendidoPorFranqueado : IMapper<ProdVendidoPorFranqueado,int , int>
    {
        IEnumerable<ProdVendidoPorFranqueado> GetOutOfStock(double percentage, int franqId);
        IEnumerable<ProdVendidoPorFranqueado> GetAll();
        IEnumerable<ProdVendidoPorFranqueado> GetAllInFranchisee(int franqId);

        void DeleteAllWithProdId(int productId);
        void DeleteAllWithFranqId(int franqId);

        IEnumerable<TopSales> SalesByFranchiseeThisYear();
        IEnumerable<AvgSale> AvgSalesInPresentYear(int productId);
        void UpdateInBulk(int franqId,List<ProdutoViewInStore> sale);
    }
}
