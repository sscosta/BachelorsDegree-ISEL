using BomEBarato.DALAbstraction;
using BomEBarato.Dto;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;
using ProdVendidoPorFranqueado = ViewController.ProdVendidoPorFranqueado;

namespace BomEBarato.DALProdVendidoProFranqueadoInterfaces
{
    public interface IMapperProdVendidoPorFranqueado : IMapper<ProdVendidoPorFranqueado,int , int>
    {
        List<TopSales> SalesByFranchiseeThisYear();
        List<AvgSale> AvgSalesInPresentYear(int prodId);
        List<ProdVendidoPorFranqueado> GetOutOfStock(double percentagemRutura, int franqId);
        List<ProdVendidoPorFranqueado> GetAllInFranchisee(int franqId);

        void DeleteAllWithFranqId(int franqId);
        void DeleteAllWithProdId(int prodId);
        void UpdateInBulk(int franqId, List<ProdutoViewInStore> sale);
    }
}
