using BomEBarato.DALAbstraction;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;
using Produto = ViewController.Produto;

namespace BomEBarato.DALProdutoInterfaces
{
    public interface IMapperProduto : IMapper<Produto,int>
    {
        List<Produto> GetAll();
        void FulfillOrders(List<Proposta> encs);
        void Delete(int prodId);
    }
}
