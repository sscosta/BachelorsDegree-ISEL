using BomEBarato.DALAbstraction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.DALFranqueadoInterfaces
{
    public interface IMapperFranqueado : IMapper<Franqueado, int>
    {
        void Delete(int franqId);
        List<Franqueado> GetAll();
    }
}
