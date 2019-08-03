using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ViewController;

namespace BomEBarato.DALAbstraction
{
    public interface ISession : IDisposable
    {
        void EndTransaction(bool MyVote, bool isMyTransaction, bool pessimisticLock);
        void CloseConnection(bool isMyTransaction);
        void CreateScope(bool pessimisticLock, out bool isMyTr, out bool isMyConn);
        SI2_Bom_e_BaratoEntities GetDBCtx();

    }
}
