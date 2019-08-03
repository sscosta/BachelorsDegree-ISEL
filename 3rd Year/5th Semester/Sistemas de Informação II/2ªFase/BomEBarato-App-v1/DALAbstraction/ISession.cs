using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALAbstraction
{
    public interface ISession : IDisposable
    {
        DataAccessScope CreateDataAccessScope(bool requiresTransaction);
        void EndTransaction(bool MyVote, bool isMyTransaction);
        void CloseConnection(bool isMyTransaction);

        SqlCommand CreateCommand();

    }
}
