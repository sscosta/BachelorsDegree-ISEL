using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.DALAbstraction
{
    public class DataAccessScope : IDisposable
    {
        private ISession MySession;
        private bool isMyTransaction = false;
        private bool isMyConnection = false;
        private bool MyVote = false;

        public DataAccessScope(ISession s, bool startTrans, bool startConnection)
        {
            MySession = s;
            isMyTransaction = startTrans;
            isMyConnection = startConnection;
        }

        public void Commit()
        {
            MyVote = true;
        }

        public void Dispose()
        {

            MySession.EndTransaction(MyVote, isMyTransaction);
            MySession.CloseConnection(isMyConnection);
        }
    }
}
