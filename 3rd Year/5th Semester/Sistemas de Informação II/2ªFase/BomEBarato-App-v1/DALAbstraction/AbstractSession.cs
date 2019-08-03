using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Configuration;
namespace BomEBarato.DALAbstraction
{
    public abstract class AbstractSession : ISession
    {
        private SqlTransaction currentTrans = null;
        private SqlConnection currentConn = null;
        private string cs;
        private bool TransactionVotes;

        public AbstractSession()
        {
            cs = ConfigurationManager.ConnectionStrings["base dados"].ConnectionString;
        }

        public void CloseConnection(bool isMyConnection)
        {
            if (isMyConnection && currentConn != null)
            {
                currentConn.Close();
                currentConn = null;
            }
        }

        public SqlCommand CreateCommand()
        {
            SqlCommand cmd = currentConn.CreateCommand();

            cmd.Transaction = currentTrans;

            return cmd;
        }

        public DataAccessScope CreateDataAccessScope(bool requiresTransaction)
        {
            bool sc, st;
            if (currentConn == null)
            {
                currentConn = new SqlConnection(cs);
                currentConn.Open();
                sc = true;
            }
            else sc = false;
            if (requiresTransaction && currentTrans == null)
            {
                currentTrans = currentConn.BeginTransaction();
                TransactionVotes = true;
                st = true;
            }
            else
                st = false;
            return new DataAccessScope(this, st, sc);
        }

        public void Dispose()
        {
            if (currentConn != null)
                currentConn.Dispose();
        }

        public void EndTransaction(bool MyVote, bool isMyTransaction)
        {
            TransactionVotes &= MyVote;
            if (isMyTransaction)
            {
                if (TransactionVotes)
                    currentTrans.Commit();
                else
                    currentTrans.Rollback();
                currentTrans = null;
            }
        }
    }
}
