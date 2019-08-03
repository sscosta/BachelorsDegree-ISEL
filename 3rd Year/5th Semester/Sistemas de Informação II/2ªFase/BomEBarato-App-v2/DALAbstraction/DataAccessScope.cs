using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace BomEBarato.DALAbstraction
{

    public class DataAccessScope : IDisposable
    {
        private ISession MySession;
        private bool isMyTransaction = false;
        private bool pessimisticLock = false;
        private bool isMyConnection = false;
        private bool MyVote = false;


        public DataAccessScope(bool pessimisticLock)
        {//NOTA:
         // pessimisticLock só tem efeito no priemeiro DataAcessScope a ser criado
         // Portanto, deve ser encarado do seguinte moddo: Se for eu a iniciar
         // o processo (não) pretendo usar pessimistic locking
            bool first = false;
            MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
            if (MySession == null)
            {
                Session.Init();
                MySession = (ISession)Thread.GetData(Thread.GetNamedDataSlot("ThreadSession"));
                first = true;

            }
            MySession.CreateScope(pessimisticLock && first, out isMyTransaction, out isMyConnection);
            this.pessimisticLock = pessimisticLock & first;



        }

        public void Commit()
        {
            MyVote = true;
        }

        public void Dispose()
        {

            MySession.EndTransaction(MyVote, isMyTransaction, pessimisticLock);
            MySession.CloseConnection(isMyConnection);

        }


    }
}
