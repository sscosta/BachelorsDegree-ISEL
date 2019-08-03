using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Mocky;
namespace Mocky
{
    public class DynamicMockerBase
    {
        MockMethod[] mms;

        public DynamicMockerBase(MockMethod[] mms)
        {
            this.mms = mms;
        }

        public MockMethod RetrieveMethod(MethodInfo mi)
        {
            foreach (MockMethod m in mms)
            {
                if (MockerUtils.AreAllCompatible(m.Method, mi))
                {
                    return m;
                }
            }
            throw new InvalidOperationException();
        }

    }
}
