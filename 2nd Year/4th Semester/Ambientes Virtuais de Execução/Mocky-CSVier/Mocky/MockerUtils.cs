using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Mocky
{
    public class MockerUtils
    {
        public static bool AreAllCompatible(MethodInfo m1, MethodInfo m2)
        {
            if (!m1.Name.Equals(m2.Name))
                return false;
            if (m1.ReturnType != m2.ReturnType)
                return false;
            IEnumerable<Type> pis1 = m1.GetParameters().Select(p => p.ParameterType);
            IEnumerable<Type> pis2 = m2.GetParameters().Select(p => p.ParameterType);
            if (pis1.Count() != pis2.Count())
                return false;
            return pis1.SequenceEqual(pis2);
        }

        public static bool AreAllCompatible(Type[] ts1, Type[] ts2)
        {
            if (ts1.Length == 0 && ts2.Length == 0)
                return true;
            if (ts1.Length != ts2.Length)
                return false;
            return ts1.SequenceEqual(ts2);
        }
    }
}
