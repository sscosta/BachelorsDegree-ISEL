using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Mocky
{
    public class Dummy : DynamicMockerBase
    {
        Dummy(MockMethod[] mms) : base(mms)
        {
        }
        int Add(int a, int b)
        {
            MethodInfo mi = typeof(Dummy).GetMethod("Add", new Type[] { typeof(int), typeof(int) });
            return (int)RetrieveMethod(mi).Call(a, b);
        }
        int Sub(int a, int b)
        {
            MethodInfo mi = typeof(Dummy).GetMethod("Sub", new Type[] { typeof(int), typeof(int) });
            return (int)RetrieveMethod(mi).Call(a, b);
        }
        int Mul(int a, int b)
        {
            MethodInfo mi = typeof(Dummy).GetMethod("Mul", new Type[] { typeof(int), typeof(int) });
            return (int)RetrieveMethod(mi).Call(a, b);
        }

        int Div(int a, int b)
        {
            MethodInfo mi = typeof(Dummy).GetMethod("Div", new Type[] { typeof(int), typeof(int) });
            return (int)RetrieveMethod(mi).Call(a, b);
        }
        int Add(int a, int b, int c)
        {
            MethodInfo mi = typeof(Dummy).GetMethod("Add", new Type[] { typeof(int), typeof(int), typeof(int) });
            return (int)RetrieveMethod(mi).Call(a, b, c);
        }

    }
}
