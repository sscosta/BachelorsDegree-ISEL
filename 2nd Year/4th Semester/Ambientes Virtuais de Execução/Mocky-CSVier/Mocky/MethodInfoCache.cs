using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Mocky
{
    class MethodInfoCache
    {
        static Dictionary<Type, List<MethodInfo>> im = new Dictionary<Type, List<MethodInfo>>();

        public static IEnumerable<MethodInfo> FindMethod(Type t, string name)
        {
            List<MethodInfo> mi = GetCachedMethods(t);
            foreach(MethodInfo m in mi)
            {
                if (m.Name.Equals(name))
                    yield return m;
            }
        }

        public static MethodInfo FindMethod(Type t, string name, Type[] parameterTypes)
        {
            List<MethodInfo> mi = GetCachedMethods(t);
            foreach (MethodInfo m in mi)
            {
                if (m.Name.Equals(name) && m.GetParameters().Select(p => p.ParameterType).SequenceEqual(parameterTypes))
                    return m;
            }
            throw new InvalidOperationException("no such method");
        }

        public static List<MethodInfo> GetCachedMethods(Type t)
        {
            List<MethodInfo> m;
            if (!im.TryGetValue(t, out m))
            {
                m = GetInterfaceHierarchyMethods(t);
                im.Add(t, m);
            }
            return m;
        }

        private static List<MethodInfo> GetInterfaceHierarchyMethods(Type klass)
        {
            List<MethodInfo> methods = new List<MethodInfo>(klass.GetMethods(
    BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.Instance));
            foreach (Type interf in klass.GetInterfaces())
            {
                foreach (MethodInfo method in interf.GetMethods(
                    BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.Instance))
                    if (!methods.Contains(method))
                        methods.Add(method);
            }
            return methods;
        }
    }
}
