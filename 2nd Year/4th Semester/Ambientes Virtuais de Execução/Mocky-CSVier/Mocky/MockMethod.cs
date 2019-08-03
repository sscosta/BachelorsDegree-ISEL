using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;
using Queries;

namespace Mocky
{
    public class MockMethod
    {
        private readonly Type klass;
        private MethodInfo meth;
        private IEnumerable<MethodInfo> methods;
        private Dictionary<object[], object> results;

        private object[] args;
        private Delegate del;

        private string Name { get; set; }
        public MethodInfo Method { get { return meth;  } }

        public MockMethod(Type type, string name)
        {
            this.klass = type;
            this.Name = name;
            this.methods = MethodInfoCache.FindMethod(klass, name);
            if (methods.Count() == 0)
                throw new ArgumentException("There is no method " + name + " in type " + type);
            this.results = new Dictionary<object[], object>();

        }

        public MockMethod With(params object[] args)
        {
            if (this.args != null)
                throw new InvalidOperationException("You already called With() !!!!  Cannot call it twice without calling Return() first!");
            if (meth == null)
                this.meth = MethodInfoCache.FindMethod(klass, Name, args.Select(a => a.GetType()).ToArray());
            this.args = args;
            return this;
        }

        private MethodInfo GetMethodFromTypes(object[] args)
        {
            return methods.Where(
                m => MockerUtils.AreAllCompatible(
                    m.GetParameters().Select(p=> p.ParameterType).ToArray(),
                    args.Select(a => a.GetType()).ToArray())).First();
        }

        public MockMethod Then(Action p)
        {
            HandleThen(p);
            return this;
        }

        public MockMethod Then<T>(Action<T> p)
        {
            HandleThen(p);
            return this;
        }

        public MockMethod Then<T, R>(Func<T,R> p)
        {
            HandleThen(p);
            return this;
        }
        public MockMethod Then<T1, T2, T3>(Func<T1, T2, T3> p)
        {
            HandleThen(p);
            return this;
        }

        public MockMethod Then<T1, T2, T3, T4>(Func<T1, T2, T3,T4> p)
        {
            HandleThen(p);
            return this;
        }

        public void HandleThen(Delegate del)
        {
            this.del = del;
            ValidateTypeParameters();
        }

        private void ValidateTypeParameters()
        {
            Type delType = del.GetType();
            if (!IsThereAnyMethodCompatibleWithDelegate(delType))
                throw new InvalidOperationException("Invalid type argument for method " + Name);
                
        }

        private bool IsThereAnyMethodCompatibleWithDelegate(Type delType)
        {
            Type[] delParams = LazyQueries.Convert<ParameterInfo, Type>(delType.GetMethod("Invoke").GetParameters(), pi => pi.ParameterType).ToArray();
            foreach (MethodInfo mi in methods)
            {
                Type[] methParams = mi.GetParameters().Select(p => p.ParameterType).ToArray();                  
                if ((methParams.Length == 0 && delParams.Length == 0)
                    ||
                    (MockerUtils.AreAllCompatible(methParams, delParams)
                    && delType.GetMethod("Invoke").ReturnType == mi.ReturnType
                    && this.Name.Equals(mi.Name)))
                {
                    meth = mi;
                    return true;
                }
                    
            }
            return false;
        }

        public MockMethod Return(object res)
        {
            if (del != null)
                throw new InvalidOperationException("Cannot call Return after calling Then! Use Then => With instead.");
            results.Add(args, res);
            this.args = null;
            return this;
        }

        public object Call(params object [] args)
        {
            if (meth.ReturnType == typeof(void))
                return null;
            if (del != null)
                return del.DynamicInvoke(args);
            if (results.Count == 0)
                throw new InvalidOperationException();
            foreach(object[] key in results.Keys)
            {
                if (key.SequenceEqual(args))
                    return results[key];
            }
            return GetDefaultValue(meth.ReturnType);
        }
       

        object GetDefaultValue(Type t)
        {
            if (t.IsValueType)
                return Activator.CreateInstance(t);

            return null;
        }
    }
}