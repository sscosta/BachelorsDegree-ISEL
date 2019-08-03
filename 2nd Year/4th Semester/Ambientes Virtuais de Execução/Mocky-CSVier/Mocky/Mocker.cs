using Request;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
namespace Mocky
{
    public class Mocker
    {
        private readonly Type klass;
        public List<MockMethod> ms;

        public Mocker(Type klass)
        {
            this.klass = klass;
            this.ms = new List <MockMethod>();
        }

        public MockMethod When(string name)
        {
            MockMethod m = new MockMethod(klass, name);
            ms.Add(m);
            return m;
        }

        public object Create()
        {
            Type t = BuildType();
            return Activator.CreateInstance(t, new object[1] { ms.ToArray()});
        }

        private Type BuildType()
        {
            AssemblyName aName = new AssemblyName("DynamicMockerFor" + klass.Name);
            AssemblyBuilder ab =
                AppDomain.CurrentDomain.DefineDynamicAssembly(
                    aName,
                    AssemblyBuilderAccess.RunAndSave);

            // For a single-module assembly, the module name is usually
            // the assembly name plus an extension.
            ModuleBuilder mb =
                ab.DefineDynamicModule(aName.Name, aName.Name + ".dll");

            TypeBuilder tb = mb.DefineType(
                "Mock" + klass.Name,
                 TypeAttributes.Public,
                 typeof(DynamicMockerBase),
                 //list of interfaces implemented by the type
                 new Type[] { klass }
                 );

            DefineCtor(tb);
            
            foreach (MethodInfo mi in MethodInfoCache.GetCachedMethods(klass))
            {
                DefineInterfaceMethods(tb, mi);
            }
           
            // Finish the type.
            Type t = tb.CreateType();

            // Saves the single-module assembly.
            ab.Save(aName.Name + ".dll");
            return t;
        }

        private void DefineCtor(TypeBuilder tb)
        {
            ConstructorBuilder ctorMocker = tb.DefineConstructor(
               MethodAttributes.Public,
               CallingConventions.Standard,
               new Type[] { typeof(MockMethod[]) });

            ConstructorInfo baseCtor = typeof(DynamicMockerBase).GetConstructor(new[] { typeof(MockMethod[]) });

            ILGenerator il = ctorMocker.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Call, typeof(DynamicMockerBase).GetConstructor(new Type[] { typeof(MockMethod[])}));
            il.Emit(OpCodes.Ret);
        }

        private Type[] ToTypeArray(ParameterInfo[] parameterInfos)
        {
            Type[] paramTypes = new Type[parameterInfos.Length];
            for(int i = 0; i< parameterInfos.Length; i++)
            {
                paramTypes[i] = parameterInfos[i].ParameterType;
            }
            return paramTypes;
        }

        private void DefineInterfaceMethods(TypeBuilder tb, MethodInfo mi)
        {
            MockMethod[] mm = ms.Where(m => MockerUtils.AreAllCompatible(m.Method, mi)).ToArray();
            if (mm.Count() > 0)
                DefineMockMethod(tb, mm[0]);
            else
                DefineDefaultInterfaceMethodBehaviour(tb, mi);
            
                
        }

        private void DefineMockMethod(TypeBuilder tb, MockMethod mockMethod)
        {
            MethodInfo mi = mockMethod.Method;
            Type[] argTypes = ToTypeArray(mockMethod.Method.GetParameters());
            MethodBuilder mb = tb.DefineMethod(mi.Name,
                 MethodAttributes.Virtual | MethodAttributes.Public | MethodAttributes.ReuseSlot | MethodAttributes.HideBySig,
                 mi.ReturnType,
                 argTypes);

            ILGenerator il = mb.GetILGenerator();

            LocalBuilder V_0 = il.DeclareLocal(typeof(MethodInfo));
            LocalBuilder V_1;
            if (mi.ReturnType != typeof(void))
                V_1 = il.DeclareLocal(mi.ReturnType);
            LocalBuilder V_2 = il.DeclareLocal(typeof(Type[]));
            LocalBuilder V_3 = il.DeclareLocal(typeof(object[]));

            Label afterjump = il.DefineLabel();

            il.Emit(OpCodes.Ldtoken, tb.TypeToken.Token);
            il.Emit(OpCodes.Call, typeof(Type).GetMethod("GetTypeFromHandle"));
            il.Emit(OpCodes.Ldstr, mi.Name);
            if(mi.ReturnType != typeof(void))
            {
                il.Emit(OpCodes.Ldc_I4, argTypes.Length);
                il.Emit(OpCodes.Newarr, typeof(Type));
                il.Emit(OpCodes.Stloc_2);
                il.Emit(OpCodes.Ldloc_2);

                for (int i = 0; i < argTypes.Length; i++)
                {
                    il.Emit(OpCodes.Ldc_I4, i);
                    il.Emit(OpCodes.Ldtoken, argTypes[i]);
                    il.Emit(OpCodes.Call, typeof(Type).GetMethod("GetTypeFromHandle"));
                    il.Emit(OpCodes.Stelem_Ref);
                    il.Emit(OpCodes.Ldloc_2);
                }
            }
            else
            {
                il.Emit(OpCodes.Ldsfld, typeof(Type).GetField("EmptyTypes"));
            }
            

            il.Emit(OpCodes.Call, typeof(Type).GetMethod("GetMethod", new Type[] { typeof(string), typeof(Type[]) }));
            il.Emit(OpCodes.Stloc_0);
            
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Call, typeof(DynamicMockerBase).GetMethod("RetrieveMethod", new Type[] { typeof(MethodInfo)}));
            il.Emit(OpCodes.Stloc_0);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ldc_I4, argTypes.Length);
            il.Emit(OpCodes.Newarr, typeof(object));

            if (mi.ReturnType != typeof(void))
                il.Emit(OpCodes.Stloc_3);

            for (int i = 0; i < argTypes.Length; i++)
            {
                il.Emit(OpCodes.Ldloc_3);
                il.Emit(OpCodes.Ldc_I4, i);
                il.Emit(OpCodes.Ldarg, i + 1);
                if (argTypes[i].IsValueType)
                    il.Emit(OpCodes.Box, argTypes[i]);
                il.Emit(OpCodes.Stelem_Ref);
            }
            if (mi.ReturnType != typeof(void))
                il.Emit(OpCodes.Ldloc_3);
            MethodInfo cm = typeof(MockMethod).GetMethod("Call", new Type[] { typeof(object[]) });
            il.Emit(OpCodes.Callvirt, cm);

            if (mi.ReturnType == typeof(void))
                il.Emit(OpCodes.Pop);
            else if (mi.ReturnType.IsValueType && !cm.ReturnType.IsValueType)
                il.Emit(OpCodes.Unbox_Any, mi.ReturnType);
            else
                il.Emit(OpCodes.Castclass, mi.ReturnType);
            il.Emit(OpCodes.Ret);

        }

        private void DefineDefaultInterfaceMethodBehaviour(TypeBuilder tb, MethodInfo mi)
        {
            MethodBuilder mb = tb.DefineMethod(mi.Name,
                 MethodAttributes.Virtual | MethodAttributes.Public | MethodAttributes.ReuseSlot | MethodAttributes.HideBySig,
                 mi.ReturnType,
                 mi.GetParameters().Select(p => p.ParameterType).ToArray());
            ILGenerator il = mb.GetILGenerator();
            il.Emit(OpCodes.Newobj, typeof(NotImplementedException).GetConstructor(new Type[0]));
            il.Emit(OpCodes.Throw);
        }
        int Add(int a, int b)
        {
            return (int)ms.Where(
                mm => mm.Method.Name.Equals("Add") &&
                mm.Method.GetParameters().Length == 2 &&
                mm.Method.GetParameters().Equals(new Type[] { typeof(int), typeof(int)})
                ).First().Call(a, b);
        }
    }
}