using ColumnSelector;
using Configurator;
using Csvier.Managers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Csvier
{
    public class CtorManager<T>
    {
        Type klass;

        private List<string> argNames = new List<string>();
        private List<int> csvCols = new List<int>();
        //To support Array Args
        private List<string> arrayArgNames = new List<string>();
        private List<Pair<int, int>> arrayCols = new List<Pair<int, int>>();

        private ConstructorInfo[] cis;
        private ParameterInfo[] Parameters { get; set; }
        private ConfiguratorAttribute[] paramAttrs;
        

        public CtorManager()
        {
            this.klass = GetType().GetGenericArguments()[0];
        }

        public void AddArg(string argName, int csvColIdx)
        {
            argNames.Add(argName);
            csvCols.Add(csvColIdx);
        }
        public void AddArrayArg(string argName, int colBeg, int colEnd)
        {
            arrayArgNames.Add(argName);
            arrayCols.Add(new Pair<int,int>(colBeg,colEnd));
        }
        private ConstructorInfo GetCtor()
        {
            ConstructorInfo ctor = FindCtorWithCustomAttributes();
            return ctor ?? FindCtorFromFramework();
        }

        private ConstructorInfo FindCtorWithCustomAttributes()
        {
            cis = klass.GetConstructors();
            foreach (ConstructorInfo ci in cis)
            {
                if (ci.GetCustomAttribute(typeof(SelectedAttribute)) != null)
                {
                    //ValidateParameterNames
                    Parameters = ci.GetParameters();
                    GetParameterNames(ci);
                    if (!IsSameSignatureByName(Parameters, GetParameterNames(ci) ))
                        throw new TargetParameterCountException("Missing constructor parameters in Custom Attributes"); 
                    return ci;
                }
                    
            }
            return null;
        }

        private List<string> GetParameterNames(ConstructorInfo ci)
        {
            List<string> parameterNames = new List<string>();
            paramAttrs = (ConfiguratorAttribute[]) ci.GetCustomAttributes(typeof(ConfiguratorAttribute), false);
            foreach (ConfiguratorAttribute ca in paramAttrs)
            {
                MemberInfo[] m = klass.GetMember(ca.GetName(), BindingFlags.Public | BindingFlags.Static| BindingFlags.Instance | BindingFlags.NonPublic);
                if (m == null || m.Length == 0)
                    throw new MissingMemberException(String.Format("There is no member {0} in class {1}", ca.GetName(), klass));
                parameterNames.Add(m[0].Name);
            }
            return parameterNames;
        }

        private ConstructorInfo FindCtorFromFramework()
        {
            foreach (ConstructorInfo ci in cis)
            {
                ParameterInfo[] pis = ci.GetParameters();
                List<string> parameterNames = argNames.Concat(arrayArgNames).ToList();
                if (IsSameSignatureByName(pis, parameterNames))
                {
                    Parameters = pis;
                    return ci;
                }
            }
            throw new InvalidOperationException(string.Format("There is no constructor in type {0} with these parameters ", klass));
        }

        bool IsSameSignatureByName(ParameterInfo[] parameters, List<string> argNames)
        {
            if (parameters.Length != argNames.Count)
                return false;
            for (int i = 0; i < parameters.Length; i++)
            {
                if (!parameters[i].Name.ToLower().Equals(argNames[i].ToLower()))
                {
                    return false;
                }
            }
            return true;
        }

        public T InvokeCtor<T>(CsvParser<T> csvParser)
        {
            ConstructorInfo ctor = GetCtor();
            if (ctor.GetCustomAttribute(typeof(SelectedAttribute)) == null)
                return (T) ctor.Invoke(GetParsedValues(csvParser));
            else
            {
                return (T)  ctor.Invoke(GetCustomAttributeValues(ctor,csvParser));
            }
        }
        private object[] GetParsedValues<T>(CsvParser<T> csvParser)
        {
            object[] parameters = new object[csvCols.Count + arrayCols.Count];
            for(int i = 0; i< csvCols.Count; i++)
            {
                int col = csvCols[i];
                parameters[i] = csvParser.GetParsedValue(Parameters[i].ParameterType, csvParser.CurrentLine().ElementAt(col));
            }
            for(int j = 0; j < arrayCols.Count; j++)
            {
                parameters[csvCols.Count + j] = csvParser.GetParsedArrayValue(Parameters[csvCols.Count + j].ParameterType, csvParser.CurrentLine(), arrayCols[j]);
            }
            return parameters;
        }

        private object[] GetCustomAttributeValues<T>(ConstructorInfo ctor, CsvParser<T> csvParser)
        {
            object[] parameters = new object[paramAttrs.Length];
            for (int i = 0; i < paramAttrs.Length; i++)
            {
                ConfiguratorAttribute attr = paramAttrs[i];
                parameters[i] = csvParser.GetParsedValue(Parameters[i].ParameterType, csvParser.CurrentLine().ElementAt(attr.GetCol()));
            }
            return parameters;
        }
    }
}
