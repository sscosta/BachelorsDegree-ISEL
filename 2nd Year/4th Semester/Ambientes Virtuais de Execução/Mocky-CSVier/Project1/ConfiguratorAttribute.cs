using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ColumnSelector
{
    [AttributeUsage (AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Constructor, AllowMultiple = true)]
    public class ConfiguratorAttribute : Attribute
    {
        readonly string name;
        readonly int col;
        public ConfiguratorAttribute(string name, int col)
        {
            this.name = name;
            this.col = col;
        }
        
        public int GetCol()
        {
            return col;
        }
        public string GetName()
        {
            return name;
        }
    }
}
