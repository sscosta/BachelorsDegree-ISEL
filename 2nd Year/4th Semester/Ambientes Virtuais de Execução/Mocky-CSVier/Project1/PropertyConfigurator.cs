using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ColumnSelector
{
    class PropertyConfigurator : IConfigurator
    {
        string name;
        int col;
        PropertyInfo pi;

        PropertyConfigurator(string name, int col)
        {
            this.name = name;
            this.col = col;
        }
        public bool Validate(Type klass)
        {
            pi = klass.GetProperty(name);
            return pi != null;
        }
        public void Configure(object target, string[] line)
        {
            if (pi == null)
                throw new InvalidOperationException();
            pi.SetValue(target, line[col]);
        }
    }
}
