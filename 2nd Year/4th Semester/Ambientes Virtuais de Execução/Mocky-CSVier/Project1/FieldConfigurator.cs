using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ColumnSelector
{
    class FieldConfigurator : IConfigurator
    {
        string name;
        int col;
        FieldInfo fi;

        FieldConfigurator(string name, int col)
        {
            this.name = name;
            this.col = col;
        }

        public bool Validate(Type klass)
        {
            fi = klass.GetField(name);
            return fi != null;
        }

        public void Configure(object target, string[] line)
        {
            if (fi == null) throw new InvalidOperationException();
            fi.SetValue(target, line[col]);
        }
    }
}
