using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ColumnSelector
{
    class CtorConfigurator : IConfigurator
    {

        string name;
        int col;
        ConstructorInfo ci;

        CtorConfigurator(string name,int col)
        {
            this.name = name;
            this.col = col;
        }

        public bool Validate(Type klass)
        {
            throw new NotImplementedException();
        }

        public void Configure(object target, string[] line)
        {
            if (ci == null)
                throw new InvalidOperationException();
            ci.Invoke(target, new object[] { line[col] });
        }
    }
}
