using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ColumnSelector
{
    interface IConfigurator
    {
        bool Validate(Type klass);
        void Configure(object target, string[] line);
    }
}
