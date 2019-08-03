using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace Csvier
{
    public class Property : Member
    {
        public Property(PropertyInfo pi) : base(pi)
        {

        }

        internal override void SetValue(object target, object value)
        {
            PropertyInfo pi = (PropertyInfo)member;
            pi.SetValue(target, value);
        }

        internal sealed override Type GetMemberType()
        {
            PropertyInfo pi = (PropertyInfo)member;
            return pi.PropertyType;
        }
    }
}
