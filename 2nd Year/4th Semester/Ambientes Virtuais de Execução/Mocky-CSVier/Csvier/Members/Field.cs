using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Csvier
{
    public class Field : Member
    {
        public Field(FieldInfo fi) : base(fi)
        {

        }

        internal override void SetValue(object target, object value)
        {
            FieldInfo fi = (FieldInfo)member;
            fi.SetValue(target, value);
        }

        internal override Type GetMemberType()
        {
            FieldInfo fi = (FieldInfo)member;
            return fi.FieldType;
        }

    }
}
