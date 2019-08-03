using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Csvier
{

    public abstract class Member
    {
        protected MemberInfo member;

        public Member(MemberInfo mi)
        {
            this.member = mi;
        }
      
        internal abstract void SetValue(object target, object value);
        internal abstract Type GetMemberType();
    }
}
