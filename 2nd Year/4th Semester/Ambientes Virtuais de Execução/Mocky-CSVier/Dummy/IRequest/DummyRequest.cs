using Mocky;
using System;
using System.Reflection;

public class DummyBase
    {
        MockMethod[] mms;

        public DummyBase(MockMethod[] mms)
        {
            this.mms = mms;
        }

        public MockMethod RetrieveMethod(MethodInfo mi)
        {
            foreach (MockMethod m in mms)
            {
                if (MockerUtils.AreAllCompatible(m.Method, mi))
                {
                    return m;
                }
            }
            throw new InvalidOperationException();
        }

    }

public class DummyRequest : DummyBase{

	public DummyRequest(MockMethod[] mms) : base(mms){
	}

	string GetBody(string url){
		MethodInfo mi = typeof(DummyRequest).GetMethod("GetBody",new Type[]{typeof(string)});
		return (string) RetrieveMethod(mi).Call(url);
	}
	
	void Dispose(){
		MethodInfo mi = typeof(DummyRequest).GetMethod("Dispose", Type.EmptyTypes);
		RetrieveMethod(mi).Call();
	}
}