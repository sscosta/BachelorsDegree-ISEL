using Mocky;
using System;

public class DummyBase{
MockMethod[] mms;

public DummyBase(MockMethod[] mms){
	this.mms = mms;
}

	public MockMethod RetrieveMethod(string name){
	foreach(MockMethod m in mms){
		if(m.Method.Name.Equals(name))
			return m;
	}
	throw new InvalidOperationException();
}

}

public class DummyRequest : DummyBase{

	public DummyRequest(MockMethod[] mms) : base(mms){
	}

	string GetBody(string url){
		MockMethod m = RetrieveMethod("GetBody");
		return (string) m.Call(url);
	}
	
	void Dispose(){
		MockMethod m = RetrieveMethod("Dispose");
		m.Call();
	}
}