using Mocky;
using System.Net;
using System;
public class DummyForIHttpRequest{
	MockMethod[] mm;
	private readonly WebClient client = new WebClient();
	
	public DummyForIHttpRequest(MockMethod[] mm){
		this.mm =  mm;
	}
	
	public void Dispose(){
		for(int i = 0; i < mm.Length; i++){
			MockMethod m = mm[i];
			if(m.Method.Name.Equals("GetBody"))
				 m.Call();
		}
		throw new InvalidOperationException();
	}
	
	public string GetBody(string url)
    {
		for(int i = 0; i < mm.Length; i++){
			MockMethod m = mm[i];
			if(m.Method.Name.Equals("GetBody"))
				return (string) m.Call(url);
		}
		throw new InvalidOperationException();
    }
	
}