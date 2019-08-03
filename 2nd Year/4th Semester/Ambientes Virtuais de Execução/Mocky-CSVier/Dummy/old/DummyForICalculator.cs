using Mocky;
using System;
using System.Collections.Generic;

class DummyForICalculator
{
	MockMethod[] mms;
	DummyForICalculator(MockMethod[] mms){
		this.mms = mms;
	}
    int Add(int a, int b)
    {
		for(int i  = 0; i < mms.Length; i++){
			if(mms[i].Method.Name.Equals("Add"))
				return (int) mms[i].Call(a, b);
		}
        throw new InvalidOperationException();
    }
    int Sub(int a, int b)
    {
        for(int i  = 0; i < mms.Length; i++){
			if(mms[i].Method.Name.Equals("Add"))
				return (int) mms[i].Call(a, b);
		}
		 throw new InvalidOperationException();
    }
    int Mul(int a, int b)
    {
       	for(int i  = 0; i < mms.Length; i++){
			if(mms[i].Method.Name.Equals("Add"))
				return (int) mms[i].Call(a, b);
		}
		 throw new InvalidOperationException();
    }

    int Div(int a, int b)
    {		
		for(int i  = 0; i < mms.Length; i++){
			if(mms[i].Method.Name.Equals("Add"))
				return (int) mms[i].Call(a, b);
		}
		 throw new InvalidOperationException();
    }

}