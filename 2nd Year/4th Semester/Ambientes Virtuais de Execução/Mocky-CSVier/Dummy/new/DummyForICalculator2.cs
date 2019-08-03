using Mocky;
using System;
using System.Collections.Generic;
using System.Reflection;

class DummyForICalculator : DynamicMockerBase
{

	DummyForICalculator(MockMethod[] mms):base(mms){
		
	}
    int Add(int a, int b)
    {
		MethodInfo mi = typeof(DummyForICalculator).GetMethod("Add",new Type[]{typeof(int),typeof(int)});
		return (int) RetrieveMethod(mi).Call(a,b);
    }
    int Sub(int a, int b)
    {
        MethodInfo mi = typeof(DummyForICalculator).GetMethod("Sub",new Type[]{typeof(int),typeof(int)});
		return (int) RetrieveMethod(mi).Call(a,b);
    }
    int Mul(int a, int b)
    {
       	MethodInfo mi = typeof(DummyForICalculator).GetMethod("Mul",new Type[]{typeof(int),typeof(int)});
		return (int) RetrieveMethod(mi).Call(a,b);
    }

    int Div(int a, int b)
    {		
		MethodInfo mi = typeof(DummyForICalculator).GetMethod("Div",new Type[]{typeof(int),typeof(int)});
		return (int) RetrieveMethod(mi).Call(a,b);
    }
	int Add(int a, int b, int c){
		MethodInfo mi = typeof(DummyForICalculator).GetMethod("Add",new Type[]{typeof(int),typeof(int),typeof(int)});
		return (int) RetrieveMethod(mi).Call(a,b,c);
	}

}