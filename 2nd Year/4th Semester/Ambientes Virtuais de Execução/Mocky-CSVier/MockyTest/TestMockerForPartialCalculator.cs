using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Mocky.Test
{
    [TestClass]
    public class TestMockerForPartialCalculator
    {
        readonly ICalculator calc;

        public TestMockerForPartialCalculator()
        {
            Mocker mock = new Mocker(typeof(ICalculator));
            mock.When("Add").With(5, 7).Return(12).With(3, 4).Return(7);
            mock.When("Mul").With(3, 3).Return(9);
            calc = (ICalculator)mock.Create();
        }
        
        [TestMethod]
        public void TestCalculatorSuccessfully()
        {
            Assert.AreEqual(12, calc.Add(5, 7));
            Assert.AreEqual(7, calc.Add(3, 4));
            Assert.AreEqual(0, calc.Add(4, 1)); // Returns 0 rather than 5 because that behavior was not defined for Add
            Assert.AreEqual(9, calc.Mul(3, 3));
        }

        [TestMethod]
        [ExpectedException(typeof(NotImplementedException))]
        public void TestCalculatorFailing()
        {
            Assert.AreEqual(calc.Sub(2, 1), 1); // NotImplementedException
        }
    }
}
