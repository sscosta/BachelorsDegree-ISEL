using System;
using System.Net;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Request;

namespace Mocky.Test
{
    [TestClass]
    public class MockyGenericsDelegates
    {

        [TestMethod]
        public void ShouldAddTwoNumbersWithDelegate()
        {
            Mocker mockCalc = new Mocker(typeof(ICalculator));
            mockCalc.When("Add").Then<int, int, int>((a, b) => a + b).With(3,5);
            ICalculator calc = (ICalculator)mockCalc.Create();

            int actual = calc.Add(3, 5);
            int expected = 8;
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void ShouldDisposeWithDelegate()
        {
            Mocker mockReq = new Mocker(typeof(IHttpRequest));
            mockReq.When("Dispose").Then(() => {/* do nothing */}).With();
            IHttpRequest req = (IHttpRequest)mockReq.Create();

            req.Dispose();

        }

        [TestMethod]
        public void ShouldGetBodyWithDelegate()
        {
            string oportoSearchUrl = "http://api.worldweatheronline.com/premium/v1/search.ashx?query=oporto&format=tab&key=ccb0b4929d574c7cb5f183401191305";

            Mocker mockReq = new Mocker(typeof(IHttpRequest));
            mockReq.When("GetBody").Then<string, string>(url => new WebClient().DownloadString(url)).With(oportoSearchUrl);
            IHttpRequest req = (IHttpRequest)mockReq.Create();

            string expected = "#The Search API\r\n#Data returned is laid out in the following order:-\r\n#AreaName    Country     Region(If available)    Latitude    Longitude   Population(if available)    Weather Forecast URL\r\n#\r\nOporto\tSpain\tGalicia\t42.383\t-7.100\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=42.3833,-7.1\nOporto\tPortugal\tPorto\t41.150\t-8.617\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=41.15,-8.6167\nOporto\tSouth Africa\tLimpopo\t-22.667\t29.633\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-22.6667,29.6333\nEl Oporto\tMexico\tTamaulipas\t23.266\t-98.768\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=23.2658,-98.7675\nPuerto Oporto\tBolivia\tPando\t-9.933\t-66.417\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-9.9333,-66.4167\nOporto\tCuba\tSantiago de Cuba\t20.233\t-76.167\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=20.2333,-76.1667\n";

            string actual = req.GetBody(oportoSearchUrl);
            Assert.AreEqual(expected, actual);
        }


        [TestMethod]
        [ExpectedException(typeof(InvalidOperationException))]
        public void ShouldThrowExceptionOnDoubleResultOfAdd()
        {
            Mocker mockCalc = new Mocker(typeof(ICalculator));
            mockCalc.When("Add").Then<int, int, double>((a, b) => a + b);
            ICalculator calc = (ICalculator)mockCalc.Create();

        }

        [TestMethod]
        [ExpectedException(typeof(InvalidOperationException))]
        public void ShouldThrowExceptionOnStringArgOfDispose()
        {
            Mocker mockReq = new Mocker(typeof(IHttpRequest));
            mockReq.When("Dispose").Then<string>((arg) => {/* do nothing */});
            IHttpRequest calc = (IHttpRequest)mockReq.Create();

        }

        [TestMethod]
        public void ShouldFindCorrectOverload()
        {
            Mocker mockCalc = new Mocker(typeof(ICalculator));
            mockCalc.When("Add").Then<int, int, int, int>((a, b, c) => a + b + c).With(1,1,1);
            mockCalc.When("Add").Then<int, int, int>((a, b) => a + b).With(3, 5);
            ICalculator calc = (ICalculator)mockCalc.Create();

            int actual = calc.Add(1,1,1);
            int expected = 3;
            Assert.AreEqual(expected, actual);

            int actual1 = calc.Add(3, 5);
            int expected1 = 8;
            Assert.AreEqual(expected1, actual1);
        }
    }
}
