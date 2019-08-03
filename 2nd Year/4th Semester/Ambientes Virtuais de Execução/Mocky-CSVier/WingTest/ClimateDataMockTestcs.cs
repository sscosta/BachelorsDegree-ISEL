using System;
using Clima;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Mocky;
using Request;
using System.Net;
using System.Collections.Generic;
using System.Linq;

namespace Csvier.Test
{
    [TestClass]
    public class ClimateDataMockTestcs
    {
        private readonly string oportoSearchUrl = "http://api.worldweatheronline.com/premium/v1/search.ashx?query=oporto&format=tab&key=ccb0b4929d574c7cb5f183401191305";
        private readonly string oportoSearchBody = "#The Search API\r\n#Data returned is laid out in the following order:-\r\n#AreaName    Country     Region(If available)    Latitude    Longitude   Population(if available)    Weather Forecast URL\r\n#\r\nOporto\tSpain\tGalicia\t42.383\t-7.100\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=42.3833,-7.1\nOporto\tPortugal\tPorto\t41.150\t-8.617\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=41.15,-8.6167\nOporto\tSouth Africa\tLimpopo\t-22.667\t29.633\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-22.6667,29.6333\nEl Oporto\tMexico\tTamaulipas\t23.266\t-98.768\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=23.2658,-98.7675\nPuerto Oporto\tBolivia\tPando\t-9.933\t-66.417\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-9.9333,-66.4167\nOporto\tCuba\tSantiago de Cuba\t20.233\t-76.167\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=20.2333,-76.1667\n";

        [TestMethod]
        public void TestLoadSearchOportoOnMock()
        {
            Mocker mocker = new Mocker(typeof(IWeatherWebApi));
            mocker
                  .When("Search")
                  .With("oporto")
                  .Return(new LocationInfo[] {
            null,
            null,
            null,
            null,
            null,
            new LocationInfo("Cuba", "", 0, 0)});
            mocker
                .When("Dispose").Then(() => { }).With();
            using (IWeatherWebApi api = (IWeatherWebApi)mocker.Create())
            {
                IEnumerable<LocationInfo> locals = api.Search("oporto");
                Assert.AreEqual(6, locals.Count());
                Assert.AreEqual("Cuba", locals.ElementAt(5).Country);
            }
        }

        [TestMethod]
        public void TestLoadSearchOportoOnRequestMock()
        {
            Mocker mocker = new Mocker(typeof(IHttpRequest));
            mocker.When("GetBody")
                .With(oportoSearchUrl)
                .Return(oportoSearchBody);
            mocker
                .When("Dispose").Then(() => { }).With();

            IHttpRequest req = (IHttpRequest)mocker.Create();

            using (IWeatherWebApi api = new WeatherWebApi(req))
            {
                IEnumerable<LocationInfo> locals = api.Search("oporto");
                Assert.AreEqual(6, locals.Count());
                Assert.AreEqual("Cuba", locals.ElementAt(5).Country);
            }
        }
    }
}
