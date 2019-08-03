using Microsoft.VisualStudio.TestTools.UnitTesting;
using Clima;
using System.Collections.Generic;
using System.Globalization;
using System;
using System.Linq;

namespace Csvier.Test
{
    [TestClass]
    public class WorldWeatherTest
    {
        [TestMethod]
        public void ShouldCheckPastWeatherOnJanuaryAndMaximumTempC()
        {
            //Arrange
            using (WeatherWebApi api = new WeatherWebApi())
            {
                IEnumerable<WeatherInfo> infos = api.PastWeather(37.017, -7.933, DateTime.Parse("2019-01-01", CultureInfo.CreateSpecificCulture("pt-PT"), DateTimeStyles.AdjustToUniversal | DateTimeStyles.AssumeLocal), DateTime.Parse("2019-01-30", CultureInfo.CreateSpecificCulture("pt-PT"), DateTimeStyles.AdjustToUniversal | DateTimeStyles.AssumeLocal));
                //Act
                int max = int.MinValue;
                foreach (WeatherInfo wi in infos)
                {
                    if (wi.TempC > max) max = wi.TempC;
                }
                //Assert
                Assert.AreEqual(19, max);
            }
        }

        [TestMethod]
        public void ShouldLoadSearchOporto()
        {
            using (WeatherWebApi api = new WeatherWebApi())
            {
                IEnumerable<LocationInfo> locals = api.Search("oporto");
                Assert.AreEqual(6, locals.Count());
                Assert.AreEqual("Cuba", locals.ElementAt(5).Country);
            }
        }
    }
}
