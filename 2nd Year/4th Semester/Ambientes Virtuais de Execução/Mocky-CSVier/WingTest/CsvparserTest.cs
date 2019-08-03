using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Clima;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace Csvier.Test
{
    [TestClass]
    public class CsvParserTest
    {
        [TestMethod]
        [ExpectedException(typeof(InvalidOperationException), "There is no field Desc")]
        public void TestShouldThrowExceptionOnInvalidFieldArg()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";
            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .FieldArg("Desc", 10);
            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                    .Load(sampleWeatherInLisbonFiltered)
                    .Parse();
            //Assert
        }


        [TestMethod]
        [ExpectedException(typeof(InvalidOperationException), "There is no property MinTempC")]
        public void TestShouldThrowExceptionOnInvalidPropArg()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";
            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .FieldArg("MinTempC", 6);
            //Act
            //Assert
            IEnumerable<WeatherInfo> items = pastWeather
                .Load(sampleWeatherInLisbonFiltered)
                .Parse();
        }

        //Operation Tests
        [TestMethod]
        public void TestCtorWithTwoParamsAndParse()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";
            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2);
            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                    .Load(sampleWeatherInLisbonFiltered)
                    .Parse();
            //Assert
            Assert.AreEqual(4, items.Count());
        }

        [TestMethod]
        [ExpectedException(typeof(System.Reflection.TargetInvocationException), "DateTime.Parse(string) does not receive year=a")]
        public void TestShouldThrowExceptionOnInvalidFormatOfData()
        {
            //Arrange
            string sampleInvalidWeatherSample = @"a-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56";
            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2);
            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                    .Load(sampleInvalidWeatherSample)
                    .Parse();
            //Assert
            foreach( WeatherInfo wi in items.ToArray())
            {
                Console.WriteLine(wi.ToString());
            }
        }

        [TestMethod]
        public void TestRemoveCount()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"
2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";

            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);

            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                                .Load(sampleWeatherInLisbonFiltered)
                                .Remove(1)
                                .Parse();
            //Assert
            Assert.AreEqual(4, items.Count());
        }

        [TestMethod]
        public void TestRemoveEmpties()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"#
#
2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";

            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);

            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                                .Load(sampleWeatherInLisbonFiltered)
                                .RemoveWith("#")
                                .Parse();
            //Assert

            Assert.AreEqual(4, items.Count());
        }

        [TestMethod]
        public void TestRemoveWithSubstring()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"#
#
2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";

            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);
            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                                .Load(sampleWeatherInLisbonFiltered)
                                .RemoveWith("#")
                                .Parse();
            //Assert
            Assert.AreEqual(4, items.Count());
        }

        [TestMethod]
        public void TestRemoveEven()
        {
            string sampleWeatherInLisbonFiltered = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";

            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);
            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                                .Load(sampleWeatherInLisbonFiltered)
                                .RemoveEvenIndexes()
                                .Parse();
            //Assert
            Assert.IsTrue(items.ElementAt(0).Date.Equals(DateTime.Parse("2019 - 01 - 02")));
            Assert.IsTrue(items.ElementAt(1).Date.Equals(DateTime.Parse("2019 - 01 - 04")));
        }

        [TestMethod]
        public void TestRemoveOdd()
        {
            //Arrange
            string sampleWeatherInLisbonFiltered = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";

            CsvParser<WeatherInfo> pastWeather = new CsvParser<WeatherInfo>()
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);

            //Act
            IEnumerable<WeatherInfo> items = pastWeather
                                .Load(sampleWeatherInLisbonFiltered)
                                .RemoveOddIndexes()
                                .Parse();

            //Assert
            Assert.IsTrue(items.ElementAt(0).Date.Equals(DateTime.Parse("2019 - 01 - 01")));
            Assert.IsTrue(items.ElementAt(1).Date.Equals(DateTime.Parse("2019 - 01 - 03")));
        }
    }
}
