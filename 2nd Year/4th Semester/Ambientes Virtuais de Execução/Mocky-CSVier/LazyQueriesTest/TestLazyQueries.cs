using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Queries;
using System.Collections.Generic;
using Clima;
using Csvier;
using System.Linq;

namespace LazyQueriesTest
{
    [TestClass]
    public class TestLazyQueries
    {
        [TestMethod]
        public void ShouldTestRemoveOddIdxes()
        {
            //Arrange
            IEnumerable<string> src = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };
            IEnumerable<string> expected = new string[] { "linha 0", "linha 2" };

            //Act
            IEnumerable<string> actual = LazyQueries.RemoveOddIndexes<string>(src);

            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected,actual));
        }
        [TestMethod]
        public void ShouldTestRemoveEvenIdxes()
        {            
           //Arrange
            IEnumerable<string> src = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };
            IEnumerable<string> expected = new string[] { "linha 1", "linha 3" };

            //Act
            IEnumerable<string> actual = LazyQueries.RemoveEvenIndexes<string>(src);

            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));
        }
        [TestMethod]
        public void ShouldTestSkip()
        {
            //Arrange
            IEnumerable<string> src = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };
            IEnumerable<string> expected = new string[] {"linha 3" };

            //Act
            IEnumerable<string> actual = LazyQueries.Skip<string>(src,3);

            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));
        }
        [TestMethod]
        public void ShouldTestFilter()
        {
            //Arrange
            IEnumerable<string> src = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };
            IEnumerable<string> expected = new string[] { "linha 3" };

            //Act
            IEnumerable<string> actual = LazyQueries.Filter<string>(src, line => line.Contains("3"));

            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));
        }
        [TestMethod]
        public void ShouldTestSplitString()
        {           
            //Arrange
            string src = "linha 0\nlinha 1\nlinha 2\nlinha 3\n";
            IEnumerable<string> expected = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };

            //Act
            IEnumerable<string> actual = LazyQueries.Split(src, '\n');

            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));
        }
        [TestMethod]
        public void ShouldTestSplitWithMultipleSeps()
        {
            //Arrange
            string src = @"2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56
2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56
2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54
2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55";
            IEnumerable<string> expected = new string[]{
            "2019-01-01,24,17,63,6,10,74,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1031,43,14,57,6,43,13,56,11,17,13,56",
            "2019-01-02,24,18,64,6,9,179,S,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,57,10,1030,15,14,57,6,42,13,56,11,17,13,56",
            "2019-01-03,24,16,60,7,11,89,E,113,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0001_sunny.png,Sunny,0.0,67,10,1026,3,13,55,7,45,12,54,11,18,12,54",
            "2019-01-04,24,16,60,9,15,78,ENE,116,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.1,73,10,1028,27,14,57,9,48,13,55,14,23,13,55"
            };
            //Act
            IEnumerable<string> actual = LazyQueries.Split(src, "\r\n", "\r", "\n");
            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));
        }

        [TestMethod]
        public void ShouldTestSplitWithMultipleSepsRemoveEmpties()
        {
            string src = "#The Search API\r\n#Data returned is laid out in the following order:-\r\n#AreaName    Country     Region(If available)    Latitude    Longitude   Population(if available)    Weather Forecast URL\r\n#\r\nOporto\tSpain\tGalicia\t42.383\t-7.100\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=42.3833,-7.1\nOporto\tPortugal\tPorto\t41.150\t-8.617\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=41.15,-8.6167\nOporto\tSouth Africa\tLimpopo\t-22.667\t29.633\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-22.6667,29.6333\nEl Oporto\tMexico\tTamaulipas\t23.266\t-98.768\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=23.2658,-98.7675\nPuerto Oporto\tBolivia\tPando\t-9.933\t-66.417\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-9.9333,-66.4167\nOporto\tCuba\tSantiago de Cuba\t20.233\t-76.167\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=20.2333,-76.1667\n";
            
            IEnumerable<string> expected = new string [] {"Oporto\tSpain\tGalicia\t42.383\t-7.100\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=42.3833,-7.1",
                "Oporto\tPortugal\tPorto\t41.150\t-8.617\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=41.15,-8.6167",
                "Oporto\tSouth Africa\tLimpopo\t-22.667\t29.633\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-22.6667,29.6333",
                "El Oporto\tMexico\tTamaulipas\t23.266\t-98.768\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=23.2658,-98.7675",
                "Puerto Oporto\tBolivia\tPando\t-9.933\t-66.417\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-9.9333,-66.4167",
                "Oporto\tCuba\tSantiago de Cuba\t20.233\t-76.167\t0\thttp://api-cdn.worldweatheronline.com/v2/weather.aspx?q=20.2333,-76.1667" };
            //Act
            IEnumerable<string> intermediate = LazyQueries.Split(src, "\r\n", "\r", "\n");
            IEnumerable<string> actual = LazyQueries.Filter<string>(LazyQueries.Filter<string>(intermediate, line => !line.Equals("")),line=>!line.StartsWith("#"));
            //Assert
            Assert.IsTrue(AreIEnumerableEqual(expected, actual));


        }

        [TestMethod]
        public void ShouldTestCount()
        {
            //Arrange
            IEnumerable<string> src = new string[] { "linha 0", "linha 1", "linha 2", "linha 3" };
            int expected = 4;

            //Act
            int actual = LazyQueries.Count<string>(src);

            //Assert
            Assert.AreEqual(expected, actual);
        }
        private bool AreIEnumerableEqual(IEnumerable<string> seq1, IEnumerable<string> seq2)
        {
            IEnumerator<String> iter1 = seq1.GetEnumerator();
            IEnumerator<String> iter2 = seq2.GetEnumerator();
            while (iter1.MoveNext() && iter2.MoveNext())
            {
                if (!iter1.Current.Equals(iter2.Current))
                    return false;
            }
            return !iter1.MoveNext() && !iter2.MoveNext();
        }
    }
}
