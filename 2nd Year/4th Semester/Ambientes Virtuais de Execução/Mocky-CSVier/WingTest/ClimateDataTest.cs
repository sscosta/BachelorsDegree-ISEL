using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using WorldBankWeather;
using Mocky;

namespace Csvier.Test
{
    [TestClass]
    public class WorldBankClimateDataTest
    {
        [TestMethod]
        public void ShouldReturnMaxAverageTemp()
        {
            using (ClimateDataWebApi api = new ClimateDataWebApi())
            {
                //Arrange
                IEnumerable<AnnualAverage> avgs = api.PastAnnualAverage("tas", "1980", "1999", "AFG");

                //Act
                double max = int.MinValue;
                foreach (AnnualAverage aa in avgs)
                {
                    if (aa.Average > max) max = aa.Average;
                }
                //Assert
                Assert.AreEqual(14.053876749678569, max);
            }
        }
        [TestMethod]
        public void ShouldReturnMaxAveragePrecip()
        {
            using (ClimateDataWebApi api = new ClimateDataWebApi())
            {
                //Arrange
                IEnumerable<AnnualAverage> avgs = api.PastAnnualAverage("pr", "1980", "1999", "AFG");

                //Act
                double max = int.MinValue;
                foreach (AnnualAverage aa in avgs)
                {
                    if (aa.Average > max) max = aa.Average;
                }
                //Assert
                Assert.AreEqual(637.92411015763332, max);
            }
        }
        [TestMethod]
        public void ShouldGetPastMontlyAverageAndRet()
        {
            using (ClimateDataWebApi api = new ClimateDataWebApi())
            {
                //Arrange
                IEnumerable<MonthlyAverage> avgs = api.PastMonthlyAverage("tas", "1980", "1999", "PRT");
                //Act
                double max = int.MinValue;
                foreach (MonthlyAverage ma in avgs)
                {
                    double value = ma.GetMax();
                    if (value > max) max = value;
                }
                //Assert
                Assert.AreEqual(25.00979396, max);

            }

        }
    }
}
