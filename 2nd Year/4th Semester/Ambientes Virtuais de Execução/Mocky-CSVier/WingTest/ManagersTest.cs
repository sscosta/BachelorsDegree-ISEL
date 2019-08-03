using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Clima;

namespace Csvier.Test
{
    [TestClass]
    public class ManagersTest
    {
        
        [TestMethod]
        public void TestMethod1()
        {
            //Arrange
            string body = @"Oporto	Spain	Galicia	42.383	-7.100	0	http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=42.3833,-7.1
Oporto Portugal    Porto   41.150 - 8.617  0   http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=41.15,-8.6167
            Oporto South Africa Limpopo -22.667 29.633  0   http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-22.6667,29.6333
            El Oporto   Mexico Tamaulipas  23.266 - 98.768 0   http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=23.2658,-98.7675
            Puerto Oporto   Bolivia Pando   -9.933 - 66.417 0   http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=-9.9333,-66.4167
            Oporto Cuba    Santiago de Cuba    20.233 - 76.167 0   http://api-cdn.worldweatheronline.com/v2/weather.aspx?q=20.2333,-76.1667";
            CtorManager<LocationInfo> ctorManager = new Csvier.CtorManager<LocationInfo>();
            ctorManager.AddArg("country",1);
            ctorManager.AddArg("region",2);
            ctorManager.AddArg("latitude",3);
            ctorManager.AddArg("longitude",4);
            
            //Act
            //object o = (LocationInfo) ctorManager.InvokeCtor(new CsvParser(typeof(LocationInfo)).Load(body));
        }
    }
}
