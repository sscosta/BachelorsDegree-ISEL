using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Request;

namespace Mocky.Test
{
    [TestClass]
    public class TestIHttpRequestForPartialRequest
    {
        readonly IHttpRequest req;
        readonly string pastWeatherForLisbonUrl = "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?q=38.716,-9.13&date=1/4/2019&enddate=12/4/2019&tp=24&format=csv&key=81586e5bb87e417894f94122191403";
        readonly string pastWeatherForLisbon = @"#The CSV format is in following way:-
#The day information is available in following format:-
#date,maxtempC,maxtempF,mintempC,mintempF,sunrise,sunset,moonrise,moonset,moon_phase,moon_illumination
#
#Hourly information follows below the day in the following way:-
#date,time,tempC,tempF,windspeedMiles,windspeedKmph,winddirdegree,winddir16point,weatherCode,weatherIconUrl,weatherDesc,precipMM,humidity,visibilityKm,pressureMB,cloudcover,HeatIndexC,HeatIndexF,DewPointC,DewPointF,WindChillC,WindChillF,WindGustMiles,WindGustKmph,FeelsLikeC,FeelsLikeF
#
Not Available
2019-04-01,21,69,13,55,07:22 AM,08:00 PM,05:45 AM,04:24 PM,Waning Crescent,9
2019-04-01,24,21,69,4,7,207, SSW,356, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0010_heavy_rain_showers.png,Moderate or heavy rain shower,16.1,67,9,1013,71,16,61,10,49,16,61,7,11,16,61
2019-04-02,21,69,12,53,07:21 AM,08:01 PM,06:16 AM,05:21 PM,Waning Crescent,1
2019-04-02,24,21,69,6,10,247, WSW,116, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,67,10,1017,9,16,60,9,48,15,60,8,12,15,60
2019-04-03,16,61,10,50,07:19 AM,08:02 PM,06:45 AM,06:18 PM,New Moon,0
2019-04-03,24,16,61,15,25,351, N,116, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,59,10,1013,2,13,55,5,40,11,52,20,33,11,52
2019-04-04,16,60,9,47,07:18 AM,08:02 PM,07:13 AM,07:17 PM,New Moon,0
2019-04-04,24,16,60,12,19,295, WNW,266, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0017_cloudy_with_light_rain.png,Light drizzle,1.8,60,9,1008,43,12,54,4,39,10,50,16,26,10,50
2019-04-05,14,57,9,49,07:16 AM,08:03 PM,07:41 AM,08:15 PM,New Moon,0
2019-04-05,24,14,57,15,24,292, WNW,356, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0010_heavy_rain_showers.png,Moderate or heavy rain shower,11.6,61,10,1005,78,12,53,4,39,9,48,22,36,9,48
2019-04-06,15,60,10,50,07:14 AM,08:04 PM,08:08 AM,09:16 PM,Waxing Crescent,6
2019-04-06,24,15,60,14,22,295, WNW,353, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0009_light_rain_showers.png,Light rain shower,2.1,62,10,1010,66,12,54,5,41,10,51,19,31,10,51
2019-04-07,18,64,11,52,07:13 AM,08:05 PM,08:38 AM,10:17 PM,Waxing Crescent,14
2019-04-07,24,18,64,11,18,241, WSW,353, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0009_light_rain_showers.png,Light rain shower,2.2,76,8,1013,82,14,57,10,49,13,55,16,25,13,55
2019-04-08,17,63,11,52,07:11 AM,08:06 PM,09:11 AM,11:20 PM,Waxing Crescent,21
2019-04-08,24,17,63,11,17,267, W,176, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0009_light_rain_showers.png,Patchy rain possible,1.0,67,9,1016,65,14,58,8,46,13,56,15,24,13,56
2019-04-09,17,63,12,53,07:10 AM,08:07 PM,09:49 AM,No moonset, Waxing Crescent,29
2019-04-09,24,17,63,13,21,296,WNW,176,http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0009_light_rain_showers.png,Patchy rain possible,1.4,65,10,1018,62,14,57,7,45,13,55,18,28,13,55
2019-04-10,18,64,11,53,07:08 AM,08:08 PM,10:31 AM,12:23 AM,Waxing Crescent,36
2019-04-10,24,18,64,10,17,328, NNW,353, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0009_light_rain_showers.png,Light rain shower,3.1,73,10,1021,60,14,57,9,48,13,55,15,23,13,55
2019-04-11,19,66,10,50,07:07 AM,08:09 PM,11:22 AM,01:26 AM,First Quarter,44
2019-04-11,24,19,66,9,15,335, NNW,116, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0002_sunny_intervals.png,Partly cloudy,0.0,66,19,1021,25,14,57,7,45,13,56,13,20,13,56
2019-04-12,20,69,10,50,07:05 AM,08:10 PM,12:19 PM,02:25 AM,First Quarter,52
2019-04-12,24,20,69,7,11,320, NW,119, http://cdn.worldweatheronline.net/images/wsymbols01_png_64/wsymbol_0003_white_cloud.png,Cloudy,0.0,61,20,1020,21,15,58,6,43,14,57,9,15,14,57";

        public TestIHttpRequestForPartialRequest()
        {
            Mocker mock = new Mocker(typeof(IHttpRequest));
            mock.When("GetBody").With(pastWeatherForLisbonUrl).Return(pastWeatherForLisbon);
            req = (IHttpRequest)mock.Create();
        }
        [TestMethod]
        public void TestRequestSuccessfully()
        {
            Assert.AreEqual(pastWeatherForLisbon, req.GetBody(pastWeatherForLisbonUrl) );
        }

        [TestMethod]
        [ExpectedException(typeof(NotImplementedException))]
        public void TestRequestFailing()
        {
            req.Dispose();
        }
    }
}
