using Csvier;
using Request;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Clima
{
    public sealed class WeatherWebApi : IWeatherWebApi
    {
        const string WEATHER_KEY = "ccb0b4929d574c7cb5f183401191305";
        const string WEATHER_HOST = "http://api.worldweatheronline.com/premium/v1/";
        const string PATH_WEATHER = WEATHER_HOST + "past-weather.ashx?q={0},{1}&date={2}&enddate={3}&tp=24&format=csv&key=" + WEATHER_KEY;
        const string SEARCH = WEATHER_HOST + "search.ashx?query={0}&format=tab&key=" + WEATHER_KEY;
        readonly CsvParser<WeatherInfo> pastWeather;
        readonly CsvParser<LocationInfo> locations;
        readonly IHttpRequest req;

        public WeatherWebApi() : this(new HttpRequest())
        {
        }
        
        public WeatherWebApi(IHttpRequest req)
        {
            this.req = req;
            pastWeather = new CsvParser<WeatherInfo>();
            locations = new CsvParser<LocationInfo>('\t');
        }

        public void Dispose()
        {
            req.Dispose();
        }

        public IEnumerable<WeatherInfo> PastWeather(double lat, double log, DateTime from, DateTime to)
        {
            string latStr = lat.ToString("0.000", CultureInfo.InvariantCulture);
            string logStr = log.ToString("0.000", CultureInfo.InvariantCulture);
            string fromStr = from.ToString("dd-MM-yyyy");
            string toStr = to.ToString("dd-MM-yyyy");

            String url = String.Format(PATH_WEATHER, latStr, logStr, fromStr, toStr, WEATHER_KEY);

            string body = req.GetBody(url);
            pastWeather
                .CtorArg("date", 0)
                .CtorArg("tempC", 2)
                .PropArg("PrecipMM", 11)
                .PropArg("Desc", 10);

            IEnumerable<WeatherInfo> items = pastWeather
                    .Load(body)
                    .RemoveWith("#")
                    .Remove(1)
                    .RemoveEvenIndexes()
                    .Parse();

            return items;
        }

        public IEnumerable<LocationInfo> Search(string query) {
            string search = string.Format(SEARCH, query);

            string body = req.GetBody(search);
            locations
                .CtorArg("country", 1)
                .CtorArg("region", 2)
                .CtorArg("latitude", 3)
                .CtorArg("longitude", 4);
            IEnumerable<LocationInfo> items = locations
                    .Load(body)
                    .RemoveWith("#")
                    .RemoveEmpties()
                    .Parse();
            return items;
        }
    }
}
