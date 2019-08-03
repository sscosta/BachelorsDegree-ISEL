using Clima;
using System;
using System.Collections.Generic;

namespace Clima
{
    public interface IWeatherWebApi : IDisposable
    {
        IEnumerable<LocationInfo> Search(String location);
        IEnumerable<WeatherInfo> PastWeather(double lat, double log, DateTime from, DateTime to);
    }
}