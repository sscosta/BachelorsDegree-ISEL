using ColumnSelector;
using Configurator;
using Queries;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Clima
{
    public class WeatherInfo
    {
        public DateTime Date { get;  }
        public int TempC { get; }
        [Configurator("PrecipMM", 11)]
        public double PrecipMM { get; set; }
        [Configurator("Desc",10)]
        public String Desc { get; set; }

        public WeatherInfo()
        {
        }
        [Configurator("Date", 0)]
        public WeatherInfo(DateTime date)
        {
            this.Date = date;
        }
        
        [Configurator("Date",0)]
        [Configurator("TempC",2)]
        [Selected]
        public WeatherInfo(DateTime date, int tempC)
        {
            this.Date = date;
            this.TempC = tempC;
        }

        public override String ToString()
        {
            return "WeatherInfo{" +
                "date=" + Date +
                ", tempC=" + TempC +
                ", precipMM=" + PrecipMM +
                ", desc='" + Desc + '\'' +
                '}';
        }

        public static WeatherInfo Parse(string line)
        {
            //IEnumerable<string> parsedLine = line.Split(',');
            IEnumerable<string> parsedLine = LazyQueries.Split(line,',');
            return new WeatherInfo(
                DateTime.Parse(parsedLine.ElementAt(0)),
                int.Parse(parsedLine.ElementAt(2)));
        }

    }
}
