using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WorldBankWeather
{
    public class BaseClimateAverage
    {
        public string Gcm { get; set; }
        public string Measure { get; set; }
        public string Beginning { get; set; }
        public string End { get; set; }

        public BaseClimateAverage(string gcm, string measure)
        {
            this.Gcm = gcm;
            this.Measure = measure;
        }

    }
}
