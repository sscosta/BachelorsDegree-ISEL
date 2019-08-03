using Csvier;
using Request;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WorldBankWeather
{
    public class ClimateDataWebApi : IDisposable
    {
        const string CLIMATE_HOST = "http://climatedataapi.worldbank.org/climateweb/rest";
        const string PATH_ANNUAL_AVERAGE = CLIMATE_HOST + "/v1/country/annualavg/{0}/{1}/{2}/{3}.csv";
        const string PATH_MONTHLY_AVERAGE = CLIMATE_HOST + "/v1/country/mavg/{0}/{1}/{2}/{3}.csv";
        readonly CsvParser<AnnualAverage> annualAvg;
        readonly CsvParser<MonthlyAverage> monthlyAvg;
        readonly IHttpRequest req;

        public ClimateDataWebApi() : this(new HttpRequest())
        {
        }

        public ClimateDataWebApi(IHttpRequest req)
        {
            this.req = req;
            annualAvg = new CsvParser<AnnualAverage>();
            monthlyAvg = new CsvParser<MonthlyAverage>();
        }
        public void Dispose()
        {
            req.Dispose();
        }
        public IEnumerable<AnnualAverage> PastAnnualAverage(string var, string start, string end, string countryCode)
        {
            string url = GetAnnualAvgUrl(var, start, end, countryCode);

            string body = req.GetBody(url);

            annualAvg
                .CtorArg("gcm", 0)
                .CtorArg("measure", 1)
                .CtorArg("average", 4)
                .PropArg("Beginning", 2)
                .PropArg("End", 3);

            IEnumerable<AnnualAverage> items = annualAvg
                    .Load(body)
                    .Remove(1)
                    .RemoveEmpties()
                    .Parse();

            return items;
        }

        private string GetAnnualAvgUrl(string var, string start, string end, string countryCode)
        {
            return String.Format(PATH_ANNUAL_AVERAGE, var, start, end, countryCode);
        }
    
        public IEnumerable<MonthlyAverage> PastMonthlyAverage(string var, string start, string end, string countryCode)
        {
            string url = GetMonthlyAvgUrl(var,start,end,countryCode);

            string body = req.GetBody(url);

            monthlyAvg
                .CtorArg("gcm", 0)
                .CtorArg("measure", 1)
                .CtorArg("monthly", 4,15)
                .PropArg("Beginning", 2)
                .PropArg("End", 3);

            IEnumerable<MonthlyAverage> items = monthlyAvg
                    .Load(body)
                    .Remove(1)
                    .RemoveEmpties()
                    .Parse();

            return items;
        }

        private string GetMonthlyAvgUrl(string var, string start, string end, string countryCode)
        {
            return String.Format(PATH_MONTHLY_AVERAGE, var, start, end, countryCode);
        }
    }
}
