using Csvier;
using Request;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AppUsage
{
    class FlurryWebApi
    {
        const string FLURRY_TOKEN = "eyJhbGciOiJIUzI1NiIsImtpZCI6ImZsdXJyeS56dXVsLnByb2Qua2V5c3RvcmUua2V5LjIifQ.eyJpc3MiOiJodHRwczovL3p1dWwuZmx1cnJ5LmNvbTo0NDMvdG9rZW4iLCJpYXQiOjE1NTMxOTgwNzcsImV4cCI6MzMxMTAxMDY4NzcsInN1YiI6IjQ0MjM5MiIsImF1ZCI6IjQiLCJ0eXBlIjo0LCJqdGkiOiI4NTM1In0.iS3aA0c-2Rozhe-0Yk4BJWDziyfJd8lKsjlq6FF5yfE";
        const string FLURRY_HOST = "https://api-metrics.flurry.com/public/v1/data/";
        const string PATH_APP_USAGE = FLURRY_HOST + "appUsage/{0}?metrics={1}&dateTime={2}/{3}&format=csv&token=" + FLURRY_TOKEN;
        readonly CsvParser appUsage;
        readonly IHttpRequest req;

        public FlurryWebApi() : this(new HttpRequest())
        {
        }

        public FlurryWebApi(IHttpRequest req)
        {
            this.req = req;
            appUsage = new CsvParser(typeof(AppUsageData));
        }
        public void Dispose()
        {
            req.Dispose();
        }

        public AppUsageData[] PastAppUsage(string timeGrain, string table, string[] metric, DateTime from, DateTime to)
        {
            string dateRange = String.Format(from.ToString("dd-MM-yyyy"), "/", to.ToString("dd-MM-yyyy"));
            String url = String.Format(PATH_APP_USAGE, timeGrain, metric, dateRange, FLURRY_TOKEN);

            string body = req.GetBody(url);

            //Configuration
            appUsage
                .Load(body)
                .CtorArg("xxx", 0);
            //Utilization
            object[] items = appUsage
                    .Load(body)
                    .RemoveWith("#")
                    .Remove(1)
                    .RemoveEvenIndexes()
                    .Parse();

            return (AppUsageData[])items;
        }
    }
}
