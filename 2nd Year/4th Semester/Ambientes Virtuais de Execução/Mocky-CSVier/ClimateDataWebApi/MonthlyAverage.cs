using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WorldBankWeather
{
    public class MonthlyAverage : BaseClimateAverage
    {
        double[] Monthly { get; set; }
        public double max;
            
        public MonthlyAverage(string gcm,string measure, double[] monthly) : base(gcm, measure)
        {
            this.Monthly = monthly;
        }

        public double GetMax()
        {
            if (this.max != 0)
            {
                return this.max;
            }
            double max = double.MinValue;
            foreach (double m in Monthly)
            {
                if (m > max)
                    max = m;
            }
            return max;
        }
    }
}
