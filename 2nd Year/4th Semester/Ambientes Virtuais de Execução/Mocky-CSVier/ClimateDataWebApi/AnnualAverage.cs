namespace WorldBankWeather
{
    public class AnnualAverage : BaseClimateAverage
    {

        public double Average { get; set; }

        public AnnualAverage(string gcm, string measure, double average) : base(gcm, measure)
        {
            this.Average = average;
        }
    }
}