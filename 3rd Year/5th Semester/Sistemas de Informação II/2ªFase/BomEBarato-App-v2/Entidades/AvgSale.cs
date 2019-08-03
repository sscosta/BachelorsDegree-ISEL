using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.Entidades
{
    public class AvgSale
    {
        public int ProdId { get; set; }
        public double Avg { get; set; }
        public AvgSale(int prodId, double avg)
        {
            this.ProdId = prodId;
            this.Avg = avg;
        }

        public AvgSale()
        {

        }

        public static AvgSale Parse(SqlDataReader dr)
        {
            return new AvgSale(
                dr[0] is DBNull ? 0 : Convert.ToInt32(dr[0]),
                dr[1] is DBNull ? 0 : Convert.ToDouble(dr[1])
                );
        }

    }
}
