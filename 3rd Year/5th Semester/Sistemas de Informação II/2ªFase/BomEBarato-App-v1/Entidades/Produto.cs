using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Text;

namespace BomEBarato.Entidades
{
    public class Produto
    {
        public int Id { get; set; }
        public string Cod { get; set; }
        public string Tipo { get; set; }
        public string Descrição { get; set; }
        public int StockTotal { get; set; }
        public int StockMin { get; set; }
        public int StockMax { get; set; }
        Produto(int id,string cod, string tipo, string descrição, int stockTotal,int stockMin, int stockMax)
        {
            this.Id = id;
            this.Cod = cod;
            this.Tipo = tipo;
            this.Descrição = descrição;
            this.StockTotal = stockTotal;
            this.StockMin = stockMin;
            this.StockMax = stockMax;
        }
        public Produto()
        {

        }

        public static Produto Parse(SqlDataReader dr)
        {
            return new Produto(
                dr[0] is DBNull? 0 : Convert.ToInt32(dr[0]),
                dr[1] is DBNull? "" : Convert.ToString(dr[1]),
                dr[2] is DBNull? "" : Convert.ToString(dr[2]),
                dr[3] is DBNull? "" : Convert.ToString(dr[3]),
                dr[4] is DBNull ? 0 : Convert.ToInt32(dr[4]),
                dr[5] is DBNull ? 0 : Convert.ToInt32(dr[5]),
                dr[6] is DBNull ? 0 : Convert.ToInt32(dr[6])
                );
        }
    }
}
