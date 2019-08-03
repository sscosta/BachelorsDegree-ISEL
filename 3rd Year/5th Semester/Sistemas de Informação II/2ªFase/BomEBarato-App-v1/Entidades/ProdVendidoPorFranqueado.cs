using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Text;

namespace BomEBarato.Entidades
{
    public class ProdVendidoPorFranqueado
    {
        public int FranqId { get; private set; }
        public int ProdId { get; private set; }
        public decimal PrecoUnitario { get; private set; }
        public int StockTotal { get; private set; }
        public int StockMin { get; private set; }
        public int StockMax { get; private set; }
        public DateTime DataUltimaVenda { get; private set; }
        public long QtdVendas { get; set; }

        public ProdVendidoPorFranqueado(int franqId, int prodId, decimal precoUnitario, int stockTotal, int stockMin, int stockMax, DateTime dataUltimaVenda, long qtdVendas)
        {
            this.FranqId = franqId;
            this.ProdId = prodId;
            this.PrecoUnitario = precoUnitario;
            this.StockTotal = stockTotal;
            this.StockMin = stockMin;
            this.StockMax = stockMax;
            this.DataUltimaVenda = dataUltimaVenda;
            this.QtdVendas = qtdVendas;
        }

        public override string ToString()
        {
            return "FranqId = " + FranqId + ", " +
                    "ProdId = " + ProdId + ", " +
                    "Preço Unitario = " + PrecoUnitario + ", " +
                    "Stock Total = " + StockTotal + ", " +
                    "Stock Min = " + StockMin + ", " +
                    "Stock Max = " + StockMax + ", " +
                    "Data Ultima Venda = " + DataUltimaVenda.ToString("MMMM dd, yyyy") + ", " +
                    "Qtd Vendas = " + QtdVendas;
        }

        public static ProdVendidoPorFranqueado Parse(SqlDataReader dr)
        {
            return new ProdVendidoPorFranqueado(
                    dr[0] is DBNull ? 0 : Convert.ToInt32(dr[0]),
                    dr[1] is DBNull ? 0 : Convert.ToInt32(dr[1]),
                    dr[2] is DBNull ? 0 : Convert.ToDecimal(dr[2]),
                    dr[3] is DBNull ? 0 : Convert.ToInt32(dr[3]),
                    dr[4] is DBNull ? 0 : Convert.ToInt32(dr[4]),
                    dr[5] is DBNull ? 0 : Convert.ToInt32(dr[5]),
                    dr[6] is DBNull ? DateTime.MinValue : Convert.ToDateTime(dr[6]),
                    dr[7] is DBNull ? 0 : Convert.ToInt64(dr[7]));
        }
    }
}
