using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Text;

namespace BomEBarato.Entidades
{
    public class Fornecedor
    {
        public int Id { get; set; }
        public long Nif { get; set; }
        public string Nome { get; set; }

        public Fornecedor(int id, long nif, string nome)
        {
            this.Id = id;
            this.Nif = nif;
            this.Nome = nome;
        }
        public Fornecedor()
        {

        }

        public static Fornecedor Parse(SqlDataReader dr)
        {
            return new Fornecedor(
                dr[0] is DBNull ? 0 : Convert.ToInt32(dr[0]),
                dr[1] is DBNull ? 0 : Convert.ToInt64(dr[1]),
                dr[2] is DBNull ? "": Convert.ToString(dr[2])
                );
        }
    }
}
