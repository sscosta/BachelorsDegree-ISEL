using System;
using System.Data.SqlClient;

namespace BomEBarato.Entidades
{
    public class Franqueado
    {
        public int Id { get; set; }
        public int Nif { get; set; }
        public string Nome { get; set; }
        public string Morada { get; set; }

        public Franqueado(int id, int nif, string nome, string morada)
        {
            this.Id = id;
            this.Nif = nif;
            this.Nome = nome;
            this.Morada = morada;
        }

        public Franqueado()
        {
        }

        public static Franqueado Parse(SqlDataReader dr)
        {
            return new Franqueado(
                Convert.ToInt32(dr[0]),
                Convert.ToInt32(dr[1]),
                Convert.ToString(dr[2]),
                Convert.ToString(dr[3]));
        }
    }
}
