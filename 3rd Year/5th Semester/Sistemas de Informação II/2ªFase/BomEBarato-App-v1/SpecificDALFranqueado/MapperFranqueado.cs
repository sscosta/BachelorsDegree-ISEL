using BomEBarato.DALFranqueadoInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFranqueado
{
    public class MapperFranqueado : IMapperFranqueado
    {
        private ISessionFranqueado MySession;

        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }
        public MapperFranqueado(ISessionFranqueado s)
        {

            MySession = s;

        }
        public void Create(Franqueado entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "INSERT INTO FRANQUEADO (nif,nome,morada) OUTPUT INSERTED.ID VALUES(@Nif,@Nome,@Morada);";
                SqlParameter p1 = new SqlParameter("@Nif", entity.Nif);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Nome", entity.Nome);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@Morada", entity.Morada);
                cmd.Parameters.Add(p3);
                
                try
                {
                    entity.Id = Convert.ToInt32(cmd.ExecuteScalar());
                    Console.WriteLine("Franchisee inserted with id: {0}", entity.Id);
                }
                catch (SqlException e)
                {
                    Console.WriteLine("SQL Error inserting franchisee: " + e.GetBaseException().Message);
                }

                das.Commit(); // my vote is YES
            }
        }

        public Franqueado Read(int id)
        {
            Franqueado f = new Franqueado();
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "SELECT TOP 1 @Nome=Nome,@Morada = Morada, @Nif = Nif from Franqueado WHERE @Id=Id";
                SqlParameter p1 = cmd.Parameters.Add("@Nome", SqlDbType.VarChar, 100);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@Morada", SqlDbType.VarChar, 100);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = cmd.Parameters.Add("@Nif", SqlDbType.Decimal);
                p3.Direction = ParameterDirection.Output;
                p3.Precision = 10;
                p3.Scale = 0;

                SqlParameter p4 = new SqlParameter("@Id", id);
                cmd.Parameters.Add(p4);

                cmd.ExecuteNonQuery();


                if (p1.Value is System.DBNull)
                    Console.WriteLine("No franchisee found with id " + id);
                else {
                    f.Id = id;
                    f.Nome = (string)p1.Value;
                    f.Morada = (string)p2.Value;
                    f.Nif = Convert.ToInt32(p3.Value);

                    das.Commit();
                }
            }
            return f;
        }

        public void Update(Franqueado entity)
        {
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "UPDATE FRANQUEADO SET Nome=@Nome, Morada=@Morada, Nif=@Nif WHERE Id=@Id;";
                SqlParameter p1 = new SqlParameter("@Nome", entity.Nome);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Morada", entity.Morada);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@Nif", entity.Nif);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@Id", entity.Id);
                cmd.Parameters.Add(p4);

                int nRows = cmd.ExecuteNonQuery();

                das.Commit();

                if (nRows > 0)
                    Console.WriteLine("Franchisee {0} updated.", entity.Nif);
                else Console.WriteLine("Error updating franchisee {0}", entity.Nif);
            }
        }
        public void Delete(Franqueado entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from FRANQUEADO WHERE @Id = Id";
            SqlParameter p4 = new SqlParameter("@Id", entity.Id);
            cmd.Parameters.Add(p4);

            int nRows = cmd.ExecuteNonQuery();

            if (nRows > 0)
                Console.WriteLine("Francisee with ID {0} removed.", entity.Id);

        }

        public void Delete(int franqId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from FRANQUEADO WHERE @Id = Id";
            SqlParameter p1 = new SqlParameter("@Id", franqId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public IEnumerable<Franqueado> GetAll()
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select * from franqueado;";
                SqlDataReader dr = cmd.ExecuteReader();

                while (dr.Read())
                {
                    yield return Franqueado.Parse(dr);
                }
                dr.Close();
                das.Commit();
            }
        }
    }
}
