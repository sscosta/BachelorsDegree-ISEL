using BomEBarato.DALFornecedorInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFornecedor
{
    public class MapperFornecedor : IMapperFornecedor
    {
        private ISessionFornecedor MySession;

        public MapperFornecedor(ISessionFornecedor s)
        {

            MySession = s;

        }
        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }
        public void Create(Fornecedor entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "INSERT INTO Fornecedor (nif,nome) OUTPUT INSERTED.ID VALUES(@Nif,@Nome);";
                SqlParameter p1 = new SqlParameter("@Nif", entity.Nif);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Nome", entity.Nome);
                cmd.Parameters.Add(p2);

                entity.Id = Convert.ToInt32(cmd.ExecuteScalar());

                das.Commit(); // my vote is YES
            }

        }
        public Fornecedor Read(int id)
        {
            Fornecedor f = new Fornecedor();
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Select TOP 1 @Nome=Nome, @Nif = Nif from Fornecedor where ID = @Id;";
                SqlParameter p1 = cmd.Parameters.Add("@Nome", SqlDbType.VarChar, 100);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@Nif", SqlDbType.Decimal);
                p2.Direction = ParameterDirection.Output;
                p2.Precision = 10;
                p2.Scale = 0;
                SqlParameter p3 = new SqlParameter("@Id", id);
                cmd.Parameters.Add(p3);

                cmd.ExecuteNonQuery();
                if (p1.Value is System.DBNull)
                    throw new Exception("Não existe Fornecedor com id " + id);

                f.Id = Convert.ToInt32(p3.Value);
                f.Nome = (string)p1.Value;
                f.Nif = Convert.ToInt32(p2.Value);

                das.Commit();

            }
            return f;
        }
        public void Update(Fornecedor entity)
        {
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "UPDATE FORNECEDOR SET Nome=@Nome, Nif=@Nif WHERE Id=@Id;";
                SqlParameter p1 = new SqlParameter("@Nome", entity.Nome);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Nif", entity.Nif);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@Id", entity.Id);
                cmd.Parameters.Add(p3);

                int nRows = cmd.ExecuteNonQuery();

                das.Commit();
            }
        }
        public void Delete(Fornecedor entity)
        {
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Delete from FORNECEDOR WHERE @Nome=Nome and @Nif=Nif and @Id = Id";
                SqlParameter p1 = new SqlParameter("@Nome", entity.Nome);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Nif", entity.Nif);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@Id", entity.Id);
                cmd.Parameters.Add(p3);

                int nRows = cmd.ExecuteNonQuery();
                das.Commit();
            }
        }
    }
}
