using BomEBarato.DALTipoProdutoInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALTipoProduto
{
    class MapperTipoProduto : IMapperTipoProduto
    {
        private ISessionTipoProduto MySession;

        public MapperTipoProduto(ISessionTipoProduto MySession)
        {
            this.MySession = MySession;
        }

        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }
        public void Create(TipoProduto entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into TipoProduto values(@Descricao);";
                SqlParameter p1 = new SqlParameter("@Descricao", entity.Descrição);
                cmd.Parameters.Add(p1);

                cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public TipoProduto Read(string id)
        {
            TipoProduto t;
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Select TOP 1 @desc = descricao from TipoProduto where @id = descricao;";
                SqlParameter p1 = cmd.Parameters.Add("@desc", SqlDbType.VarChar, 20);
                p1.Direction = ParameterDirection.Output;

                SqlParameter p2 = new SqlParameter("@id", id);
                cmd.Parameters.Add(p2);

                cmd.ExecuteNonQuery();
                
                if (p1.Value is System.DBNull)
                    throw new Exception("Não existe Produto com id " + id);

                t = new TipoProduto((string)p1.Value);
                das.Commit();
            }
            return t;
        }

        public void Update(TipoProduto entity)
        {
            throw new NotImplementedException();
        }

        public void Delete(TipoProduto entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from TipoProduto WHERE descricao=@Desc;";
            SqlParameter p1 = new SqlParameter("@Desc", entity.Descrição);
            cmd.Parameters.Add(p1);
            cmd.ExecuteNonQuery();

        }

        public IEnumerable<TipoProduto> ReadAll()
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Select * from TipoProduto;";
            SqlDataReader dr = cmd.ExecuteReader();
            while (dr.Read())
            {
                yield return new TipoProduto(dr[0].ToString());
            }
            dr.Close();
        }
    }
}
