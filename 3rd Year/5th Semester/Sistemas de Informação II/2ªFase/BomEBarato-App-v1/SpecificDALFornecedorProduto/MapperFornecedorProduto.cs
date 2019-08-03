using BomEBarato.DALFornecedorInterfaces;
using BomEBarato.DALFornecedorProdutoInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALFornecedorProduto
{
    public class MapperFornecedorProduto : IMapperFornecedorProduto
    {
        private ISessionFornecedorProduto MySession;

        public MapperFornecedorProduto(ISessionFornecedorProduto s)
        {
            this.MySession = s;
        }
        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }

        public void Create(FornecedorProduto entity)
        {
            using(var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into FornecedorProduto (forn_id,prod_id,dummy) values(@forn_id,@prod_id,1);";
                SqlParameter p1 = new SqlParameter("@forn_id", entity.FornId);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p2);
                cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public FornecedorProduto Read(int id, int id2)
        {
            FornecedorProduto fp;
            using(var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select top 1 @forn_id = forn_id, @prod_id = prod_id from FornecedorProduto where forn_id = @fid and prod_id = @pid;";
                SqlParameter p1 = cmd.Parameters.Add("@forn_id", SqlDbType.Int);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@prod_id", SqlDbType.Int);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = new SqlParameter("@fid", id);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@pid", id2);
                cmd.Parameters.Add(p4);

                cmd.ExecuteNonQuery();
                if (p1.Value is System.DBNull)
                    throw new Exception("Não existe Produto com franqId " + id + " e prodId " + id2);

                fp = new FornecedorProduto(Convert.ToInt32(p1.Value), Convert.ToInt32(p2.Value), true);
            }
            return fp;
        }

        public void Update(FornecedorProduto entity)
        {
            throw new InvalidOperationException();
        }
        public void Delete(FornecedorProduto entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from FornecedorProduto where forn_id = @forn_id and prod_id = @prod_id;";
            SqlParameter p1 = new SqlParameter("@forn_id", entity.FornId);
            cmd.Parameters.Add(p1); ;
            SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
            cmd.Parameters.Add(p2);

            cmd.ExecuteNonQuery();
        }

        public void DeleteAllWithProdId(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from FornecedorProduto where @prod_id = prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public IEnumerable<Fornecedor> GetAllFornecedoresForProduto(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "select forn_id,nif,nome from FornecedorProduto inner join Fornecedor on FornecedorProduto.forn_id = Fornecedor.id where prod_id=@prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            SqlDataReader dr = cmd.ExecuteReader();

            while (dr.Read())
            {
                yield return Fornecedor.Parse(dr);
            }
            dr.Close();
        }
    }
}
