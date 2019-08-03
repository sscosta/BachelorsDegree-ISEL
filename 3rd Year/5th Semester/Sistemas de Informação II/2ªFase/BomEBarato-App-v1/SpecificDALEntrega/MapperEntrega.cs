using BomEBarato.DALEntregaInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALEntrega
{
    public class MapperEntrega : IMapperEntrega
    {
        private ISessionEntrega MySession;

        public MapperEntrega(ISessionEntrega s)
        {

            MySession = s;

        }
        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }

        public void Create(Entrega entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into Entrega (franq_id,prod_id,valor_ped,valor_forn) values(@franq_id,@prod_id,@valor_ped,@valor_forn);";
                SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@valor_ped", entity.ValorPed);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@valor_forn", entity.ValorForn);
                cmd.Parameters.Add(p4);

                int nRows = cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public Entrega Read(int franqId, int prodId)
        {
            Entrega e;
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select Top 1 @franq_id = franq_id, @prod_id = prod_id, @valor_ped = valor_ped, @valor_forn = valor_forn from Entrega where franq_id = @fid and prod_id = @pid ;";
                SqlParameter p1 = cmd.Parameters.Add("@franq_id", SqlDbType.Int);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@prod_id", SqlDbType.Int);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = cmd.Parameters.Add("@valor_ped", SqlDbType.Int);
                p3.Direction = ParameterDirection.Output;
                SqlParameter p4 = cmd.Parameters.Add("@valor_forn", SqlDbType.Int);
                p4.Direction = ParameterDirection.Output;

                SqlParameter p5 = new SqlParameter("@fid", franqId);
                cmd.Parameters.Add(p5);
                SqlParameter p6 = new SqlParameter("@pid", prodId);
                cmd.Parameters.Add(p6);

                cmd.ExecuteNonQuery();
                if (p1.Value is System.DBNull)
                    throw new Exception("Não existe Entrega com franqId " + franqId + " e prodId " + prodId);

                e = new Entrega(Convert.ToInt32(p1.Value), Convert.ToInt32(p2.Value), p3.Value is DBNull ? 0:Convert.ToInt32(p3.Value), p4.Value is DBNull ? 0: Convert.ToInt32(p4.Value));
            }
            return e;
        }

        public void Update(Entrega entity)
        {
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Update Entrega set valor_ped = @valor_ped, valor_forn = @valor_forn where franq_id = @fid and prod_id = @pid;";
                SqlParameter p1 = new SqlParameter("@valor_ped", entity.ValorPed);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@valor_forn", entity.ValorForn);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@fid", entity.FranqId);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@pid", entity.ProdId);
                cmd.Parameters.Add(p4);
                int nRows = cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public void Delete(Entrega entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from Entrega where franq_id = @franq_id and prod_id = @prod_id and valor_forn = @valor_forn and valor_ped = @valor_ped;";
            SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
            cmd.Parameters.Add(p1);
            SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
            cmd.Parameters.Add(p2);
            SqlParameter p3 = new SqlParameter("@valor_forn", entity.ValorForn);
            cmd.Parameters.Add(p3);
            SqlParameter p4 = new SqlParameter("@valor_ped", entity.ValorPed);
            cmd.Parameters.Add(p4);

            int nRows = cmd.ExecuteNonQuery();
        }

        public void OrderOutOfStock(IEnumerable<Entrega> productsOutOfStock)
        { 
            foreach(Entrega e in productsOutOfStock)
            {
                OrderSingle(e);
            }
        }

        private void OrderSingle(Entrega e)
        {
            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmdCnt = CreateCommand();
                cmdCnt.CommandText = "select count(*) from Entrega where franq_id = @franq_id and prod_id = @prod_id;";
                cmdCnt.Parameters.AddWithValue("@franq_id", e.FranqId);
                cmdCnt.Parameters.AddWithValue("@prod_id", e.ProdId);

                int count = (int)cmdCnt.ExecuteScalar();

                SqlCommand cmd = CreateCommand();

                if (count > 0)
                {
                    cmd.CommandText = "Update Entrega set valor_ped = valor_ped + @valor_ped where franq_id = @franq_id and prod_id = @prod_id;";
                }
                else
                {
                    cmd.CommandText = "Insert into Entrega (valor_ped,franq_id,prod_id) values(@valor_ped,@franq_id,@prod_id);";
                }
                cmd.Parameters.AddWithValue("@valor_ped", e.ValorPed);
                cmd.Parameters.AddWithValue("@franq_id", e.FranqId);
                cmd.Parameters.AddWithValue("@prod_id", e.ProdId);
                int rowsUpdated = cmd.ExecuteNonQuery();
                das.Commit();
            }
        }

        public void DeleteAllWithProdId(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from Entrega where prod_id = @prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            int nRows = cmd.ExecuteNonQuery();
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from Entrega where franq_id = @franq_id;";
            SqlParameter p1 = new SqlParameter("@franq_id", franqId);
            cmd.Parameters.Add(p1);

            int nRows = cmd.ExecuteNonQuery();
        }
    }
}
