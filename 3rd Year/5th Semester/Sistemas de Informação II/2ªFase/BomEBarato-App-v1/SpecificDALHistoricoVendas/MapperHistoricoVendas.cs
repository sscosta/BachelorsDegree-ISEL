using BomEBarato.DALHistoricoVendasInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALHistoricoVendas
{
    public class MapperHistoricoVendas : IMapperHistoricoVendas
    {
        private ISessionHistoricoVendas MySession;

        public MapperHistoricoVendas(ISessionHistoricoVendas s)
        {
            this.MySession = s;
        }
        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }
        public void Create(HistoricoVendas entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into HistoricoVendas (franq_id,prod_id,hist_3_anos) values(@franq_id,@prod_id,@hist_3_anos);";
                SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@hist_3_anos", entity.Hist3Anos);
                cmd.Parameters.Add(p3);

                cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public HistoricoVendas Read(int id, int id2)
        {
            HistoricoVendas hv;
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select top 1 @franq_id = franq_id, @prod_id = prod_id, @hist_3_anos = hist_3_anos from HistoricoVendas where franq_id = @fid and prod_id = @pid;";
                SqlParameter p1 = cmd.Parameters.Add("@franq_id", SqlDbType.Int);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@prod_id", SqlDbType.Int);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = cmd.Parameters.Add("@hist_3_anos", SqlDbType.Int);
                p3.Direction = ParameterDirection.Output;
                SqlParameter p4 = new SqlParameter("@fid", id);
                cmd.Parameters.Add(p4);
                SqlParameter p5 = new SqlParameter("@pid", id2);
                cmd.Parameters.Add(p5);

                cmd.ExecuteNonQuery();
                if (p1.Value is System.DBNull)
                    throw new Exception("Não existe Historico de Vendas com franqId " + id + " e prodId " + id2);

                hv = new HistoricoVendas(Convert.ToInt32(p1.Value), Convert.ToInt32(p2.Value), Convert.ToInt32(p3.Value));
            }
            return hv;
        }

        public void Update(HistoricoVendas entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "UPDATE HistoricoVendas set hist_3_anos = @hist_3_anos where franq_id = @franq_id and prod_id = @prod_id;";
                SqlParameter p1 = new SqlParameter("@hist_3_anos", entity.Hist3Anos);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@franq_id", entity.FranqId);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p3);

                int nRows = cmd.ExecuteNonQuery();
                das.Commit();
            }
        }

        public void Delete(HistoricoVendas entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from HistoricoVendas where franq_id = @franq_id and prod_id = @prod_id and hist_3_anos = @hist_3_anos;";
            SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
            cmd.Parameters.Add(p1);
            SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
            cmd.Parameters.Add(p2);
            SqlParameter p3 = new SqlParameter("@hist_3_anos", entity.Hist3Anos);
            cmd.Parameters.Add(p3);

            cmd.ExecuteNonQuery();
        }

        public void DeleteAllWithProdId(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from HistoricoVendas where @prod_id = prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "delete from HistoricoVendas where @franq_id = franq_id;";
            SqlParameter p1 = new SqlParameter("@franq_id", franqId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }
    }
}
