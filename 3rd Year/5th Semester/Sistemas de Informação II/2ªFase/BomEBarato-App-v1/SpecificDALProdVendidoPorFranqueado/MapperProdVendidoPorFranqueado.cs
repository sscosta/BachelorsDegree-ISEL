using BomEBarato.DALProdVendidoProFranqueadoInterfaces;
using BomEBarato.Dto;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALProdVendidoPorFranqueado
{
    public class MapperProdVendidoPorFranqueado : IMapperProdVendidoPorFranqueado
    {
        private ISessionProdVendidoPorFranqueado MySession;

        public MapperProdVendidoPorFranqueado(ISessionProdVendidoPorFranqueado s)
        {
            this.MySession = s;
        }
        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }

        public void Create(ProdVendidoPorFranqueado entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into ProdVendidoPorFranqueado (franq_id,prod_id,preco_unitario,stock_total,stock_min,stock_max,dt_ultima_venda,qtd_vendas) values(@franq_id, @prod_id, @preco_unitario, @stock_total, @stock_min,@stock_max,@dt_ultima_venda, @qtd_vendas);";
                SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@preco_unitario", entity.PrecoUnitario);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@stock_total", entity.StockTotal);
                cmd.Parameters.Add(p4);
                SqlParameter p5 = new SqlParameter("@stock_min", entity.StockMin);
                cmd.Parameters.Add(p5);
                SqlParameter p6 = new SqlParameter("@stock_max", entity.StockMax);
                cmd.Parameters.Add(p6);
                SqlParameter p7 = new SqlParameter("@dt_ultima_venda", entity.DataUltimaVenda);
                cmd.Parameters.Add(p7);
                SqlParameter p8 = new SqlParameter("@qtd_vendas", entity.QtdVendas);
                cmd.Parameters.Add(p8);

                try {
                    cmd.ExecuteNonQuery();
                    Console.WriteLine($"Product {entity.ProdId} added for sale on Franchisee {entity.FranqId} successfully!");
                    das.Commit();
                } catch(SqlException) {
                    Console.WriteLine("An error occurred adding this Product for sale...");
                }
            }
        }

        public ProdVendidoPorFranqueado Read(int id, int id2)
        {
            ProdVendidoPorFranqueado pvpf;
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select top 1 @franq_id = franq_id, @prod_id = prod_id , @preco_unitario = preco_unitario, @stock_total = stock_total, @stock_min = stock_min, @stock_max = stock_max, @dt_ultima_venda = dt_ultima_venda, @qtd_vendas = qtd_vendas from ProdVendidoPorFranqueado where franq_id = @fid and prod_id = @pid;";
                SqlParameter p1 = cmd.Parameters.Add("@franq_id", SqlDbType.Int);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@prod_id", SqlDbType.Int);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = cmd.Parameters.Add("@preco_unitario", SqlDbType.Money);
                p3.Direction = ParameterDirection.Output;
                SqlParameter p4 = cmd.Parameters.Add("@stock_total", SqlDbType.Int);
                p4.Direction = ParameterDirection.Output;
                SqlParameter p5 = cmd.Parameters.Add("@stock_min", SqlDbType.Int);
                p5.Direction = ParameterDirection.Output;
                SqlParameter p6 = cmd.Parameters.Add("@stock_max", SqlDbType.Int);
                p6.Direction = ParameterDirection.Output;
                SqlParameter p7 = cmd.Parameters.Add("@dt_ultima_venda", SqlDbType.SmallDateTime);
                p7.Direction = ParameterDirection.Output;
                SqlParameter p8 = cmd.Parameters.Add("@qtd_vendas", SqlDbType.Int);
                p8.Direction = ParameterDirection.Output;
                SqlParameter p9 = new SqlParameter("@fid", id);
                cmd.Parameters.Add(p9);
                SqlParameter p10 = new SqlParameter("@pid", id2);
                cmd.Parameters.Add(p10);

                cmd.ExecuteNonQuery();
                if (p1.Value is DBNull)
                    throw new Exception("Não há Produto" + id2 + " em venda no franqueado " + id);

                pvpf = new ProdVendidoPorFranqueado(
                    Convert.ToInt32(p1.Value),
                    Convert.ToInt32(p2.Value),
                    Convert.ToDecimal(p3.Value),
                    Convert.ToInt32(p4.Value),
                    Convert.ToInt32(p5.Value),
                    Convert.ToInt32(p6.Value),
                    Convert.ToDateTime(p7.Value),
                    Convert.ToInt64(p8.Value));
            }
            return pvpf;
        }

        public void Update(ProdVendidoPorFranqueado entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "UPDATE ProdVendidoPorFranqueado set preco_unitario = @preco_unitario, stock_total = @stock_total, stock_min = @stock_min, stock_max = @stock_max, dt_ultima_venda = @dt_ultima_venda, qtd_vendas = @qtd_vendas where franq_id = @franq_id and prod_id = @prod_id;";
                SqlParameter p1 = new SqlParameter("@preco_unitario", entity.PrecoUnitario);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@stock_total", entity.StockTotal);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@stock_min", entity.StockMin);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@stock_max", entity.StockMax);
                cmd.Parameters.Add(p4);
                SqlParameter p5 = new SqlParameter("@dt_ultima_venda", entity.DataUltimaVenda);
                cmd.Parameters.Add(p5);
                SqlParameter p6 = new SqlParameter("@qtd_vendas", entity.QtdVendas);
                cmd.Parameters.Add(p6);
                SqlParameter p7 = new SqlParameter("@franq_id", entity.FranqId);
                cmd.Parameters.Add(p7);
                SqlParameter p8 = new SqlParameter("@prod_id", entity.ProdId);
                cmd.Parameters.Add(p8);

                int nRows = cmd.ExecuteNonQuery();

                das.Commit();
            }
        }

        public void Delete(ProdVendidoPorFranqueado entity)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from ProdVendidoPorFranqueado where franq_id = @franq_id and prod_id = @prod_id;";
            SqlParameter p1 = new SqlParameter("@franq_id", entity.FranqId);
            cmd.Parameters.Add(p1);
            SqlParameter p2 = new SqlParameter("@prod_id", entity.ProdId);
            cmd.Parameters.Add(p2);

            cmd.ExecuteNonQuery();

        }
        
        public IEnumerable<ProdVendidoPorFranqueado> GetOutOfStock(double percentage, int franqId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "select * from ProdVendidoPorFranqueado where franq_id= @franq_id and stock_total < @percentagemRutura * stock_max;";
           
            SqlParameter p1 = new SqlParameter("@franq_id", franqId);
            cmd.Parameters.Add(p1);
            SqlParameter p2 = new SqlParameter("@percentagemRutura", percentage);
            cmd.Parameters.Add(p2);

            SqlDataReader dr = cmd.ExecuteReader();
            while (dr.Read())
            {
                yield return ProdVendidoPorFranqueado.Parse(dr);
            }
            dr.Close();
        }

        public IEnumerable<ProdVendidoPorFranqueado> GetAll()
        {
            using(var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select * from ProdVendidoPorFranqueado;";

                SqlDataReader dr = cmd.ExecuteReader();
                while (dr.Read())
                {
                    yield return ProdVendidoPorFranqueado.Parse(dr);
                }
                dr.Close();
                das.Commit();
            }
        }

        public void DeleteAllWithProdId(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from ProdVendidoPorFranqueado where prod_id = @prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public void DeleteAllWithFranqId(int franqId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from ProdVendidoPorFranqueado where franq_id = @franq_id;";
            SqlParameter p1 = new SqlParameter("@franq_id", franqId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public IEnumerable<TopSales> SalesByFranchiseeThisYear()
        {
            using(var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "select franq_id, sum(qtd_vendas * preco_unitario) as vendasAnoCorrente  from ProdVendidoPorFranqueado group by franq_id;";

                SqlDataReader dr = cmd.ExecuteReader();
                while (dr.Read())
                {
                    yield return new TopSales(
                        dr[0] is DBNull ? 0 : Convert.ToInt32(dr[0]),
                        dr[1] is DBNull ? 0 : Convert.ToDecimal(dr[1])
                        );
                }
                dr.Close();
            }
        }

        public IEnumerable<ProdVendidoPorFranqueado> GetAllInFranchisee(int franqId)
        {
            using(var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Select * from ProdVendidoPorFranqueado where franq_id = @franq_id;";
                SqlParameter p1 = new SqlParameter("@franq_id", franqId);
                cmd.Parameters.Add(p1);

                SqlDataReader dr = cmd.ExecuteReader();
                while (dr.Read())
                {
                    yield return ProdVendidoPorFranqueado.Parse(dr);
                }
                dr.Close();
            }
           
        }
        private void UpdateDueToSale(int franqId, int prodId, int quantidade)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "update ProdVendidoPorFranqueado set stock_total = stock_total - @quantidade  where franq_id = @franq_id and prod_id = @prod_id;";
                SqlParameter p1 = new SqlParameter("@quantidade", quantidade);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@franq_id", franqId);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@prod_id", prodId);
                cmd.Parameters.Add(p3);

                cmd.ExecuteNonQuery();
                das.Commit();

            }
        }

        public void UpdateInBulk(int franqId, List<ProdutoViewInStore> sale)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                foreach (ProdutoViewInStore p in sale)
                {
                    UpdateDueToSale(franqId, p.ProdId, p.Quantidade);
                }
                das.Commit();
            }
        }

        public IEnumerable<AvgSale> AvgSalesInPresentYear(int productId)
        {
            SqlCommand cmd = MySession.CreateCommand();
            cmd.CommandText = "select prod_id, avg(Cast(qtd_vendas as Float)) as Media from ProdVendidoPorFranqueado where prod_id = @prod_id group by prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            SqlDataReader dr = cmd.ExecuteReader();
            while (dr.Read())
            {
                yield return AvgSale.Parse(dr);
            }
            dr.Close();
        }
    }
}
