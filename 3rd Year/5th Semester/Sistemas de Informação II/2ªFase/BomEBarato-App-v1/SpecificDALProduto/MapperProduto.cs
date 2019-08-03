using BomEBarato.DALProdutoInterfaces;
using BomEBarato.Entidades;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BomEBarato.SpecificDALProduto
{
    public class MapperProduto : IMapperProduto
    {
        private ISessionProduto MySession;

        public MapperProduto(ISessionProduto MySession)
        {
            this.MySession = MySession;
        }

        private SqlCommand CreateCommand()
        {
            return MySession.CreateCommand();
        }

        public void Create(Produto entity)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Insert into Produto (cod, tipo,descricao,stock_total,stock_min,stock_max) OUTPUT INSERTED.ID values (@Cod,@Tipo,@Descricao,@Stock_total, @Stock_min,@Stock_max)";
                SqlParameter p1 = new SqlParameter("@Cod", entity.Cod);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@Tipo", entity.Tipo);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@Descricao", entity.Descrição);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@Stock_total", entity.StockTotal);
                cmd.Parameters.Add(p4);
                SqlParameter p5 = new SqlParameter("@Stock_min", entity.StockMin);
                cmd.Parameters.Add(p5);
                SqlParameter p6 = new SqlParameter("@Stock_max", entity.StockMax);
                cmd.Parameters.Add(p6);

                try {
                    entity.Id = Convert.ToInt32(cmd.ExecuteScalar());
                    Console.WriteLine("Product inserted with id: {0}", entity.Id);
                } catch(SqlException e)
                {
                    Console.WriteLine("SQL Error inserting product: " + e.GetBaseException().Message);
                }
                das.Commit();
            }
        }

        public Produto Read(int id)
        {
            Produto p = new Produto();

            using (var das = MySession.CreateDataAccessScope(true))
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "Select TOP 1 @cod = cod, @tipo = tipo,@descricao = descricao,@stock_total = stock_total, @stock_min = stock_min, @stock_max = stock_max from Produto where id = @id;";
                SqlParameter p1 = cmd.Parameters.Add("@cod", SqlDbType.Char, 13);
                p1.Direction = ParameterDirection.Output;
                SqlParameter p2 = cmd.Parameters.Add("@tipo", SqlDbType.VarChar, 20);
                p2.Direction = ParameterDirection.Output;
                SqlParameter p3 = cmd.Parameters.Add("@descricao", SqlDbType.VarChar, 100);
                p3.Direction = ParameterDirection.Output;
                SqlParameter p4 = cmd.Parameters.Add("@stock_total", SqlDbType.Int);
                p4.Direction = ParameterDirection.Output;
                SqlParameter p5 = cmd.Parameters.Add("@stock_min", SqlDbType.Int);
                p5.Direction = ParameterDirection.Output;
                SqlParameter p6 = cmd.Parameters.Add("@stock_max", SqlDbType.Int);
                p6.Direction = ParameterDirection.Output;

                SqlParameter p7 = new SqlParameter("@Id", id);
                cmd.Parameters.Add(p7);
                
                cmd.ExecuteNonQuery();
                if (p1.Value is System.DBNull) {
                    Console.WriteLine("Não existe Produto com id " + id);
                }
                //throw new Exception("Não existe Produto com id " + id);
                else {
                    p.Id = Convert.ToInt32(p7.Value);
                    p.Cod = (string)p1.Value;
                    p.Tipo = (string)p2.Value;
                    p.Descrição = (string)p3.Value;
                    p.StockTotal = Convert.ToInt32(p4.Value);
                    p.StockMin = Convert.ToInt32(p5.Value);
                    p.StockMax = Convert.ToInt32(p6.Value);

                    das.Commit();
                }
            }
                return p;
        }

        public void Update(Produto entity)
        {

            using (var das = MySession.CreateDataAccessScope(true)) // Porquê true?
            {
                SqlCommand cmd = CreateCommand();
                cmd.CommandText = "UPDATE PRODUTO SET cod = @cod, tipo = @tipo, descricao = @descricao ,stock_total = @stock_total, stock_min = @stock_min, stock_max = @stock_max from produto where Id = @id;";
                SqlParameter p1 = new SqlParameter("@cod", entity.Cod);
                cmd.Parameters.Add(p1);
                SqlParameter p2 = new SqlParameter("@tipo", entity.Tipo);
                cmd.Parameters.Add(p2);
                SqlParameter p3 = new SqlParameter("@descricao", entity.Descrição);
                cmd.Parameters.Add(p3);
                SqlParameter p4 = new SqlParameter("@stock_total", entity.StockTotal);
                cmd.Parameters.Add(p4);
                SqlParameter p5 = new SqlParameter("@stock_min", entity.StockMin);
                cmd.Parameters.Add(p5);
                SqlParameter p6 = new SqlParameter("@stock_max", entity.StockMax);
                cmd.Parameters.Add(p6);
                SqlParameter p7 = new SqlParameter("@id", entity.Id);
                cmd.Parameters.Add(p7);

                // SQL Exception on CHECK constraint stockMax,stockMin
                int nRows=0;
                try {
                   nRows = cmd.ExecuteNonQuery();
                    das.Commit();
                } catch(Exception e)
                {
                    Console.WriteLine("SQL Error updating product: " + e.GetBaseException().Message);
                }


                if (nRows > 0)
                     Console.WriteLine("Product {0} updated.", entity.Cod);
                else Console.WriteLine("Error updating Product {0}", entity.Cod);
                
            }
        }

        public void Delete(Produto entity)
        {

            SqlCommand cmd = CreateCommand();
            //cmd.CommandText = "Delete from Produto WHERE cod=@Cod, tipo=@Tipo, descricao=@Descricao, stock_total=@Stock_total and stock_min=@Stock_min and stock_max=@Stock_max and id=@Id";
            cmd.CommandText = "Delete from Produto WHERE id=@Id";

            SqlParameter p7 = new SqlParameter("@Id", entity.Id);
            cmd.Parameters.Add(p7);

            int nRows=0;

            try {
                nRows = cmd.ExecuteNonQuery();
            } catch(SqlException e)
            {
                Console.WriteLine("SQL Error removing product: " + e.GetBaseException().Message);
            }

            if (nRows > 0)
                Console.WriteLine("Product with ID {0} removed.", entity.Id);


        }

        public void Delete(int productId)
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "Delete from Produto WHERE id = @prod_id;";
            SqlParameter p1 = new SqlParameter("@prod_id", productId);
            cmd.Parameters.Add(p1);

            cmd.ExecuteNonQuery();
        }

        public IEnumerable<Produto> GetAll()
        {
            SqlCommand cmd = CreateCommand();
            cmd.CommandText = "select * from Produto;";

            SqlDataReader dr = cmd.ExecuteReader();
            while (dr.Read())
            {
                yield return Produto.Parse(dr);
            }
            dr.Close();
        }

        public void FulfillOrders(List<Proposta> ordersToSuppliers)
        {
            using (var das = MySession.CreateDataAccessScope(true))
            {
                foreach (Proposta p in ordersToSuppliers)
                {
                    SqlCommand cmd = CreateCommand();
                    cmd.CommandText = "update produto set stock_total = stock_total + @encomenda where id = @prod_id;";
                    SqlParameter p1 = new SqlParameter("@encomenda", p.Qtd);
                    cmd.Parameters.Add(p1);
                    SqlParameter p2 = new SqlParameter("@prod_id", p.ProdId);
                    cmd.Parameters.Add(p2);

                    cmd.ExecuteNonQuery();
                }
                das.Commit();
            }
        }
    }
}
