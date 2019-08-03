
use SI2_Bom_e_Barato;

--inserir produto, 
insert into Produto(descricao,cod,tipo,stock_max,stock_total,stock_min) values('Agriões', '4444444444444','Alimentar', 250, 220,8);

declare @x int = (SELECT MAX(id) FROM Produto);
-- inserir em stock no franqueado + para venda
insert into ProdVendidoPorFranqueado(franq_id,prod_id,stock_max,stock_min,stock_total,preco_unitario) values(1,@x, 150,50,80,1.39);

-- apagar o produto das tres tabelas por esta ordem
declare @y int = (SELECT MAX(id) FROM Produto);
delete from ProdVendidoPorFranqueado where prod_id = @y;
delete from HistoricoVendas where prod_id = @y;
delete from Entrega where prod_id = @y;
delete from Produto where id = @y;

--verificar que as queries seguintes retornam vazio
select * from Produto where id= @y;
select * from ProdVendidoPorFranqueado where prod_id = @y;
select * from HistoricoVendas;
select * from Entrega;