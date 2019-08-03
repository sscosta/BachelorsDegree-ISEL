--2.l) Obter a média das vendas de um dado produto em todos os franqueados no ano corrente
use SI2_Bom_e_Barato;
if exists (select * from sysobjects where id = object_id(N'MediaVendasPorFranqueado') 
    and xtype in (N'FN', N'IF', N'TF'))
	drop function MediaVendasPorFranqueado
go
create function MediaVendasPorFranqueado(@prod_id int)
returns int
as begin
	declare @media int;
	declare @nFranqueados int = (select count(*) from Franqueado);

	set @media =
	(select  sum(qtd_vendas)
	from ProdVendidoPorFranqueado
	where prod_id = @prod_id) / @nFranqueados;
	
	return  @media;
end;

go
--inserir dados de teste
--insert into TipoProduto( descricao) values('Alimentar');
insert into Produto(cod,tipo,descricao, stock_total,stock_min,stock_max) values('4444444444','Alimentar','Café Delta Q',400,60,500);

insert into ProdVendidoPorFranqueado(franq_id,prod_id,preco_unitario,stock_total,stock_min,stock_max,dt_ultima_venda,qtd_vendas) values(1,5,3.69,35,20,50,'20190502',30);
insert into HistoricoVendas(franq_id,prod_id,hist_3_anos) values(1,5,30);
insert into ProdVendidoPorFranqueado(franq_id,prod_id,preco_unitario,stock_total,stock_min,stock_max,dt_ultima_venda,qtd_vendas) values(3,5,2.76,50,30,80,'20190502',25);
insert into HistoricoVendas(franq_id,prod_id,hist_3_anos) values(3,5,25);

go
--verificar que a média é 27 = 30 + 25 / 2
--considerar o ultimo produto adicionado
declare @produto_id int = (SELECT MAX(id) FROM Produto);
select dbo.MediaVendasPorFranqueado(@produto_id);