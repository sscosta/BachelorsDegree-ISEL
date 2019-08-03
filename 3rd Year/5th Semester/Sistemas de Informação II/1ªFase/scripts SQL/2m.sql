--2.m) definir vista tudo sobre produto
use SI2_Bom_e_Barato;
go

drop view if exists TudoSobreProduto;
go

create view TudoSobreProduto as
select
	p.id as produto_id,
	p.cod as produto_cod,
	p.tipo as produto_tipo,
	p.descricao as produto_descricao,
	p.stock_total as stock_total_na_sede,
	p.stock_min as stock_min_na_sede,
	p.stock_max as stock_max_na_sede,
	f.id as franqueado_id,
	pv.preco_unitario,
	pv.stock_total as stock_total_no_franqueado,
	pv.stock_min as stock_min_no_franqueado,
	pv.stock_max as stock_max_no_franqueado,
	pv.dt_ultima_venda,
	pv.qtd_vendas as total_vendas_produto_no_franqueado,
	f.nif as nif_franqueado,
	f.nome as nome_franqueado,
	f.morada as morada_franqueado
from
Produto p
inner join ProdVendidoPorFranqueado pv
on p.id = pv.prod_id
inner join Franqueado f
on f.id = pv.franq_id;

go

select * from dbo.TudoSobreProduto