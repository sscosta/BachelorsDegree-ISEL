-- manter valores histórico actualizados
use SI2_Bom_e_Barato;
go
create trigger keepHistoricUpdated
on ProdVendidoPorFranqueado
after insert, update
as
begin

-- updated
if (rowcount_big() = 0)
begin

	update HistoricoVendas set
					hist_3_anos = hist_3_anos + d.qtd_vendas - i.qtd_vendas
	from
	inserted as i
	inner join deleted as d
	on i.franq_id = d.franq_id and i.prod_id=d.prod_id
	where d.qtd_vendas > i.qtd_vendas;
		--o registo não existia na tabela historico de vendas
	if @@ROWCOUNT = 0
	begin
	insert into HistoricoVendas(franq_id,prod_id,hist_3_anos)
	select i.franq_id,i.prod_id, d.qtd_vendas - i.qtd_vendas
	from inserted as i
	inner join deleted as d
	on i.franq_id = d.franq_id and i.prod_id=d.prod_id
	inner join HistoricoVendas as h
	on d.franq_id=h.franq_id and d.prod_id = h.prod_id
	where d.qtd_vendas > i.qtd_vendas;
	end
end
select * from Franqueado
select * from ProdVendidoPorFranqueado
select * from Produto
insert into ProdVendidoPorFranqueado (prod_id,franq_id,preco_unitario,stock_total,stock_min,stock_max,dt_ultima_venda,qtd_vendas)
values(60,73,1.10,20,10,25,'2019-07-15',10);
select * from HistoricoVendas
insert into HistoricoVendas (prod_id,franq_id,hist_3_anos) values(60,73,20);
--o produto foi inserido na tabela de venda no franqueado
	else
	begin
		insert into HistoricoVendas(franq_id,prod_id,hist_3_anos)
		select i.franq_id,i.prod_id, i.qtd_vendas
		from inserted as i
	end
end
go

select * from HistoricoVendas

select * from ProdVendidoPorFranqueado 
update ProdVendidoPorFranqueado set stock_total = 25, qtd_vendas=10 where franq_id = 1 and prod_id = 6;