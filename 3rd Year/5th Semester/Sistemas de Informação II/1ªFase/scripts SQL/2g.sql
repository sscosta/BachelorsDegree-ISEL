use SI2_Bom_e_Barato;

select * from ProdVendidoPorFranqueado
select * from Produto;
select * from Franqueado;
--i.login

declare @tab table(
					qtVend int,
					loja int,
					produto numeric(4),
					valor_a_pagar money,
					preco_unitario money
					);


--ii.passar produto no leitor de codigo de barras
insert into @tab(qtVend,loja,produto) values 
					(2,1,1);

select * from ProdVendidoPorFranqueado;

--iii.finalizar a conta - atualizar tabelas e emitir fatura
begin transaction;
update ProdVendidoPorFranqueado set
						dt_ultima_venda = GETDATE(),
						qtd_vendas= qtd_vendas + temp.qtVend,
						stock_total = stock_total - temp.qtVend
						from ProdVendidoPorFranqueado as t1 inner join @tab as temp
						on t1.franq_id = temp.loja and t1.prod_id = temp.produto;
update temp set
		valor_a_pagar = qtVend * t1.preco_unitario,
		preco_unitario =  t1.preco_unitario
		from ProdVendidoPorFranqueado as t1 inner join @tab as temp
		on t1.franq_id = temp.loja and t1.prod_id = temp.produto;

select * from ProdVendidoPorFranqueado;

update HistoricoVendas set
						hist_3_anos = hist_3_anos + temp.qtVend
						from ProdVendidoPorFranqueado as t1 inner join @tab as temp
						on t1.franq_id = temp.loja and t1.prod_id = temp.produto;

commit;
--emissão recibo com lista de compras
declare @total money = (select sum(valor_a_pagar) from @tab);
print('o valor a pagar são ' + CAST(@total AS VARCHAR) + ' euros');

select
	Produto.descricao,
	lista.qtVend,
	lista.valor_a_pagar,
	lista.preco_unitario
from @tab as lista inner join Produto
on lista.produto = Produto.id
inner join Franqueado
on lista.loja = Franqueado.id




