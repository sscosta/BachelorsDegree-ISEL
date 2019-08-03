--2.h) pedir fornecimento no franqueado dos produtos em rutura de stock
use SI2_Bom_e_Barato;

--drop procedure FranqueadoPedidoProdutosEmRutura;
go

if exists (select * from sys.objects where name = 'FranqueadoPedidoProdutosEmRutura') 
	drop procedure FranqueadoPedidoProdutosEmRutura
go

create procedure FranqueadoPedidoProdutosEmRutura
			@id_franqueado numeric(4)
as
begin
	declare @produtosEmRutura table(
								prod_id int,
								qtd_a_pedir int
								);
	--considera-se que o produto está em rutura de stock se o franqueado tem em stock menos de 20% da quantidade máxima de stock
	declare @percentagemRutura real = 0.2;

	--inserção de produtos em rutura numa tabela auxiliar
	insert into @produtosEmRutura(prod_id,qtd_a_pedir) select prod_id, stock_max-stock_total from ProdVendidoPorFranqueado where franq_id= @id_franqueado and ProdVendidoPorFranqueado.stock_total < @percentagemRutura * stock_max;

	--fazer pedido à sede. se ja existem encomendas, actualizar o valor pedido
	update Entrega set
					franq_id = @id_franqueado,
					prod_id = temp.prod_id,
					valor_ped= valor_ped + temp.qtd_a_pedir
	from @produtosEmRutura as temp inner join Entrega as t1
	on franq_id = @id_franqueado and t1.prod_id = temp.prod_id;
	
	if @@ROWCOUNT = 0
	begin
		insert into Entrega(franq_id,prod_id,valor_ped) select @id_franqueado, prod_id,qtd_a_pedir from @produtosEmRutura;
	end
end

go

--TESTE
--Verifica-se que foram inseridas linhas em Entrega
select * from Entrega;
EXEC FranqueadoPedidoProdutosEmRutura @id_franqueado = 1;
select * from Entrega;


