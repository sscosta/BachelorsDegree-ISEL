use SI2_Bom_e_Barato;

if OBJECT_ID('propostas') is not null
	drop procedure [dbo].propostas
go

declare @propostas table (
					fornecedor int,
					preco_unitario money,
					qtd_vendas int,
					prod_id numeric(4));
-- inserir dados para teste
insert into @propostas exec consultar_fornecedor @preco_unitario =1.01, @qtd_vendas = 80, @prod_id = 1;
insert into @propostas exec consultar_fornecedor @preco_unitario =2.50, @qtd_vendas = 20, @prod_id = 8;
insert into @propostas ( fornecedor,preco_unitario,qtd_vendas,prod_id) values(3,0.50,20,1);


go
create Procedure [dbo].propostas
@id int

as 
begin
	begin try
	set transaction isolation level read uncommitted 
		begin transaction
		declare @quantidadePedida int
		declare @quantidadeVendida int
		declare @quantidadeFornecidade int
		declare @prod_id int
		declare @preco_medio int
		set @quantidadeFornecida = 0

		--
		select @quantidadePedida = valor_ped from Entrega where prod_id=@id
		select @preco_medio = AVG(preco_unitario) from @propostas where prod_id = @id
		delete from @proposta where (preco_unitario > @preco_medio*0.3+@preco_medio OR preco_unitario < @preco_medio-@preco_medio*0.3) AND prod_id = @id

		declare crs cursor local for select qtd_vendas from @proposta where prod_id = @id order by preco_unitario ASC
		open crs
		fetch next from crs into @quantidadeVendida

		while (@@FETCH_STATUS = 0)
			begin
				set @quantidadeFornecidade += @quantidadeVendida
				if(@quantidadeFornecidade >= @quantidadePedida)
				begin
					break
				end
				fetch next from crs into @quantidadeVendida
			end
		commit
	end try
	begin catch
		print ERROR_MESSAGE()
		rollback
	end catch
end
go

				 
