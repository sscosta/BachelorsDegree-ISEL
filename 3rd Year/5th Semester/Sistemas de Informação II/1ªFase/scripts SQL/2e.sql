use SI2_Bom_e_Barato;
--inserir para venda num franqueado
insert into Produto(cod, descricao, tipo, stock_max , stock_min, stock_total) values('3333333333333','Maracujás','Alimentar', 50,20,40);
insert into ProdVendidoPorFranqueado(franq_id, prod_id, stock_total, stock_min, stock_max, preco_unitario) values(1,3,10,10,100,1.69);
select * from Produto;
select * from ProdVendidoPorFranqueado;
