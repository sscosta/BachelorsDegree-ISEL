use SI2_Bom_e_Barato;
-- inserir um franqueado
insert into Franqueado(nif, nome, morada) values(111111111, 'Bom & Barato Lourinhã', 'Av. António José de Almeida Lote 12, 2530-113 Lourinhã');
--inserir o tipo do produto
insert into TipoProduto values ('Alimentar');
insert into TipoProduto values ('Higiene Pessoal');
insert into TipoProduto values ('Casa');
--inserir um produto
insert into Produto(cod,descricao, tipo, stock_max , stock_min, stock_total) values('1111111111','Maçãs', 'Alimentar', 200, 15, 150);
--inserir outro produto
insert into Produto(cod,descricao, tipo, stock_max , stock_min, stock_total) values('2222222222222', 'Morangos', 'Alimentar', 100, 20, 100);
select * from Produto;
select * from Franqueado;
-- inserir produto1 em stock no franqueado
insert into ProdVendidoPorFranqueado(franq_id, prod_id, stock_total, stock_max, preco_unitario) values(1,1,10,100,1.69);
-- eliminar produto 1 (falha)
delete from Produto where id=1;
-- eliminar produto 2 (sucesso)
delete from Produto where id=2;
--verificar que foi apagado apenas o registo com id 2 da tabela Produto
select * from Produto;