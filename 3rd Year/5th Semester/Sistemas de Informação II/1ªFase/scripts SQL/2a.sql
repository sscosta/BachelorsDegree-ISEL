-- 2 a) criação do modelo físico


use master;

if not exists (SELECT name FROM master.dbo.sysdatabases WHERE name = N'SI2_Bom_e_Barato')
	BEGIN
		create database SI2_Bom_e_Barato
		print('Base de dados criada!')
	END
ELSE
	print('A base de dados SI2_Bom_e_Barato já existe.')
GO 


----drop database SI2_Bom_e_Barato;
--create database SI2_Bom_e_Barato;


use SI2_Bom_e_Barato;


if OBJECT_ID('Franqueado') is null
create table Franqueado(
						id numeric(4) identity(1,1) primary key,
						nif numeric(10) unique not null,
						nome varchar(100) not null,
						morada varchar(100) not null,
						CHECK(nif>0)
						);
go


if OBJECT_ID('TipoProduto') is null
create table TipoProduto( descricao varchar(20) primary key);
go


if OBJECT_ID('Produto') is null
create table Produto(
					id int identity(1,1) primary key,
					cod char(13) not null unique,
					tipo varchar(20) references TipoProduto(descricao) not null,
					descricao varchar(100)not null,
					stock_total int not null,
					stock_min int not null,
					stock_max int not null,
					CHECK(stock_total>=0 and stock_min>=0 and stock_max>=0 and stock_min<=stock_max)
					); 
go


if OBJECT_ID('Entrega') is null
create table Entrega(
					franq_id numeric(4) references Franqueado(id),
					prod_id int references Produto(id),
					valor_ped int not null,
					valor_forn int,
					primary key(franq_id,prod_id)
					);
go


if OBJECT_ID('ProdVendidoPorFranqueado') is null
create table ProdVendidoPorFranqueado(
						franq_id numeric(4) references Franqueado(id),
						prod_id int references Produto(id),
						preco_unitario money not null,
						stock_total int,
						stock_min int,
						stock_max int,
						dt_ultima_venda date,
						qtd_vendas bigint,
						primary key(franq_id,prod_id),
						CHECK(stock_total>=0 and stock_min>=0 and stock_max>=0 and stock_min<=stock_max)
						);
go


if OBJECT_ID('HistoricoVendas') is null
create table HistoricoVendas(
						franq_id numeric(4) references Franqueado(id),
						prod_id int references Produto(id),
						hist_3_anos bigint not null,
						primary key (franq_id, prod_id)
						);
go


if OBJECT_ID('Fornecedor') is null
create table Fornecedor(
					id int identity(1,1) primary key,
					nif numeric(10),
					nome varchar(100),
					CHECK(nif>0)
					);
go


if OBJECT_ID('FornecedorProduto') is null
create table FornecedorProduto(
					forn_id int references Fornecedor(id),
					prod_id int references Produto(id),
					dummy varchar(1)
					);
