--remover o modelo físico


use SI2_Bom_e_Barato;

IF OBJECT_ID('FornecedorProduto') IS NOT NULL
drop table FornecedorProduto;

IF OBJECT_ID('ProdEmStockNoFranq') IS NOT NULL
drop table ProdEmStockNoFranq;

IF OBJECT_ID('ProdVendidoPorFranqueado') IS NOT NULL
drop table ProdVendidoPorFranqueado;

IF OBJECT_ID('Entrega') IS NOT NULL
drop table Entrega;

IF OBJECT_ID('HistoricoVendas') IS NOT NULL
drop table HistoricoVendas;

IF OBJECT_ID('Produto') IS NOT NULL
drop table Produto;

IF OBJECT_ID('TipoProduto') IS NOT NULL
drop table TipoProduto;

IF OBJECT_ID('Fornecedor') IS NOT NULL
drop table Fornecedor;

IF OBJECT_ID('Franqueado') IS NOT NULL
drop table Franqueado;


use master;
drop database SI2_Bom_e_Barato;
