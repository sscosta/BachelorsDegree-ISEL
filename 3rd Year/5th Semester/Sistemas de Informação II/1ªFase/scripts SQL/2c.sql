use SI2_Bom_e_Barato;
--inserir informa��o de um franqueado
insert into Franqueado(nif, nome, morada) values(123456789, 'Bom & Barato Lisboa Oriental','R. Laureano de Oliveira 19 A, 1885-051 Lisboa');
insert into Franqueado(nif, nome, morada) values(111111111, 'Bom & Barato Areeiro','Av. Estados Unidos da Am�rica 35, 1700-268 Lisboa');
--tabela tem dois registos
select * from Franqueado;
--remover informa��o de um franqueado
delete from Franqueado where id = 2;
--alterar informa��o de um franqueado
update Franqueado set nome = 'Meu Super Moscavide' where nif = 123456789;
--tabela tem um registo
select * from Franqueado;