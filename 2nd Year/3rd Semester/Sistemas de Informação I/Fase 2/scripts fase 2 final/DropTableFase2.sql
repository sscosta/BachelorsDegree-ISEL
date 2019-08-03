begin try
begin transaction
drop table AvaliacaoDeRequisitoDoProcesso;
drop table Respostas;
drop table Relatorios;
drop table Alteracao;
drop table Acredita;
drop table ProfessorAvaliadorDoProcesso;
drop table Processos;
drop table Requisitos;
drop table Tipos;
drop table Estados;
drop table ProfessorNaInstituicao;
drop table Professores;
drop table Cursos;
drop table Instituicoes;
commit
end try
begin catch
rollback
end catch