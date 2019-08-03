use SI1_1718i

begin try
begin transaction


create table Instituicoes(
							ID_Instituicao int identity(1,1) primary key,
							Nome varchar(256) default '',
							EnderecoMorada varchar(250) default '',
							EnderecoPostal varchar(10) default '',
							EnderecoLocalidade varchar(100) default '',
							Email varchar(250) default '',
							Telefone varchar(9) default ''
							);
create table Cursos(
							Sigla varchar(5) not null,
							Instituicao int not null,
							Nome varchar(256) not null,
							primary key(Sigla,Instituicao),
							foreign key (Instituicao) references Instituicoes(ID_Instituicao)
							);
create table Professores(
							ID_Professor int identity(1,1)primary key,
							Nome varchar(250)
							);
create table ProfessorNaInstituicao(
							ID_Inst int not null,
							ID_Prof int not null,
							Data_Inicio datetime,
							Cargo varchar(100) default '',
							Categoria varchar(100) default '',
							primary key(ID_Inst,ID_Prof),
							foreign key(ID_Inst) references Instituicoes(ID_Instituicao),
							foreign key(ID_Prof) references Professores(ID_Professor)
							);
create table Estados(
							ID_Estado tinyint not null primary key,
							Estado varchar(50) default ''
							);	
							--identity(0,1)
create table Tipos(
							ID_Tipo smallint identity(0,1) not null,
							Nome varchar(50) not null default '',
							Preco decimal(9,3) not null,
						 	primary key(ID_Tipo)
							);
create table Requisitos(
							ID_Requisito char(10) not null,
							Requisito varchar(max),
							Obrigatorio bit default 0,
							Tipo smallint not null,
							primary key(ID_Requisito,Tipo),
							foreign key (Tipo) references Tipos(ID_Tipo)
							);
create table Processos(
							ID_Processo int identity(1,1) primary key,
							DATA_UAlteracao datetime,
							Data_Submissao date,
							Texto_Submissao varchar(max) default '',
							ProfResponsavel int,
							EstadoAtual tinyint default 0,
							Tipo smallint not null,
							Preco decimal(9,3) not null,
							foreign key (ProfResponsavel) references Professores(ID_Professor),
							foreign key (EstadoAtual) references Estados(ID_Estado),
							foreign key (Tipo) references Tipos(ID_Tipo)
							);
create table ProfessorAvaliadorDoProcesso (
							Processo int not null,
							Professor int not null,
							Presidente bit default 0,
							Primary Key(Processo,Professor),
							foreign key (Processo) references Processos(ID_Processo),
							foreign key (Professor) references Professores(ID_Professor)
							);
create table Acredita(
							Processo int not null,
							Instituicao int not null,
							Curso varchar(5) not null,
							primary key (Processo,Instituicao,Curso),
							foreign key (Processo) references Processos(ID_Processo),
							foreign key (Curso,Instituicao) references Cursos(Sigla,Instituicao)
							);		
create table Alteracao(
							ProcessoOrigem int not null,
							ProcessoAlteracao int not null,
							Curso varchar(5) not null,
							Instituicao int not null,
							Qtd_Alteracoes smallint not null default 0,
							ECTS_Proporcionais bit default 0,
							primary key(ProcessoOrigem, ProcessoAlteracao),
							foreign key(ProcessoOrigem) references Processos(ID_Processo),
							foreign key(Curso,Instituicao) references Cursos(Sigla,Instituicao)
							);		
create table Relatorios(
							Processo int not null,
							DataRelatorio date not null,
							Texto varchar(max) default '',
							primary key(Processo,DataRelatorio),
							foreign key (Processo) references Processos(ID_Processo)
							);
create table Respostas(
							Processo int not null,
							DataRelatorio date not null,
							Num_Sequencial smallint identity(1,1),
							DataResposta date not null,
							Texto varchar(max),
							primary key(Processo,DataRelatorio,Num_Sequencial),
							foreign key(Processo,DataRelatorio) references Relatorios(Processo,DataRelatorio)
							);
create table AvaliacaoDeRequisitoDoProcesso(
							Processo int not null,
							Cumpre bit not null default 0,
							Requisito char(10) not null,
							RequisitoTipo smallint not null,
							primary key(Processo,Requisito),
							foreign key(Processo) references Processos(ID_Processo),
							foreign key(Requisito,RequisitoTipo) references Requisitos(ID_Requisito,Tipo)
							);
							
commit
end try
begin catch
rollback
end catch