Insert into Tipos(Nome,Preco)
	values('',0);
Insert into Tipos(Nome,Preco)
	values('Acreditação Inicial',1000);
Insert into Tipos(Nome,Preco)
	values('Alteração',500);

Insert into Estados(ID_Estado,Estado)
	values(0,'');
Insert into Estados(ID_Estado,Estado)
	values(1,'Registado');
Insert into Estados(ID_Estado,Estado)
	values(2,'Em Análise');
Insert into Estados(ID_Estado,Estado)
	values(3,'Não Acreditado');
Insert into Estados(ID_Estado,Estado)
	values(4,'Condicionalmente Acreditado');
Insert into Estados(ID_Estado,Estado)
	values(5,'Acreditado');
	
	
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESD','Rua da Academia das Ciências, nº 7','1200-003','Lisboa','geral@esd.ipl.pt','213244770');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESELx','Campus de Benfica do IPL','1549-003','Lisboa','eselx@eselx.ipl.pt','217115500');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESML','Campus de Benfica do IPL','1500-651','Lisboa','esml@esml.ipl.pt','210464800');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESTC','Avenida Marquês de Pombal, 22 B','2700-571','Amadora','estc@estc.ipl.pt','214989400');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ISCAL','Avenida Miguel Bombarda, 20','1069 - 035','Lisboa','iscal@iscal.ipl.pt','217984500');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESCS','Campus de Benfica do IPL','1549-014','Lisboa','servicos_academicos@escs.ipl.pt','217119000');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ISEL','Rua Conselheiro Emídio Navarro, 1' ,'1959-007','Lisboa','licenciaturas@isel.pt','218317000');
Insert into Instituicoes(Nome,EnderecoMorada,EnderecoPostal,EnderecoLocalidade,Email,Telefone)
	values ('ESTSL','Av. D. João II, lote 4.69.01, Parque das Nações','1990-096','Lisboa','comunicacao@estesl.ipl.pt','218980400');
	
	
	
Insert into Cursos(Sigla,Instituicao,Nome)
	values('D',1,'Dança');
	
Insert into Cursos(Sigla,Instituicao,Nome)
	values ('AVT',2,'Artes Visuais e Tecnologias');
Insert into Cursos(Sigla,Instituicao,Nome)
	values ('MAC',2,'Mediação Artistica e Cultural');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('EB',2,'Educação Básica');

Insert into Cursos(Sigla,Instituicao,Nome)
	values('M',3,'Música');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('TM',3,'Tecnologias da Música');
	
Insert into Cursos(Sigla,Instituicao,Nome)
	values('C',4,'Cinema');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('T',4,'Teatro');

Insert into Cursos(Sigla,Instituicao,Nome)
	values('CA',5,'Contabilidade e Administração');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('FE',5,'Finanças Empresariais');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('G',5,'Gestão');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('CNI',5,'Comércio e Negócios Internacionais');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('S',5,'Solicitadoria');
	
Insert into Cursos(Sigla,Instituicao,Nome)
	values('AM',6,'Audiovisual e Multimédia');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('J',6,'Jornalismo');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('PM',6,'Publicidade e Marketing');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('RPCE',6,'Relações Públicas e Comunicação Empresarial');
	
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEC',7,'Engenharia Civil');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEETC',7,'Engenharia Eletrónica e Telecomunicações e de Computadores');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEIM',7,'Engenharia Informática e Multimédia');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEE',7,'Engenharia Eletrotécnica');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEIC',7,'Engenharia Informática e de Computadores');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEIRT',7,'Engenharia Informática, Redes e Telecomunicações');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LEQB',7,'Engenharia Química e Biológica');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LMATE',7,'Matemática Aplicada à Tecnologia e à Empresa');
Insert into Cursos(Sigla,Instituicao,Nome)
	values('LTGM',7,'Tecnologias de Gestão Municipal');
	

insert into Professores(Nome)
	values('Carlos Martins');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(7,1,'1979-05-18T10:34:09','','Professor Adjunto');
insert into Professores(Nome)
	values('Constantino Soares');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(7,2,'1983-01-01T08:30:00','','Professor Coordenador');
insert into Professores(Nome)
	values('Fernando Sousa');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(7,3,'1990-01-01T08:30:00','Coordenador LEIC','Professor Coordenador');

insert into Professores(Nome)
	values('Bárbara Griggi');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(1,4,'2014-01-01T08:30:00','','Professor Assistente');
insert into Professores(Nome)
	values('Madalena Silva');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(1,5,'2003-01-01T08:30:00','','Professor Adjunto');
	
	

insert into Professores(Nome)
	values('Vanda Nascimento');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(1,6,'2000-01-01T08:30:00','','Professor Coordenador');
insert into Professores(Nome)
	values('Amélia Bentes');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(1,7,'2001-01-01T08:30:00','','Professor Adjunto');
	

insert into Professores(Nome)
	values('Joana Campos');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(2,8,'2006-01-01T08:30:00','','Professor Adjunto');
insert into Professores(Nome)
	values('Carla  Rocha');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(2,9,'1996-01-01T08:30:00','','Professor Auxiliar');

insert into Professores(Nome)
	values('António Pinho Vargas');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(3,10,'1995-01-01T08:30:00','','Professor Adjunto');
insert into Professores(Nome)
	values('Luis Tinoco');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(3,11,'2001-01-01T08:30:00','','Professor Adjunto');

insert into Professores(Nome)
	values('Fátima Chinita');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(4,12,'1995-01-01T08:30:00','','Professor Adjunto');
insert into Professores(Nome)
	values('Fátima Ribeiro');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(4,13,'2003-01-01T08:30:00','','Professor Auxiliar');
	
insert into Professores(Nome)
	values('Carlos Nunes');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(5,14,'1989-01-01T08:30:00','','Professor Coordenador');
insert into Professores(Nome)
	values('Raúl Navas');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(5,15,'2014-01-01T08:30:00','','Professor Assistente');
insert into Professores(Nome)
	values('Jorge Veríssimo');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(6,16,'1999-01-01T08:30:00','','Professor Coordenador');
insert into Professores(Nome)
	values('António Belo');
insert into ProfessorNaInstituicao(ID_Inst,ID_Prof,Data_Inicio,Cargo,Categoria)
	values(6,17,'1990-01-01T08:30:00','','Professor Adjunto');
	
--Processos
--1
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
	values('2016-06-06T12:40:00','2016-06-12T12:40:00','Primeiro Processo(Acreditação) Gestão ISCAL',3,1,1,1000);
Insert into Acredita
           (Processo
           ,Instituicao
           ,Curso)
     values(1,5,'G');
INSERT INTO ProfessorAvaliadorDoProcesso
           (Processo
           ,Professor
           ,Presidente)
     VALUES(1,3,1);

	 
--2

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2016-06-06T12:40:00','2016-06-12T12:40:00','Segundo Processo(Primeiro de alteração) Gestão ISCAL',2,3,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(1,2,'G',5);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(2,2,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)	 
	 VALUES(2,17,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(2,16,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(2,15,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(2,14,0);

--3

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2016-06-06T12:40:00','2016-06-12T12:40:00','Terceiro Processo(segundo de alteração) Gestão ISCAL',3,3,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(1,3,'G',5);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(3,3,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	VALUES(3,12,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)	
	VALUES(3,13,0);
--4

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2016-06-06T12:40:00','2016-07-12T12:40:00','Quarto Processo(terceiro de alteração) Gestão ISCAL',1,3,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(1,4,'G',5);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(4,1,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	VALUES(4,11,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)	
	VALUES(4,10,0);
	
--5

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
	values('2016-06-06T12:40:00','2016-06-12T12:40:00','Quinto Processo(Quarto de alteração) Gestão ISCAL',4,3,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(1,5,'G',5);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(5,4,1);
	 
--6
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2016-06-06T12:40:00','2016-06-12T12:40:00','Sexto Processo(Quinto de alteração) Gestão ISCAL',5,1,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(1,6,'G',5);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(6,5,1);
--7

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2016-06-06T12:40:00','2016-06-12T12:40:00','Primeiro Processo(Acreditação) LEIC ISEL',3,5,1,1000);
Insert into Acredita
           (Processo
           ,Instituicao
           ,Curso)
     values(7,7,'LEIC');
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(7,3,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(7,6,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(7,7,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(7,8,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(7,9,0);
--8
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2016-06-12T12:40:00','Segundo Processo(Primeiro Alteracao) LEIC ISEL',10,5,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao)
     VALUES
	(7,8,'LEIC',7);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(8,10,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(8,1,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(8,2,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(8,3,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(8,4,0);
		   
--9
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-08-07T12:40:00','2017-07-07T12:40:00','Segundo Processo(Primeiro Alteracao) LEIC ISEL',3,5,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao)
     VALUES
	(7,9,'LEIC',7);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(9,3,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(9,10,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(9,11,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(9,12,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(9,13,0);
--10
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2017-07-30T12:40:00','3º Processo(Segundo Alteracao) LEIC ISEL',6,5,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao)
     VALUES
	(7,10,'LEIC',7);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(10,6,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(10,17,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(10,16,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(10,15,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
		   VALUES(10,14,0);
--11
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2016-06-12T12:40:00','1º Processo Musica ESML',11,1,1,1000);
Insert into Acredita
           (Processo
           ,Instituicao
           ,Curso)
     values(11,3,'M');
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(11,11,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(11,1,0);
--12
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2016-06-12T12:40:00','2º Processo(1ºAlteração) Musica ESML',9,2,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(11,12,'M',3);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(12,9,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(12,2,0);
	 
--13

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2016-06-12T12:40:00','3º Processo(2ºAlteração) Musica ESML',8,3,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(11,13,'M',3);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(13,8,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(13,3,0);
	 
--14

Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2016-06-12T12:40:00','4º Processo(3ºAlteração) Musica ESML',7,4,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(11,14,'M',3);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(14,7,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(14,4,0);
	 
--15
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2017-06-12T12:40:00','5º Processo(4ºAlteração) Musica ESML',3,5,2,500);
INSERT INTO Alteracao
           (ProcessoOrigem
           ,ProcessoAlteracao
           ,Curso
           ,Instituicao
           )
     VALUES
	(11,15,'M',3);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(15,3,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(15,6,0);
--16
Insert into Processos(Data_Submissao,DATA_UAlteracao,Texto_Submissao,ProfResponsavel,EstadoAtual,Tipo,Preco)
values('2017-06-06T12:40:00','2017-06-12T12:40:00','1º Tecnologias da Musica ESML',3,5,1,1000);
Insert into Acredita
           (Processo
           ,Instituicao
           ,Curso)
     values(16,3,'TM');
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
     VALUES(16,3,1);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(16,14,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(16,15,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(16,16,0);
Insert into ProfessorAvaliadorDoProcesso(Processo
           ,Professor
           ,Presidente)
	 VALUES(16,17,0);
	 
--insercao de relatorios e respostas para os processos 4,9 e 10
Insert into Relatorios(
							Processo,
							DataRelatorio,
							Texto
							)
			values(4,'2017-07-19','Primeiro Relatório Alteração Curso Gestão');
			
Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
			values(4,'2017-07-19','2017-07-20','1ª Resposta ao primeiro relatorio alteração curso gestão iscal');
			
Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
			values(4,'2017-07-19','2017-07-21','2ªResposta ao primeiro relatorio alteração curso gestão iscal');
			
Insert into Relatorios(
							Processo,
							DataRelatorio,
							Texto
							)
		values(9,'2017-06-07','Primeiro Relatório Alteração Curso LEIC');

Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
		values(9,'2017-06-07','2017-07-08','Resposta ao primeiro relatorio alteração curso LEIC');
Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
		values(9,'2017-06-07','2017-07-19','2ªResposta ao primeiro relatorio alteração curso LEIC');		
		
Insert into Relatorios(
							Processo,
							DataRelatorio,
							Texto
							)
		values(10,'2017-06-07','Primeiro Relatório Alteração Curso LEIC');
Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
		values(10,'2017-06-07','2017-06-08','Resposta ao Primeiro Relatório Alteração Curso LEIC');
Insert into Respostas(
							Processo,
							DataRelatorio,
							DataResposta,
							Texto
							)
		values(10,'2017-06-07','2017-06-08','2ªResposta ao Primeiro Relatório Alteração Curso LEIC');			