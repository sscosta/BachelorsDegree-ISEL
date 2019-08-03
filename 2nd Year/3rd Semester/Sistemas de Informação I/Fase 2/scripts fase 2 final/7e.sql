select
--a.Processo
--,I.ID_Instituicao as id_inst
I.Nome as NomeInstituicao
,C.Sigla sigla
,p.data_submissao
from
Instituicoes I
inner JOIN
Cursos c
on(c.instituicao=I.ID_Instituicao)
inner JOIN
Acredita a
on(a.Instituicao=I.ID_Instituicao
	and a.Curso=C.Sigla)
inner JOIN
Processos p
on(p.ID_Processo=a.processo)
group by I.Nome, C.Sigla,p.Data_Submissao
union all

select
--al.ProcessoAlteracao
--,I2.ID_Instituicao as id_inst
I2.Nome as NomeInstituicao
,C2.Sigla sigla
,p2.data_submissao
from
Instituicoes I2
inner JOIN
Cursos c2
on(c2.instituicao=I2.ID_Instituicao)
inner JOIN
alteracao al
on(al.curso=C2.Sigla and al.Instituicao=I2.ID_Instituicao)
inner JOIN
Processos p2
on(p2.ID_Processo=al.processoAlteracao)
group by I2.Nome ,C2.Sigla,p2.Data_Submissao




