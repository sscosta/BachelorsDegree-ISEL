select A.Processo, A.Instituicao, I.Nome,A.Curso, B.Texto_Submissao
FROM
(select P.Texto_Submissao,P.ID_Processo
from Processos P
where
YEAR(P.DATA_UAlteracao)=2017) B
inner JOIN
(select
			Ac.Processo 
		, Ac.Instituicao 
		, Ac.Curso 
	from 
		Acredita Ac
	union all
	select
			al.ProcessoAlteracao
		, Al.Instituicao 
		, Al.Curso

	from
		Alteracao Al
	) A
	on(B.ID_Processo=A.Processo)
	inner JOIN
	Instituicoes I
	on(I.ID_Instituicao=A.Instituicao)
	order by I.Nome,A.Curso