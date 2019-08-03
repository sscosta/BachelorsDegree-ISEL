Select sum(Preco)
from
(select
	  A.Processo
from
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
		Alteracao Al) A
where a.Instituicao=7 and a.Curso='LEIC'
)B
inner join
Processos P
on(P.ID_Processo=B.Processo)