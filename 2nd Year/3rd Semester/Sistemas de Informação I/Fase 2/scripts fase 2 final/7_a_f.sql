select Instituicoes.Nome,Cursos.Nome
from
(select
	  a.Instituicao 
	, a.Curso 
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
		Alteracao Al
	) A
group by
	a.Instituicao 
	, a.Curso 
having
	count(a.Processo )>5)
	as Pairs
	left join Cursos
	on(Pairs.Curso = Cursos.Sigla)
	left join Instituicoes
	on(Pairs.Instituicao=Instituicoes.ID_Instituicao)