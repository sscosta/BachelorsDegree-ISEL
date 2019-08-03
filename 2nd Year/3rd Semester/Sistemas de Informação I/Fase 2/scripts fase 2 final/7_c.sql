Select *
FROM
	(Select Processo
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
		where a.Instituicao=7 and a.Curso='LEIC')B
	inner join
	Processos P
	on(P.ID_Processo=B.Processo)
	where
	data_UAlteracao between convert(datetime, '2016-07-01') and convert(datetime, '2017-07-31') and
	(estadoAtual=5 or estadoatual=3))C
Where C.Processo in(
	select distinct Processo
	from
	respostas R
	where
	(datediff(dayofyear,  R.DataRelatorio, R.DataResposta)<15)
	)


