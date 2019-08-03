SELECT        
	TOP (1) 
	PAP.Professor
FROM            
	Processos as P
	INNER JOIN ProfessorAvaliadorDoProcesso as PAP
	ON P.ID_Processo = PAP.Processo
WHERE        
	(PAP.Presidente = 1) AND (P.Tipo = 2)
GROUP BY 
	PAP.Professor
HAVING        
	(COUNT(PAP.Processo) = MAX(PAP.Processo))
	
	
	
Select TOP(1) PAP.Professor,Count(Distinct ID_Processo)
from 
Processos as P
	INNER JOIN ProfessorAvaliadorDoProcesso as PAP
	ON P.ID_Processo = PAP.Processo
where (PAP.Presidente=1) AND (P.Tipo=2)
Group By PAP.Professor
Having count(Distinct ID_Processo) = (	Select max(CountProc)
										from
											(Select count(distinct ID_Processo) as CountProc
											from
											(Processos as P
												INNER JOIN ProfessorAvaliadorDoProcesso as PAP
												ON P.ID_Processo = PAP.Processo
											where (PAP.Presidente=1) AND (P.Tipo=2))
												Group By PAP.Professor))


SELECT PAP.Professor--,count(PAP.Processo)
FROM
Processos P 
INNER JOIN ProfessorAvaliadorDoProcesso PAP
ON P.ID_Processo = PAP.Processo
WHERE        (P.Tipo = 2) AND (PAP.Presidente = 1)
Group By
PAP.Professor
Having count(PAP.Processo)= (select MAX(CountProc)
 	from
 	(Select count(distinct PAP.Processo) as CountProc
 		from Processos P
 		INNER JOIN ProfessorAvaliadorDoProcesso PAP
		ON P.ID_Processo = PAP.Processo
		WHERE        (P.Tipo = 2) AND (PAP.Presidente = 1)
		Group by Professor));
		
		

		
		


select
	I.Nome Instituicao_Nome
	, i.EnderecoMorada 
	, sum(case when p.EstadoAtual<5 then 1 else 0 end ) NaoAcreditado
	, count(p.ID_Processo ) Submetidos
from 
	Instituicoes I
	inner join (select
					Ac.Processo 
					,Ac.Instituicao 
				from 
					Acredita Ac
				union all
				select
					al.ProcessoAlteracao
					, Al.Instituicao 
				from
					Alteracao Al
					) A
	On A.Instituicao =I.ID_Instituicao 
	Inner Join Processos P
	On p.ID_Processo =a.Processo
group by 	
	I.Nome
	, i.EnderecoMorada