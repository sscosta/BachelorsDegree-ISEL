SELECT        
	PReferencia.Professor
FROM            
	(SELECT        
		PAP.Professor
    FROM            
		Processos P
		INNER JOIN ProfessorAvaliadorDoProcesso PAP
		ON p.ID_Processo = PAP.Processo
    WHERE        
		(YEAR(p.Data_Submissao) = 2016)

	) AS PReferencia LEFT OUTER JOIN
	(SELECT        
		PAP.Professor
    FROM            
		Processos P
		INNER JOIN ProfessorAvaliadorDoProcesso PAP
		ON p.ID_Processo = PAP.Processo
    WHERE        
		(YEAR(p.Data_Submissao) = 2017 )
	) AS PEmFalta 
	ON PReferencia.Professor = PEmFalta.Professor
WHERE
	PEmFalta.Professor is null