SELECT        
	  P.Nome as Professor
	, PI.Cargo
	, PI.Categoria 
	, I.Nome as Instituicao
FROM            
	Professores AS P 
	INNER JOIN ProfessorNaInstituicao AS PI 
	ON P.ID_Professor = PI.ID_Prof 
	INNER JOIN Instituicoes AS I 
	ON PI.ID_Inst = I.ID_Instituicao
WHERE        
	(P.ID_Professor = 3 )