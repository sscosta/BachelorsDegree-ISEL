select
	I.Nome Instituicao_Nome
	, i.EnderecoMorada 
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
					--, Al.Curso
					, Al.Instituicao 
				from
					Alteracao Al
					) A
	On A.Instituicao =I.ID_Instituicao 
	Inner Join Processos P
	On p.ID_Processo =a.Processo --and p.EstadoAtual >0
group by 	
	I.Nome
	, i.EnderecoMorada 
having 
	min(p.EstadoAtual)=5