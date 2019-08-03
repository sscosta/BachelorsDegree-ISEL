		.section startup
		.org 0
		jmp year_days
		
		/****************************************************/
		/*	Calculates number of days of year to date.		*/
		/*	input - r0 year of reference					*/
		/*			r1 month								*/
		/*			r2 day									*/	
		/*	output - r0	number of days						*/
		/*	destroys - r3 and r4							*/
		/****************************************************/
		
		.section directdata
		.org 4

		.data
		.equ TABLE_MONTH_DIM, 13
table_month:
		.word 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365

		/*---variaveis de entrada para teste---*/
teste_ano:
		.word 2016
teste_mes:
		.word 1
teste_dia:
		.word 10
		/*---variaveis de entrada para teste---*/

		
		.text
year_days:
		ld		r0,teste_ano			; r0 ano
		ld		r1,teste_mes			; r1 mes
		ld		r2,teste_dia			; r2 dia

		dec		r2						; decrementa um dia
		mov 	r4,r2					; adiciona ao registo de retorno - nao utiliza r0 pq r0 ainda retem uma informacao
		
		sub		r3,r1,#1				; decrementa o mes e guarda em r3
		ldi 	r2, #low(table_month)
		ldih 	r2, #high(table_month)
		ld		r2,[r2,r3]				; obtem o numero de dia da tabela em relacao ao mes
		add		r4,r4,r2				; adiciona o numero de dias á variavel de resposta
		
		sub 	r3,r1,#2				; verifica se mes>2
		jc		set_result
		jmpl	is_leap					;verifica se ano bissexto, ano em r0 e resposta em r0
		add		r4,r4,r0
		
set_result:
		mov		r0,r4
		jmp		exit
	
		

		/****************************************************/
		/*	Calculates if year is a leap year.				*/
		/*	input - r0 year to calculate					*/
		/*	output - r0 1 if year is leap year; 0 if not	*/
		/****************************************************/

		.section directdata
is_leap_r1:
		.word	0
		
		.text
is_leap:
		st		r1,is_leap_r1
		
		shr		r1,r0,#1,0
   		jc		is_leap_naobissexto
   		shr		r1,r1,#1,0
   		jc		is_leap_naobissexto
is_leap_bissexto:
   		ldi		r0,#0x01
		jmp		is_leap_end
is_leap_naobissexto:
		ldi		r0,#0x00
   		jmp		is_leap_end
is_leap_end:	
		ld		r1,is_leap_r1
		ret
		
		

		
exit:		
		jmp $

		.END
		
		