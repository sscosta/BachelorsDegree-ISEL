/*
uint16 days_since(uint16 year_ref,uint16 year,uint8 month,uint8 day){
	uint16 days = 0;
	for (uint16 y = year_ref; y < year; ++y)
		days += 365 + is_leap(y);
	return days + year_days(year, month, day);
}
*/
		/****************************************************/
		/*	Calculates number of days of year since 		*/
		/*	the beggining of the year used as reference.	*/
		/*	input - r0 year of reference					*/
		/*			r1 year									*/
		/*			r2 month								*/
		/*			r3 day									*/
		/*	output - r0	number of days						*/
		/*	destroys - r4									*/
		/*b)the size of this function in memory is 150b		*/
		/*c)takes 1188 cycles to complete this function		*/
		/*	passing the inputs given as example.			*/
		/****************************************************/
 .section section_days_since
 .org 0
 jmp days_since
.data
days_of_year:
		.word 365
		
year:
		.word 	0
days_since_link:
		.word	0
	
 .text
days_since:
		;st		 r0,_year_ref; guarda parametros de entrada
		st		 r1,year
		;st		 r2,_month
		;st		 r3,_day
		mov		r4,r3
		mov		r3,r0			;r3<Ano ref    prepara variaveis para a funcao year_days
		mov		r0,r1			;r0<Ano
		mov		r1,r2			;r1<Mes
		mov		r2,r4			;r2<dia
		st		r5,days_since_link
		jmpl	year_days
		ld		r5,days_since_link
		mov		r4,r0			;r4<resultado    mover resultado para r4

years_for_increment:		
		ld		r1,year			;r1<Ano
		sub	 	r2, r3, r1; diff r3<Ano ref - ano
		jnc		days_since_end
		ld		r2,days_of_year
		add		r4,r4,r2
		mov		r0,r3
		st		r5,days_since_link
		jmpl	is_leap
		ld		r5,days_since_link
		add		r4,r4,r0
		inc		r3
		jmp		years_for_increment	
days_since_end:		
		mov 	r0,r4
		jmp exit
		
		/****************************************************/
		/*	Calculates number of days of year to date.		*/
		/*	input - r0 year of reference					*/
		/*			r1 month								*/
		/*			r2 day									*/
		/*	output - r0	number of days						*/
		/****************************************************/

		.section section_year_days
		.data
		.equ TABLE_MONTH_DIM, 13
table_month:
		.word 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
year_days_r3:
		.word	0
year_days_r4:
		.word	0
year_days_link:
		.word	0
		
		.text
year_days:
		st		r3,year_days_r3
		st		r4,year_days_r4
		
		dec		r2						; decrementa um dia
		mov 	r4,r2					; adiciona ao registo de retorno - nao utiliza r0 pq r0 ainda retem uma informacao
		
		sub		r3,r1,#1				; decrementa o mes e guarda em r3
		ldi 	r2, #low(table_month)
		ldih 	r2, #high(table_month)
		ld		r2,[r2,r3]				; obtem o numero de dia da tabela em relacao ao mes
		add		r4,r4,r2				; adiciona o numero de dias รก variavel de resposta
		
		sub 	r3,r1,#2				; verifica se mes>2
		jc		year_days_end
		st		r5,year_days_link
		jmpl	is_leap					;verifica se ano bissexto, ano em r0 e resposta em r0
		ld		r5,year_days_link
		add		r4,r4,r0
		
year_days_end:
		mov		r0,r4
		ld		r3,year_days_r3
		ld		r4,year_days_r4
		ret
			
		

		/****************************************************/
		/*	Calculates if year is a leap year.				*/
		/*	input - r0 year to calculate					*/
		/*	output - r0 1 if year is leap year; 0 if not	*/
		/****************************************************/

		.section section_is_leap
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
		