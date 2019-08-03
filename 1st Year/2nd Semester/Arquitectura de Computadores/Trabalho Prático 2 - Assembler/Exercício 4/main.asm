/*
uint16 days;
uint16 year1 = 2017;
uint8 month1 = 3;
uint8 day1 = 30;
uint16 year2 = 1995;
uint8 month2 = 3;
uint8 day2 = 8;
void main() {
days = days_since(1970, year1, month1, day1) -
 days_since(1970, year2, month2, day2);
}

*/
		/****************************************************/
		/*	Calculates number of days between two dates		*/
		/*	saved in memory.								*/
		/*	input - year1, month1, day1	- date end			*/
		/*			year2, month2, day2	- date beggining	*/
		/*	output - days - number of days					*/
		/****************************************************/
		
.section section_main
 .org 0
 jmp main
.data
days:
	.word 0
year1:
	.word 2017
month1:
	.word 3
day1:
	.word 30
year2:
	.word 1995
month2:
	.word 3
day2:
	.word 8
year_ref:
	.word 1970

main:
	ld r0, year_ref
	ld r1, year1
	ld r2, month1
	ld r3, day1
	jmpl days_since
	st r0, days
	ld r0, year_ref
	ld r1, year2
	ld r2, month2
	ld r3, day2
	jmpl days_since
	ld r1, days
	sub r2,r1,r0
	st r2, days
	jmp exit
	
		/****************************************************/
		/*	Calculates number of days of year since 		*/
		/*	the beggining of the year used as reference.	*/
		/*	input - r0 year of reference					*/
		/*			r1 year									*/
		/*			r2 month								*/
		/*			r3 day									*/
		/*	output - r0	number of days						*/
		/****************************************************/
 .section section_days_since
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
		ret
		
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