/*uint8 is_leap(uint16 year) {
    return year % 4 == 0 && year%400==0;
    }
    */
		/****************************************************/
		/*	Calculates if year is a leap year.				*/
		/*	input - r0 year to calculate					*/
		/*	output - r0 1 if year is leap year; 0 if not	*/
		/*	destroys - r1									*/
		/****************************************************/
	
	.section is_leap
   	.org 0
_year:
   	.word 2017
   
   		ld r0,_year
_shift:	shr r1,r0,#1,0
   		jc _naobissexto
   		shr r1,r1,#1,0
   		jc _naobissexto
_bissexto:
   		ldi r0,#0x01
		jmp _after
_naobissexto:
		ldi r0,#0x00
   		jmp _after

_after:	jmp $
	
.end