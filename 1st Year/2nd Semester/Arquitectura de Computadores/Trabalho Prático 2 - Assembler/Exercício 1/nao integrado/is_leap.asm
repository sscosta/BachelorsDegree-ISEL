


.section is_leap
   	.org 40
_result:
		word 0
		
   		ld r1,_yearinsearch
_shift:	shr r2,r1,#1,0
   		jc _naobissexto
   		shr r2,r2,#1,0
   		jc _naobissexto
_bissexto:
   		st _result,#0x01
		jmp _after
_naobissexto:
		st _result,#0x00
   		jmp _after

_after:	
		ret