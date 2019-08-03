        .section code
        .org    0
        ld	r7,[r7,#1]
        ld	r7,[r7,#1]
	.word	main, ISR

        .section DIRECT_DATA
ISR_STATUS_table:
	.word	ISR_STATE_W4_BUTTONS, ISR_STATE_W4_FIFTEEN_S, ISR_STATE_W4_TWO_MINUTES_S, ISR_STATE_W4_FIFTEEN_T, ISR_STATE_W4_TWO_MINUTES_T, ISR_STATE_ALERT 
ISR_STATUS_index:
	.space	2
ADDR_IO:
        .word   0xFF00
ADDR_NCS_EXT:
        .word   0xFF40
MASK_DI:
	.word	0xF
MASK_EI:
	.word	0x10

TIMER_START_const:
	.word	10
	
MASK_RS_set:
	.word 10000000b
MASK_GS_set:
	.word 01000000b
MASK_GT_set:
	.word 00000010b
MASK_RT_set:
	.word 00000001b
MASK_RS_reset:
	.word 01111111b
MASK_GS_reset:
	.word 10111111b
MASK_GT_reset:
	.word 11111101b
MASK_RT_reset:
	.word 11111110b

MASK_GS_reset_RS_set_GT_reset_RT_set:
	.word 10000001b
MASK_GS_set_RS_set_GT_set_RT_set:
	.word 11000011b

time_2min:
	.word	0x78	
number_of_people:
        .word   0
flag_BS:
        .word   0
flag_BT:
        .word   0
Timer:
        .word   0
Current_Sensors_Image:
        .word   0
LIGHTS_word:
	.word	0
BLINKS_word:
	.word	0	
	
Current_Sensor_T:
        .word   0
Current_Sensor_S:
        .word   0
Last_Sensor_T:
        .word   0
Last_Sensor_S:
        .word   0
Accounting_Link:
        .word   0
Accounting_link1:
        .word   0
table_count:
        .word   0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0
table_flip:
        .word   0,  2,  1,  3
Psw_main_link:
        .word   0
main:
        ld      r0,MASK_DI
        anlf    r6,r6,r0
	
        ldi     r1,#0
        ld      r2,ADDR_IO
        st      r1,[r2,#0]
	st	r1, ISR_STATUS_index
	st	r1, BLINKS_word
        st      r1,number_of_people
        st      r1,flag_BS
        st      r1,flag_BT
        st      r1,Timer
TIMER_START:
        ldi     r0,#10
        ld      r1,ADDR_NCS_EXT
        st      r0,[r1,#0]
        ld      r0,[r1,#0]
Interrupt_Enable:
        ld      r0,MASK_EI
        orlf    r6,r6,r0
loop:
        jmpl    Input_Data
        jmpl    ParseInput
        jmpl    Accounting
        jmp loop    

        ret
        
input_data:
        ld      r0, ADDR_IO
        ld      r1, [r0,#0]
        st      r1, Current_Sensors_Image
        ret
        
ParseInput:
        ld      r0, Current_Sensors_Image
        ldi     r1, #3  ;mask to obtain sensor
        ldi     r2, #1  ;mask to obtain botton
ParseInput_Sensor_T:
        anl     r3, r0, r1
        ldi     r4,#low(table_flip)
        ldih    r4,#high(table_flip)
        ld      r3, [r4, r3]
        st      r3, Current_Sensor_T
ParseInput_Button_T:
	shr     r0, r0, #2, 0
        ld      r3,flag_BT
        orl     r3,r3,r3
        jnz     ParseInput_Button_S
        anl     r3, r0, r2
        jz      ParseInput_Button_S
        ldi     r3,#1
        st      r3,flag_BT
ParseInput_Button_S:
        shr     r0, r0, #3, 0
        ld      r3,flag_BS
        orl     r3,r3,r3
        jnz     ParseInput_Sensor_S
        anl     r3, r0, r2
        jz      ParseInput_Sensor_S
        ldi     r3,#1
        st      r3,flag_BS
ParseInput_Sensor_S:
        shr     r0, r0, #1, 0
        anl     r3, r0, r1
        st      r3, Current_Sensor_S
        ret
        
Accounting:
        st      r5, Accounting_link1
        jmpl    Accounting_Sensor_S
        jmpl    Accounting_Sensor_T
	ld      r5, Accounting_link1
        ret

        /********************************************************/
        /*  Account from Sensor S                               */
        /*  input   -                                           */
        /*  output  -                                           */
        /********************************************************/
Accounting_Sensor_S:
        ld      r0, Last_Sensor_S
        ;sub     r1,r0,#3                        ;apenas contabiliza se last_sensor=b11
        ;jnz      Accounting_Sensor_T_End	; comentado, pois assim nao atualiza last_sensor, que e sempre 0
        ld      r1, Current_Sensor_S
        xrl     r2,r0,r1
        jz      Accounting_Sensor_S_End          ;se r0 e r1 iguais nao existe alteracao de sensor
        st      r5, Accounting_Link
        jmpl    Accounting_people
        ld      r5, Accounting_link
        st      r1, Last_Sensor_S
Accounting_Sensor_S_End:
        ret    

        /********************************************************/
        /*  Account from Sensor T                               */
        /*  input   -                                           */
        /*  output  -                                           */
        /********************************************************/
Accounting_Sensor_T:
        ld      r0, Last_Sensor_T
        ;sub     r1,r0,#3                        ;apenas contabiliza se last_sensor=b11
        ;jnz      Accounting_Sensor_T_End	;ver linhas iguais na rotina acima
        ld      r1, Current_Sensor_T
        xrl     r2,r0,r1
        jz      Accounting_Sensor_T_End          ;se r0 e r1 iguais nao existe alteracao de sensor
        st      r5, Accounting_Link
        jmpl    Accounting_people
        ld      r5, Accounting_link
        st      r1, Last_Sensor_T
Accounting_Sensor_T_End:
        ret

Accounting_People:
        ldi     r2,#low(table_count)
        ldih    r2,#high(table_count)
        shl     r3, r1, #2, 0
        add     r3, r3, r0
        ld      r4, [r2,r3]
        orl     r4,r4,r4
        jz      Accounting_People_End        ;se nao tem pessoa para adicionar nao adiciona
        ld      r3, number_of_people
        add     r3,r3,r4
        st      r3,number_of_people
	
Accounting_People_End:
        ret

	/*********** RAMO BS ***********/
ISR_STATE_W4_FIFTEEN_S:
	ld	r3,Timer
	orl	r3,r3,r3
	jz	ISR_set_STATE_W4_TWO_MINUTES_S		;jump if T15=0
	ld	r3,flag_BT
	orl	r3,r3,r3
	jz	ISR_STATE_W4_FIFTEEN_S_eval_BS
	ld 	r3,MASK_RT_set
	orl	r2,r2,r3
	ld	r3,Timer
	dec	r3
	st	r3,Timer
	jmp	ISR_State_End
ISR_STATE_W4_FIFTEEN_S_eval_BS:
	ld	r3,flag_BS
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_S_1		;jump if flag_BS=0
	ld	r3,Timer
	dec	r3
	st	r3,Timer
	jmp	ISR_State_End

ISR_set_STATE_W4_FIFTEEN_S_3:
	ld 	r3,MASK_RS_reset
	anl	r2,r2,r3
ISR_set_STATE_W4_FIFTEEN_S_2:
	ld 	r3,MASK_GS_set
	orl	r1,r1,r3
	ld 	r3,MASK_RS_reset
	anl	r1,r1,r3
	ldi	r0,#1
	st	r0,ISR_STATUS_index
ISR_set_STATE_W4_FIFTEEN_S_1:
	ldi	r3,#0xF
	;ldi	r3,#0x8
	st	r3,Timer
	ldi	r3,#0
	st	r3,flag_BS
	ld 	r3,MASK_GS_set
	jmp	ISR_State_End	
	
	
ISR_set_STATE_W4_TWO_MINUTES_S:
	ldi	r3,#2
	st	r3,ISR_STATUS_index
	ld	r3,time_2min
	st	r3,Timer
	ld 	r3,MASK_RS_set
	orl	r1,r1,r3
	ld 	r3,MASK_GS_reset
	anl	r1,r1,r3
	jmp	ISR_State_End
	
ISR_STATE_W4_TWO_MINUTES_S:
	ld	r3,number_of_people
	orl	r3,r3,r3
	jnz	ISR_STATE_W4_TWO_MINUTES_ST_people
	ld	r3,flag_BT
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_T_3
	ld	r3,flag_BS
	orl	r3,r3,r3
	jz	ISR_set_STATE_W4_BUTTONS
	jmp	ISR_set_STATE_W4_FIFTEEN_S_3


	/********************************************************/
        /*							*/
        /*							*/
        /*							*/
        /********************************************************/
	
ISR:
        st      r0, Psw_main_link
	;ld	r2,ISR_STATUS_table
	ld	r0, ISR_STATUS_index
	ld	r1, LIGHTS_word
	ld	r2, BLINKS_word
	ldi	r3, #low(ISR_STATUS_table)
	ldih	r3, #high(ISR_STATUS_table)
	ld	r7,[r3,r0]
	
ISR_set_STATE_W4_BUTTONS:
	ldi	r0,#0
	st	r0,ISR_STATUS_index
	mov	r2,r0
	jmp	ISR_State_End
	
ISR_STATE_W4_BUTTONS:
	ld 	r1,MASK_GS_reset_RS_set_GT_reset_RT_set
	ld	r3,flag_BS
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_S_2
	ld	r3,flag_BT
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_T_2
	jmp	ISR_State_End
	
ISR_set_STATE_ALERT:
	ld 	r1,MASK_GS_set_RS_set_GT_set_RT_set
	mov	r2,r1
	ldi	r3,#5
	st	r3,ISR_STATUS_index
	jmp	ISR_State_End
ISR_STATE_ALERT:
	jmp	ISR_State_End
	
ISR_State_End:
	xrl	r1,r1,r2
	st	r1,LIGHTS_word
	st	r2,BLINKS_word
	ld	r3,ADDR_IO
	st	r1,[r3,#0]

	;ldi     r3,#1
        ld      r2,ADDR_NCS_EXT
        ;st      r3,[r2,#0]
        ld      r3,[r2,#0]
        ld      r0, Psw_main_link
        iret	
	
	
	
	/*********** RAMO BT ***********/
	
ISR_STATE_W4_FIFTEEN_T:
	ld	r3,Timer
	orl	r3,r3,r3
	jz	ISR_set_STATE_W4_TWO_MINUTES_T		;jump if T15=0
	ld	r3,flag_BS
	orl	r3,r3,r3
	jz	ISR_STATE_W4_FIFTEEN_T_eval_BT
	ld 	r3,MASK_RS_set
	orl	r2,r2,r3
	ld	r3,Timer
	dec	r3
	st	r3,Timer
	jmp	ISR_State_End
ISR_STATE_W4_FIFTEEN_T_eval_BT:
	ld	r3,flag_BT
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_T_1		;jump if flag_BT=0
	ld	r3,Timer
	dec	r3
	st	r3,Timer
	jmp	ISR_State_End

ISR_set_STATE_W4_FIFTEEN_T_3:
	ld 	r3,MASK_RT_reset
	anl	r2,r2,r3
ISR_set_STATE_W4_FIFTEEN_T_2:
	ld 	r3,MASK_GT_set
	orl	r1,r1,r3
	ld 	r3,MASK_RT_reset
	anl	r1,r1,r3
	ldi	r0,#3
	st	r0,ISR_STATUS_index
ISR_set_STATE_W4_FIFTEEN_T_1:
	ldi	r3, #0xF
	;ldi	r3,#0x8
	st	r3,Timer
	ldi	r3,#0
	st	r3,flag_BT
	ld 	r3,MASK_GT_set
	jmp	ISR_State_End	
	
ISR_set_STATE_W4_TWO_MINUTES_T:
	ldi	r3,#4
	st	r3,ISR_STATUS_index
	ld	r3,time_2min
	st	r3,Timer
	ld 	r3,MASK_RT_set
	orl	r1,r1,r3
	ld 	r3,MASK_GT_reset
	anl	r1,r1,r3
	jmp	ISR_State_End

ISR_STATE_W4_TWO_MINUTES_T:
	ld	r3,number_of_people
	orl	r3,r3,r3
	jnz	ISR_STATE_W4_TWO_MINUTES_ST_people
	ld	r3,flag_BS
	orl	r3,r3,r3
	jnz	ISR_set_STATE_W4_FIFTEEN_S_3
	ld	r3,flag_BT
	orl	r3,r3,r3
	jz	ISR_set_STATE_W4_BUTTONS
	jmp	ISR_set_STATE_W4_FIFTEEN_T_3

	
	
ISR_STATE_W4_TWO_MINUTES_ST_people:	
	ld	r3,flag_BT
	orl	r3,r3,r3
	jz	ISR_STATE_W4_TWO_MINUTES_ST_people_eval_bs
	ld 	r3,MASK_RT_set
	orl	R2,r2,r3
ISR_STATE_W4_TWO_MINUTES_ST_people_eval_bs:
	ld	r3,flag_BS
	orl	r3,r3,r3
	jz	ISR_STATE_W4_TWO_MINUTES_ST_people_eval_2min
	ld 	r3,MASK_RS_set
	orl	r2,r2,r3
ISR_STATE_W4_TWO_MINUTES_ST_people_eval_2min:
	ld	r3,Timer
	orl	r3,r3,r3
	jz	ISR_set_STATE_ALERT
	dec	r3
	st	r3,Timer
	jmp	ISR_State_End



