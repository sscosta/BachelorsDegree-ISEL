/* Copyright(C) 1997, Swedish Institute of Computer Science */

#define FD_DUP_RANGE	0
#define FD_RANGE_OO	1
#define FD_RANGE_OC	2
#define FD_RANGE_CO	3
#define FD_RANGE_CC	4
#define FD_SETADD	5
#define FD_SETSUB	6
#define FD_SETMOD	7
#define FD_COMPL_T	8
#define FD_COMPL_D	9
#define FD_UNION_TT	10
#define FD_UNION_TD	11
#define FD_UNION_DT	12
#define FD_UNION_DD	13
#define FD_INTER_TT	14
#define FD_INTER_TD	15
#define FD_INTER_DT	16
#define FD_INTER_DD	17
#define FD_QVAL		18
#define FD_ADD		19
#define FD_SUB		20
#define FD_MULT_IMM	21
#define FD_DIVD_IMM	22
#define FD_DIVU_IMM	23
#define FD_MOD		24
#define FD_VAL		25  
#define FD_VAL_0	26  
#define FD_VAL_1	27  
#define FD_VAL_2	28  
#define FD_DOM		29  
#define FD_DOM_0	30  
#define FD_DOM_1	31  
#define FD_DOM_2	32  
#define FD_MIN		33  
#define FD_MIN_0	34  
#define FD_MIN_1	35  
#define FD_MIN_2	36  
#define FD_MAX		37  
#define FD_MAX_0	38  
#define FD_MAX_1	39  
#define FD_MAX_2	40  
#define FD_CONST        41  
#define FD_MULT_VAL	42
#define FD_DIVD_VAL	43
#define FD_DIVU_VAL	44
#define FD_PRUNE_RANGE_OO	45
#define FD_PRUNE_RANGE_OC	46
#define FD_PRUNE_RANGE_CO	47
#define FD_PRUNE_RANGE_CC	48
#define FD_PRUNE_TERM_COMPL	49
#define FD_PRUNE_COMPL	50
#define FD_PRUNE_TERM	51
#define FD_PRUNE	52
#define FD_TEST_RANGE_OO	53
#define FD_TEST_RANGE_OC	54
#define FD_TEST_RANGE_CO	55
#define FD_TEST_RANGE_CC	56
#define FD_TEST_TERM_COMPL	57
#define FD_TEST_COMPL	58
#define FD_TEST_TERM	59
#define FD_TEST		60
#define FD_CHECK_UNION	61
#define FD_CHECK	62
#define FD_UNIONOF	63
#define FD_UNIONOF_NEXT	64
#define FD_CARD		65
#define FD_CARD_0	66  
#define FD_CARD_1	67  
#define FD_CARD_2	68  
#define FD_SWITCH	69
#define FD_POPJ		70
#define FD_GDOM_1	71
#define FD_ERROR	72
#define FD_SETNEG	73
#define FD_SETPLUS	74
#define FD_SETMINUS	75
#define FD_SUBTRACT_TT	76
#define FD_SUBTRACT_TD	77
#define FD_SUBTRACT_DT	78
#define FD_SUBTRACT_DD	79
#define FD_MULT_QVAL	80
#define FD_DIVD_QVAL	81
#define FD_DIVU_QVAL	82

/* hpux seems to wreck "&&" labels in dynamically loaded code */
/* NeXT, although using gcc, does not provide gcc extensions */
/* On 'x86, incredibly, gcc seems to emit incorrect code */
/* Permanently disabled for sanity of save/restore */
#if __GNUC__ >= 2000 && !(defined(SPDLL) && (__hpux__ || hpux || __hpux)) && !__NeXT__ && !__i386__


static void **global_insn_table;

#define CaseX(Insn) Insn##x
#define Prefetch      void *nextpc = (void *)(*code++)
#define DispatchFirst for (;;)
#define DispatchHead  goto *(void *)((*code++) + (TAGGED)&&FD_DUP_RANGEx);
#define Dispatch      goto *(void *)((TAGGED)nextpc + (TAGGED)&&FD_DUP_RANGEx);
#define DispatchDef \
static void *insn_table[] = { \
   &&FD_DUP_RANGEx, \
   &&FD_RANGE_OOx, \
   &&FD_RANGE_OCx, \
   &&FD_RANGE_COx, \
   &&FD_RANGE_CCx, \
   &&FD_SETADDx, \
   &&FD_SETSUBx, \
   &&FD_SETMODx, \
   &&FD_COMPL_Tx, \
   &&FD_COMPL_Dx, \
   &&FD_UNION_TTx, \
   &&FD_UNION_TDx, \
   &&FD_UNION_DTx, \
   &&FD_UNION_DDx, \
   &&FD_INTER_TTx, \
   &&FD_INTER_TDx, \
   &&FD_INTER_DTx, \
   &&FD_INTER_DDx, \
   &&FD_QVALx, \
   &&FD_ADDx, \
   &&FD_SUBx, \
   &&FD_MULT_IMMx, \
   &&FD_DIVD_IMMx, \
   &&FD_DIVU_IMMx, \
   &&FD_MODx, \
   &&FD_VALx, \
   &&FD_VAL_0x, \
   &&FD_VAL_1x, \
   &&FD_VAL_2x, \
   &&FD_DOMx, \
   &&FD_DOM_0x, \
   &&FD_DOM_1x, \
   &&FD_DOM_2x, \
   &&FD_MINx, \
   &&FD_MIN_0x, \
   &&FD_MIN_1x, \
   &&FD_MIN_2x, \
   &&FD_MAXx, \
   &&FD_MAX_0x, \
   &&FD_MAX_1x, \
   &&FD_MAX_2x, \
   &&FD_CONSTx, \
   &&FD_MULT_VALx, \
   &&FD_DIVD_VALx, \
   &&FD_DIVU_VALx, \
   &&FD_PRUNE_RANGE_OOx, \
   &&FD_PRUNE_RANGE_OCx, \
   &&FD_PRUNE_RANGE_COx, \
   &&FD_PRUNE_RANGE_CCx, \
   &&FD_PRUNE_TERM_COMPLx, \
   &&FD_PRUNE_COMPLx, \
   &&FD_PRUNE_TERMx, \
   &&FD_PRUNEx, \
   &&FD_TEST_RANGE_OOx, \
   &&FD_TEST_RANGE_OCx, \
   &&FD_TEST_RANGE_COx, \
   &&FD_TEST_RANGE_CCx, \
   &&FD_TEST_TERM_COMPLx, \
   &&FD_TEST_COMPLx, \
   &&FD_TEST_TERMx, \
   &&FD_TESTx, \
   &&FD_CHECK_UNIONx, \
   &&FD_CHECKx, \
   &&FD_UNIONOFx, \
   &&FD_UNIONOF_NEXTx, \
   &&FD_CARDx, \
   &&FD_CARD_0x, \
   &&FD_CARD_1x, \
   &&FD_CARD_2x, \
   &&FD_SWITCHx, \
   &&FD_POPJx, \
   &&FD_GDOM_1x, \
   &&FD_ERRORx, \
   &&FD_SETNEGx, \
   &&FD_SETPLUSx, \
   &&FD_SETMINUSx, \
   &&FD_SUBTRACT_TTx, \
   &&FD_SUBTRACT_TDx, \
   &&FD_SUBTRACT_DTx, \
   &&FD_SUBTRACT_DDx, \
   &&FD_MULT_QVALx, \
   &&FD_DIVD_QVALx, \
   &&FD_DIVU_QVALx, \
0}; \
if (!Global) {global_insn_table = insn_table; return 0;}
#define DispatchLabel(I) ((TAGGED)global_insn_table[I] - (TAGGED)global_insn_table[0])

#else

#define CaseX(Insn) case Insn
#define Prefetch
#define DispatchFirst for (;;)
#define DispatchHead  switch (*code++)
#define Dispatch      continue
#define DispatchDef   if (!Global) {return 0;}
#define DispatchLabel(I) (I)

#endif
