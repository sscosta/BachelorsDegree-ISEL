/*
   Support for global references
   ch, 9.8.95
*/

#include <sicstus/sicstus.h>

#if 1
#include "chr_glue.h"
#else
extern SP_term_ref global_term_ref(long i);
extern SP_term_ref global_term_ref_0(void);
extern SP_term_ref global_term_ref_1(void);
#endif

SP_term_ref SPCDECL global_term_ref(SPAPI_ARG_PROTO_DECL
				    long i)
{
  if (i>=0 && i<=1) return( i); /* xref fli_stack_init, sicstus.c */
  else {
    SP_term_ref culprit = SP_new_term_ref();
    SP_term_ref argno = SP_new_term_ref();
    SP_term_ref domain = SP_new_term_ref();
    SP_term_ref t1 = SP_new_term_ref();

    SP_put_integer(culprit,i);
    SP_put_integer(argno,1);
    SP_put_string(domain,">=0 & =<1");
    SP_put_variable(t1);
    SP_cons_functor(t1, SP_atom_from_string("global_term_ref"), 2,
		    culprit, t1);
    SP_cons_functor(t1, SP_atom_from_string("domain_error"), 4,
		    t1, argno, domain, culprit);
    SP_raise_exception(t1);
    return 0;
  }
}

SP_term_ref SPCDECL global_term_ref_0(SPAPI_ARG_PROTO_DECL0)
{
  (void)(SPAPI_ARG0-0);
  return(0);
}

SP_term_ref SPCDECL global_term_ref_1(SPAPI_ARG_PROTO_DECL0)
{
  (void)(SPAPI_ARG0-0);
  return(1);
}


