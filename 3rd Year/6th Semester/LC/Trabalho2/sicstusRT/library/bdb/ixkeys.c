#include <sicstus/sicstus.h>

#include <db.h>                 /* needed for types used in bdb_glue.h */
#include "bdb.h"                /* needed for types used in bdb_glue.h */

#include "bdb_glue.h"           /* [PM] 3.9 splfr-generated */

#define NA -1                   /* not applicable */
#define NI -2                   /* instantiatedness */

/*
  ixkeys(spec, term, list): list is the list of arguments of term in the
  `+' positions of spec.  Compound terms are represented by their functors.
  The return value is the number of `+' positions in spec or NA if the
  functors of spec and term are not equal or NI if term contains a variable
  on some `+' position.
 */
long ixkeys(SP_term_ref spec, SP_term_ref term, SP_term_ref list)
{
  unsigned long sname, tname, plus;
  int sarity, tarity, i;
  long ret = 0;
  SP_term_ref arg = SP_new_term_ref(), tmp = SP_new_term_ref();

  SP_get_functor(spec, &sname, &sarity);
  SP_get_functor(term, &tname, &tarity);
  if (sname != tname || sarity != tarity) return NA;

  plus = SP_atom_from_string("+");
  for (i = sarity; i > 0; --i) { /* sarity and i are never used together */
    unsigned long t;

    SP_get_arg(i, spec, arg);
    SP_get_atom(arg, &t);       /* no error checking */
    if (t != plus) continue;

    SP_get_arg(i, term, arg);
    switch (SP_term_type(arg)) {
    case SP_TYPE_VARIABLE:
      return NI;
    case SP_TYPE_COMPOUND:
      SP_get_functor(arg, &tname, &tarity);
      SP_put_integer(tmp, (long)tarity);
      SP_cons_list(list, tmp, list);
      SP_put_atom(arg, tname);
      break;
    }
    SP_cons_list(list, arg, list);
    ++ret;
  }
  return ret;
}
