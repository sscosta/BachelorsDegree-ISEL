/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif /* MULTI_SP_AWARE */

long SPCDECL prolog_fd_statistics MAGIC (HIDDEN_PROTO long key)
{
  long l=0;
  
  switch (key)
    {
    case 0: l = fd.resumptions; fd.resumptions = 0; break;
    case 1: l = fd.entailments; fd.entailments = 0; break;
    case 2: l = fd.prunings; fd.prunings = 0; break;
    case 3: l = fd.failures; fd.failures = 0; break;
    case 4: l = fd.constraints; fd.constraints = 0; break;
    }
  return l;
}


long SPCDECL prolog_fd_debugging MAGIC (HIDDEN_PROTO_VOID)
{
  return fd.debugging;
}



void SPCDECL prolog_fd_debug MAGIC (HIDDEN_PROTO long d)
{
  fd.debugging = d;
}
