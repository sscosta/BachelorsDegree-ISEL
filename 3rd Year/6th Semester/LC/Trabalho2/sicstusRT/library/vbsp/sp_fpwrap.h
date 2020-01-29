#ifndef SP_FPWRAP_INCLUDED
#define SP_FPWRAP_INCLUDED
/*
  Problem:
  SICStus intentionally divides by (floating point) zero as part of
  its initialization (SP_initialize). By default Win32 does not trap
  floating point exceptions so this divide by zero causes a pending
  exception but is otherwise harmless.

  Some applications change the default floating point exception
  behaviour which causes problems when embedding SICStus (perhaps as
  part of some kind of plug-in). A typical symptom is

  Exception: 0xc000008e (EXCEPTION_FLT_DIVIDE_BY_ZERO)

  This problem has been observed with Filemaker, Rational Rose and
  Visio. Some of these exhibit this problem when using the Visual
  Basic module (vbsp.dll) from VBA. The Visual Basic Module now uses
  this header and can serve as an example of how to use these macros.

  Solution:
  Wrap calls to SICStus API routines so that the floating point
  exception behaviour is as Win32 default during the call to SICStus.

  The macros below hides the details of this. If your code previously
  did some call to a SICStus API routine, e.g.,

  if (!SP_initialize(...)) { ... }

  It should now be wrapped as:

  SP_FP_WRAP_BEGIN;
  if (SP_initialize(...)) { ... }
  SP_FP_WRAP_END;

  Note that, if you do callbacks to the hosting application then you
  may need to ensure that the floating point exception behaviour is
  restored during the call back to the hosting application. In that
  case you may need to use SP_GetFPState() etc. directly.

*/

#include <float.h>

typedef unsigned int SP_FPState;

/* get FPU flags */
#define SP_GetFPState() _controlfp(0,0)

/* turn off any trapping of FPU exceptions */
#define SP_SetSaneFPState() _controlfp(_MCW_EM, _MCW_EM)

/* Clear any pending exception and restore saved FPU state */
#define SP_RestoreFPState(FP_STATE) do{_clearfp(); _controlfp((FP_STATE), _MCW_EM);}while(0)



/* Convenient wrappers, hiding even more of the details. Use as:

 SP_FP_WRAP_BEGIN;
 ...
 if (!SP_initialize(...)) ...
 ...
 ... SP_query(...) 
 ...
 SP_FP_WRAP_END;

 Alternatively do it the hard way:

{
  SP_FPState sp_fp_state = SP_GetFPState(); // Get callers FP exception handling state
  SP_SetSaneFPState(); // Ensure Win32 default (lack of) FP exception handling
  ...
  if (!SP_initialize(...)) ...
  ...
  ... SP_query(...)
  ...
  SP_RestoreFPState(sp_fp_state); // Restore caller FP exception handling
}

*/

#define SP_FP_WRAP_BEGIN { SP_FPState sp_fp_state = SP_GetFPState(); SP_SetSaneFPState();
#define SP_FP_WRAP_END SP_RestoreFPState(sp_fp_state); }

#endif /* SP_FP_WRAP_INCLUDED */
