/*
 * Copyright (c) 1998-99 SICS 
 */

/* 3.9, Only one thread may call SP API and (some future version of
   se.sics.jasper will ensure this). Thus no need for Java side
   monitor handling.
*/
#define NEW_WORLD_ORDER 1


/*
  Scratch area for [PM]:
  Things that need SP-API: (- means need to pass on SP-API but does not use it.)
  jasperi_process_meta_args
  SPJavaGlueGetObjectM
  SPJavaGlueErrorM
  SPJavaGlueSPTermM
  SPJavaGlueGetNativeTermRefM-
  SPJavaGlueGetNativeTermRefInJavaContextM
  SPJavaGluePropagateJavaExceptionM
  SPJavaGlueSPCanonicalAtomM-
  SPJavaGlueGetAtomM-
  SPJavaGluePostPutCharsM-
  SPJavaGlueStringBufferM-
  SPJavaGluePostToStringM-
  SPJavaGluePostPutStrM
  jasperi_call_static1_C
  jasperi_call_instance1_C
  jasper_int_handle_exception-
  termref_to_jobject
  jasperi_lookup_class_from_obj-
  jasperi_lookup_method-
  init_jasper
  (deinit_jasper)
  jasperi_jvm_C
  jasper_int_get_jnienv-
  jasper_int_raise_exception
  jasperi_initialize_pl_C-
  jasperi_initialize
  jasper_strcat
  jasperi_new_simple_object-
  jasperi_lookup_method-
  (jasperi_deinitialize_C)
  jasperi_object_class_name_C
  jasperi_create_global_ref_C-
  jobject_to_termref
  jasperi_create_local_ref_C-
  jasperi_delete_global_ref_C-
  jasperi_delete_local_ref_C-
  jasperi_is_same_object_C-
  jasperi_is_instance_of_C-
  jasperi_lookup_class-

  jasper_push_context (really only if DBG)

  Call tree
  "FOREIGN": A foreign predicate (called from prolog)
  "API": Calls SICStus API functions
  "*": Accounted for already in some other call tree node.
  "?": I have not checked this node yet.

  jasperi_process_meta_args [API]         -> SPJavaGlueGetObjectM [API]  -> SPJavaGlueErrorM [API]
                                          -> SPJavaGlueSPTermM [API]     -> SPJavaGlueErrorM*
                                          -> SPJavaGlueGetNativeTermRefM -> SPJavaGlueErrorM*
                                                                        -> SPJavaGluePropagateJavaExceptionM [API]
                                          -> SPJavaGlueSPCanonicalAtomM  -> SPJavaGlueErrorM*
                                          -> SPJavaGlueGetAtomM          -> SPJavaGlueGetAtomM*
                                                                        -> SPJavaGluePropagateJavaExceptionM*
                                          -> SPJavaGluePostPutCharsM     -> SPJavaGlueErrorM*
                                          -> SPJavaGlueStringBufferM     -> SPJavaGlueErrorM*
                                          -> SPJavaGluePostToStringM     -> SPJavaGlueErrorM*
                                          -> SPJavaGluePostPutStrM [API] -> SPJavaGlueErrorM*
                                          -> SPJavaGlueErrorM*
                                          -> SPJavaGluePropagateJavaExceptionM*
  jasperi_call_static1_C [FOREIGN,API]       <same as jasperi_call_instance1_C>
  jasperi_call_instance1_C [FOREIGN,API]  -> jasperi_process_meta_args*
                                          -> jasper_int_handle_exception -> jasper_DescribeException (BOGUS!)
                                                                         -> jasper_int_raise_exception*
                                          -> termref_to_jobject [API]
                                          -> jasperi_lookup_class_from_obj -> jasper_int_handle_exception*
                                          -> jasperi_lookup_method       -> jasper_int_handle_exception*
                                          -> jasper_leave_context_monitor (no-op)
                                          -> SPJavaGlueGetNativeTermRefInJavaContextM*
                                          -> jasper_int_handle_exception*
                                          -> jasper_enter_context_monitor (no-op)
                                          -> SPJavaGluePropagateJavaExceptionM*
                                          -> jasper_pop_context -> jasper_pop_context1 -> jasper_return_to_prolog (no-op)

  init_jasper [FOREIGN,API]
  deinit_jasper [FOREIGN]
  jasperi_jvm_C [FOREIGN,API] -> jasper_int_get_jnienv -> jasper_int_raise_exception [API]
  jasperi_initialize_pl_C [FOREIGN] -> jasperi_initialize [API] -> jasper_set_jvm
                                                                -> jasper_strcat [API]
                                                                -> jasper_int_get_jnienv*
                                                                -> jasper_get_sicstus_object [SPGLOBAL!]
                                                                -> jasperi_new_simple_object -> jasperi_lookup_class*
                                                                                             -> jasperi_lookup_method -> jasper_int_handle_exception*
                                                                                             -> jasper_int_handle_exception*
                                                                -> jasper_enter_sicstus_monitor -> jasper_ensure_monitor


  jasperi_deinitialize_C [FOREIGN]
  jasperi_object_class_name_C [FOREIGN,API] -> termref_to_jobject*
                                            -> jasper_int_handle_exception*
  jasperi_create_global_ref_C [FOREIGN] -> termref_to_jobject*
                                        -> jobject_to_termref [API]
  jasperi_create_local_ref_C [FOREIGN] <same as jasperi_create_global_ref_C>
  jasperi_delete_global_ref_C [FOREIGN] -> termref_to_jobject*
                                         -> jasper_int_handle_exception*
  jasperi_delete_local_ref_C [FOREIGN] <same as jasperi_delete_global_ref_C>
  jasperi_is_same_object_C [FOREIGN] -> termref_to_jobject*
                                     -> jasper_int_handle_exception*
  jasperi_is_instance_of_C [FOREIGN] -> termref_to_jobject*
                                     -> jasperi_lookup_class -> jasper_int_handle_exception*

                                  
*/


/* [PD] threadsafe jasper-server hack */
#define THREADSERVER 1

/* [PM] 3.8.5 Must be zero!! the threading support added in 3.8.5beta1 does *not* work!  */
#define JASPER_THREADING 0

/* [PM] 3.8.6 On Solaris and Linux JDK 1.3 and 1.3.1 beta MonitorExit
   will sometimes return -1 when there is a pending exception. 
   This is tracked as
   <http://developer.java.sun.com/developer/bugParade/bugs/4458083.html>.
   (It turns out that the book may be incorrect since the spec does
    not mention MonitorExit as being safe to call).

   I sent (almost) the following bug report:

     There appears to be a bug in the Hotspot JNI function MonitorExit in
     JDK 1.3 (at leaston Linux, I have not found the Hotspot source for
     1.3.0) and JDK 1.3.1 (certainly on Linux but from the sources it seems
     to be platform independent).

     The documentation (The Java Native Interface by S. Liang) says that
     MonitorExit is one of the few JNI functions that can safely be called
     while an exception is pending (p. 162). Furthermore it says that
     MonitorExit "Returns a negative number if and only if an invocation of
     this function has thrown an exception." (p. 259). I take this to mean
     that MonitorExit should not return a negative number just because
     there is an exception pending.

     The observed behavior is that if there is an exception pending then
     MonitorExit will return -1. If my code is changed to do an
     ExceptionClear() before calling MonitorExit() then zero is returned as
     expected (i.e., there is nothing wrong with the call to MonitorExit()).

     The source 1.3.1beta/hotspot1.3.1/src/share/vm/prims/jni.cpp looks like:

     JNI_ENTRY(jint, jni_MonitorExit(JNIEnv *env, jobject jobj))
       ...
       ObjectSynchronizer::jni_exit(obj(), CHECK_(JNI_ERR));
       return JNI_OK;

     Expanding CHECK_(JNI_ERR) gives:

       ObjectSynchronizer::jni_exit(obj(), THREAD);
       if (HAS_PENDING_EXCEPTION) return JNI_ERR;
       (0);

     However, this does not take into account that the pending exception
     *after* the call to jni_exit may have been present before the call to
     jni_exit and is therefore not an indication that something went wrong
     in jni_exit().
*/
#ifndef MONITOR_EXIT_EXCEPTION_BUG
#define MONITOR_EXIT_EXCEPTION_BUG 1
#endif

#define JASPER_SOURCE 1
#include "jasper.h"
#include "jasper_glue.h"        /* [PM] 3.9 splfr-generated */

#include <stdlib.h>
#include <string.h>


#ifdef _MSC_VER
#define BreakPoint()        _asm { int 3h }
#endif

#if DBG && 0
#define DbgBreakPoint() BreakPoint()
#else
#define DbgBreakPoint()
#endif

#if 0                           /* done in sicstus.h */
#if MULTI_SP_AWARE
/* This is where SP_put_list et al find the dispatch table, it is
   passed as an argument to all functions that use the SICStus API. */
#undef SICStusDISPATCHVAR /* Undefine the default from spaux.h */
#define SICStusDISPATCHVAR SPAPI_ARG_NAME
#endif /* MULTI_SP_AWARE */
#endif

/* [PM] 3.9 This a temporary solution until ENTER_C_EXT provides a stash */
#if !SP_FOREIGN_STASH
#error "jasper need SP_FOREIGN_STASH"



static struct stash_struc {
  void *key;                    /* the SP API */
  void *stash;                  /* the stash where this foreign resource can keep its state */
  } stashes[42] = {{0,0}};

static void **find_stash(SPAPI_ARG_PROTO_DECL const char*dbg_string)
{
  static int n_stashed = 0;
  struct stash_struc *p = &stashes[0];


#if DBG
  fprintf(stderr, "find_stash(api==%p, %s)\n", ((void*) SICStusDISPATCHVAR), dbg_string);
#endif

  for (; p < &stashes[n_stashed]; p++)
    {
      if (p->key == SICStusDISPATCHVAR)
        {
          return &p->stash;     /* found */
        }
    }
  /* not present, add it (p == &stashes[n_stashed]) */
  if (n_stashed < (sizeof stashes)/(sizeof *stashes))
    {
      p->key = SICStusDISPATCHVAR;
      p->stash = 0;
      n_stashed++;
      return &p->stash;
    }
  #if DBG
  fprintf(stderr, "*** FATAL ERROR find_stash(%p) overflow", SICStusDISPATCHVAR);
  abort();
  #endif
  return NULL;
}

#define SP_API_STASH(DBGSTRING) (*find_stash(SPAPI_ARG DBGSTRING))

#else  /* SP_FOREIGN_STASH */
#define SP_API_STASH(DBGSTRING) (*SPAPI_STASH_NAME)
#endif /* SP_FOREIGN_STASH */



/* [PM] 3.8.6 Support passing and receiving null object references */
#ifndef NULL_OBJREF
#define NULL_OBJREF 1
#endif

#if defined(JASPER_DBG)
#undef DBG
#define DBG JASPER_DBG
#endif

/* Used also when not DBG */
#define STRINGISIZE1(X) # X
#define STRINGISIZE(X) STRINGISIZE1(X)

/*
  Per Mildner 3.8.5 Major overhaul. See SICStus.java for more information.

  Starting with 3.8.2 we allowed arbitrary (java) threads to call into
  SICStus (in 3.9 this will be arbitrary threads). The JNI env
  structure is only valid within one native thread so we cannot cache
  the jnienv since we can be running in more than one thread over
  time. Furthermore "Java 2 SDK release 1.2 do not support creating
  more than one virtual machine instance in a single process", so we
  do not have to deal with multiple Java VMs. The consequences of this
  is that the jnienv need to be picked up dynamically using
  GetEnv/AttachCurrentThread and we can cache or otherwise obtain the
  JVM (using JNI_GetCreatedJavaVMs). Therefore we can completely
  ignore the $jvm(XXX) term passed from prolog (it so happens that XXX
  is actually a jnienv pointer which we now know is useless anyway).

  
 */


/*
  [Jojo, 000209]: Subsumes the notes below. 

  NOTES on Jasper and native threads, and how to allow calls from Prolog to
  Java and back, and forwards, and backwards and upside-down, and
  inside-out... etc.
  
  These notes are my attempt to clarify some of the issues in using Jasper
  to connect SICStus and Java, especially with respect to native threads.

  Do no rely on these notes to figure out exactly how certain versions of
  Jasper work; some of these notes describe how things should be, not how
  they actually are.

  There are two basic scenarios. 

  1. Java is the toplevel application. SICStus is loaded into the JVM as a
  native code library.

  2. SICStus is the toplevel application. Java is loaded into SICStus as a
  foreign resource.

  In the first case, a JVM already exists when SICStus comes into the
  picture. In the second case, we need to create a JVM using options
  specified by the user.
  
  At the time of writing this, it is unclear whether native-threads are
  tightly or loosely coupled with their Java thread, i.e. if it is allowed
  for a JVM implementation to multiplex several Java threads on top of a
  fixed set of native threads. Reading between the lines of the JVM spec,
  I get the feeling that this one-to-one mapping is not required, but that
  all JVMs I know of uses a hard one-to-one mapping.

  One of the trickiest issues in this interface is to balance two 
  important issues:

  1. Since the SICStus emulator is not thread-safe with respect to native
  threads, we have to prevent any two native threads from accidentally
  calling the SICStus emulator simultaneously. 

  2. However, we do not want to restrict the way Prolog and Java can
  call each other. We want to allow multi-level callbacks, for example.

  Let us study the first scenario (Java as toplevel application).

  1a. Java -> Prolog. This is equivalent to a simple call to a C-function.
      This presents no problem, as long as the Java-methods performing the
      call are synchronized on the SICStus object, such that another native
      thread cannot call Prolog until the first native thread has
      returned. This is implemented in 3.8.2.

  1b. Java -> Prolog -> Java. This is equivalent to Java calling a
      C-function which in turn makes a call to Java. This complicates
      matters:

      - What Java thread should we call?

      - Should we even allow the user to specify another Java-thread to
      call (than the one which called Java?).

      - Is it at all possible to call another Java-thread than the one
      "belonging" to the current native thread?

      The current implementation does not allow the user to specify which
      Java thread to call.
      
  1c. Java -> Prolog -> Java -> Prolog. This is where it gets really
      interesting. 

      If the 1b-scenario always calls the same Java-thread, and we always
      have a one-to-one mapping between Java-threads and native threads,
      then this is probably quite problem-free. When we return to Java, we
      will still be in the (Java-)thread which holds the lock on the
      SICStus object and we can safely call Prolog. 

      If the 1b-scenario allows the user to call another Java-thread, we
      face the situation where we're inside the JVM, but in a different
      Java-thread than the one which called Prolog. Probably even in a
      different native thread.
     
      This should be possible, as long as SICStus does not do any black magic
      regarding its C-stack. For example:

      1. No longjmp():s which goes past foreign code (i.e. C-code outside
      the SICStus C-interface).

      2. No assumptions about the C-stack except what is visible from a
      regular C-function.

      3. No thread-local data used.

      If these restrictions hold, calling Prolog recursively from another
      native thread should not be any different than doing it from the same
      native thread.

      Of course, when SICStus MT comes into life and supports native
      threads, then things become *really* interesting.


  2a. Prolog -> Java. Again, this is no problem at all. There are no
      threads at the Prolog level. 

      The discussion under 1b above may apply a little, though. One might
      want to allow the user to specify what Java-thread to call.

  2b. Prolog -> Java -> Prolog. See 1c.

 */

/* NOTES on the implementation of Jasper. See also spnative.c.
 * ===========================================================
 * (These notes should probably make it into the User's Manual
 * in some form or another).
 *
 * - Jasper can run in two modes, either with Java as a toplevel
 * application or with SICStus as toplevel application.
 *
 * - In order to avoid problems due to the fact that the SICStus
 * emulator is not reentrant (i.e. only one native thread is allowed
 * inside the emulator at any given time), we must assure that when
 * SICStus is called, it is always done from the same Java (native)
 * thread.
 * [Jojo, 000209] This is too restrictive. It is probably enough that 
 * no two native threads call the emulator at the same time.
 * 
 * - JNI maintains what is known as a environment pointer
 * (jnienv). This uniquely identifies which Java thread the native
 * code is currently attached to, and so also determines which Java
 * thread will be executing any Java method calls made.  So, to assure
 * that SICStus is always called from the same Java thread, we keep
 * our own copy of jnienv. This enables us to check if the current
 * Java thread is "allowed" to call SICStus. (this check is however
 * not fully implemented yet). 
 * [Jojo] See note above.
 *
 * - When SICStus is loaded into a JavaVM, spInitialize() will be
 * called. spInitialize() will store the jnienv with which it was
 * called, so only the thread which created the SICStus object will be
 * allowed to call SICStus native methods. When Java is loaded into
 * SICStus, jasper_initialize() is called, either explicitly by the
 * user, or implicitly by the glue code of a foreign Java-method
 * (i.e. declared by foreign(<Method>,java,<Pred>)).
 *
 * jasper_initialize() can be called repeatedly to get a handle to
 * jnienv (which on the Prolog side is a term
 * '$jvm'(<ptr>)). jasper_deinitialize() can be called to unload the
 * JVM. This is however not implemented (as of JDK 1.2).  
 *
 * Unresolved issues: When Java is started from Prolog, how does the
 * Java code get a reference to the SICStus object?
 * 
 * Presumably this has to be done by not only creating a JVM, but also
 * creating a SICStus object which can be accessed by Java methods called
 * from Prolog.
 */

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (!(TRUE))
#endif

/* These belong in a header file. This declarations is copied in spnative.c */

#if !NEW_WORLD_ORDER

extern volatile jobject jasper_sp_global; /* the global SICStus object, set from spnative.c */
extern int jasper_enter_sicstus_monitor(JNIEnv *jnienv);
extern int jasper_leave_sicstus_monitor(JNIEnv *jnienv);
#endif 

static jvalue
SPCDECL CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...);



/************************ State ***************************/

#if MULTI_SP_AWARE

#define INIT_STASH() ((SP_API_STASH("INITING")) = (void *)SP_malloc(sizeof(struct jasper_state)))
#define SP_STATE(DBGSTRING) (*(struct jasper_state *)SP_API_STASH(DBGSTRING))

#define DECLARE_STATE_BEGIN struct jasper_state {
#define DECLARE_STATE_VARIABLE(TYPE, NAME) \
   TYPE NAME;                   /* struct field */
#define STATE_VARIABLE(NAME) (SP_STATE(#NAME)).NAME
#define DECLARE_STATE_END };

#else /* !MULTI_SP_AWARE */

#define INIT_STASH() 

#define DECLARE_STATE_BEGIN
#define DECLARE_STATE_VARIABLE(TYPE, NAME) \
   static TYPE NAME;
#define STATE_VARIABLE(NAME) NAME
#define DECLARE_STATE_END

#endif /* !MULTI_SP_AWARE */

#define INIT_STATE_VARIABLE(NAME, VALUE) \
   STATE_VARIABLE(NAME) = (VALUE)


/* [PM] 3.9
  For each SP run-time we keep the following state (together with the API dispatch table):
  atoms
  jvm         The same for all since we only have one JavaVM
  jnienv?     Now that SP should only be called from one thread we can
              keep the (thread specific) jnienv 
//  [PD] 3.9 jasper_sp_global is eliminated.
  jasper_sp_global       The SICStus object corresponding to this SP-run-time (legacy name)
  
*/
DECLARE_STATE_BEGIN

DECLARE_STATE_VARIABLE(JavaVM *, jvm)
DECLARE_STATE_VARIABLE(JNIEnv *, jnienv)
#if 0             /* [PD] 3.9 Use sp_(set/get)_jasper_magic() instead */
DECLARE_STATE_VARIABLE(jobject, jasper_sp_global)
#endif
DECLARE_STATE_VARIABLE(volatile unsigned long, context_counter)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_atom)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_boolean)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_byte)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_char)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_chars)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_dollar_java_object)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_double)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_false)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_float)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_integer)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_jvm)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_long)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_object)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_plus_sign)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_short)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_string)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_term)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_true)
#if THREADSERVER
#if 0      /* [PD] 3.9 use sp_(set/get)_jasper_threadservermode() instead */
DECLARE_STATE_VARIABLE(jboolean, threadservermode) /* [PD] 3.9 threadserver */
#endif
#endif

DECLARE_STATE_END

#if 0                           /* [PM] 3.9 */
   /* We don't support more than one JVM at a time. */
   #define MAX_JVMS 1

   /* Array of created Java VMs, as returned by
      JNI_GetCreatedJavaVMs(). */
   static JavaVM *jvms[MAX_JVMS];
   static jint numjvms, numjvms_prev;
#endif

#if 0                           /* [PM] 3.9 */
   /*  If we have created a JVM, keep a pointer to it so we know if we 
    *  need to destroy anything when unloading the resource.
    */
   static JavaVM *jvm;
   static JavaVM *created_jvm;     /* JVM created by us */
#endif

#if !NEW_WORLD_ORDER

   #define SICSTUS_MONITOR_IS_OBJECT 1
   #if SICSTUS_MONITOR_IS_OBJECT
   #define jasper_sicstus_monitor jasper_sp_global
   #else /* !SICSTUS_MONITOR_IS_OBJECT */
   /* Global ref to SICStus class used to ensure exclusive access to SICStus runtime */
   static volatile jclass jasper_sicstus_monitor = NULL;
   #endif /* !SICSTUS_MONITOR_IS_OBJECT */

   static volatile long jasper_sicstus_lock_count = 0; /* DBG */
#endif /* !NEW_WORLD_ORDER */


#if 0 /* 3.8.5 [PM] does not make sense when SICStus can be called
         from arbitrary threads (jnienv is thread specific). */
/* Reference to the Java thread to which we are attached. */
static JNIEnv *jnienv;
#endif




/***************************************************/
#if !NEW_WORLD_ORDER
static jobject jasperi_new_simple_object(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, char *className);
#endif

#if NEW_WORLD_ORDER
static jobject jasperi_new_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *env);
#endif


#if THREADSERVER                /* [PD] 3.9 */
static jobject jasper_get_server(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv);
#endif


/* [PM] 3.9 These are rewritten from the 3.8.6 versions in jasper.h
SPJavaGlueErrorM
SPJavaGluePropagateJavaExceptionM
SPJavaGlueGetObjectM
SPJavaGlueSPTermM
SPJavaGlueGetNativeTermRefM
SPJavaGlueGetNativeTermRefInJavaContextM
SPJavaGlueSPCanonicalAtomM
SPJavaGlueGetAtomM
SPJavaGluePostPutCharsM
SPJavaGlueStringBufferM
SPJavaGluePostToStringM
SPJavaGluePostPutStrM
*/

/* err_msg can be NULL */
static void SPJavaGlueErrorM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg)
{
#if DBG+0 > 0
  fprintf(stderr, "SPJavaGlueErrorM: arg# %d, `%s'\n", argnum, ((err_msg != NULL) ? err_msg : "<<NULL>>"));
#endif
  if (jnienv && (*jnienv)->ExceptionCheck(jnienv))
    {
#if DBG
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
#endif 
      (*jnienv)->ExceptionClear(jnienv);
    }
  {
    SP_term_ref excp_term = SP_new_term_ref();
    SP_put_string(excp_term, ((err_msg != NULL) ? err_msg : "Error in Java glue"));
    SP_raise_exception(excp_term);
  }
}

/* 0 if there was no java exception to propagate.
   err_msg can be NULL
 */
static int SPJavaGluePropagateJavaExceptionM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg /*IGNORED*/)
{
  jobject java_excp = NULL;
  
  java_excp = (*jnienv)->ExceptionOccurred(jnienv);
  if (java_excp)
    {
      SP_term_ref excp_term = SP_new_term_ref();
      SP_term_ref excp_obj = SP_new_term_ref();

#if DBG
      fprintf(stderr, "SPJavaGluePropagateJavaExceptionM: arg# %d, `%s'\n", argnum, err_msg);
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
      /* abort(); */
#endif

      SP_put_integer(excp_obj, (long)(*jnienv)->NewGlobalRef(jnienv,java_excp));
      SP_cons_functor(excp_term, SP_atom_from_string("$java_object"), 1, excp_obj);
      SP_raise_exception(excp_term);
      (*jnienv)->ExceptionClear(jnienv); /* not needed since ExceptionDescribe clears it */
    }

  if (java_excp)
    {
      (*jnienv)->DeleteLocalRef(jnienv, java_excp);
      return 1;
    }
  return 0;
}


static int SPJavaGlueGetObjectM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref tobj, jobject *pjobject, int argnum, const char *err_msg)
{
  long obj_ptr;
  int rc;
  jobject jobj;

  if (!( SP_get_arg(1, tobj, tobj)
         && SP_get_integer(tobj,&obj_ptr) ))
    {
      rc = 0;
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
      jobj = NULL;
    }
  else
    {
      rc = 1;
      jobj = (obj_ptr ? (jobject) obj_ptr : NULL );
    }
  *pjobject = jobj;
  return rc;
}

#if THREADSERVER
static jobject SPJavaGlueSPTermM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref term, int argnum, const char *err_msg, int init, jboolean new_style_interface)
#else
static jobject SPJavaGlueSPTermM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref term, int argnum, const char *err_msg, int init)
#endif
{
  jvalue result;
  jboolean hasException;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);
#if THREADSERVER
  int threadservermode;
#endif
  
  (void)err_msg;

#if DBG
  fprintf(stderr, "SPJavaGlueSPTermM %s term=%d \n", (init ? "init from" : "ignored"), term);
#endif

  if (!sp_obj) {
    result.l = NULL;
    goto finish;
  }

#if THREADSERVER
  if(!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) {
    result.l = NULL;
    goto finish;
  }
#endif

  if (init)
    {
      result = CallMethodByName(jnienv,
                                &hasException,
#if 0
                                STATE_VARIABLE(jasper_sp_global),
#else
                                sp_obj,
#endif
                                "newGlueTerm",
                                "(J)Lse/sics/jasper/SPTerm;",
                                (jlong)term);
#if THREADSERVER
/* This should only be done if we previously found a method which has a
   se/sics/jasper/Term as one of its arguments AND we are running in thread-
   server mode. */
      if (threadservermode && new_style_interface) {
        jvalue res = CallMethodByName(jnienv, &hasException,
                                      jasper_get_server(SPAPI_ARG jnienv),
                                      "newGlueJasperTerm",
                                      "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
//                                      "(Lse/sics/jasper/SPTerm;)Ljava/lang/Object;",
                                      result);
        result = res;
      }
#endif
    }
  else
    {
      result = CallMethodByName(jnienv,
                                &hasException,
#if 0
                                STATE_VARIABLE(jasper_sp_global),
#else
                                sp_obj,
#endif
                                "newGlueTerm",
                                "()Lse/sics/jasper/SPTerm;");
#if THREADSERVER
/* This should only be done if we previously found a method which has a
   se/sics/jasper/Term as one of its arguments AND we are running in thread-
   server mode. */
      if (threadservermode && new_style_interface) {
        jvalue res = CallMethodByName(jnienv, &hasException,
                                      jasper_get_server(SPAPI_ARG jnienv),
                                      "newGlueJasperTerm",
                                      "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
//                                      "(Lse/sics/jasper/SPTerm;)Ljava/lang/Object;",
                                      result);
        result = res;
      }
#endif
    }

 finish:
  if (hasException || result.l == NULL)
    {
#if THREADSERVER
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class se/sics/jasper/SPTerm or se/sics/jasper/jasper/Jasper$JasperTerm");
#else
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class se/sics/jasper/SPTerm");
#endif
      goto cleanup;
    }
  return result.l;

 cleanup:
  return NULL;
}

/* 0 on error (after callling SPJavaGlueErrorM) */
static int SPJavaGlueGetNativeTermRefM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref, int argnum, const char *err_msg)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
      SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  return rc;
}

/* called in Java context so may not call any prolog routines. Returns 0 on error without reporting error. */
static int SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG_PROTO_DECL*/ JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
      /* would call prolog run-time: SPJavaGlueError(jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
      /* would call prolog run-time: SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  return rc;
}

#if THREADSERVER
static jobject SPJavaGlueSPCanonicalAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_atom atom, int argnum, jboolean new_style_interface)
#else
static jobject SPJavaGlueSPCanonicalAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_atom atom, int argnum)
#endif
{

  jstring string = NULL;
  const char *chars;
  jvalue result;
#if THREADSERVER
  int threadservermode;
  jboolean hasException = JNI_FALSE;
#else
  jboolean hasException;
#endif
  const char *err_msg = "Cannot create object of class se/sics/jasper/SPCanonicalAtom";
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

#if DBG
  fprintf(stderr, "SPJavaGlueSPTermM atom=0x%lx \n", (unsigned long)atom);
#endif

  if (!sp_obj) { goto barf; }

#if THREADSERVER
  if(!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) goto barf;
#endif

  {
    chars = SP_string_from_atom(atom);
    if (!chars) { JASPER_DBGLINE(SPJavaGlueSPTermM); goto barf;}
    string = (*jnienv)->NewStringUTF(jnienv, chars);
    if (!string) { JASPER_DBGLINE(SPJavaGlueSPTermM); goto barf;}
  }

#if THREADSERVER
  /* If we are in threadserver mode AND have found a method which has a
     java/lang/String as one of its arguments, we should return the string. */
  if (threadservermode && new_style_interface) {
    result.l = string;
  } else {
#endif
  result = CallMethodByName(jnienv,
                            &hasException,
#if 0
                            STATE_VARIABLE(jasper_sp_global),
#else
                            sp_obj,
#endif
                            "newGlueAtom",
                            "(Ljava/lang/String;)Lse/sics/jasper/SPCanonicalAtom;",
                            string);
#if THREADSERVER
  }
#endif

  if (hasException || result.l == NULL) goto barf;
  return result.l;

 barf:
  SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
  if (string) (*jnienv)->DeleteLocalRef(jnienv, string);
  return NULL;
}

/* return 0 on failure (will call SPJavaGlueError) */
static int SPJavaGlueGetAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject obj /* SPCanonicalAtom or SPTerm */, SP_atom *p_atom, int argnum, const char *err_msg)
{
  int rc = 0;
  jclass clazz = NULL;
  jstring string = NULL;
  const char *chars = NULL;
  SP_atom atom = 0;             /* 0 marks invalid atom */


  clazz = (*jnienv)->GetObjectClass(jnienv, obj);


  /* This block should set atom to the atom number (or zero) (or leave a pending exception) */
  {
    const char *methodName;
    const char *typesig;
    jmethodID methodID;

    methodName = "getAtom";
    typesig = "()J";
    methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);

    if (methodID != 0)
      {
        JASPER_DBGLINE(SPJavaGlueGetAtomM);
        atom = (SP_atom) (*jnienv)->CallLongMethod(jnienv, obj, methodID);
      }
    else                          /* no getAtom method (SPTerm), try getString (SPCanonicalAtom) */
      {
        JASPER_DBGLINE(SPJavaGlueGetAtomM);
        (*jnienv)->ExceptionClear(jnienv); /* clear the failed method lookup */

#if THREADSERVER
                          /* [PD] 3.9 SPCanonicalAtom has the method
                           getString, but SPCanonicalAtom is not used in
                           threadserver mode, I think. Actually I don't
                           remember what's going on here, except that
                           things failed if this code tried to call the
                           "getString" method. */
        methodName = "toString";
#else
        methodName = "getString";
#endif
        typesig = "()Ljava/lang/String;";
        methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);

        if (methodID == 0) goto generic_glue_error; /* neither getAtom nor getString found */
        if ((  (string = (*jnienv)->CallObjectMethod(jnienv, obj, methodID)) == NULL
               ||
               (*jnienv)->ExceptionCheck(jnienv)
               ||
               (chars = (*jnienv)->GetStringUTFChars(jnienv, string, NULL)) == NULL))
          {
            goto propagate_java_exception;
          }
        /* chars is a UTF8 string here */
        atom = SP_atom_from_string(chars);/* zero if malformed UTF8 (e.g., embedded NUL or too long) */
      }
  }
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if ((*jnienv)->ExceptionCheck(jnienv)) goto propagate_java_exception;

  /* atom valid or zero here */
  if (atom == 0) goto generic_glue_error;
  *p_atom = atom;

  rc = 1;
  /* fall through */
 cleanup:
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if (chars) (*jnienv)->ReleaseStringUTFChars(jnienv, string, chars);
  if (string) (*jnienv)->DeleteLocalRef(jnienv, string);
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return rc;
 propagate_java_exception:                     /* propagate any java exception (ExceptionCheck() probably true here */
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if (SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg)) goto cleanup;
  /* fall through */
 generic_glue_error:                    /* report a generic 'error in glue' (ignore and clear pending java exceptions) */
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
  goto cleanup;
}

/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutCharsM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *chars = NULL;

  JASPER_DBGLINE(SPJavaGluePostPutCharsM);
  chars = (*jnienv)->GetStringUTFChars(jnienv, string_obj, NULL);

#if DBG
  fprintf(stderr, "SPJavaGluePostPutCharsM \"%s\"\n", (chars ? chars : "<<ERROR: chars==NULL!! >>"));
#endif
  JASPER_DBGLINE(SPJavaGluePostPutCharsM);
  if((!chars)
     || (!SP_put_list_chars(tstring, SP_new_term_ref(), (char *)chars)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutCharsM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutCharsM);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutCharsM);
  if (chars)
    {
      JASPER_DBGLINE(SPJavaGluePostPutCharsM);
      (*jnienv)->ReleaseStringUTFChars(jnienv, string_obj, chars);
    }

  return rc;
}

/* return NULL on error (will call SPJavaGlueError) */
static jobject SPJavaGlueStringBufferM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum)
{
  jclass clazz = NULL;
  jmethodID methodID;
  jobject stringBuffer = NULL;
  
  clazz = (*jnienv)->FindClass(jnienv, "java/lang/StringBuffer");
  if (clazz==NULL)
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot find class java/lang/StringBuffer for -atom or -string");
      goto cleanup;
    }

   methodID = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "()V");
   if (methodID == 0)
     {
       SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot find constructor for class `java/lang/StringBuffer'");
       goto cleanup;
     }
   stringBuffer = (*jnienv)->NewObject(jnienv, clazz, methodID);
   if (stringBuffer == NULL)
     {
       SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class java/lang/StringBuffer");
       goto cleanup;
     }
 cleanup:
   (*jnienv)->DeleteLocalRef(jnienv, clazz); /* safe to call with NULL */
   return stringBuffer;
}

/* return a new local ref to a String. Does not delete obj.
   Typical use:
   {
      jstring tmp = SPJavaGluePostToString(jnienv, obj, 42);
      (*jnienv)->DeleteLocalRef(jnienv, obj);
      obj = tmp;
   }
*/
static jstring SPJavaGluePostToStringM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject obj, int argnum)
{
  const char *methodName = "toString";
  const char *typesig = "()Ljava/lang/String;";
  jmethodID methodID;
  jclass clazz = NULL;
  jstring string_obj = NULL;

  clazz = (*jnienv)->GetObjectClass(jnienv, obj);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);
  if (methodID == 0)
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Postconversion failed"); /* NOTE: better error message needed */
      goto cleanup;
    }
  string_obj = (jstring)(*jnienv)->CallObjectMethod(jnienv, obj, methodID);

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return string_obj;
}

/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutStrM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *chars = NULL;
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  chars = (*jnienv)->GetStringUTFChars(jnienv, string_obj, NULL);
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  if((!chars)
     || (!SP_put_string(tstring, chars)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutStrM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  if (chars)
    {
      JASPER_DBGLINE(SPJavaGluePostPutStrM);
      (*jnienv)->ReleaseStringUTFChars(jnienv, string_obj, chars);
    }

  return rc;
}

/***************************************************/


#if DBG
#define ASSERT_NO_EXCP(JNIENV, STRING) \
   do{ \
      if ( ((JNIENV)==NULL) || (( *(JNIENV) )->ExceptionCheck((JNIENV))) ) { \
         fprintf(stderr, "ERROR: Pending Java exception (%s)\n", (STRING)); \
         jasper_DescribeException((JNIENV));\
      } \
   } while (0)
#else /* no DBG */
#define ASSERT_NO_EXCP(JNIENV, STRING)
#endif


#if 0                           /* 1 to disable delete of refs (DEBUG) NOTE: */
#define DELETE_LOCAL_REF(ENV, REF) fprintf(stderr, "NOT deleting local ref %ld\n", (long)(REF))
#define DELETE_GLOBAL_REF(ENV, REF) fprintf(stderr, "NOT deleting global ref %ld\n", (long)(REF))
#else /* The normal definition */
#define DELETE_LOCAL_REF(ENV, REF) ((*(ENV))->DeleteLocalRef((ENV), (REF)))
#define DELETE_GLOBAL_REF(ENV, REF) ((*(ENV))->DeleteGlobalRef((ENV), (REF)))
#endif

static void dbg_print_object_class(JNIEnv *jnienv, jobject jobj, char *extra_info);
#if DBG
#define DBG_PRINT_OBJECT_CLASS(JNIENV, JOBJ, EXTRA_INFO) dbg_print_object_class((JNIENV), (JOBJ), (EXTRA_INFO))
#else
#define DBG_PRINT_OBJECT_CLASS(JNIENV, JOBJ, EXTRA_INFO)
#endif



#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#if 1                           /* 3.9 */
static void jasper_set_jvm(SPAPI_ARG_PROTO_DECL0)
{
  jint numjvms;
  /* Luckily "Java 2 SDK release 1.2 do not support creating more than one
     virtual machine instance in a single process" */
  JNI_GetCreatedJavaVMs(&STATE_VARIABLE(jvm), 1, &numjvms);
#if DBG
  if (numjvms > 1)              /* Need re-design if this becomes possible in some future SDK release */
    {
      fprintf(stderr, "**** WARNING: Inside jasper_set_jvm(): numjvms = %ld\n", (long)numjvms);
    }
#endif

  if (! (numjvms > 0))
    STATE_VARIABLE(jvm) = NULL;
}
#else  /* 3.8.6 */
static void jasper_set_jvm(void) {
  numjvms_prev = numjvms;
  /* Luckily "Java 2 SDK release 1.2 do not support creating more than one
     virtual machine instance in a single process" */
  JNI_GetCreatedJavaVMs(jvms, MAX_JVMS, &numjvms);
  if (numjvms > 0)
    jvm = jvms[0];
  else
    jvm = NULL;
#if 0
  fprintf(stderr, "Inside jasper_set_jvm(): numjvms = %d\n", numjvms);
#endif
}
#endif

/* SP_foreach(): loops through the Prolog list "list", setting "elem"
   to each element in turn, "tail" to the tail of each node. "index"
   is a zero-based index for each element. */
#define SP_foreach(list,elem,tail,index)     \
for (SP_put_term((tail),(list)),(index) = 0; \
     SP_get_list((tail),(elem),(tail));      \
     (index)++)


/* Metacall method type definitions. */
typedef jobject (JNICALL *StaticMethodCallTypeObj)(JNIEnv *, jclass, jmethodID, jvalue *);
typedef jobject (JNICALL *MethodCallTypeObj)(JNIEnv *, jobject, jmethodID, jvalue *);

#if DBG && 0

#define SET_JNIENV_EXTRA(JNIENV) do{ \
   fprintf(stderr, "Ensuring extra capacity (%d)\n", (int)(*(JNIENV))->EnsureLocalCapacity((JNIENV), 42)); \
   ASSERT_NO_EXCP((JNIENV), "SET_JNIENV_EXTRA"); \
}while(0)

#else

#define SET_JNIENV_EXTRA(JNIENV)

#endif

/* Will throw exception if cannot attach */
#define SET_JNIENV(jnienv,tr,FAILACTION) \
   do{ \
      (void) (tr); \
      if (NULL == ((jnienv) = jasper_int_get_jnienv(SPAPI_ARG 0 /* ! from glue */))) \
         { \
           FAILACTION ; \
         } \
      else \
         { \
           SET_JNIENV_EXTRA(jnienv); \
         } \
   }while(0)


/* BROKEN, all callers need error handling
#define GET_JNIENV(tr,jnienv) do{if (NULL == ((jnienv) = jasperi_get_jnienv((tr)))) { return; }}while(0)
#define GET_JNIENV_RV(tr,jnienv,rv) do{if (NULL == ((jnienv) = jasperi_get_jnienv(tr))) { return (rv); }}while(0)
*/

#define return_onzero(rv,call)                     \
do{ (rv) = (call);                                 \
     if((rv) == 0) return;                         \
}while(0)

#define SAVE_ERROR(Type, Msg, Culprit)            \
do{                                               \
    SP_save_error((Type), (Msg), (Culprit));      \
    goto _error;                                  \
}while(0)

#define RAISE_ERROR(PredName, Arity, ArgNo)     \
{                                               \
 _error:                                        \
  SP_raise_error(PredName, Arity, ArgNo);       \
  return;                                       \
}

#define RAISE_ERROR_NO_RETURN(PredName, Arity, ArgNo)     \
{                                               \
 _error:                                        \
  SP_raise_error(PredName, Arity, ArgNo);       \
}

#define RAISE_ERROR_VAL(PredName, Arity, ArgNo, ReturnValue)    \
{                                                               \
 _error:                                                        \
  SP_raise_error(PredName, Arity, ArgNo);                       \
  return ReturnValue;                                           \
}

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *vm, void *reserved) {
  (void)vm;
  (void)reserved;

  return JNI_VERSION_1_2;
}

static void jasper_int_raise_exception(SPAPI_ARG_PROTO_DECL char *type, char *desc)
{
  SP_term_ref 
    excp_term = SP_new_term_ref(),
    arg1 = SP_new_term_ref();
  
  SP_put_string(arg1, desc);
  SP_cons_functor(excp_term, SP_atom_from_string(type), 1, arg1);
                  
  SP_raise_exception(excp_term);
  return;
}

/* Does (*jnienv)->ExceptionDescribe(jnienv) but does *not* clear the pending exception */
static void jasper_DescribeException(JNIEnv *jnienv)
{
  jthrowable pending = NULL;

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
      (*jnienv)->ExceptionDescribe(jnienv); /* clears it  */
      (*jnienv)->Throw(jnienv, pending); /* restores it */
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
}


/* A copy is in spnative.c, keep in synch */
static jvalue SPCDECL
CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...)
{
  va_list args;
  jclass clazz;
  jmethodID mid;
  jvalue result;

  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      clazz = (*env)->GetObjectClass(env, obj);
      mid = (*env)->GetMethodID(env, clazz, name, descriptor);
      if (mid) {
        const char *p = descriptor;
        /* skip over argument types to find out the 
         * return type */
        while (*p != ')') p++;
        /* skip ')' */
        p++;
        va_start(args, descriptor);
        switch (*p) {
        case 'V':
          (*env)->CallVoidMethodV(env, obj, mid, args);
          break;
        case '[':
        case 'L':
          result.l = (*env)->CallObjectMethodV(
                                               env, obj, mid, args);
          break;
        case 'Z':
          result.z = (*env)->CallBooleanMethodV(
                                                env, obj, mid, args);
          break;
        case 'B':
          result.b = (*env)->CallByteMethodV(
                                             env, obj, mid, args);
          break;
        case 'C':
          result.c = (*env)->CallCharMethodV(
                                             env, obj, mid, args);
          break;
        case 'S':
          result.s = (*env)->CallShortMethodV(
                                              env, obj, mid, args);
          break;
        case 'I':
          result.i = (*env)->CallIntMethodV(
                                            env, obj, mid, args);
          break;
        case 'J':
          result.j = (*env)->CallLongMethodV(
                                             env, obj, mid, args);
          break;
        case 'F':
          result.f = (*env)->CallFloatMethodV(
                                              env, obj, mid, args);
          break;
        case 'D':
          result.d = (*env)->CallDoubleMethodV(
                                               env, obj, mid, args);
          break;
        default:
          (*env)->FatalError(env, "illegal descriptor");
        }
        va_end(args);
      }
      (*env)->DeleteLocalRef(env, clazz);
    }
  if (hasException)
    {
      *hasException = (*env)->ExceptionCheck(env);
    }
  return result;
}

static jvalue SPCDECL
CallStaticMethodByName(JNIEnv *env,
                       jboolean *hasException,
                       const char *clazzName,
                       const char *name,
                       const char *descriptor,
                       ...)
{
  va_list args;
  jclass clazz = NULL;          /* NULL for cleanup */
  jmethodID mid;
  jvalue result;

  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      if (((clazz = (*env)->FindClass(env, clazzName)) != NULL)
          &&
          ((mid = (*env)->GetStaticMethodID(env, clazz, name, descriptor)) != NULL))
        {
          const char *p = descriptor;
          /* skip over argument types to find out the 
           * return type */
          while (*p != ')') p++;
          /* skip ')' */
          p++;
          va_start(args, descriptor);
          switch (*p) {
          case 'V':
            (*env)->CallStaticVoidMethodV(env, clazz, mid, args);
            break;
          case '[':
          case 'L':
            result.l = (*env)->CallStaticObjectMethodV(
                                                       env, clazz, mid, args);
            break;
          case 'Z':
            result.z = (*env)->CallStaticBooleanMethodV(
                                                        env, clazz, mid, args);
            break;
          case 'B':
            result.b = (*env)->CallStaticByteMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'C':
            result.c = (*env)->CallStaticCharMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'S':
            result.s = (*env)->CallStaticShortMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'I':
            result.i = (*env)->CallStaticIntMethodV(
                                                    env, clazz, mid, args);
            break;
          case 'J':
            result.j = (*env)->CallStaticLongMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'F':
            result.f = (*env)->CallStaticFloatMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'D':
            result.d = (*env)->CallStaticDoubleMethodV(
                                                       env, clazz, mid, args);
            break;
          default:
            /* We should not get here unless GetMethodID goofed up */
            (*env)->FatalError(env, "illegal descriptor");
          }
          va_end(args);
        }
      if (clazz) (*env)->DeleteLocalRef(env, clazz);
    }
  if (hasException)
    {
      *hasException = (*env)->ExceptionCheck(env);
    }
  return result;
}


#if 0                           /*  REMOVE THIS */
/* err_msg can be NULL */
static void SPJavaGlueErrorMMULTI(JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg)
{
#if DBG > 0
  fprintf(stderr, "SPJavaGlueErrorMMULTI: arg# %d, `%s'\n", argnum, ((err_msg != NULL) ? err_msg : "<<NULL>>"));
#endif
  if (jnienv && (*jnienv)->ExceptionCheck(jnienv))
    {
#if DBG
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
#endif
      (*jnienv)->ExceptionClear(jnienv);
    }
  {
    SP_term_ref excp_term = SP_new_term_ref();
    SP_put_string(excp_term, ((err_msg != NULL) ? err_msg : "Error in Java glue"));
    SP_raise_exception(excp_term);
  }
}
#endif









#if !NEW_WORLD_ORDER
#if SICSTUS_MONITOR_IS_OBJECT

static jobject jasper_ensure_monitor(JNIEnv *jnienv)
{
  fprintf(stderr, "FATAL ERROR: no SP monitor JNI %p\n", jnienv);
  exit(EXIT_FAILURE);
  return NULL;
}

#else  /* ! SICSTUS_MONITOR_IS_OBJECT */

static jobject jasper_ensure_monitor(JNIEnv *jnienv)
{
  jclass clazz;
  jclass monitor;

#if DBG>1
  fprintf(stderr, "Creating SICStus monitor\n");
#endif

  clazz = (*jnienv)->FindClass(jnienv, "se/sics/jasper/SICStus");
  
  if (!clazz)
    {
#if DBG
      fprintf(stderr, "ERROR: Could not find se/sics/jasper/SICStus class (jasper_ensure_monitor)\n");
#endif
      return NULL;
    }

  
  (*jnienv)->MonitorEnter(jnienv, clazz);
      
  if (!jasper_sicstus_monitor)
    {
      jasper_sicstus_monitor = (*jnienv)->NewGlobalRef(jnienv, clazz);
    }

  (*jnienv)->MonitorExit(jnienv, clazz);

  (*jnienv)->DeleteLocalRef(jnienv, clazz);

  return monitor;
}

#endif  /* ! SICSTUS_MONITOR_IS_OBJECT */
#endif /* !NEW_WORLD_ORDER */

#if !NEW_WORLD_ORDER
/* Used by spnative.c even if !JASPER_THREADING */
/* NOTE: DOC */
int jasper_enter_sicstus_monitor(JNIEnv *jnienv)
{
#if DBG>1
  fprintf(stderr, "Attempting to enter SICStus monitor (JNI %p)\n", jnienv);
#endif

  if (!jasper_sicstus_monitor) jasper_ensure_monitor(jnienv);
  if ((*jnienv)->MonitorEnter(jnienv, jasper_sicstus_monitor))
    {
#if DBG
      fprintf(stderr, "ERROR: Entering SICStus monitor (JNI %p)\n", jnienv);
#endif
      return 0;
    }
  jasper_sicstus_lock_count++;
#if DBG>1
  fprintf(stderr, "Entered SICStus monitor (JNI %p) count %ld%s\n", jnienv, jasper_sicstus_lock_count, ((jasper_sicstus_lock_count==1) ? " (LOCKED)":""));
#endif
      
  return 1;
}
#endif /* !NEW_WORLD_ORDER */


#if !NEW_WORLD_ORDER
/* Used by spnative.c even if !JASPER_THREADING */
/* NOTE: DOC */
int jasper_leave_sicstus_monitor(JNIEnv *jnienv)
{
  int res;


#if DBG>1
  fprintf(stderr, "Exiting SICStus monitor (JNI %p) count %ld%s\n", jnienv, jasper_sicstus_lock_count, ((jasper_sicstus_lock_count==1) ? " (RELEASING)":""));
#endif

  jasper_sicstus_lock_count--;
  
  if (!jasper_sicstus_monitor)
    {
      jthrowable excpt = (*jnienv)->ExceptionOccurred(jnienv);

      if (excpt)
        {
#if DBG>1
          fprintf(stderr, "Entered jasper_leave_sicstus_monitor with pending exception (This is expected)\n");
          jasper_DescribeException(jnienv);
#endif

          /* need to clear it in order to safely call jasper_ensure_monitor */
          (*jnienv)->ExceptionClear(jnienv);
        }

      jasper_ensure_monitor(jnienv);

      if (excpt)
        {
#if DBG
          fprintf(stderr, "\nRe-raising pending exception in jasper_leave_sicstus_monitor (JNI %p) (This is expected)\n", jnienv);
#endif
          (*jnienv)->Throw(jnienv, excpt);
          (*jnienv)->DeleteLocalRef(jnienv, excpt);
        }
    }
  
  {
    int have_exception = (*jnienv)->ExceptionCheck(jnienv);
    jint meErr;
#if MONITOR_EXIT_EXCEPTION_BUG
    jthrowable pending = NULL;
#endif

    {
#if DBG
      if (have_exception)
        {
          fprintf(stderr, "\nHave exception before exiting SICStus monitor (JNI %p) (this is expected)\n", jnienv);
        }
#endif

#if MONITOR_EXIT_EXCEPTION_BUG
      {
        if (have_exception)
          {
            pending = (*jnienv)->ExceptionOccurred(jnienv);
            (*jnienv)->ExceptionClear(jnienv); /* clears it  */
#if DBG
            fprintf(stderr, "\nCleared the exception before exiting SICStus monitor (JNI %p) (this is expected)\n", jnienv);
#endif /* DBG */
          }
      }
#else  /* !MONITOR_EXIT_EXCEPTION_BUG */
      {
#if DBG       
        if (have_exception)
          {
            fprintf(stderr, "\nNot clearing the exception before exiting SICStus monitor (JNI %p) (this is expected)\n", jnienv);
          }
#endif /* DBG */
      }
#endif /* !MONITOR_EXIT_EXCEPTION_BUG */

    }

    /* Docs says safe to call with pending exceptions. But see MONITOR_EXIT_EXCEPTION_BUG */
    meErr = (*jnienv)->MonitorExit(jnienv, jasper_sicstus_monitor);
    if (meErr!=0)
      {
#if DBG
        int foo = 0;
        int meErr1;
        fprintf(stderr, "\nERROR: Exiting SICStus monitor (JNI %p, err=%d)%s\n", jnienv, (int)meErr, (have_exception ? " Already had exception before exiting" : ""));
        if (foo)                /* set from debugger */
          {
            meErr1 = (*jnienv)->MonitorExit(jnienv, jasper_sicstus_monitor);
          }
        jasper_DescribeException(jnienv);
#endif /* DBG */
        res = 0;
        goto cleanup;
      }

    res = 1;

  cleanup:
    ;
#if MONITOR_EXIT_EXCEPTION_BUG
    {
      if (have_exception)
        {
#if DBG
          fprintf(stderr, "\nRestoring the exception (JNI %p) (this is expected)\n", jnienv);
#endif /* DBG */

          (*jnienv)->Throw(jnienv, pending); /* restores it */
          (*jnienv)->DeleteLocalRef(jnienv, pending);
        }
    }
#endif /* MONITOR_EXIT_EXCEPTION_BUG */

  }
#if DBG>1
  fprintf(stderr, "\nExited SICStus monitor (JNI %p, res=%d)\n", jnienv, res);
#endif
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/* return 0 on failure. SICStus monitor should already be entered */
static unsigned long jasper_leave_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  unsigned long context;

  (void)jnienv;
  /* we are in Prolog context */

  context = ++STATE_VARIABLE(context_counter);

#if DBG>1
  fprintf(stderr, "Pushing context to level %lu\n", context);
#endif

#if JASPER_THREADING

  /* We are about to to call Java, ensure other Java threads
     can call Prolog while not in Prolog runtime. */

  jasper_leave_sicstus_monitor(jnienv);

#endif /* JASPER_THREADING */

  /* we are in Java context */

  return context;
}


#if JASPER_THREADING
/* Do Java code: sp_global.wait(); */
static void jasper_wait_sicstus_monitor(JNIEnv *jnienv)
{
  const char *method = "wait";
  jboolean hasException;

#if DBG>1
  fprintf(stderr, "jasper_wait_sicstus_monitor call wait() JNI %p\n", jnienv);
#endif
  (void) CallMethodByName(jnienv, &hasException, jasper_sicstus_monitor, method, "()V");
#if DBG>1
  fprintf(stderr, "jasper_wait_sicstus_monitor return wait() JNI %p\n", jnienv);
#endif

#if DBG
  if (hasException)
    {
      fprintf(stderr, "ERROR: jasper_wait_sicstus_monitor:\n");
      jasper_DescribeException(jnienv);
      exit(1);
    }
#endif
}
#endif /* JASPER_THREADING */

#if JASPER_THREADING
/* Do Java code: sp_global.notifyAll(); */
static void jasper_notify_sicstus_monitor(JNIEnv *jnienv)
{
  const char *method = "notifyAll";
  jboolean hasException;

#if DBG>1
  fprintf(stderr, "jasper_wait_sicstus_monitor call notifyAll() JNI %p\n", jnienv);
#endif

  (void) CallMethodByName(jnienv, &hasException, jasper_sicstus_monitor, method, "()V");
#if DBG>1
  fprintf(stderr, "jasper_wait_sicstus_monitor return notifyAll() JNI %p\n", jnienv);
#endif

#if DBG
  if (hasException)
    {
      fprintf(stderr, "ERROR: jasper_notify_sicstus_monitor:\n");
      jasper_DescribeException(jnienv);
      exit(1);
    }
#endif
}
#endif /* JASPER_THREADING */

/*
 * NOTE: DOC
 */
static unsigned long jasper_return_to_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, unsigned long context)
{

#if DBG>0
  fprintf(stderr, "Returning to prolog level %lu\n", context);
#endif

#if JASPER_THREADING
  {
  /* We are about to return to Prolog, ensure no other Java threads
     can call Prolog while in Prolog runtime.
     At the same time this protects context_counter */
  if (!jasper_enter_sicstus_monitor(jnienv))
    {
#if DBG
      fprintf(stderr, "ERROR: Popped context %lu could not leave monitor JNI %p\n", context, jnienv);
      jasper_DescribeException(jnienv);
#endif
      return 0;
    }
#if DBG
  fprintf(stderr, "Acquired monitor. Current level is %lu (want %lu) JNI %p%s\n", 
          STATE_VARIABLE(context_counter), context, jnienv, ( context != STATE_VARIABLE(context_counter) ? " CONTEXT MISMATCH" : "" ));
#endif


  /* Prolog run-time monitor is owned by this thread */

  while (context != STATE_VARIABLE(context_counter))
    {
#if DBG
      fprintf(stderr, "Waiting for other threads. Current level is %lu (want %lu) JNI %p%s\n", 
              STATE_VARIABLE(context_counter), context, jnienv, ( context != STATE_VARIABLE(context_counter) ? " CONTEXT MISMATCH" : "" ));
#endif

      jasper_wait_sicstus_monitor(jnienv); /* release the monitor and wait for higher levels to return to prolog */

#if DBG>0
      fprintf(stderr, "Awakened from waiting for other threads. Current level is %lu (want %lu) JNI %p%s\n", 
              STATE_VARIABLE(context_counter), context, jnienv, ( context != STATE_VARIABLE(context_counter) ? " CONTEXT MISMATCH" : "" ));
#endif
    }
  }
#endif /* JASPER_THREADING */

  /* Prolog run-time monitor is owned by this thread and context==context_counter */

#if DBG
  {
  if (context != STATE_VARIABLE(context_counter))
    {
      fprintf(stderr, "ERROR: Returned to prolog level context==%lu, context_counter==%lu JNI %p\n", context, STATE_VARIABLE(context_counter), jnienv);
    }
  fprintf(stderr, "Returned to prolog level %lu JNI %p\n", context, jnienv);
  }
#endif /* DBG */

  STATE_VARIABLE(context_counter)--;

#if JASPER_THREADING
  {
  if (STATE_VARIABLE(context_counter) > 0)
    {
#if DBG
      fprintf(stderr, "Notifying other threads that context level decreased to %lu JNI %p\n", STATE_VARIABLE(context_counter), jnienv);
#endif /* DBG */
      
      jasper_notify_sicstus_monitor(jnienv);
    }
  else
    {
      ;
#if DBG
      fprintf(stderr, "Not notifying other threads since context level decreased to %lu JNI %p\n", STATE_VARIABLE(context_counter), jnienv);
#endif /* DBG */
    }
  }
#endif /* JASPER_THREADING */
                                /* We are in Prolog context */
  return STATE_VARIABLE(context_counter)+1;
}

/* Called when monitor is owned by thread (i.e., in Prolog context) */
static int jasper_notify_java_return_to_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  jboolean hasException;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return 0;

  (void) CallMethodByName(jnienv, &hasException, /*STATE_VARIABLE(jasper_sp_global)*/ sp_obj, method, "()V");

  if (hasException)
    {
#if DBG
      jasper_DescribeException(jnienv);
#endif
      return 0;
    }
  return 1;
}

/* Called when monitor is owned by thread (i.e., in Prolog context) */
static int jasper_notify_java_leave_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  jboolean hasException;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return 0;

  (void) CallMethodByName(jnienv, &hasException, /*STATE_VARIABLE(jasper_sp_global)*/ sp_obj, method, "()V");

  if (hasException)
    {
#if DBG
      jasper_DescribeException(jnienv);
#endif
      return 0;
    }
  return 1;
}


/* [PM] 3.8.5 called by (meta/generated) glue code to release prolog
 *            monitor and enter Java context
 *            NOTE: You may not call any SP runtime routines after
 *            this procedure returns.
 *            Must preserve pending exception
 */
static unsigned long jasper_push_context1(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  unsigned long context;
  jthrowable pending = NULL;
  
  JASPER_DBGLINE(jasper_push_context1);

  /* we are in Prolog context */

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  JASPER_DBGLINE(jasper_push_context1);
  if (pending)
    {
      JASPER_DBGLINE(jasper_push_context1);
#if DBG
      fprintf(stderr, "Entered jasper_push_context1 with pending exception (this is expected)\n");
#endif
      (*jnienv)->ExceptionClear(jnienv);
    }
  JASPER_DBGLINE(jasper_push_context1);
  /* important that this is done while still owning prolog */
  jasper_notify_java_leave_prolog(SPAPI_ARG jnienv, method);
  
  JASPER_DBGLINE(jasper_push_context1);

#if DBG
  fprintf(stderr, "jasper_push_context1 (: SP_term_refs==%u\n", (int)SP_new_term_refs(0));
#endif

  /* we are in Prolog context */
  {
    /* NOTE: Error handling? */
    context = jasper_leave_prolog(SPAPI_ARG jnienv);
  }
  /* We are in Java context */

#if DBG
  fprintf(stderr, "jasper_push_context1 context==%lu\n", (unsigned long)context);
#endif

  JASPER_DBGLINE(jasper_push_context1);

  if (pending
      /* (arbitrary) give precedence to exceptions from SICStus.<method>  */
      && !(*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(jasper_push_context1);
      (*jnienv)->Throw(jnienv, pending);
    }

  if (pending)
    {
      JASPER_DBGLINE(jasper_push_context1);
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
  JASPER_DBGLINE(jasper_push_context1);

  return context;
}

/* [PM] 3.8.5 called by (meta/generated) glue code (from Java context) to
 *      re-acquire prolog monitor and return to Prolog context
 *      Must preserve pending exception
 */
static unsigned long jasper_pop_context1(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, unsigned long context, const char *method)
{
  unsigned long new_context;
  int res;
  jthrowable pending = NULL;

  /* we are in Java context */
  
  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
#if DBG
      fprintf(stderr, "Entered jasper_pop_context1 with pending exception (this is expected)\n");
#endif
      (*jnienv)->ExceptionClear(jnienv);
    }
  /* In Java context */
  {
    new_context = jasper_return_to_prolog(SPAPI_ARG jnienv, context);
  }
  /* In Prolog context */


#if DBG
  fprintf(stderr, "jasper_pop_context1 context==%lu\n", (unsigned long)context);
  fprintf(stderr, "jasper_pop_context1 ): SP_term_refs==%u\n", (int)SP_new_term_refs(0));
#endif

  /* important that this is done when owning prolog */
  res = jasper_notify_java_return_to_prolog(SPAPI_ARG jnienv, method);


  if (pending
      /* (arbitrary) give precedence to exceptions from SICStus.<method>  */
      && !(*jnienv)->ExceptionCheck(jnienv))
    {
      (*jnienv)->Throw(jnienv, pending);
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }

  /* we are in Prolog context */
  return new_context;
}

/* [PM] 3.8.5 called by meta glue code to enter Java context
   Must be called before creatig any SPTerm (for parameter
   passing). This is so the SPTerms get the right SPQuery context. 
   As of 3.8.5beta2/final leaving prolog monitor is done separately
   (although currently a no-op).
 */
static unsigned long jasper_push_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  return jasper_push_context1(SPAPI_ARG jnienv, "MetaGlue_PushContext");
}

static int jasper_leave_context_monitor(JNIEnv *jnienv, unsigned long context)
{
  (void)jnienv;
  (void)context;

  /* a no-op since we had to abandon the free-threading after 3.8.5beta1*/
  return 1;
}

static int jasper_enter_context_monitor(JNIEnv *jnienv, unsigned long context)
{
  (void)jnienv;
  (void)context;
  
  /* a no-op since we had to abandon the free-threading after 3.8.5beta1*/
  return 1;
}

/* [PM] 3.8.5 called by meta glue code to leave Java context
 *            (no longer: and acquire the prolog monitor)
 *            to return to prolog
 *            NOTE: You may not call any SP runtime routines between
 *            returning from Java and this procedure returns.
 */
static unsigned long jasper_pop_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, unsigned long context)
{
  return jasper_pop_context1(SPAPI_ARG jnienv, context, "MetaGlue_PopContext");
}

#if JASPER_RESOURCES
/* [PM] 3.8.5 called by generated glue code (from Prolog context) to
 *      (no longer: release prolog monitor)
 *      enter Java context
 */ 
JASPER_EXPORT unsigned long SPCDECL jasper_glue_push_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  return jasper_push_context1(SPAPI_ARG jnienv, "Glue_PushContext");
}
#endif /* JASPER_RESOURCES */

#if JASPER_RESOURCES
/* [PM] 3.8.5 called by generated glue code (from Java context) to
 *      (no longer: re-acquire prolog monitor and)
 *      return to Prolog context 
 */
JASPER_EXPORT unsigned long SPCDECL jasper_glue_pop_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, unsigned long context)
{
  return jasper_pop_context1(SPAPI_ARG jnienv, context, "Glue_PopContext");
}
#endif /* JASPER_RESOURCES */

#if JASPER_RESOURCES
/* 3.8.5 called by generated glue to aquire the prolog monitor (which is currently no-op)
 */
JASPER_EXPORT int SPCDECL jasper_glue_enter_context_monitor(JNIEnv *jnienv, unsigned long context)
{
  return jasper_enter_context_monitor(jnienv, context);
}
#endif /* JASPER_RESOURCES */

#if JASPER_RESOURCES
/* 3.8.5 called by generated glue to release the prolog monitor (which is currently no-op)
 */
JASPER_EXPORT int SPCDECL jasper_glue_leave_context_monitor(JNIEnv *jnienv, unsigned long context)
{
  return jasper_leave_context_monitor(jnienv, context);
}
#endif /* JASPER_RESOURCES */


/* Check if there is a pending Java exception. If there is, we print a
 * description of it on stderr (actually we would want to include it
 * in a Prolog exception, but that's more tricky).  If error is
 * non-zero, we throw a Prolog exception regardless. If error is zero,
 * we only throw a Prolog exception if there was a pending Java
 * exception. 
 * Return: non-zero if ERROR != 0 or Java exception 
 * Post: SP exception raised if either ERROR != 0 or Java exception.
 *       Java exception cleared
 * NOTE: Will always raise an java_exception('description'), never a $java_object
 */
static int jasper_int_handle_exception(SPAPI_ARG_PROTO_DECL JNIEnv *env, char *desc, int error)
{
  jboolean have_jexcp;
  
  /* Handle exceptions from recursive calls to Java */
  /* [PM] 3.8.5 Was: excp = (*env)->ExceptionOccurred(env); */
  have_jexcp = (*env)->ExceptionCheck(env);

  if (have_jexcp) {
    jasper_DescribeException(env); /* NOTE: Questionable to unconditionally write things (used to do ExceptionDescribe) */

    /* [PM] 3.8.5 Never leave lingering Java exceptions. */ 
    (*env)->ExceptionClear(env);
  }

  if (error != 0 || have_jexcp)
    {
      jasper_int_raise_exception(SPAPI_ARG "java_exception",desc);
      return 1;
    }
  return 0;
}

#define DECLARE_ATOM(VAR,NAME)                      \
{                                                   \
   STATE_VARIABLE(VAR) = SP_atom_from_string(NAME); \
   SP_register_atom(STATE_VARIABLE(VAR)) ;          \
}


/* Return 0 on error and sets *pobj to NULL
   Return true on success (null objref is not an error)
*/
static int termref_to_jobject(SPAPI_ARG_PROTO_DECL SP_term_ref tr, jobject *pobj)
{
  int rc;
  jobject jobj;

#if DBG
  if ( (sizeof jobj) != (sizeof(long)))
    {
      fprintf(stderr, "FATAL ERROR ( sizeof jobj (%ld) != sizeof long (%ld))\n", (long) (sizeof jobj), (long) (sizeof (long)));
    }
#endif

  {
    SP_term_ref t0 = SP_new_term_ref();
    long tmp;
    if (SP_get_arg(1,tr,t0) && SP_get_integer(t0,&tmp))
      {
        rc = 1;
        if (tmp == 0)
          {
            jobj = NULL;
          }
        else
          {
            jobj = (jobject)tmp;
          }
      }
    else
      {
        rc = 0;
#if DBG
        fprintf(stderr, "termref_to_jobject failed\n");
#endif  
        jobj = NULL;
      }
    /* 
       not needed:
       SP_reset_term_refs(t0);
    */

    *pobj = (jobject)tmp;
    return rc;
  }
}

/* [PM] 3.8.5 added tr arg */
/* Return 0 if NULL object (tr will be '$java_object'(0)) */
static int jobject_to_termref(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj, SP_term_ref tr)
{
  int rc;
  if (jobj != NULL)
    {
      ASSERT_NO_EXCP(jnienv, "jobject_to_termref");
      rc = 1;
      DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jobject_to_termref");

#if DBG && 0                    /* NOTE: debug, enable this to globalize all */
      {
        jobject gobj = (*jnienv)->NewGlobalRef(jnienv,jobj);
        fprintf(stderr, "Globalizing %ld got %ld\n", (long) jobj, (long) gobj);
        ASSERT_NO_EXCP(jnienv, "jobject_to_termref 2");
        DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "Class of global");
        jobj = gobj;
      }
#endif
      SP_put_integer(tr,(long)jobj);

#if DBG
      {
        long tmp;
        if ( (!SP_get_integer(tr, &tmp))
             ||
             ((jobject)tmp) != jobj )
          {
            fprintf(stderr, "ERROR: converting jobj to term and back failed (%p -> %ld -> %p)\n", (void*)jobj, (long)jobj, (void*)tmp);
          }
      }
#endif
      
    }
  else
    {
      rc = 0;
#if DBG
      fprintf(stderr, "jobj==NULL in jobject_to_termref\n");
#endif

#if 1                           /* 3.8.6 Use 0 to signify null object (as opposed to invalid) */
      SP_put_integer(tr,0);
#else
      SP_put_integer(tr,-1);
#endif
    }
  SP_cons_functor(tr,STATE_VARIABLE(jasper_atom_dollar_java_object),1,tr);
  return rc;
}

/* A global ref to the SICStus object (if any) */
static jobject jasper_get_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  ASSERT_NO_EXCP(jnienv, "jasper_get_sicstus_object");
#if 0
  return STATE_VARIABLE(jasper_sp_global);
#else
  return sp_get_jasper_magic(SICSTUS_VERSION);
#endif
}

#if 0                           /* [PD] 3.9 Not necessary since introduction of
                                            sp_(set/get)_jasper_magic() */
/* FOREIGN */
void SPCDECL jasperi_set_sicstus_object_C(SPAPI_ARG_PROTO_DECL long spobj)
{
  #if DBG
  fprintf(stderr,
          "jasperi_set_sicstus_object_C("
          #if MULTI_SP_AWARE
          "API==%p, "
          #endif
          "SPOBJ==%p)\n",
          #if MULTI_SP_AWARE
          (void*)SPAPI_ARG_NAME,
          #endif
          (void*)spobj);
  #endif/* DBG */
#if 0
  STATE_VARIABLE(jasper_sp_global) = (jobject)spobj;
#else
  sp_set_jasper_magic(spobj,SICSTUS_VERSION);
#endif
}
#endif


/* FOREIGN */
void SPCDECL init_jasper(SPAPI_ARG_PROTO_DECL int when)
{
  DbgBreakPoint();

  (void)when;

  INIT_STASH();

  /* We can not load Java into a sbrk()-ed SICStus, except on
   * Linux. See memalloc.c for more info. */
#if 1 /* [PM] 3.8.6 Was: !linux.
         MM_USE_SBRK uses brk which, even on Linux, is not safe in
         multithreaded apps that use malloc (which use sbrk) */
  if (sp_get_mem_usage() == MM_USE_SBRK)
    {
      SAVE_ERROR(SYSTEM_ERROR,"Attempted to load Java engine into sbrk'd SICStus system (try starting SICStus with -m option)",SP_new_term_ref());
    }
#endif
#if !JNI_VERSION_1_2
  SAVE_ERROR(SYSTEM_ERROR,"Jasper requires JDK 1.2 or later", SP_new_term_ref());
#endif


  /* Init state variables */
  {
    /* M-X sort-lines */
    DECLARE_ATOM(jasper_atom_atom,"atom");
    DECLARE_ATOM(jasper_atom_boolean,"boolean");
    DECLARE_ATOM(jasper_atom_byte,"byte");
    DECLARE_ATOM(jasper_atom_char,"char");
    DECLARE_ATOM(jasper_atom_chars,"chars");
    DECLARE_ATOM(jasper_atom_dollar_java_object,"$java_object"); /* xref jasper.pl */
    DECLARE_ATOM(jasper_atom_double,"double");
    DECLARE_ATOM(jasper_atom_false,"false");
    DECLARE_ATOM(jasper_atom_float,"float");
    DECLARE_ATOM(jasper_atom_integer,"integer");
    DECLARE_ATOM(jasper_atom_jvm,"$jvm");
    DECLARE_ATOM(jasper_atom_long,"long");
    DECLARE_ATOM(jasper_atom_object,"object");
    DECLARE_ATOM(jasper_atom_plus_sign,"+");
    DECLARE_ATOM(jasper_atom_short,"short");
    DECLARE_ATOM(jasper_atom_string,"string");
    DECLARE_ATOM(jasper_atom_term,"term");
    DECLARE_ATOM(jasper_atom_true,"true");

    INIT_STATE_VARIABLE(jvm, NULL);
    INIT_STATE_VARIABLE(jnienv, NULL);
#if 0        /* [PD] 3.9 Use sp_(set/get)_jasper_magic() instead */
    INIT_STATE_VARIABLE(jasper_sp_global, NULL);
#endif
    INIT_STATE_VARIABLE(context_counter, 0);
#if 0      /* [PD] 3.9 use sp_(set/get)_jasper_threadservermode() instead */
    INIT_STATE_VARIABLE(threadservermode, JNI_FALSE); // [PD] default is single thread mode
#endif
  }
  /* Sets STATE_VARIABLE(jvm)  */
  jasper_set_jvm(SPAPI_ARG0);

  return;
#if (!JNI_VERSION_1_2 ) || 1 /* [PM] 3.8.6 Was: || ( !linux ) */
  RAISE_ERROR("init_jasper",0,0);
#endif
}


/* FOREIGN */
void SPCDECL deinit_jasper(SPAPI_ARG_PROTO_DECL int when)
{
  DbgBreakPoint();
  #if DBG
  fprintf(stderr, "\ndeinit_jasper(%d)\n", when); fflush(stderr);
  #endif
  (void)when;
#if MULTI_SP_AWARE
  (void)SPAPI_ARG_NAME;         /* avoid -Wunused */
#endif /* MULTI_SP_AWARE */

  /* no-op for now */
}

/* [PM] 3.8.5 Now ONLY used by this file. See SET_JNIENV
   Return current jnienv or NULL
   if NULL and not FROM_GLUE then raises SP exception.
   [PM] 3.8.7 Safe to call with silent==true even if init_jasper barfed about MM_SBRK
*/
static JNIEnv * jasper_int_get_jnienv(SPAPI_ARG_PROTO_DECL int from_glue /* a.k.a silent */)
{
  jint rv = 0;
  JNIEnv *jnienv = NULL;
  char *err_msg = NULL;


  /* [PM] See notes at top of file */

#if DBG>1
  fprintf(stderr, "in jasperi_int_get_jnienv(): JVM = %p (from_glue=%d)\n", STATE_VARIABLE(jvm), (int)from_glue);
#endif
    
  if (!STATE_VARIABLE(jvm)) goto cleanup;

  /* [PM] 3.9 Note that, even if we constrain SP API calls to happen
     in only one thread we will come here unattached if Java was
     already initialized by someone else (in some other thread). */

  /* Attach ourselves to the JVM, unless this has already been done. */
  rv = (*STATE_VARIABLE(jvm))->GetEnv(STATE_VARIABLE(jvm), (void **)&jnienv, JNI_VERSION_1_2);
  if (rv) jnienv = NULL;

  if (rv == JNI_EDETACHED)
    {
      if (0 > (*STATE_VARIABLE(jvm))->AttachCurrentThread(STATE_VARIABLE(jvm),(void **)&jnienv,NULL))
        {
          jnienv = NULL;
          err_msg = "Failed to attach thread to Java VM";
          goto cleanup;
        }
    }
  else if (rv == JNI_EVERSION)
    {
      err_msg = "Jasper requires JNI version 1.2";
      goto cleanup;
    }

 cleanup:

  /* if we're being called from within glue-code we return NULL and
     let glue raise exception instead)
  */

#if DBG>1
  if ( jnienv == NULL )
    {
      fprintf(stderr, "jnienv == NULL in jasperi_int_get_jnienv\n");
    }
#endif

  if ( jnienv == NULL && !from_glue )
    {
      jasper_int_raise_exception(SPAPI_ARG "java_exception", (err_msg ? err_msg :"Java engine not initialized"));
    }

  return jnienv;
}

#if JASPER_RESOURCES
/* [PM] 3.8.5 Now ONLY used by glue code (which should already have
   called jasperi_initialize_C to ensure jvm is setup. See SET_JNIENV
   [PM] 3.8.7 Safe to call even if init_jasper barfed about MM_SBRK
*/
JASPER_EXPORT JNIEnv * SPCDECL jasperi_get_jnienv(SPAPI_ARG_PROTO_DECL SP_term_ref ref)
{

#if DBG>1
  fprintf(stderr, "getting jnienv from within glue code\n");
#endif
  return jasper_int_get_jnienv(SPAPI_ARG 1 /* from glue */);
}
#endif /* JASPER_RESOURCES */

/* FOREIGN */
void SPCDECL jasperi_jvm_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref)
{
  JNIEnv *jnienv;
  
  jnienv = jasper_int_get_jnienv(SPAPI_ARG 1 /* silent */);
  
  if (jnienv != NULL)
    {
      SP_put_integer(jvmref,(long)STATE_VARIABLE(jvm));
      SP_cons_functor(jvmref,STATE_VARIABLE(jasper_atom_jvm),1,jvmref);
    }
  /* else jvmref is [] which is recognized in jasper.pl */
}

/* [PM] 3.8.5 New way */

/* strcat with automatic growing of SP_malloced buffer */
static char *jasper_strcat(SPAPI_ARG_PROTO_DECL char *buf, const char *str)
{
  unsigned int length = strlen(buf);
  unsigned int size = length+1;

  length += strlen(str);
#if DBG>42
  fprintf(stderr, "jasper_strcat(SPAPI_ARG \"%s\" (%u), \"%s\" (%u)) \n", buf, (unsigned)strlen(buf), str, (unsigned)strlen(str));
#endif

  if (length >= size)
    {
      char *tmp_buf;

      size = length+1;
#if DBG>42
      fprintf(stderr, "SP_realloc(\"%s\" (%u), %u)\n", buf, (unsigned)strlen(buf), (unsigned) size);
#endif

      tmp_buf = SP_realloc(buf, size);
      if (tmp_buf == NULL)
        {
          SP_free(buf);
          return NULL;
        }
      buf = tmp_buf;
    }
#if DBG>42
  fprintf(stderr, "jasper_strcat: strcat(\"%s\" (%u), \"%s\" (%u)) \n", buf, (unsigned)strlen(buf), str, (unsigned)strlen(str));
#endif

  strcat(buf,str);
  return buf;
}



/* 3.8.5 If this has to create a JVM and thus a SICStus object then it
   should enter the SICStus monitor (representing the fact that the
   current thread is still in the prolog run-time)
*/
static SP_term_ref jasperi_initialize(SPAPI_ARG_PROTO_DECL int numopts, SP_term_ref classpath, SP_term_ref tr, int from_glue)
{

  int i;
  SP_term_ref elem, tail, jref, dummy;
  char *bootpath, *p;
  char *shlibpath = NULL;
  char *full_classpath = NULL;  /* Complete classpath */
#if 0
  char sp_cpath[MAXPATHLEN];    /* [...]/jasper.jar */
  char *env_cpath = NULL;       /* Classpath specified through environment */
#endif
  char *prop_cpath = "-Djava.class.path=";

  JavaVMOption *options = NULL;
  JavaVMInitArgs vm_args;
  JNIEnv *jnienv = NULL;
  int jvm_already_present;

#if SP_WIN32
  char *pathsep = ";";
#else
  char *pathsep = ":";
#endif

#if DBG    
  fprintf(stderr, "Enter: jasperi_initialize\n");
#endif

  /* [PM] 3.8.7 Barf if incompatible memalloc used (SPRM 2209) */
  if (sp_get_mem_usage() == MM_USE_SBRK)
    {
      jref = 0;                 /* silence compiler. */
      SAVE_ERROR(SYSTEM_ERROR,"Attempted to load Java engine into sbrk'd SICStus system (try starting SICStus with -m option)",SP_new_term_ref());
    }

  elem = SP_new_term_ref();
  tail = SP_new_term_ref();
  jref = SP_new_term_ref();
  dummy = SP_new_term_ref();

  /* [PM] 3.9 ignore arguments passed from glue (for now). */
  if (from_glue)
    {
      numopts = 0;
      classpath = -1;
      tr = -1;
    }


  jasper_set_jvm(SPAPI_ARG0);

  /* "Is there anybody out there?" --Pink Floyd */
  if (STATE_VARIABLE(jvm) != NULL)
    {
#if DBG    
      fprintf(stderr, "jasperi_initialize found JVM, not creating\n");
#endif

      jvm_already_present = 1;
      /* Yes, someone was out there. Warn the user that any
       * options weren't used, since no JVM was created. */
#if DBG    
      if (numopts > 1)
        {
          fprintf(stderr, "* No new JVM was created (JVM startup options ignored)\n");
        }
#endif
    }
  else
    { /* No JVM available. Lets create one. */


#if DBG
      /* We depend on wm_args.ignoreUnrecognized to be true when DBG */
      char * extra_debug_opts[] = {
#if 0
        "-Xcheck:jni",          /* Not supported by JDK 1.3 HotSpot, Solaris 1.2.2.06 */
#endif /* disable "-Xcheck:jni" */
#if 0
       #error "Do something about -Xrs"
        "-Xrs",                 /* [PM] 3.8.6 Only JDK 1.3.1. Reduced use of signals */
#endif /* disable -Xcheckrs */

        "-verbose:jni",
        "-Dse.sics.jasper.SICStus.debugLevelDefault=" STRINGISIZE(DBG),
        "-Dse.sics.jasper.SICStus.checkSPTermAgeDefault=true",
        "-Dse.sics.jasper.SICStus.reuseTermRefsDefault=true"
         /* , "-Xfuture" does not work even with ignoreUnrecognized !? */
      };

      const int NUM_EXTRA_DBG_OPTS = (sizeof extra_debug_opts)/(sizeof *extra_debug_opts);
#else                     /* !DBG */
      const int NUM_EXTRA_DBG_OPTS = 0;
#endif                    /* !DBG */
      /* We add 2 extra options ourselves, -Djava.class.path and
       * -Djava.library.path */
      const int NUM_EXTRA_OPTS =
        1
#if !SP_WIN32                   /* 3.9, see -Djava.library.path */

        +
        1
#endif /* !SP_WIN32 */
        +NUM_EXTRA_DBG_OPTS;


#if DBG    
      fprintf(stderr, "jasperi_initialize found no JVM, creating\n");
#endif
      jvm_already_present = 0;

      numopts += NUM_EXTRA_OPTS;
      options = SP_malloc((numopts) * sizeof(JavaVMOption));
      if (!options)
        {
          if (from_glue) goto cleanup;
          SAVE_ERROR(SYSTEM_ERROR,"memory allocation failure",dummy); /* goes to error_: */
        }

      vm_args.version = JNI_VERSION_1_2;
      vm_args.options = options;
      vm_args.nOptions = numopts;
      {
        jboolean ignoreUnrecognized = JNI_FALSE;
#if DBG
        ignoreUnrecognized = JNI_TRUE;
#endif
        vm_args.ignoreUnrecognized = ignoreUnrecognized;
      }

      /* 3.8.5 new way to build classpath */
      {
        const char *sep = "";               /* set to pathsep at first path element */
#if DBG
        fprintf(stderr, "Building classpath list \n");
#endif
        full_classpath = (char *) SP_malloc(1);

        if (full_classpath)
          {
            full_classpath[0] = '\0';
            full_classpath = jasper_strcat(SPAPI_ARG full_classpath, prop_cpath);
          }

        if (!from_glue)
          {
            if (SP_is_list(classpath))
              {
                SP_foreach(classpath,elem,tail,i)
                  {
                    if (!SP_get_string(elem,&p))
                      {
                        SAVE_ERROR(SYSTEM_ERROR,"Malformed classpath",dummy); /* goes to error_: */
                      }
#if DBG
                    fprintf(stderr, "Concatentating classpath list element '%s' + '%s' %s\n", sep, p, (full_classpath?"":"<<NULL>>"));
#endif
                    if (p[0])
                      {
                        if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,sep);
                        if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,p);
                        sep = pathsep;
                      }
                  }
              }
            else
              {
                if (!SP_get_string(classpath,&p))
                  {
                    SAVE_ERROR(SYSTEM_ERROR,"Malformed classpath",dummy); /* goes to error_: */
                  }
                if (p[0])
                  {
                    if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,sep);
                    if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,p);
                    sep = pathsep;
                  }
              }
          }

        {                       /* NOTE: Should clean-up the way we get SP classpath */
          char sp_cpath[MAXPATHLEN];    /* [...]/jasper.jar */
          sp_get_classpath(sp_cpath,(sizeof sp_cpath)); /* xref sicstus.c */
          if (sp_cpath[0])
            {
              if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,sep);
              if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,sp_cpath);
              sep=pathsep;
            }
        }

        {
          const char *env_cpath = getenv("CLASSPATH");
          if (env_cpath && env_cpath[0])
            {
              if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,sep);
              if (full_classpath) full_classpath = jasper_strcat(SPAPI_ARG full_classpath,env_cpath);
              sep = pathsep;
            }
        }

        /* done */
        if (!full_classpath)
          {
            if (from_glue) goto cleanup;
            SAVE_ERROR(SYSTEM_ERROR,"memory allocation failure",dummy); /* goes to error_: */
          }
        options[0].optionString = full_classpath;
      }

      bootpath = sp_get_boot_path();

      /* <hack type=horrible>
         This is a horrible hack in order for Java to be able to find the
         "right" copy of libjasper.so. See jasper.pl for a discussion. */

      shlibpath = SP_malloc(strlen(bootpath)+40); /* 40 (over-) compensates for the
                                                     format-string below */
      if (!shlibpath)
        {
          if (from_glue) goto cleanup;
          SAVE_ERROR(SYSTEM_ERROR,"memory allocation failure",dummy); /* goes to _error: */
        }

#if 1
 /* 3.9 (actually more an issue in 3.8). On Win32 java.library.path
    cannot help since it will not be used to resolve the dependence on
    sprt.dll so sprt.dll need to be in PATH and thus, so will the
    JNI-stuff (jasper.dll in 3.8, ??? in 3.9) */

#if !SP_WIN32
      sprintf(shlibpath,"-Djava.library.path=%s/../..",bootpath);
      options[1].optionString = shlibpath;
#endif /* !SP_WIN32 */

#else  /* 3.8.6 Sets up java.library.path unnecessarily in Win32 */

#if SP_WIN32
      sprintf(shlibpath,"-Djava.library.path=%s",bootpath);
#else  /* #if !SP_WIN32 */
      sprintf(shlibpath,"-Djava.library.path=%s/../..",bootpath);
#endif /* !SP_WIN32 */
      options[1].optionString = shlibpath;
      /* </hack> */
#endif

#if DBG
      {
        int i;
        for ( i = 0; i < NUM_EXTRA_DBG_OPTS; i++)
          {
            fprintf(stderr, "DBG: Adding extra JVM option #%d=\"%s\"\n", i, extra_debug_opts[i]);
            options[(NUM_EXTRA_OPTS-NUM_EXTRA_DBG_OPTS)+i].optionString = extra_debug_opts[i];
          }
      }
#endif

      /* If tr < 0, it means that this function is being called from within the
       * glue code mechanism, without user-specified options. */
    
      if (!from_glue)
        {
          /* Assemble user-specified JVM options */
          for (SP_put_term(tail,tr),i = NUM_EXTRA_OPTS; SP_get_list(tail,elem,tail); i++)
            {
              if (!SP_get_string(elem,&options[i].optionString))
                {
                  SAVE_ERROR(SYSTEM_ERROR,"Malformed options",dummy); /* goes to _error: */                  
                }
            }
        }
#if DBG
      for (i = 0; i < numopts; i++)
        {
          fprintf(stderr, "option-string[%d] = %s\n",i,options[i].optionString);
        }
#endif     


      /* [PM] 3.8.6 Call a hook that turns off some signal handlers
         (for JDK 1.3 compatibility, a.k.a -Xrs.) */
      {
        SP_pred_ref signals_hook_pred = SP_predicate("reduce_use_of_os_signals", 0, "prolog");
        
        if (signals_hook_pred)
          {
            SP_query_cut_fail(signals_hook_pred);
          }
      }
    
      if (0 > JNI_CreateJavaVM(&STATE_VARIABLE(jvm), (void **)&jnienv, &vm_args))
        {
          jnienv = NULL;
          if (from_glue) goto cleanup;
          SAVE_ERROR(SYSTEM_ERROR, "Could not create Java VM",dummy); /* goes to _error: */
        }
#if DBG
      fprintf(stderr, "*** Created JVM=%p\n", STATE_VARIABLE(jvm));
#endif

#if !NEW_WORLD_ORDER
      created_jvm = jvm;
#endif /* !NEW_WORLD_ORDER */
    }

  /* if we get here jvm is set-up */

  
  /* Ensure this thread is attached to the JVM */
  if (!jnienv)                  /* jvm was already created so we may not be attached */
    {
      if ( (jnienv = jasper_int_get_jnienv(SPAPI_ARG 1 /* silent */)) == NULL )
        {
          if (from_glue) goto cleanup;
          SAVE_ERROR(SYSTEM_ERROR, "Failed to attach thread to Java VM", dummy);
        }
    }

  {
    /* Create a term sent to the user. This is actually a dummy; the
       value is currently not used (see comment at top of file).
    */

    SP_term_ref tmp = SP_new_term_ref();
    
    SP_put_integer(tmp,(long)STATE_VARIABLE(jvm));
    SP_cons_functor(jref,STATE_VARIABLE(jasper_atom_jvm),1,tmp);
  }


  /*
   * if we're being called from the glue-code we need to ensure there is a
   * SICStus object as well. (3.8.4 unconditionally created a new SICStus
   * object, great fun with finalizer that does SP_deinitialize...)
   */
#if 0
  if (/* 3.8.5 always ensure there is a (locked) SICStus object
         Was: from_glue && */
      (jasper_get_sicstus_object(SPAPI_ARG jnienv) == NULL))
#else
      /* [PD] 3.9 jasper_get_sicstus_object() now calls sp_get_jasper_magic()
                  which returns NULL as an error (crash! bang! if DBG>0).
                  If there is no JVM present there is no SICStus.
                  If there is a JVM there better be a SICStus. 
                  Only create a new SICStus object if there is no JVM.
      */
    if (!jvm_already_present)
#endif
    {
#if DBG>1
      fprintf(stderr, "*** Creating SICStus object (JNI %p, JVM %p)\n", (void*)STATE_VARIABLE(jnienv), (void*)STATE_VARIABLE(jvm));
#endif

#if NEW_WORLD_ORDER
      {
        jobject spobj, gspobj;

        spobj = jasperi_new_sicstus_object(SPAPI_ARG jnienv);
        if (spobj == NULL) goto cleanup;
        gspobj = (*jnienv)->NewGlobalRef(jnienv, spobj);
        if (gspobj == NULL) goto cleanup;
#if 0
        STATE_VARIABLE(jasper_sp_global) = gspobj;
#else  /* [PD] 3.9 */
        if (!sp_set_jasper_magic(gspobj, SICSTUS_VERSION)) {
          SAVE_ERROR(SYSTEM_ERROR,"Mismatch in Jasper versions", dummy);
        }
#endif
      }
#else  /* 3.8 */
      if (jasperi_new_simple_object(SPAPI_ARG jnienv, "se/sics/jasper/SICStus") == NULL) goto cleanup;
#endif /* 3.8 */

      /* jasper_get_sicstus_object() should be non-NULL here */
#if DBG
      if (jasper_get_sicstus_object(SPAPI_ARG jnienv) == NULL)
        {
#if 0
          fprintf(stderr, "\n**** ERROR: Creating SICStus object failed to set jasper_sp_global\n");
#else  /* [PD] 3.9 */
          fprintf(stderr, "\n**** ERROR: Creating SICStus object failed to set the magic global SICStus object\n");
#endif
          abort();
        }
#endif
      
    }

  if (!jvm_already_present)
    {

#if !NEW_WORLD_ORDER
#if DBG>1
      fprintf(stderr, "*** Initial entry to SICStus monitor (JNI %p)\n", jnienv);
#endif
      jasper_enter_sicstus_monitor(jnienv);
#endif

    }

 cleanup:
  
  if (options != NULL) SP_free(options);
  if (full_classpath != NULL) SP_free(full_classpath);
  if (shlibpath != NULL) SP_free(shlibpath);

  if (from_glue && !jnienv) return -1; /* tell glue we failed */

  return jref;

  /* comes here from SAVE_ERROR */
 _error:
  SP_raise_error("jasperi_initialize", 0, 0);
  goto cleanup;
}

/* FOREIGN */
/* only called from jasper.pl */
SP_term_ref SPCDECL jasperi_initialize_pl_C(SPAPI_ARG_PROTO_DECL long numopts, SP_term_ref classpath, SP_term_ref tr)
{
  return jasperi_initialize(SPAPI_ARG numopts, classpath, tr, 0 /* !from glue */);
}

#if JASPER_RESOURCES
/* only called from glue code (with ignored args (0,-1,-1)) */
JASPER_EXPORT SP_term_ref SPCDECL jasperi_initialize_C(SPAPI_ARG_PROTO_DECL int /* yes, an int! */ numopts, SP_term_ref classpath, SP_term_ref tr)
{
  return jasperi_initialize(SPAPI_ARG numopts, classpath, tr, 1 /* from glue */);
}
#endif /* JASPER_RESOURCES */

/* FOREIGN */
long SPCDECL jasperi_deinitialize_C(SPAPI_ARG_PROTO_DECL SP_term_ref tr)
{
#if !NEW_WORLD_ORDER            /* [PM] in JDK 1.2 (& 1.3?) DestroyJavaVM is a no-op anyway */
  jasper_set_jvm(SPAPI_ARG0);

  if (jvm != NULL && created_jvm != NULL) {
      int rv;
      rv = (*jvm)->DestroyJavaVM(jvm);
      if (rv == 0) jasper_set_jvm(SPAPI_ARG0);

      if (rv != 0 || (numjvms != numjvms_prev - 1)) {
        return -1;
      }
  }
#endif /* NEW_WORLD_ORDER */

#if MULTI_SP_AWARE
  (void)SPAPI_ARG_NAME;         /* avoid -Wunused */
#endif
  (void)tr;
  
  return 0;
}




/* Return a local class ref for class CLASSNAME
   Post: jclass != NULL or SP exception
         Java exceptions cleared
*/
static jclass jasperi_lookup_class(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, char *className) 
{
  char buf[1024];
  jclass clazz;

  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_class");

  /* Find the class */
  clazz = (*jnienv)->FindClass(jnienv,className);
  if (!clazz)
    {
#if HAVE_SNPRINTF      
      snprintf(buf, 1023, "could not find class %s", className);
#else
      sprintf(buf, "could not find class %s", className);
#endif

#if DBG>1
      fprintf(stderr, "DBG: %s\n", buf);
#endif

      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }
  return clazz;
}

static void dbg_print_object_class(JNIEnv *jnienv, jobject jobj, char *extra_info)
{
  jclass clazz;
  jclass classclazz;
  jmethodID mid;

  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 1");

#if DBG
  fprintf(stderr, "dbg_print_object_class of object %ld\n", (long) jobj);
#endif
  if (jobj == NULL) return;

  clazz = (*jnienv)->GetObjectClass(jnienv,jobj);

#if DBG
  fprintf(stderr, "dbg_print_object_class class of %ld is %ld\n", (long) jobj, (long) clazz);
  if ((*jnienv)->ExceptionCheck(jnienv)) jasper_DescribeException(jnienv);
#endif

  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 2");
#if DBG
  if ((*jnienv)->ExceptionCheck(jnienv)) jasper_DescribeException(jnienv);
#endif
  classclazz = (*jnienv)->GetObjectClass(jnienv,clazz);
  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 3");
  mid = (*jnienv)->GetMethodID(jnienv,classclazz,"getName","()Ljava/lang/String;");
  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 4");
  if (mid)
    {
      jobject jname;
        
      jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);

      if (jname)
        {
          char *s;
          s = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
          ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 5");
          if (s)
            {
              fprintf(stderr, "DBG: %s Class of object (%ld) = '%s'\n", ( extra_info ? extra_info : "" ), (long)jobj, s);
              (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,s);
                ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 6");
            }
        }
    }
}



static jclass jasperi_lookup_class_from_obj(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj) 
{
  char buf[1024];
  jclass clazz;
  
  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_class_from_obj");

  if (!(clazz = (*jnienv)->GetObjectClass(jnienv,jobj)))
    {
      /* according to the docs this cannot happen */
#if HAVE_SNPRINTF      
      snprintf(buf, 1023, "could not find class for object %lx", (long)jobj);
#else
      sprintf(buf, "could not find class for object %lx", (long)jobj);
#endif
      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }
  else
    {

#if DBG>1
      {
        jmethodID mid;

        mid = (*jnienv)->GetMethodID(jnienv,(*jnienv)->GetObjectClass(jnienv,clazz),"getName","()Ljava/lang/String;");
      
        if (mid)
          {
            jobject jname;
        
            jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);

            if (jname)
              {
                char *s;
            
                s = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
                if (s)
                  {
                    fprintf(stderr, "jasperi_lookup_class_from_obj Class of object (%ld) = %s\n",(long)jobj, s);
                    (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,s);
                  }

              }
          }
       

      }
#endif

      return clazz;
    }
}

/* Post: mid != 0 or SP exception
         Java exceptions cleared
*/
static jmethodID jasperi_lookup_method(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jclass clazz, const char *methodName, const char *typeSig, int stat)
{
  char buf[1024];
  jmethodID mid;

#if DBG>1
  fprintf(stderr, "jasperi_lookup_method(SPAPI_ARG JNIEnv %p, jclass %p, methodName %s, typeSig %s, stat %d)\n", (void*)jnienv, (void*) clazz, methodName, typeSig, stat);
#endif

  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_method");

  /* find the method */
  if (stat)
    mid = (*jnienv)->GetStaticMethodID(jnienv,clazz,methodName,typeSig);
  else
    mid = (*jnienv)->GetMethodID(jnienv,clazz,methodName,typeSig);

  if (!mid)
    {
#if HAVE_SNPRINTF
      snprintf(buf, 1023, "could not find method %s, signature %s", methodName, typeSig);
#else
      /* unsafe version */
      sprintf(buf, "could not find method %s, signature %s", methodName, typeSig);
      /*       sprintf(buf, "could not find method"); */
#endif

      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }

  return mid;
}

/* FOREIGN */
void SPCDECL 
jasperi_object_class_name_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref, SP_term_ref name)
{
  jclass clazz = NULL;
  jclass clazzclazz = NULL;
  jobject jname = NULL;
  char *className = NULL;

  jmethodID mid;
  jobject jobj;
  JNIEnv *jnienv;
  JASPER_DBGLINE(jasperi_object_class_name_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});
  ASSERT_NO_EXCP(jnienv, "jasperi_object_class_name_C");
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
      goto cleanup;
    }
  JASPER_DBGLINE(jasperi_object_class_name_C);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  clazzclazz = (*jnienv)->GetObjectClass(jnienv, clazz); /* should be == clazz */
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if ( (mid = (*jnienv)->GetMethodID(jnienv,clazzclazz,"getName","()Ljava/lang/String;")) == NULL )
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot find method clazz.getName()",1);
      goto cleanup;
    }

  JASPER_DBGLINE(jasperi_object_class_name_C);
  jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (jname == NULL)
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot find get clazz name",1);
      goto cleanup;
    }
  JASPER_DBGLINE(jasperi_object_class_name_C);
  className = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (!SP_put_string(name, className))
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot make an atom from class name",1);
    }
 cleanup:
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (className) (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,className);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (jname) DELETE_LOCAL_REF(jnienv, jname);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (clazzclazz) DELETE_LOCAL_REF(jnienv, clazzclazz);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_object_class_name_C);
}

#if 0                           /* [PM] 3.9b5 no longer used */
static jobject jasperi_new_simple_object(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, char *className)
{
  const char *typesig = "()V";
  jclass clazz = NULL;
  jmethodID mid;
  jobject jobj = NULL;

  /* if any of these fail then an SP exception has been raised */
  if ( (clazz = jasperi_lookup_class(SPAPI_ARG jnienv,className)) == NULL ) goto cleanup;
  if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz, "<init>", typesig, 0 /*!static*/)) == 0 ) goto cleanup;

  /* create the object */
  jobj = (*jnienv)->NewObject(jnienv, clazz, mid);

  if (jasper_int_handle_exception(SPAPI_ARG jnienv,"jasperi_new_simple_object" " @" __FILE__ ":" STRINGISIZE(__LINE__),0))
    {
      DELETE_LOCAL_REF(jnienv, jobj);
      jobj = NULL;
      goto cleanup;
    }

 cleanup:
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);

  return jobj;
}
#endif /* 0 */






struct meta_args_state;

struct meta_args_state {
  int is_void;                  /* TRUE if method is void */
  int returns_term;             /* see jasperi_process_meta_args */
  int numargs;                  /* >= 0 if valid */
  jvalue jargs[256];            /* (max arity 255 + return arg at jargs[0]) */
  int is_object[256];
};

/* 
   Should be called three times after each other:
   mode = 1 (a.k.a. calling) to set-up jargs vector
   mode = 2 (a.k.a. returning) to pick out results and unify out args
   mode = 3 (a.k.a cleanup) to perform clean-up (free temporary objects).

   . If a call with mode 1 returns with a value different from
     SP_SUCCESS then no call with mode 2 should be performed.

   . If a call with mode 2 returns SP_FAIL then the call failed in
     back unification (not an error). If mode 2 returns SP_ERROR than
     an exception has already been raised.

   . A call with mode = 3 should always be done as a last step even if
     one of the previous modes did not return SP_SUCCESS.
*/
#if THREADSERVER
static int jasperi_process_meta_args(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                     SP_term_ref methodDesc,
                                     SP_term_ref args,
                                     jvalue **pjargs,
                                     int *returns_term,
                                     struct meta_args_state *state,
                                     int instance,
                                     int mode,
                                     jboolean new_style_interface)
#else
static int jasperi_process_meta_args(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                     SP_term_ref methodDesc,
                                     SP_term_ref args,
                                     jvalue **pjargs,
                                     int *returns_term,
                                     struct meta_args_state *state,
                                     int instance,
                                     int mode)
#endif
{
  const int calling = (mode==1);
  const int returning = (mode==2);
  const int cleanup = (mode==3);
  int numargs;
  int i, argno;
  int first_argno = ( instance ? 2 : 1 ); /* instance method: first arg is the instance so skip it */
  const int idx_limit = ((sizeof state->is_object)/(sizeof state->is_object[0])); /* a.k.a 256 */
  int retval_seen = 0;        /* both flag and index adjustment */
  jvalue *jargs = &(state->jargs[0]);
  int *is_object = &(state->is_object[0]); /* TRUE if this object
                                              should be deleted on cleanup
                                           */

  SP_term_ref argtype = SP_new_term_ref();
  SP_term_ref arg = SP_new_term_ref();
  /* tmp_tr is for temporary use when processing an argument. Also set
     to out and return value for arg to be unified with */
  SP_term_ref tmp_tr = SP_new_term_ref();

#if DBG
  fprintf(stderr, "jasperi_process_meta_args: %s method mode=%s\n", (instance ? "instance" : "static"), (calling ? "calling" : (returning ? "returning" : (cleanup ? "cleanup" : "ERROR"))));
#endif

  JASPER_DBGLINE(jasperi_process_meta_args);

  /* initialize state */
  if (calling)
    {
      int i;
      for (i = 0; i < idx_limit; i++) /* a.k.a. i < 256 */
        {
          is_object[i] = FALSE;
        }
      state->is_void = TRUE;    /* assume VOID until retval seen */
      state->numargs = -1;      /* mark as invalid */
      /*

        Note: Now (late 3.8.5) context popped after handling return
        values so special [-term] handling should not be needed. Still
        works though:

        Special case for [-term]. The SPTerm will be reset by
        pop_context so the term ref must be extracted immediately
        after the call. *returns_term==TRUE tells the caller to do
        this. The caller should then DeleteRef the SPTerm object and
        put the term ref in jargs[0].j and set *returns_term to FALSE
        again (so the cleanup code does not tryto DeleteRef the term
        ref).  (The same problem applies to generated glue code) */
      state->returns_term=FALSE; /* set to TRUE when [-term] seen, never reset */
      *returns_term = FALSE;    /* set to TRUE when [-term] seen, reset to false
                                   in caller when jargs[0].j set to the term ref */
      *pjargs = jargs;
    }
  numargs = state->numargs;

  if (returning || cleanup)
    {
      if (state->returns_term   /* jargs[0] used to be an SPTerm object */
          && !*returns_term)    /* jargs[0] changed by caller to jlong */
        {
          is_object[0] = FALSE;
        }
    }

  /* From now on it is safe to goto label barf et al. */
  
  if (cleanup) { JASPER_DBGLINE(jasperi_process_meta_args); goto cleanup;} /* assume caller reported error */
  JASPER_DBGLINE(jasperi_process_meta_args);

  /* onle get here when calling or returning */


  if (calling)              /* not yet valid */
    {
      SP_atom pred_name;
  
      if (!SP_get_functor(methodDesc, &pred_name, &numargs))
        {
          numargs = -1;         /* mark as invalid */
          JASPER_DBGLINE(jasperi_process_meta_args);
          goto barf;
        }
      else
        {
#if DBG
          fprintf(stderr, "jasperi_process_meta_args: numargs=%d, pred_name=%s\n", (int)numargs, SP_string_from_atom(pred_name));
#endif
          state->numargs = numargs; /* used in subsequent calls */
        }
    }

#if DBG
          fprintf(stderr, "jasperi_process_meta_args: numargs=%d\n", (int)numargs);
#endif

  JASPER_DBGLINE(jasperi_process_meta_args);

  if (numargs < 0) {
#if DBG
    fprintf(stderr, "INTERNAL ERROR: jasperi_process_meta_args numargs < 0\n"); /* illegal call sequence! */
#endif
    JASPER_DBGLINE(jasperi_process_meta_args); goto barf; }

  JASPER_DBGLINE(jasperi_process_meta_args);

  for (argno = first_argno, i = 1; argno <= numargs; argno++)
    {
      int retval = FALSE;
      int inarg;

#if DBG
      fprintf(stderr, "jasperi_process_meta_args: processing arg #%d (jargs[%d])\n", argno, i);
#endif

      if (!SP_get_arg(argno, methodDesc, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
      JASPER_DBGLINE(jasperi_process_meta_args);        
      if (SP_is_list(argtype))    /* argtype = [-Type] */
        {
          if (!SP_get_arg(1, argtype, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;} /* argtype = -Type */
          if (retval_seen) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          retval_seen = 1;
          retval = TRUE;
          state->is_void = FALSE;
        }
      JASPER_DBGLINE(jasperi_process_meta_args);
      /* argtype should be +Type or -Type here  */
      {
        int type_arity;
        SP_atom type;
        {
          SP_atom plus_or_minus;
          int one;

          if (!SP_get_functor(argtype, &plus_or_minus, &one) || one!=1) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          JASPER_DBGLINE(jasperi_process_meta_args);
          inarg = (plus_or_minus == STATE_VARIABLE(jasper_atom_plus_sign));

#if DBG
          fprintf(stderr, "jasperi_process_meta_args: plus_or_minus=`%s'==%s\n", SP_string_from_atom(plus_or_minus), (inarg ? "input arg" : "output arg"));
#endif
        }
          
        /* argtype should be +Type or -Type here  */
        JASPER_DBGLINE(jasperi_process_meta_args);
        if (!SP_get_arg(1, argtype, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);
        /* argtype is Type */
        if (!SP_get_functor(argtype, &type, &type_arity)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);
        if (!SP_get_arg(argno, args, arg)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);

#if DBG
        fprintf(stderr, "jasperi_process_meta_args: processing %s%s%s%s\n", (retval ? "[" : ""),  (inarg ? "+" : "-"), SP_string_from_atom(type), (retval ? "]" : ""));
#endif

        /* Extremely tedious but straightforward */
        if (type == STATE_VARIABLE(jasper_atom_byte))
          {
            if (inarg)        /* +byte */
              {
                if (calling)
                  {
                    long x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].b = (jbyte) x;
                  }
              }
            else
              if (retval)     /* [-byte] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].b)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -byte */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_char))
          {
            if (inarg)        /* +char */
              {
                if (calling)
                  {
                    long x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].c = (jchar) x;
                  }
              }
            else
              if (retval)     /* [-char] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].c)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -char */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_short))
          {
            if (inarg)        /* +short */
              {
                if (calling)
                  {
                    long x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].s = (jshort) x;
                  }
              }
            else
              if (retval)     /* [-short] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].s)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -short */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_integer))
          {
            if (inarg)        /* +integer */
              {
                if (calling)
                  {
                    long x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].i = (jint) x;
                  }
              }
            else
              if (retval)     /* [-integer] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].i)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -integer */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_long))
          {
            if (inarg)        /* +long */
              {
                if (calling)
                  {
                    long x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].j = (jlong) x;
                  }
              }
            else
              if (retval)     /* [-long] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, (long)jargs[0].j)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -long */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_float))
          {
            if (inarg)        /* +float */
              {
                if (calling)
                  {
                    double x;
                    if (!SP_get_float(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].f = (jfloat) x;
                  }
              }
            else
              if (retval)     /* [-float] */
                {
                  if (returning)
                    {
                      if (!SP_put_float(tmp_tr, (double) jargs[0].f)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -float */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_double))
          {
            if (inarg)        /* +double */
              {
                if (calling)
                  {
                    double x;
                    if (!SP_get_float(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].d = (jdouble) x;
                  }
              }
            else
              if (retval)     /* [-double] */
                {
                  if (returning)
                    {
                      if (!SP_put_float(tmp_tr, (double)jargs[0].d)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -double */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_boolean))
          {
            if (inarg)        /* +boolean */
              {
                if (calling)
                  {
                    SP_atom x;
                    if (!SP_get_atom(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].z = ((x==STATE_VARIABLE(jasper_atom_true)) ? JNI_TRUE : JNI_FALSE);
                  }
              }
            else
              if (retval)     /* [-boolean] */
                {
                  if (returning)
                    {
                      if (!SP_put_atom(tmp_tr, ((jargs[0].z != JNI_FALSE) ? STATE_VARIABLE(jasper_atom_true) : STATE_VARIABLE(jasper_atom_false)))) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -boolean */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_object))
          {
            if (inarg)        /* +object */
              {
                JASPER_DBGLINE(jasperi_process_meta_args);
                if (calling)
                  {
                    jobject x;
                    JASPER_DBGLINE(jasperi_process_meta_args);
                    if (!SPJavaGlueGetObjectM(SPAPI_ARG jnienv, arg, &x, -1, NULL))
                        {
                          JASPER_DBGLINE(jasperi_process_meta_args);
                          goto reported_barf;
                        }
                    #if DBG
                    fprintf(stderr, "\nx==%p\n", x);
                    #endif

                    JASPER_DBGLINE(jasperi_process_meta_args);
                    /* make it safe to DeleteRef in cleanup and also
                       safe to call jasper_delete_ref on the term
                       representation (Note that NULL is an OK arg to DeleteLocalRef */
                    jargs[i].l = (x ? (*jnienv)->NewLocalRef(jnienv, x) : NULL);
                    #if DBG+0>0
                    fprintf(stderr, "\njargs[i].l==%p\n", jargs[i].l);
                    #endif
                    JASPER_DBGLINE(jasperi_process_meta_args);
                    is_object[i] = TRUE;
                  }
              }
            else
              if (retval)     /* [-object] */
                {
                  /* is_object[0] is set to FALSE later when it is certain that we will not fail or err */
                  if (returning)
                    {
                      if (! jobject_to_termref(SPAPI_ARG jnienv, jargs[0].l, tmp_tr) )
                        {
                          #if NULL_OBJREF
                          ;     /* do nothing, null is OK */
                          #else /* !NULL_OBJREF */
                          /* conversion failed (or NULL) NOTE: better exception needed */
                          jasper_int_handle_exception(SPAPI_ARG jnienv,"null object return value in jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
                          
                          goto reported_barf;
                          #endif
                        }
                    }
                }
              else            /* -object */
                {
                  { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_term))
          {
            if (inarg)        /* +term (se/sics/jasper/SPTerm)*/
              {
                if (calling)
                  {
                    jobject x;
                    SP_term_ref arg_copy = SP_new_term_ref();
                    
                    SP_put_term(arg_copy, arg);
#if THREADSERVER
                    x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, arg_copy, -1, NULL, TRUE /* init */, new_style_interface);
#else
                    x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, arg_copy, -1, NULL, TRUE /* init */);
#endif
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
#if DBG
                    fprintf(stderr, "jasperi_process_meta_args: calling +term jargs[%d].l = %p\n", (int)i, (void*)x);
#endif

                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
#if DBG
                    fprintf(stderr, "jasperi_process_meta_args: returning +term jargs[%d].l == %p\n", (int)i, (void*)jargs[i].l);
#endif
                  }
              }
            else
              if (retval)     /* [-term] */
                {
                  /* Kludge, see doc for returns_term */

                  if (calling)
                    {
                      *returns_term = TRUE;
                      state->returns_term = TRUE;
                    }
                  else /* if (returning) */
                    {
                      if ((*returns_term) /* INTERNAL ERROR: should have been reset in caller */
                          || is_object[0] /* INTERNAL ERROR: should have been reset when Mode==returning */
                          )
                        {
#if DBG
                          fprintf(stderr, "INTERNAL ERROR: jasperi_process_meta_args ((*returns_term) || is_object[0])\n");
#endif
                          JASPER_DBGLINE(jasperi_process_meta_args); goto barf;
                        }
                      if (!SP_put_term(tmp_tr, (SP_term_ref)jargs[0].j)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -term */
                {
                  if (calling)
                    {
                      jobject x;
#if THREADSERVER
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */, new_style_interface);
#else
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */);
#endif
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      int rc;
                      rc = SPJavaGlueGetNativeTermRefM(SPAPI_ARG jnienv, jargs[i].l, &tmp_tr, -1, NULL);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_atom))
          {
            if (inarg)        /* +atom (se/sics/jasper/SPCanonicalAtom) */
              {
                if (calling)
                  {
                    SP_atom the_atom;
                    jobject x;

                    if (!SP_get_atom(arg, &the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
#if THREADSERVER
                    x = SPJavaGlueSPCanonicalAtomM(SPAPI_ARG jnienv, the_atom, -1, new_style_interface);
#else
                    x = SPJavaGlueSPCanonicalAtomM(SPAPI_ARG jnienv, the_atom, -1);
#endif
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    /*
                      (*jnienv)->DeleteLocalRef(jnienv, jargs[i].l);
                      jargs[i].l = NULL;
                    */
                  }
              }
            else
              if (retval)     /* [-atom] */
                {
                  if (returning)
                    {
                      int rc;
                      SP_atom the_atom;

                      rc = SPJavaGlueGetAtomM(SPAPI_ARG jnienv, jargs[0].l, &the_atom, -1, NULL);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      if (!SP_put_atom(tmp_tr, the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
              else            /* -atom */
                {
                  if (calling)
                    {
                      jobject x;

#if THREADSERVER
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */, new_style_interface);
#else
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */);
#endif
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      int rc;
                      rc = SPJavaGlueGetNativeTermRefM(SPAPI_ARG jnienv, jargs[i].l, &tmp_tr, -1, NULL);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      if (!SP_is_atom(tmp_tr))
                        {
                          goto failure;
                        }
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_chars))
          {
            if (inarg)        /* +chars (java/lang/String) */
              {
                if (calling)
                  {
                    char *chars;
                    jstring x;
                  
                    if (!SP_get_list_chars(arg, &chars)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                  
                    x = (*jnienv)->NewStringUTF(jnienv, chars);
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    /*
                      (*jnienv)->DeleteLocalRef(jnienv, jargs[i].l);
                      jargs[i].l = NULL;
                    */
                  }
              }
            else
              if (retval)     /* [-chars] */
                {
                  if (returning) /* (java/lang/String) */
                    {
                      int rc;
                                            
                      rc = SPJavaGluePostPutCharsM(SPAPI_ARG jnienv, jargs[0].l, tmp_tr, -1);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
              else            /* -chars (java/lang/StringBuffer) */
                {
                  if (calling)
                    {
                      jobject x;
                      x = SPJavaGlueStringBufferM(SPAPI_ARG jnienv, -1);
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      jstring string;
                      int rc;
                      
                      string = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[i].l, -1);
                      if (!string) goto reported_barf;
                      rc = SPJavaGluePostPutCharsM(SPAPI_ARG jnienv, string, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, string);

                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_string))
          {
            if (inarg)        /* +string (java/lang/String)*/
              {
                if (calling)
                  {
                    SP_atom the_atom;
                    const char *chars;
                    jstring x;
                    JASPER_DBGLINE(jasperi_process_meta_args);                  
                  
                    if (!SP_get_atom(arg, &the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    chars = SP_string_from_atom(the_atom);
                    if (!chars) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                  
                    x = (*jnienv)->NewStringUTF(jnienv, chars);
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    JASPER_DBGLINE(jasperi_process_meta_args);
                  }
              }
            else
              if (retval)     /* [-string] */
                {
                  if (returning) /* (java/lang/String) */
                    {
                      jstring x;
                      int rc;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[0].l, -1);
                      if (x==NULL) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      rc = SPJavaGluePostPutStrM(SPAPI_ARG jnienv, x, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, x);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      #if DBG
                      {
                        char *string = NULL;
                        fprintf(stderr, "[-string] = %s\n", ( SP_get_string(tmp_tr, &string) ? string : "<<ERROR: SP_get_string failed>>"));
                      }
                      #endif
                    }
                }
              else            /* -string (java/lang/StringBuffer) */
                {
                  if (calling)
                    {
                      jobject x;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGlueStringBufferM(SPAPI_ARG jnienv, -1);
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      jstring x;
                      int rc;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[i].l, 42);
                      if (x==NULL) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      rc = SPJavaGluePostPutStrM(SPAPI_ARG jnienv, x, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, x);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else
          {
#if DBG
            fprintf(stderr, "ERROR illegal arg type `%s'\n", SP_string_from_atom(type));
#endif

            { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          }
      }
      if (!retval) i++;

      /* if return value or outarg (-Type or [-Type]) then tmp_tr should be the out term */
      if (returning && !inarg)               /* a.k.a returning && (retval || !inarg) */
        {
#if DBG
          fprintf(stderr, "jasperi_process_meta_args: unifying return value (arg is %s (%d), tmp_arg is %s (%d)\n", (SP_is_variable(arg) ? "variable" : "NOT variable!"), SP_term_type(arg), (SP_is_atom(tmp_tr) ? "atom" : "not atom"), SP_term_type(tmp_tr));
#endif
          if (!SP_unify(arg, tmp_tr))
            {
              JASPER_DBGLINE(jasperi_process_meta_args);
              goto failure;
            }
        }
    } /* for */
  
  /* Got here without error or fail */

  if (returning && !state->is_void)
    {
      /* make sure the subsequent cleanup will not delete the return
         ref (now in a $jave_object wrapper
      */
      is_object[0] = FALSE;
    }

  return SP_SUCCESS;


 nyi:
  SPJavaGlueErrorM(SPAPI_ARG jnienv, -1, "Error in Java meta call: Unimplemented feature");
  { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}

 barf:
  if (!SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1, "Error in Java meta call (Java)"))
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, -1, "Error in Java meta call");
    }

  /* FALLTHROUGH */
 reported_barf:
  return SP_ERROR;

 failure:
  return SP_FAILURE;

 cleanup:
  /* 
     cleanup:
     need to go through is_object[0..numargs-1)] and DeleteLocalRef.
     previous calls (with mode==returning) will have set is_object[0]
     to FALSE if we will not fail or err (so the returned object is
     not freed). (if numargs < 0 then we should use idx_limit).
  */
  {
    int j;
    int limit;
    
    limit = ( numargs >= 0 ? numargs : idx_limit );
      
    for (j = 0; j < limit; j++)
      {
        if (is_object[j])
          {
#if DBG
            fprintf(stderr, "jasperi_process_meta_args: DeleteLocalRef(%ld,%ld)\n", (long)jnienv, (long)jargs[j].l);
#endif
            (*jnienv)->DeleteLocalRef(jnienv, jargs[j].l);
          }
      }
  }
  return SP_SUCCESS;
}

#if !THREADSERVER             /* [PD] 3.9 Old pre-threadserver code */

/* [PM] 3.8.5 NEW API */
/* FOREIGN */
void SPCDECL
jasperi_call_static1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                       char *className,
                       char *methodName,
                       char *typesig,
                       SP_term_ref methodDesc,
                       SP_term_ref args)

{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_call_static1_C */
  struct meta_args_state state;
  int result = SP_ERROR;
  int init = FALSE;             /* true if <init>, i.e., NewObject */
  
  unsigned long context = 0;

  DbgBreakPoint();
#if DBG
  fprintf(stderr, "*** jasperi_call_static1_C Calling %s in %s, signature %s.\n", methodName, className, typesig);
#endif

  init = (strcmp(methodName, "<init>")==0); /* call NewObject */

  JASPER_DBGLINE(jasperi_call_static1_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

#if DBG
  fprintf(stderr, "*** jasperi_call_static1_C jnienv: %p\n", (void*)jnienv);
#endif /* DBG */

  JASPER_DBGLINE(jasperi_call_static1_C);
  ASSERT_NO_EXCP(jnienv, "jasperi_call_static1_C");
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (clazz = jasperi_lookup_class(SPAPI_ARG jnienv,className)) == NULL ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig, ( !init ? 1/*static*/ : 0/*instance/init*/))) == 0 ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* If we do not get here then an error was thrown before going to cleanup: */

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
    char *p = typesig;
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }
  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 1 /* CALLING set up jargs for call */);
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_static1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_static1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_static1_C);
#if DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java
     context, no SP runtime calls allowed */


  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_static1_C);

#if DBG
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, typesig, jargs);
#endif

  {
    int result_is_object = FALSE;

    JASPER_DBGLINE(jasperi_call_static1_C);
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;

      if (init)                 /* new <<CLASS>> */
        {
          jargs[0].l = (*jnienv)->NewObjectA(jnienv, clazz, mid, jargs+1);
          result_is_object = TRUE;
        }
      else                      /* normal case, static method */
        {
          switch (return_type_sig[0])
            {
            case 'V':
              #if DBG>1
              {
                int i;
                for (i = 1; i <= state.numargs; i++)
                  {
                    fprintf(stderr, "CallStaticVoidMethodA %s::%s jargs[%d]==%p (%s)\n", className, methodName, i, ( state.is_object[i] ? (jargs)[i].l : 0 ), ( state.is_object[i] ? "object": "primitive"));
                  }
              }
              #endif
              (*jnienv)->CallStaticVoidMethodA(jnienv, clazz, mid, jargs+1 /* ! +1 also for VOID */);
              break;
            case '[':                   /* Arrays are objects too */
            case 'L':
              jargs[0].l = (*jnienv)->CallStaticObjectMethodA(jnienv, clazz, mid, jargs+1);
              result_is_object = TRUE;
              if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
                {                   /* see jasperi_call_static1_C */
                  SP_term_ref returned_term;
                  if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      returned_term_error = TRUE;
                    }
                  else              /* no error */
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                      returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                      jargs[0].j = (jlong) returned_term;
                    }
                }
              break;
            case 'Z':
              jargs[0].z = (*jnienv)->CallStaticBooleanMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'B':
              jargs[0].b = (*jnienv)->CallStaticByteMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'C':
              jargs[0].c = (*jnienv)->CallStaticCharMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'S':
              jargs[0].s = (*jnienv)->CallStaticShortMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'I':
              jargs[0].i = (*jnienv)->CallStaticIntMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'J':
              jargs[0].j = (*jnienv)->CallStaticLongMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'F':
              jargs[0].f = (*jnienv)->CallStaticFloatMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'D':
              jargs[0].d = (*jnienv)->CallStaticDoubleMethodA(jnienv, clazz, mid, jargs+1);
              break;
            default:
              /* We cannot call jasper_int_handle_exception in Java context */
              illegal_method = TRUE;
            }
        }

      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }


      JASPER_DBGLINE(jasperi_call_static1_C);
      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  JASPER_DBGLINE(jasperi_call_static1_C);

  /* (Still) Prolog context, monitor owned by this thread */

  /* get arg vector */
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 2 /* RETURNING handle return values */);
  JASPER_DBGLINE(jasperi_call_static1_C);

 cleanup:
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  JASPER_DBGLINE(jasperi_call_static1_C);
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 3 /* CLEANUP */);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_call_static1_C);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}


/* [PM] 3.8.5 NEW API */
/* FOREIGN */
void SPCDECL
jasperi_call_instance1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                         char *methodName,
                         char *typesig,
                         SP_term_ref methodDesc,
                         SP_term_ref args)
{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jobject jobj = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_process_meta_args */
  struct meta_args_state state;
  int result = SP_ERROR;

  unsigned long context = 0;

#if DBG
  fprintf(stderr, "*** jasperi_call_instance1_C Calling %s, signature %s.\n", methodName, typesig);
#endif

  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

  ASSERT_NO_EXCP(jnienv, "jasperi_call_instance1_C");

  {
    SP_term_ref objref = SP_new_term_ref();
   
    if (!SP_get_arg(1, args, objref))
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"No object",1);
        goto cleanup;
      }
    if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
        goto cleanup;
      }

  }
  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_call_instance1_C");

  if ( (clazz = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv,jobj)) == NULL ) goto cleanup;
  if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig,0/*!static*/)) == 0 ) goto cleanup;



  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
    char *p = typesig;
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }

  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 1 /* CALLING set up jargs for call */);
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_instance1_C);
#if DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }

  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }

  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java context, no SP runtime calls allowed */
#if 0
   #if DBG>1
     fprintf(stderr, "Call instance: Pushed context, got %ld\n", context);
   #endif
     if (context == 0)
       {
   #if DBG
         fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_push_context returned 0\n");
   #endif
         goto cleanup;             /* NOTE: Fix error handling */
       }
#endif
  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_instance1_C);

  {
    int result_is_object = FALSE;
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;

      switch (return_type_sig[0])
        {
        case 'V':
          (*jnienv)->CallVoidMethodA(jnienv, jobj, mid, jargs+1); /* ! +1 also for VOID */
          break;
        case '[':                   /* Arrays are objects too */
        case 'L':
          jargs[0].l = (*jnienv)->CallObjectMethodA(jnienv, jobj, mid, jargs+1);
          result_is_object = TRUE;
          if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
            {                   /* see jasperi_process_meta_args */
              SP_term_ref returned_term;
              if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  returned_term_error = TRUE;
                }
              else              /* no error */
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                  returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                  jargs[0].j = (jlong) returned_term;
                }
            }
          break;
        case 'Z':
          jargs[0].z = (*jnienv)->CallBooleanMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'B':
          jargs[0].b = (*jnienv)->CallByteMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'C':
          jargs[0].c = (*jnienv)->CallCharMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'S':
          jargs[0].s = (*jnienv)->CallShortMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'I':
          jargs[0].i = (*jnienv)->CallIntMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'J':
          jargs[0].j = (*jnienv)->CallLongMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'F':
          jargs[0].f = (*jnienv)->CallFloatMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'D':
          jargs[0].d = (*jnienv)->CallDoubleMethodA(jnienv, jobj, mid, jargs+1);
          break;
        default:
          /* We cannot call jasper_int_handle_exception in Java context */
          illegal_method  = TRUE;
        }
      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }

      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_instance" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  
  /* (still) Prolog context, monitor owned by this thread */
  
  /* get arg vector */
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 2 /* RETURNING handle return values */);

 cleanup:
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 3 /* CLEANUP */);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}

#else  /* #if THREADSERVER */
/* [PD] 3.9 Thread safe callback via server */

#if 0                           /* [PD] 3.9 not used anymore */
void SPCDECL jasperi_set_threadservermode_C(SPAPI_ARG_PROTO_DECL long on)
{
#if DBG
  fprintf(stderr, "jasperi_set_threadservermode_C: on==%ld\n", on); fflush(stderr);
#endif
  STATE_VARIABLE(threadservermode) = (on ? JNI_TRUE : JNI_FALSE);
}
#endif

/* Make an Object wrapper for a primitive data type */
static jboolean make_wrapper(JNIEnv *jnienv,
                             jvalue jarg,
                             char jarg_type,
                             jobject *wrapper)
{
  jclass clazz = NULL; jmethodID mid = NULL;

  switch (jarg_type)
    {
    case 'Z':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Boolean"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(Z)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.z))) { goto barf; }
        break;
      }
    case 'B':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Byte"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(B)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.b))) { goto barf; }
        break;
      }
    case 'C':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Character"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(C)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.c))) { goto barf; }
        break;
      }
    case 'S':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Short"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(S)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.s))) { goto barf; }
        break;
      }
    case 'I':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Integer"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(I)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.i))) { goto barf; }
        break;
      }
    case 'J':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Long"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(J)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.j))) { goto barf; }
        break;
      }
    case 'F':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Float"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(F)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.f))) { goto barf; }
        break;
      }
    case 'D':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Double"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(D)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.d))) { goto barf; }
        break;
      }
    case '[':
    case 'L':
      *wrapper = jarg.l;        /* already an Object */
      break;
    default:
      (*jnienv)->FatalError(jnienv, "illegal type descriptor");
      goto barf;
    }

  return JNI_TRUE;

 barf:
  if (clazz) { (*jnienv)->DeleteLocalRef(jnienv, clazz); }
  return JNI_FALSE;
}

/* Make an Object array of Object wrappers for the arguments in 'jargs' */
static jobjectArray make_arg_array(JNIEnv *jnienv,
                                   jvalue *jargs,
                                   char *typesig,
                                   int numargs)
{
  int i;
  jclass objclass = NULL;
  jobjectArray objarray = NULL;
  char *tp;

#if JASPER_PUSH_LOCAL_REFS
  if ((*jnienv)->PushLocalFrame(jnienv, numargs + 2) < 0) { return NULL; }
#endif

  if (!(objclass = (*jnienv)->FindClass(jnienv, "java/lang/Object"))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "make_arg_array (1)");
  if (!(objarray = (*jnienv)->NewObjectArray(jnienv, (jsize)(numargs),objclass, (jobject)NULL))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "make_arg_array (2)");
  tp = typesig;

  while (*tp != '(') tp++;      /* skip to arguments */
  tp++;                         /* skip to first argument type */

#if DBG>0
  fprintf(stderr, "make_arg_array: numargs==%d\n", numargs);
#endif
  ASSERT_NO_EXCP(jnienv, "make_arg_array (4)");

  for (i = 0; i < numargs; i++)
    {
      jobject value = NULL;
      if (!make_wrapper(jnienv, jargs[i], *tp, &value)) { goto barf; }

      if (*tp == '[') { tp++; }
      if (*tp == 'L') {
        while (*tp != ';') tp++; /* skip past this object type */
      } 
      tp++;                     /* skip to next argument type */

      ASSERT_NO_EXCP(jnienv, "make_arg_array (5)");

      (*jnienv)->SetObjectArrayElement(jnienv, objarray, i, value);
#if !JASPER_PUSH_LOCAL_REFS
                        /* This causes a segfault. Not immediately, but
                           later. (In SPJavaGlueSPTermM?).
                           *** INVESTIGATE THIS! *** */
      if (value) { (*jnienv)->DeleteLocalRef(jnienv, value); }
#endif
    }

  ASSERT_NO_EXCP(jnienv, "make_arg_array (6)");
#if JASPER_PUSH_LOCAL_REFS
  {
    jobjectArray result = (*jnienv)->PopLocalFrame(jnienv, objarray);
    return result;
  }
#else
  if (objclass) { (*jnienv)->DeleteLocalRef(jnienv, objclass); }
  return objarray;
#endif

 barf:
#if JASPER_PUSH_LOCAL_REFS
  (*jnienv)->PopLocalFrame(jnienv, NULL);
#else
  if (objclass) { (*jnienv)->DeleteLocalRef(jnienv, objclass); }
  if (objarray) { (*jnienv)->DeleteLocalRef(jnienv, objarray); }
#endif
  return NULL;
}

static jobject jasper_get_server(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  jboolean hasException;
  jvalue result;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return NULL;

  result = CallMethodByName(jnienv,
                            &hasException,
#if 0
                            STATE_VARIABLE(jasper_sp_global),
#else
                            sp_obj,
#endif
                            "getServer",
                            "()Lse/sics/jasper/Server;");

  if (hasException) {
#if DBG
    jasper_DescribeException(jnienv);
#endif
    return NULL;
  }
  return result.l;
}

struct call_args {
  jstring signature;
  jstring methname;
  jmethodID imid;
  jobjectArray jarray;
  jobject server;
};

static void jasper_cleanup_after_callback(JNIEnv *jnienv, struct call_args *ca)
{
  jthrowable excp = (*jnienv)->ExceptionOccurred(jnienv);
  (*jnienv)->ExceptionClear(jnienv);

  (*jnienv)->DeleteLocalRef(jnienv, ca->signature);
  (*jnienv)->DeleteLocalRef(jnienv, ca->methname);
  (*jnienv)->DeleteLocalRef(jnienv, ca->jarray);
  (*jnienv)->DeleteLocalRef(jnienv, ca->server);

  if (excp) { (*jnienv)->Throw(jnienv, excp); }
}

static jboolean jasper_setup_for_callback(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                          char *method,  /* method name */
                                          jvalue *jargs, /* argument array */
                                          char *typesig, /* type signature */
                                          int numargs, /* number of arguments */
                                          const char *callbackname, /* name of
                                                                    CB method */
                                          const char *callbacksig, /* signature
                                                                     of above */
                                          struct call_args *ca)
{
  jclass cbclass = NULL;

  if (!(ca->server = jasper_get_server(SPAPI_ARG jnienv))) { goto barf; }
  if (!(cbclass = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv, ca->server))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "jasper_setup_for_callback");

  if (!(ca->signature = (*jnienv)->NewStringUTF(jnienv, typesig))) { goto barf;}
  if (!(ca->methname = (*jnienv)->NewStringUTF(jnienv, method))) { goto barf; }

  ASSERT_NO_EXCP(jnienv, "jasper_setup_for_callback");
  if (!(ca->imid = (*jnienv)->GetMethodID(jnienv, cbclass, callbackname, callbacksig))) {goto barf; }

  /* Make an Object array with the original arguments. */
  if (!(ca->jarray = make_arg_array(jnienv, jargs, typesig, numargs))) { goto barf; }

  return JNI_TRUE;

 barf:
  jasper_cleanup_after_callback(jnienv, ca);
  return JNI_FALSE;
}

static jobject jasper_NewObjectA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                 jclass clazz,
                                 char *method,
                                 jvalue *jargs,
                                 char *typesig,
                                 int numargs)
{
  jobject retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};
  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackNewObject",
                                 "(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;",
                                 &ca)) { return NULL; }
  retval = (*jnienv)->CallObjectMethod(jnienv, ca.server, ca.imid, clazz,
                                       ca.methname, ca.jarray, ca.signature);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

/* All callBack<Foo> methods share the same signature for the arguments,
   differing only in their return value. */

#define CALLBACKSIG(RET_TYPE) "(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;Z)" #RET_TYPE

static void jasper_CallVoidMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                   jobject jobj,  /* object (or class) to
                                                     invoke method on */
                                   char *method,  /* method name */
                                   jvalue *jargs, /* argument array */
                                   char *typesig, /* type signature */
                                   int numargs,   /* number of arguments */
                                   jboolean staticP /* static or instance? */
                                   )
{
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackVoid",CALLBACKSIG(V),&ca)) {return;}
  (*jnienv)->CallVoidMethod(jnienv, ca.server, ca.imid, jobj, ca.methname,
                            ca.jarray, ca.signature, staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
}

static jobject jasper_CallObjectMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                        jobject jobj,
                                        char *method,
                                        jvalue *jargs,
                                        char *typesig,
                                        int numargs,
                                        jboolean staticP)
{
  jobject retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackObject",CALLBACKSIG(Ljava/lang/Object;),
                                 &ca)) { return NULL; }
  retval = (*jnienv)->CallObjectMethod(jnienv, ca.server, ca.imid, jobj,
                                       ca.methname, ca.jarray, ca.signature,
                                       staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jboolean jasper_CallBooleanMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                          jobject jobj,
                                          char *method,
                                          jvalue *jargs,
                                          char *typesig,
                                          int numargs,
                                          jboolean staticP)
{
  jboolean retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackBoolean", CALLBACKSIG(Z), &ca)) { return JNI_FALSE; }
  retval = (*jnienv)->CallBooleanMethod(jnienv, ca.server, ca.imid, jobj,
                                        ca.methname, ca.jarray, ca.signature,
                                        staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jbyte jasper_CallByteMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char *method,
                                    jvalue *jargs,
                                    char *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jbyte retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackByte",CALLBACKSIG(B),&ca)) { return 0;}
  retval = (*jnienv)->CallByteMethod(jnienv,ca.server, ca.imid, jobj,
                                     ca.methname, ca.jarray, ca.signature,
                                     staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jchar jasper_CallCharMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char *method,
                                    jvalue *jargs,
                                    char *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jchar retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackChar",CALLBACKSIG(C),&ca))
    { return '\0'; }
  retval = (*jnienv)->CallCharMethod(jnienv,ca.server, ca.imid, jobj, ca.methname,
                                     ca.jarray, ca.signature, staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jshort jasper_CallShortMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                      jobject jobj,
                                      char *method,
                                      jvalue *jargs,
                                      char *typesig,
                                      int numargs,
                                      jboolean staticP)
{
  jshort retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackShort",CALLBACKSIG(S),&ca)) {return 0;}
  retval = (*jnienv)->CallShortMethod(jnienv, ca.server, ca.imid, jobj,
                                    ca.methname, ca.jarray, ca.signature,
                                    staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jint jasper_CallIntMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                  jobject jobj,
                                  char *method,
                                  jvalue *jargs,
                                  char *typesig,
                                  int numargs,
                                  jboolean staticP)
{
  jint retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackInt",CALLBACKSIG(I),&ca)) {return 0;}
  retval = (*jnienv)->CallIntMethod(jnienv, ca.server, ca.imid, jobj,
                                    ca.methname, ca.jarray, ca.signature,
                                    staticP);
#if DBG
    fprintf(stderr, "jasper_CallIntMethodA: retval==%d\n", (int)retval);
#endif
    jasper_cleanup_after_callback(jnienv, &ca);
    return retval;
}

static jlong jasper_CallLongMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char *method,
                                    jvalue *jargs,
                                    char *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jlong retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackLong",CALLBACKSIG(J),&ca)) {return 0;}
  retval = (*jnienv)->CallLongMethod(jnienv, ca.server, ca.imid, jobj,
                                     ca.methname, ca.jarray, ca.signature,
                                     staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jfloat jasper_CallFloatMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                      jobject jobj,
                                      char *method,
                                      jvalue *jargs,
                                      char *typesig,
                                      int numargs,
                                      jboolean staticP)
{
  jfloat retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackFloat",CALLBACKSIG(F),&ca)) {return 0.0;}
  retval = (*jnienv)->CallFloatMethod(jnienv, ca.server, ca.imid, jobj,
                                      ca.methname, ca.jarray, ca.signature,
                                      staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jdouble jasper_CallDoubleMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                        jobject jobj,
                                        char *method,
                                        jvalue *jargs,
                                        char *typesig,
                                        int numargs,
                                        jboolean staticP)
{
  jdouble retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackDouble",CALLBACKSIG(D),&ca)) {return 0.0;}
  retval = (*jnienv)->CallDoubleMethod(jnienv, ca.server, ca.imid, jobj,
                                       ca.methname, ca.jarray, ca.signature,
                                       staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

/* If oldstring matches the beginning of oldstring_buf, append newstring to
   newstring_buf. Otherwise append oldstring to newstring_buf.
   newstring_buf will be incremented by the length of the appended string.
   oldstring_buf will be incremented by the length of the match.
*/
static void maybe_substitute(char **newstring_buf, char **oldstring_buf,
                             const char *newstring, const char *oldstring,
                             int newstring_length, int oldstring_length)
{
  char *semicolonp = *oldstring_buf;
  while (*semicolonp != ';') { semicolonp++; }
  semicolonp++;
  if (0 == strncmp(*oldstring_buf, oldstring, semicolonp - *oldstring_buf)) {
    strcpy(*newstring_buf, newstring);
    *newstring_buf += newstring_length;
    *oldstring_buf += oldstring_length;
  } else {
    strncpy(*newstring_buf, *oldstring_buf, semicolonp - *oldstring_buf);
    *newstring_buf += (semicolonp - *oldstring_buf);
    *oldstring_buf = semicolonp;
  }
}

/* Substitute all occurrences of oldType in typesig with newType.
   newtypesig is a buffer with the same size as typesig.
   Returns a char* to the newtypesig buffer.
*/
static char *make_typesig_new_style(char **newtypesig, char *typesig,
                                    const char *newType, const char *oldType)
{
  int newType_length = strlen(newType);
  int oldType_length = strlen(oldType);
  char *tp_old = typesig;
  char *tp_new = *newtypesig;

  /* Skip to arguments */
  while (*tp_old != '(') { tp_old++; }
  tp_old++;
  tp_new[0] = '(';
  tp_new++;
  /* Process argument list types */
  while (*tp_old != ')') {
    if (*tp_old == 'L') {
      maybe_substitute(&tp_new, &tp_old, newType, oldType, newType_length, oldType_length);
    } else {
      *tp_new = *tp_old;
      tp_new++;
      tp_old++;
    }
  }
  tp_old++;
  *tp_new = ')';
  tp_new++;
  /* Take care of return type */
  if (*tp_old == '[') {
      *tp_new = *tp_old;
      tp_new++;
      tp_old++;
  }
  if (*tp_old == 'L') {
    maybe_substitute(&tp_new, &tp_old, newType, oldType, newType_length, oldType_length);
  } else {
    *tp_new = *tp_old;
    tp_new++;
  }
  *tp_new = '\0';
  return *newtypesig;
}

void SPCDECL
jasperi_call_static1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                       char *className,
                       char *methodName,
                       char *typesig,
                       SP_term_ref methodDesc,
                       SP_term_ref args)

{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_call_static1_C */
  struct meta_args_state state;
  int result = SP_ERROR;
  int init = FALSE;             /* true if <init>, i.e., NewObject */
#if THREADSERVER
  char *newtypesig1 = SP_strdup(typesig); /* Remember to SP_free() this! */
  char *newtypesig2 = SP_strdup(typesig); /* Remember to SP_free() this! */
  char *realtypesig;
  jboolean new_style_interface = JNI_FALSE;
#endif

  unsigned long context = 0;

  DbgBreakPoint();
#if DBG
  fprintf(stderr, "*** jasperi_call_static1_C Calling %s in %s, signature %s.\n", methodName, className, typesig);
#endif

  init = (strcmp(methodName, "<init>")==0); /* call NewObject */

  JASPER_DBGLINE(jasperi_call_static1_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

#if DBG
  fprintf(stderr, "*** jasperi_call_static1_C jnienv: %p\n", (void*)jnienv);
#endif /* DBG */

  JASPER_DBGLINE(jasperi_call_static1_C);
  ASSERT_NO_EXCP(jnienv, "jasperi_call_static1_C");
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (clazz = jasperi_lookup_class(SPAPI_ARG jnienv,className)) == NULL ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);

#if THREADSERVER
  /* If an argument type is /se/sics/jasper/SPTerm, try the method lookup with
     that type substituted with se/sics/jasper/Term. If that fails, use the
     original type signature. */
  realtypesig = make_typesig_new_style(&newtypesig1, typesig,
                                       "Lse/sics/jasper/Term;",
                                       "Lse/sics/jasper/SPTerm;");
  realtypesig = make_typesig_new_style(&newtypesig2, newtypesig1,
                                       "Ljava/lang/String;",
                                       "Lse/sics/jasper/SPCanonicalAtom;");
  if ( (mid = (*jnienv)->GetStaticMethodID(jnienv,clazz,methodName,newtypesig2)) == 0) {
#endif
    (*jnienv)->ExceptionClear(jnienv); /* Clear exception from failed method
                                          lookup. */
    if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig, ( !init ? 1/*static*/ : 0/*instance/init*/))) == 0 ) goto cleanup;
#if THREADSERVER
    realtypesig = typesig;
  } else {
    new_style_interface = JNI_TRUE;
  }
#endif
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* If we do not get here then an error was thrown before going to cleanup: */

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
#if THREADSERVER
    char *p = realtypesig;
#else
    char *p = typesig;
#endif
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }
  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
#if THREADSERVER
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                   &returns_term, &state, FALSE/*!instance*/,
                                   1, /* CALLING set up jargs for call */
                                   new_style_interface);
#else
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 1 /* CALLING set up jargs for call */);
#endif
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_static1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_static1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_static1_C);
#if DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java
     context, no SP runtime calls allowed */


  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_static1_C);

#if DBG
#if THREADSERVER
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, realtypesig, jargs);
#else
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, typesig, jargs);
#endif
#endif

  {
    int result_is_object = FALSE;

    JASPER_DBGLINE(jasperi_call_static1_C);
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;
#if THREADSERVER
      int numargs = state.numargs;
      int threadservermode;

      if (!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) goto cleanup; /* [PD] 3.9 NOTE: Is it legal to goto cleanup here?  */
#endif

      if (init)                 /* new <<CLASS>> */
        {
#if THREADSERVER
          if (threadservermode) {
            jargs[0].l = jasper_NewObjectA(SPAPI_ARG jnienv, clazz, methodName, jargs+1,
                                           realtypesig, numargs-1);
          } else {
#endif
          jargs[0].l = (*jnienv)->NewObjectA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
          }
#endif
          result_is_object = TRUE;
        }
      else                      /* normal case, static method */
        {
          switch (return_type_sig[0])
            {
            case 'V':
#if THREADSERVER
              if (threadservermode) {
                jasper_CallVoidMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                       jargs+1, /* ! +1 also for VOID */
                                       realtypesig, numargs,
                                       JNI_TRUE); /* static */
              } else {
#endif
              #if DBG>1
              {
                int i;
                for (i = 1; i <= state.numargs; i++)
                  {
                    fprintf(stderr, "CallStaticVoidMethodA %s::%s jargs[%d]==%p (%s)\n", className, methodName, i, ( state.is_object[i] ? (jargs)[i].l : 0 ), ( state.is_object[i] ? "object": "primitive"));
                  }
              }
              #endif
              (*jnienv)->CallStaticVoidMethodA(jnienv, clazz, mid, jargs+1 /* ! +1 also for VOID */);
#if THREADSERVER
              }
#endif
              break;
            case '[':                   /* Arrays are objects too */
            case 'L':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].l = jasper_CallObjectMethodA(SPAPI_ARG jnienv,clazz, methodName,
                                                      jargs+1, realtypesig,
                                                      numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].l = (*jnienv)->CallStaticObjectMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              result_is_object = TRUE;
              if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
                {                   /* see jasperi_call_static1_C */
                  SP_term_ref returned_term;
                  if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      returned_term_error = TRUE;
                    }
                  else              /* no error */
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                      returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                      jargs[0].j = (jlong) returned_term;
                    }
                }
              break;
            case 'Z':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].z = jasper_CallBooleanMethodA(SPAPI_ARG jnienv,clazz,methodName,
                                                       jargs+1, realtypesig,
                                                       numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].z = (*jnienv)->CallStaticBooleanMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'B':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].b = jasper_CallByteMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].b = (*jnienv)->CallStaticByteMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'C':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].c = jasper_CallCharMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].c = (*jnienv)->CallStaticCharMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'S':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].s = jasper_CallShortMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                     jargs+1, realtypesig,
                                                     numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].s = (*jnienv)->CallStaticShortMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'I':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].i = jasper_CallIntMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                   jargs+1, realtypesig,
                                                   numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].i = (*jnienv)->CallStaticIntMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'J':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].j = jasper_CallLongMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].j = (*jnienv)->CallStaticLongMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'F':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].f = jasper_CallFloatMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                     jargs+1, realtypesig,
                                                     numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].f = (*jnienv)->CallStaticFloatMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'D':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].d = jasper_CallDoubleMethodA(SPAPI_ARG jnienv,clazz, methodName,
                                                      jargs+1, realtypesig,
                                                      numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].d = (*jnienv)->CallStaticDoubleMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            default:
              /* We cannot call jasper_int_handle_exception in Java context */
              illegal_method = TRUE;
            }
        }

      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }


      JASPER_DBGLINE(jasperi_call_static1_C);
      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  JASPER_DBGLINE(jasperi_call_static1_C);

  /* (Still) Prolog context, monitor owned by this thread */

  /* get arg vector */
#if THREADSERVER
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                     &returns_term, &state, FALSE/*!instance*/,
                                     2, /* RETURNING handle return values */
                                     new_style_interface);
#else
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 2 /* RETURNING handle return values */);
#endif
  JASPER_DBGLINE(jasperi_call_static1_C);

 cleanup:
#if THREADSERVER
  SP_free(newtypesig1);
  SP_free(newtypesig2);
#endif
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  JASPER_DBGLINE(jasperi_call_static1_C);
#if THREADSERVER
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc,
                                              args, &jargs, &returns_term,
                                              &state, FALSE/*!instance*/,
                                              3 /* CLEANUP */,
                                              new_style_interface);
#else
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 3 /* CLEANUP */);
#endif
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_call_static1_C);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}

void SPCDECL
jasperi_call_instance1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                         char *methodName,
                         char *typesig,
                         SP_term_ref methodDesc,
                         SP_term_ref args)
{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jobject jobj = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_process_meta_args */
  struct meta_args_state state;
  int result = SP_ERROR;
#if THREADSERVER
  char *newtypesig1 = SP_strdup(typesig); /* Remember to SP_free() this! */
  char *newtypesig2 = SP_strdup(typesig); /* Remember to SP_free() this! */
  char *realtypesig;
  jboolean new_style_interface = JNI_FALSE;
#endif

  unsigned long context = 0;

#if DBG
  fprintf(stderr, "*** jasperi_call_instance1_C Calling %s, signature %s.\n", methodName, typesig);
#endif

  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

  ASSERT_NO_EXCP(jnienv, "jasperi_call_instance1_C");

  {
    SP_term_ref objref = SP_new_term_ref();
   
    if (!SP_get_arg(1, args, objref))
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"No object",1);
        goto cleanup;
      }
    if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
        goto cleanup;
      }

  }
  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_call_instance1_C");

  if ( (clazz = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv,jobj)) == NULL ) goto cleanup;
#if THREADSERVER
  /* If an argument type is /se/sics/jasper/SPTerm, try the method lookup with
     that type substituted with se/sics/jasper/Term. If that fails, use the
     original type signature. */
  realtypesig = make_typesig_new_style(&newtypesig1, typesig,
                                       "Lse/sics/jasper/Term;",
                                       "Lse/sics/jasper/SPTerm;");
  realtypesig = make_typesig_new_style(&newtypesig2, newtypesig1,
                                       "Ljava/lang/String;",
                                       "Lse/sics/jasper/SPCanonicalAtom;");
  if ( (mid = (*jnienv)->GetMethodID(jnienv,clazz,methodName,newtypesig2)) == 0) {
    (*jnienv)->ExceptionClear(jnienv); /* Clear exception from failed method
                                          lookup. */
#endif
    if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig,0/*!static*/)) == 0 ) goto cleanup;
#if THREADSERVER
    realtypesig = typesig;
  } else {
    new_style_interface = JNI_TRUE;
  }
#endif

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
#if THREADSERVER
    char *p = realtypesig;
#else
    char *p = typesig;
#endif
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }

  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
#if THREADSERVER
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                   &returns_term, &state, TRUE/*instance*/,
                                   1 /* CALLING set up jargs for call */,
                                   new_style_interface);
#else
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 1 /* CALLING set up jargs for call */);
#endif
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_instance1_C);
#if DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }

  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }

  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java context, no SP runtime calls allowed */
#if 0
   #if DBG>1
     fprintf(stderr, "Call instance: Pushed context, got %ld\n", context);
   #endif
     if (context == 0)
       {
   #if DBG
         fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_push_context returned 0\n");
   #endif
         goto cleanup;             /* NOTE: Fix error handling */
       }
#endif
  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_instance1_C);

  {
    int result_is_object = FALSE;
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;
#if THREADSERVER
      int numargs = state.numargs - 1; /* First arg is object? */
      int threadservermode;

      if (!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) goto cleanup; /* [PD] 3.9 NOTE: Is it legal to goto cleanup here?  */
#endif

      switch (return_type_sig[0])
        {
        case 'V':
#if THREADSERVER
          if (threadservermode) {
            jasper_CallVoidMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                   jargs+1, /* ! +1 also for VOID */
                                   realtypesig, numargs,
                                   JNI_FALSE); /* instance */
          } else {
#endif
          (*jnienv)->CallVoidMethodA(jnienv, jobj, mid, jargs+1); /* ! +1 also for VOID */
#if THREADSERVER
          }
#endif
          break;
        case '[':                   /* Arrays are objects too */
        case 'L':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].l = jasper_CallObjectMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                  jargs+1, realtypesig, numargs-1,
                                                  JNI_FALSE);
          } else {
#endif
          jargs[0].l = (*jnienv)->CallObjectMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          result_is_object = TRUE;
          if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
            {                   /* see jasperi_process_meta_args */
              SP_term_ref returned_term;
              if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  returned_term_error = TRUE;
                }
              else              /* no error */
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                  returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                  jargs[0].j = (jlong) returned_term;
                }
            }
          break;
        case 'Z':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].z = jasper_CallBooleanMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                   jargs+1, realtypesig, numargs-1,
                                                   JNI_FALSE);
          } else {
#endif
          jargs[0].z = (*jnienv)->CallBooleanMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'B':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].b = jasper_CallByteMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].b = (*jnienv)->CallByteMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'C':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].c = jasper_CallCharMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].c = (*jnienv)->CallCharMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'S':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].s = jasper_CallShortMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                 jargs+1, realtypesig, numargs-1,
                                                 JNI_FALSE);
          } else {
#endif
          jargs[0].s = (*jnienv)->CallShortMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'I':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].i = jasper_CallIntMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                               jargs+1, realtypesig, numargs-1,
                                               JNI_FALSE);
          } else {
#endif
          jargs[0].i = (*jnienv)->CallIntMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'J':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].j = jasper_CallLongMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].j = (*jnienv)->CallLongMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'F':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].f = jasper_CallFloatMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                 jargs+1, realtypesig, numargs-1,
                                                 JNI_FALSE);
          } else {
#endif
          jargs[0].f = (*jnienv)->CallFloatMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'D':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].d = jasper_CallDoubleMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                  jargs+1, realtypesig, numargs-1,
                                                  JNI_FALSE);
          } else {
#endif
          jargs[0].d = (*jnienv)->CallDoubleMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        default:
          /* We cannot call jasper_int_handle_exception in Java context */
          illegal_method  = TRUE;
        }
      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }

      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_instance" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  
  /* (still) Prolog context, monitor owned by this thread */
  
  /* get arg vector */
#if THREADSERVER
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                     &returns_term, &state, TRUE/*instance*/,
                                     2 /* RETURNING handle return values */,
                                     new_style_interface);
#else
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 2 /* RETURNING handle return values */);
#endif

 cleanup:
#if THREADSERVER
  SP_free(newtypesig1);
  SP_free(newtypesig2);
#endif
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

#if THREADSERVER
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc,
                                              args, &jargs, &returns_term,
                                              &state, TRUE/*instance*/,
                                              3 /* CLEANUP */,
                                              new_style_interface);
#else
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 3 /* CLEANUP */);
#endif
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}
#endif /* THREADSERVER */


/* FOREIGN */
SP_term_ref SPCDECL
jasperi_create_global_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  int barf = 1;
  JNIEnv *jnienv;
  jobject jobj = NULL;
  SP_term_ref tr = SP_new_term_ref();

  /* GET_JNIENV_RV(jvmref,jnienv,objref); */
  SET_JNIENV(jnienv, jvmref, goto cleanup;);

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) goto cleanup;

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_create_global_ref_C (ref)");

  barf = 0;
  
 cleanup:

  if (!barf)
    {
      jobject gobj = ( jobj ? (*jnienv)->NewGlobalRef(jnienv,jobj) : NULL );
      ASSERT_NO_EXCP(jnienv, "jasperi_create_global_ref_C");
      DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "jasperi_create_global_ref_C (global ref)");

      jobject_to_termref(SPAPI_ARG jnienv, gobj, tr);
    }
  else
    {
#if DBG
      fprintf(stderr, "barf in jasperi_create_global_ref_C\n");
#endif
      jobject_to_termref(SPAPI_ARG jnienv, NULL, tr);
    }
  return tr;
}

/* FOREIGN */
SP_term_ref SPCDECL
jasperi_create_local_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  int barf = 1;
  JNIEnv *jnienv;
  jobject jobj = NULL;
  SP_term_ref tr = SP_new_term_ref();

  /* GET_JNIENV_RV(jvmref,jnienv,objref); */
  SET_JNIENV(jnienv, jvmref, goto cleanup;);

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) goto cleanup;

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_create_local_ref_C (ref)");

  barf = 0;
  
 cleanup:

  if (!barf)
    {
      jobject gobj = ( jobj ? (*jnienv)->NewLocalRef(jnienv,jobj) : NULL );
      ASSERT_NO_EXCP(jnienv, "jasperi_create_local_ref_C");
      DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "jasperi_create_local_ref_C (local ref)");

      jobject_to_termref(SPAPI_ARG jnienv, gobj, tr);
    }
  else
    {
#if DBG
      fprintf(stderr, "barf in jasperi_create_local_ref_C\n");
#endif
      jobject_to_termref(SPAPI_ARG jnienv, NULL, tr);
    }
  return tr;
}

/* FOREIGN */
void SPCDECL
jasperi_delete_global_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  JNIEnv *jnienv;
  jobject jobj;

  SET_JNIENV(jnienv,jvmref, { return; });

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) /* NULL is OK */
    {
      JASPER_DBGLINE(jasperi_delete_global_ref_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
      goto cleanup;
    }

  DELETE_GLOBAL_REF(jnienv,jobj);
 cleanup:
  ;
}

/* FOREIGN */ 
void SPCDECL
jasperi_delete_local_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  JNIEnv *jnienv;
  jobject jobj;

  SET_JNIENV(jnienv,jvmref, { return; });
  ASSERT_NO_EXCP(jnienv, "jasperi_delete_local_ref_C");
  
  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) /* NULL is OK */
    {
      JASPER_DBGLINE(jasperi_delete_local_ref_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
      goto cleanup;
    }

  DELETE_LOCAL_REF(jnienv,jobj);
 cleanup:
  ;
}

/* FOREIGN */
long SPCDECL
jasperi_is_same_object_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref obj1, SP_term_ref obj2)
{
  JNIEnv *jnienv;
  jobject jobj1,jobj2;


  SET_JNIENV(jnienv,jvmref, { return 0;});
  ASSERT_NO_EXCP(jnienv, "jasperi_is_same_object_C");

  if (!(termref_to_jobject(SPAPI_ARG obj1, &jobj1)
        && termref_to_jobject(SPAPI_ARG obj2, &jobj2))) goto barf;
  
  if ((*jnienv)->IsSameObject(jnienv,jobj1,jobj2))
    {
      return 1;
    }
  else
    {
      return 0;
    }
 barf:
  {
    JASPER_DBGLINE(jasperi_is_same_object_C);
    jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
  }
  return 0;
}

/* FOREIGN */
long SPCDECL
jasperi_is_instance_of_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref obj, char *classname)
{
  int rc = 0;          /* 0, 1 or -1 (triggers exception in calling Prolog code) */

  JNIEnv *jnienv;
  jclass clazz = NULL;
  jobject jobj = NULL;

  SET_JNIENV(jnienv,jvmref, { return 0;});
  ASSERT_NO_EXCP(jnienv, "jasperi_is_instance_of_C");

  if ((!termref_to_jobject(SPAPI_ARG obj, &jobj)) || jobj==NULL)
    {
      rc = -1;
      goto cleanup;
    }

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_is_instance_of_C");
  
  clazz = jasperi_lookup_class(SPAPI_ARG jnienv,classname);
  if (clazz == 0)
    rc =-1;
  else if ((*jnienv)->IsInstanceOf(jnienv,jobj,clazz))
    rc = 1;
  else
    rc = 0;
  
 cleanup:
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  return rc;
}

/* return a *local* ref to a new SICStus object */
static jobject jasperi_new_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *env)
{
  jvalue result;
  jboolean hasException = JNI_FALSE;
  
  result = CallStaticMethodByName(env, &hasException, "se/sics/jasper/SICStus",
                                  "getNewSICStusFromAPIPtr",
                                  "(J)Lse/sics/jasper/SICStus;",
#if SP_DISPATCH_API
  /* double cast to avoid gcc warning when sizeof jlong > sizeof ptr */
                                  (jlong) (long)SICStusDISPATCHVAR
#else  /* !SP_DISPATCH_API */

#error "Need SP_DISPATCH_API in order to pass api ptr to spnative.c via SICStus object"
                                  (jlong) -1
#endif /* !SP_DISPATCH_API */

                                  );
  if (hasException)
    {

      #if DBG
      int x =
      #endif
      jasper_int_handle_exception(SPAPI_ARG env, "Could not create SICStus object@" __FILE__ ":" STRINGISIZE(__LINE__), 1);
      #if DBG
      if (!x) {
        fprintf(stderr, "CallStaticMethodByName hasException but jasper_int_handle_exception returns 0\n");
        abort();
      }
      #endif
      return NULL;
    }
  return result.l;
}


/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 2
 *  end:
 **/

