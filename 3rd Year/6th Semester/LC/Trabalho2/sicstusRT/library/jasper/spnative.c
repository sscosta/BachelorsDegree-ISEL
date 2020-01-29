/**************************************************************************
 * Filename:    spnative.c
 * Author:      Jesper Eskilson <jojo@sics.se>
 **************************************************************************
 */

/* [PD] threadserver jasper-server hack */
#define THREADSERVER 1

/*

make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' && bin/spld --verbose --keep library/jasper/spnative.c --main=none --cflag=-LD,-IC:/jdk1.3/include,-IC:/jdk1.3/include/win32,-I./include,-DXMULTI_SP_AWARE=1 --output=bin/spnative.dll && cp -p se/sics/jasper/jasper.jar bin/jasper.jar && make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' test TESTS=jasper

make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' && bin/spld --verbose --keep library/jasper/spnative.c --main=none --cflag=-LD,-IC:/jdk1.3/include,-IC:/jdk1.3/include/win32,-I./include,-DXMULTI_SP_AWARE=1 --output=bin/spnative.dll && cp -p se/sics/jasper/jasper.jar bin

PATH="./bin:$PATH" java -Dse.sics.jasper.SICStus.debugLevel=1 -Dse.sics.jasper.SICStus.checkSPTermAge=true -Dse.sics.jasper.SICStus.reuseTermRefs=true -classpath bin/jasper.jar se.sics.jasper.SICStus
*/

/* 3.9 [PM]
   For now (3.9 beta 1) we still do not allow more than one SICStus
   run-time.
 */

/* 3.9 [PM], Only one thread may call SP API and (some future version
   of se.sics.jasper will ensure this). Thus no need for Java side
   monitor handling.

   Also, with multiple SP runtimes there is no sense in keeping a
   global sp object. Instead each SP object holds a reference to the
   SICStus API dispatch table.

   There is one instance of this file (and its global variables) in
   each Process. This contrasts with jasper.c which may exist in one
   copy for each SP runtime (if statically linked).

*/
#define NEW_WORLD_ORDER 1       /* 3.9 */

#if 1                           /* [PM] 3.8.5 read from string support */
#define CHARSIO_ENABLE 1
#endif
#if 1                           /* [PM] 3.8.5 SPPredicate no longer needed */
#define SPPREDICATE_DEPRECATED 1
#endif

#include <sicstus/sicstus.h>

#if defined(JASPER_DBG)
#undef DBG
#define DBG JASPER_DBG
#endif

#include <sicstus/jasper.h>
#include <sicstus/config.h>
#include <jni.h>
#include <stdlib.h>
#include <string.h>

#include "se_sics_jasper_SPCanonicalAtom.h"
#include "se_sics_jasper_SPException.h"
#include "se_sics_jasper_SPPredicate.h"
#include "se_sics_jasper_SPQuery.h"
#include "se_sics_jasper_SICStus.h"
#include "se_sics_jasper_SPTerm.h"
/* [PD] 3.9 */
#include "se_sics_jasper_Jasper.h"

#undef DEBUG_BREAK
#define DEBUG_BREAK()

#if DBG
#if SP_WIN32

#if defined(_MSC_VER)
#undef DEBUG_BREAK
#define DEBUG_BREAK() do {_asm { int 3h };} while(0)
#endif /* defined(_MSC_VER) */

#endif /* SP_WIN32 */
#endif /* DBG */


#ifdef _MSC_VER
#define BreakPoint()        _asm { int 3h }
#endif

#if DBG && 0
#define DbgBreakPoint() BreakPoint()
#else
#define DbgBreakPoint()
#endif

#if !NEW_WORLD_ORDER
/* These belong in a header file. The original is in jasper.c */
extern volatile jobject jasper_sp_global; /* the global SICStus object, set from spnative.c */

#if 0                           /* [PM] 3.9 now duplicated avoid depending on jasper.c */
extern void jasper_DescribeException(JNIEnv *jnienv);
#endif
extern int jasper_enter_sicstus_monitor(JNIEnv *jnienv);
extern int jasper_leave_sicstus_monitor(JNIEnv *jnienv);
#endif /* !NEW_WORLD_ORDER */

static jvalue SPCDECL
CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...);


#if DBG

/* Duplicated in jasper.c */
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

#define ASSERT_NO_EXCP(JNIENV, STRING) \
   do{ \
      if ( ((JNIENV)==NULL) || (( *(JNIENV) )->ExceptionCheck((JNIENV))) ) { \
         fprintf(stderr, "ERROR: Pending Java exception (%s)\n", (STRING)); \
         jasper_DescribeException((JNIENV)); \
      } \
   } while (0)
#define DBG_VALIDATE_TERM_REF(TERMREF) do{ \
   SP_term_ref HIGH_WATER_MARK__ = SP_new_term_refs(0); \
   SP_term_ref TERM_REF__ = (SP_term_ref)(TERMREF); \
   if (! (0 < TERM_REF__ && TERM_REF__ < HIGH_WATER_MARK__) ) { \
      fprintf(stderr, "ERROR: Illegal term-ref %d\n" __FILE__ ":" JASPER_STRINGISIZE(__LINE__) "\n",TERM_REF__); \
   } \
   /* else fprintf(stderr, "OK: term-ref %d in [1,%d]\n" __FILE__ ":" JASPER_STRINGISIZE(__LINE__) "\n",TERM_REF__,HIGH_WATER_MARK__); */\
}while(0)

#else /* no DBG */
#define ASSERT_NO_EXCP(JNIENV, STRING)
#define DBG_VALIDATE_TERM_REF(TERMREF)
#endif



/* The strange bracketing is to ensure that ENTER is put immediately
   after local variable declaration and LEAVE immedately before return
*/ 
#if NEW_WORLD_ORDER
#if SP_DISPATCH_API

/* This is where SP_put_list et al find the dispatch table, it is
   extracted from the SICStus object (named spobj, available in the
   local environment).
*/

#undef SICStusDISPATCHVAR /* Undefine the default from spaux.h */
#define SICStusDISPATCHVAR SP_ONLY_API_ARG_NAME

/* [PM] 3.9b4 renamed from SPAPI_XXX to SP_ONLY_API_XXX since the
   former will expand to two args (api-dispatch and stash) for
   multi-sp-aware foreign functions (Later in 3.9b4 no longer true,
   stash is now part of api-dispatch). Since it does not make sense to
   pass around a stash in an embedder we need a separate set of macros
*/
#define SP_ONLY_API_ARG_NAME sp_api_dispatch
#define SP_ONLY_API_ARG0 SP_ONLY_API_ARG_NAME
#define SP_ONLY_API_ARG SP_ONLY_API_ARG0, 
#define SP_ONLY_API_ARG_PROTO_DECL0 SICSTUS_API_STRUCT_TYPE *SP_ONLY_API_ARG_NAME
#define SP_ONLY_API_ARG_PROTO_DECL SP_ONLY_API_ARG_PROTO_DECL0, 

#define SP_ENTER_SICSTUS() { int jasper_enter_sicstus_monitor_dummy = 42; \
   SP_ONLY_API_ARG_PROTO_DECL0; \
   SP_ONLY_API_ARG_NAME = api_from_spobj(jnienv, spobj);

#define SP_LEAVE_SICSTUS() jasper_enter_sicstus_monitor_dummy++; }

/* We probably want a faster way to do this. Consider passing it as an argument or accessing it through a member */
static SICSTUS_API_STRUCT_TYPE *api_from_spobj(JNIEnv *jnienv, jobject spobj)
{
  jboolean hasException = JNI_FALSE;
  jvalue result;
  SICSTUS_API_STRUCT_TYPE *api = NULL;
  
  ASSERT_NO_EXCP(jnienv,"api_from_spobj 1");

  result.j = 0;

  result = CallMethodByName(jnienv, &hasException, spobj, "getSPAPI", "()J");
  ASSERT_NO_EXCP(jnienv,"api_from_spobj 1");

  if (hasException) goto barf;
  if (result.j == 0) goto barf;
  if (result.j == (jlong)-1)
    {

#if DBG
  fprintf(stderr, "ERROR: Could not extract SP api from SICStus object 0x%lx (was -1)\n", (long)spobj);
#endif  /* DBG */
#if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
#error "Need a way to handle the case that jasper foreign resource is not passing a api pointer (i.e., not SP_DISPATCH_API)"
#endif /* SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION */
      goto reported_barf;
    }

  api = (SICSTUS_API_STRUCT_TYPE *)(long)result.j; /* extra cast to avoid warning about size difference ptr vs jlong */
  return api;

 barf:
#if DBG
  fprintf(stderr, "ERROR: Could not extract SP api from SICStus object 0x%lx\n", (long)spobj);
#endif  /* DBG */

#if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
#error "Need a way to handle the case that there is no API in the SICStus object"
#endif /* SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION */

 reported_barf:
  return NULL;
}

static int set_spobj_api(JNIEnv *jnienv, jobject spobj, SICSTUS_API_STRUCT_TYPE *api)
{
  jboolean hasException = JNI_FALSE;
  jvalue result;
  jlong api_long = (jlong)(long)api; /* extra cast to avoid warning about size difference ptr vs jlong */
  
  ASSERT_NO_EXCP(jnienv,"set_spobj_api 1");

  result = CallMethodByName(jnienv, &hasException, spobj, "setSPAPI", "(J)I", api_long);
  ASSERT_NO_EXCP(jnienv,"set_spobj_api 1");

  if (hasException) goto barf;

  return result.i;

 barf:
#if DBG
  fprintf(stderr, "ERROR: Could not set SP api for SICStus object 0x%lx\n", (long)spobj);
#endif
  return SP_ERROR;
}


#else  /* !SP_DISPATCH_API */
#error "Only SP_DISPATCH_API supported"
#define SP_ENTER_SICSTUS() {
#define SP_LEAVE_SICSTUS() ; }
#endif /* !SP_LEAVE_SICSTUS */


#else /* !NEW_WORLD_ORDER */
#if MULTI_SP_AWARE
#error "not possible"
#endif
#define SP_ENTER_SICSTUS() { int jasper_enter_sicstus_monitor_dummy = 42; /*JASPER_DBGLINE(SP_ENTER_SICSTUS);*/ ASSERT_NO_EXCP(jnienv,"SP_ENTER_SICSTUS 1");jasper_enter_sicstus_monitor(jnienv); /*JASPER_DBGLINE(SP_ENTERED_SICSTUS);*/ ASSERT_NO_EXCP(jnienv,"SP_ENTER_SICSTUS 2")
#define SP_LEAVE_SICSTUS()      jasper_enter_sicstus_monitor_dummy++; /*JASPER_DBGLINE(SP_LEAVE_SICSTUS);*/ jasper_leave_sicstus_monitor(jnienv); }
#endif /* !NEW_WORLD_ORDER */


#if !NEW_WORLD_ORDER
volatile jobject jasper_sp_global = NULL;       /* [PM] 3.8.5 used by jasper.c */
#endif

char **sp_java_argv;
int sp_java_argc;
// [PM] 3.9 why returning jlong? (or rather why are most 'res' of type jlong?) 
static jlong handleExceptions(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, int query_rval);

static jint throwNCE(jobject spobj, JNIEnv *jnienv)
{
  jclass nceClass;

  nceClass = (*jnienv)->FindClass(jnienv, "se/sics/jasper/NativeCodeException");
  if (nceClass == NULL) goto error;

  /* 3.8.5 pass the sp object */
  {
    jmethodID mid;
    jobject nceObject;
    mid = (*jnienv)->GetMethodID(jnienv,nceClass,"<init>","(Lse/sics/jasper/SICStus;)V");
    if (mid == NULL) goto error;

    nceObject = (*jnienv)->NewObject(jnienv, nceClass, mid, spobj, NULL);
    if (nceObject == NULL) goto error;
    (*jnienv)->Throw(jnienv, nceObject);
  }
 error:                         /* When we get here some Java excpetion has been thrown */
  return 0;
}

/* Original is in jasper.c, keep in synch */
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

#if THREADSERVER      /* [PD] 3.9 threadserver hack */

/* [PD] 3.9 Make jasper aware that it should switch to thread server mode. */
/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spSetThreadServerMode
 * Signature: (Z)V
 */
JNIEXPORT jboolean JNICALL
  Java_se_sics_jasper_SICStus_spSetThreadServerMode (JNIEnv *jnienv,
                                                     jobject spobj,
                                                     jboolean on)
{
  jboolean res;
#if 0                           /* [PD] 3.9 old code loading library(jasper). */
  SP_pred_ref pred;
  SP_term_ref flag;
  SP_ENTER_SICSTUS();

  pred = SP_predicate("jasperi_set_threadservermode", 1, "jasper");
  if (!pred) goto barf;
  flag = SP_new_term_ref();
  if (!SP_put_integer(flag, on)) goto barf;
  if (SP_SUCCESS != SP_query_cut_fail(pred, flag)) goto barf;
  res = JNI_TRUE;

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;

 barf:
  res = JNI_FALSE;
  goto cleanup;
#else                           /* [PD] 3.9 not loading library(jasper). */
  SP_ENTER_SICSTUS();

  (void) *jnienv;               /* -Wunused */
  (void) spobj;                 /* -Wunused */

  res = ( sp_set_jasper_threadservermode((int)on, SICSTUS_VERSION)
          ? JNI_TRUE
          : JNI_FALSE );
  SP_LEAVE_SICSTUS();
  return res;
#endif
}

static jboolean setup_method_arguments(JNIEnv *jnienv,
                                       jobject spobj,
/*                                       jstring methname, */
                                       jobjectArray args,
                                       jstring typesig,
                                       jvalue *argarray)
{
  const char *tsig = NULL;
  jsize numargs;
  jsize i;
  char *tp;

  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  if (!(tsig = (*jnienv)->GetStringUTFChars(jnienv,typesig,NULL))) {goto barf;}
  tp = (char *)tsig;

  while (*tp != '(') tp++;      /* skip to arguments */
  tp++;                         /* skip to first argument type */
  
  ASSERT_NO_EXCP(jnienv, "setup_method_arguments (1)");

  for (i = 0; i < numargs; i++)
    {
      jobject arg = (*jnienv)->GetObjectArrayElement(jnienv, args, i);
      jclass wclazz = NULL;
      jmethodID wmid = NULL;

      /* If the argument is NULL we don't need its class, but it has
         to be a real object (or array), i.e. not a simple type wrapped
         in an object.
      */
      if (arg != NULL) {
        wclazz = (*jnienv)->GetObjectClass(jnienv, arg);
      } else {
        if (*tp != 'L' && *tp != '[') {
          throwNCE(spobj, jnienv);
        }
      }

      switch (*tp) {
      case 'Z':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "booleanValue", "()Z"))) { goto barf; }
        argarray[i].z = (*jnienv)->CallBooleanMethod(jnienv, arg, wmid);
        break;
      case 'B':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "byteValue", "()B"))) { goto barf; }
        argarray[i].b = (*jnienv)->CallByteMethod(jnienv, arg, wmid);
        break;
      case 'C':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "charValue", "()C"))) { goto barf; }
        argarray[i].c = (*jnienv)->CallCharMethod(jnienv, arg, wmid);
        break;
      case 'S':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "shortValue", "()S"))) { goto barf; }
        argarray[i].s = (*jnienv)->CallShortMethod(jnienv, arg, wmid);
        break;
      case 'I':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "intValue", "()I"))) { goto barf; }
        argarray[i].i = (*jnienv)->CallIntMethod(jnienv, arg, wmid);
        break;
      case 'J':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "longValue", "()J"))) { goto barf; }
        argarray[i].j = (*jnienv)->CallLongMethod(jnienv, arg, wmid);
        break;
      case 'F':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "floatValue", "()F"))) { goto barf; }
        argarray[i].f = (*jnienv)->CallFloatMethod(jnienv, arg, wmid);
        break;
      case 'D':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "doubleValue", "()D"))) { goto barf; }
        argarray[i].d = (*jnienv)->CallDoubleMethod(jnienv, arg, wmid);
        break;
      case 'L':
        argarray[i].l = arg;
        while (*tp != ';') tp++; /* skip past this object type */
        break;
      case '[':
        argarray[i].l = arg;
        if (*(tp + 1) == 'L' ) {
          while (*tp != ';') { tp++; } /* skip past this object type */
        }
        break;
      }
      tp++;                     /* skip to next argument type */
    }
  argarray[i+1].l = NULL;

  ASSERT_NO_EXCP(jnienv, "setup_method_arguments (2)");
  return JNI_TRUE;

 barf:
  if (tsig != NULL) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }
  return JNI_FALSE;
}

static jmethodID sp_get_methodID(JNIEnv *jnienv,
                                 jobject obj,
                                 jstring methname,
                                 jstring typesig,
                                 jboolean staticP)
{
  const char *mname = (*jnienv)->GetStringUTFChars(jnienv, methname, NULL);
  const char *tsig = (*jnienv)->GetStringUTFChars(jnienv, typesig, NULL);
  jmethodID mid;
  
  if (staticP) {
    /* obj is a jclazz pointer */
    mid = (*jnienv)->GetStaticMethodID(jnienv, obj, mname, tsig);
  } else {
    jclass clazz = NULL;
    clazz = (*jnienv)->GetObjectClass(jnienv, obj);    
    ASSERT_NO_EXCP(jnienv, "sp_get_methodID (1)");
    mid = (*jnienv)->GetMethodID(jnienv, clazz, mname, tsig);
    if (NULL != clazz) { (*jnienv)->DeleteLocalRef(jnienv, clazz); }
  }

  ASSERT_NO_EXCP(jnienv, "sp_get_methodID (2)");
  if (NULL != mname) { (*jnienv)->ReleaseStringUTFChars(jnienv, methname, mname); }
  if (NULL != tsig) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }

  return mid;
}

static jmethodID sp_get_init_methodID(JNIEnv *jnienv,
                                      jclass clazz,
                                      jstring methname,
                                      jstring typesig)
{
  const char *mname = (*jnienv)->GetStringUTFChars(jnienv, methname, NULL);
  const char *tsig = (*jnienv)->GetStringUTFChars(jnienv, typesig, NULL);
  jmethodID mid;
  
  mid = (*jnienv)->GetMethodID(jnienv, clazz, mname, tsig);

  ASSERT_NO_EXCP(jnienv, "sp_get_methodID");
  if (NULL != mname) { (*jnienv)->ReleaseStringUTFChars(jnienv, methname, mname); }
  if (NULL != tsig) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }

  return mid;
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spNewObject
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL
  Java_se_sics_jasper_SICStus_spNewObject(JNIEnv *jnienv,
                                          jobject spobj,
                                          jobject clazz,
                                          jstring methname,
                                          jobjectArray args,
                                          jstring typesig)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_init_methodID(jnienv, clazz, methname, typesig);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return (*jnienv)->NewObjectA(jnienv, clazz, mid, argarray);
  } else {
    return (jobject)NULL;
  }
}

/*
 * Class:     jasper_SICStus
 * Method:    spCallVoidMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
  Java_se_sics_jasper_SICStus_spCallVoidMethodByName(JNIEnv *jnienv,
                                                    jobject spobj,
                                                    jobject obj,
                                                    jstring methname,
                                                    jobjectArray args,
                                                    jstring typesig,
                                                    jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    if (staticP) 
      (*jnienv)->CallStaticVoidMethodA(jnienv, obj, mid, argarray);
    else
      (*jnienv)->CallVoidMethodA(jnienv, obj, mid, argarray);
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallObjectMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL
  Java_se_sics_jasper_SICStus_spCallObjectMethodByName(JNIEnv *jnienv,
                                                      jobject spobj,
                                                      jobject obj,
                                                      jstring methname,
                                                      jobjectArray args,
                                                      jstring typesig,
                                                      jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticObjectMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallObjectMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jobject)NULL;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallBooleanMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_se_sics_jasper_SICStus_spCallBooleanMethodByName(JNIEnv *jnienv,
                                                     jobject spobj,
                                                     jobject obj,
                                                     jstring methname,
                                                     jobjectArray args,
                                                     jstring typesig,
                                                     jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticBooleanMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallBooleanMethodA(jnienv, obj, mid, argarray));
  } else {
    return JNI_FALSE;           /* Well, do you have a better suggestion?  */
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallByteMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)B
 */
JNIEXPORT jbyte JNICALL
Java_se_sics_jasper_SICStus_spCallByteMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticByteMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallByteMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jbyte)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallCharMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)C
 */
JNIEXPORT jchar JNICALL
Java_se_sics_jasper_SICStus_spCallCharMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticCharMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallCharMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jchar)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallShortMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)S
 */
JNIEXPORT jshort JNICALL
Java_se_sics_jasper_SICStus_spCallShortMethodByName(JNIEnv *jnienv,
                                                   jobject spobj,
                                                   jobject obj,
                                                   jstring methname,
                                                   jobjectArray args,
                                                   jstring typesig,
                                                   jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticShortMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallShortMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jshort)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallIntMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_se_sics_jasper_SICStus_spCallIntMethodByName(JNIEnv *jnienv,
                                                 jobject spobj,
                                                 jobject obj,
                                                 jstring methname,
                                                 jobjectArray args,
                                                 jstring typesig,
                                                 jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
#if !DBG
    return
#else
      { int retval =
#endif
          ( staticP
            ? (*jnienv)->CallStaticIntMethodA(jnienv, obj, mid, argarray)
            : (*jnienv)->CallIntMethodA(jnienv, obj, mid, argarray));
#if DBG
      fprintf(stderr, "spCallIntMethodByName: retval==%d\n", retval);
      return retval;
      }
#endif
  } else {
    return (jint)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallLongMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_se_sics_jasper_SICStus_spCallLongMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticLongMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallLongMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jlong)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallFloatMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)F
 */
JNIEXPORT jfloat JNICALL
Java_se_sics_jasper_SICStus_spCallFloatMethodByName(JNIEnv *jnienv,
                                                   jobject spobj,
                                                   jobject obj,
                                                   jstring methname,
                                                   jobjectArray args,
                                                   jstring typesig,
                                                   jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticFloatMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallFloatMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jfloat)0.0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallDoubleMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)D
 */
JNIEXPORT jdouble JNICALL
Java_se_sics_jasper_SICStus_spCallDoubleMethodByName(JNIEnv *jnienv,
                                                    jobject spobj,
                                                    jobject obj,
                                                    jstring methname,
                                                    jobjectArray args,
                                                    jstring typesig,
                                                    jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  if (mid != NULL) {
    setup_method_arguments(jnienv, spobj, /*methname,*/ args, typesig, argarray);
    return ( staticP
             ? (*jnienv)->CallStaticDoubleMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallDoubleMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jdouble)0.0;
  }
}
#endif /* THREADSERVER */


/*
 * Class:     jasper_SPTerm
 * Method:    spPutString
 * Signature: (Ljasper/SICStus;JLjava/lang/String;)V
 */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutString(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  
  DBG_VALIDATE_TERM_REF(termref);

  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_string((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutString(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  
  DBG_VALIDATE_TERM_REF(termref);

  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_string((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutListChars(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;
  SP_term_ref emptylist;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  emptylist = SP_new_term_ref();
  
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);
  if (s != NULL)
    {
      rc = SP_put_list_chars((SP_term_ref)termref, emptylist, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }
  SP_reset_term_refs(emptylist);

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutListChars(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;
  SP_term_ref emptylist;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  emptylist = SP_new_term_ref();
  
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);
  if (s != NULL)
    {
      rc = SP_put_list_chars((SP_term_ref)termref, emptylist, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }
  SP_reset_term_refs(emptylist);

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutNumberChars(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_number_chars((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS()
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutNumberChars(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_number_chars((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS()
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutAtom(JNIEnv *jnienv, jobject spobj, jlong termref, jlong canonical)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_atom((SP_term_ref)termref, (unsigned long)canonical))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutAtom(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jlong canonical)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_atom((SP_term_ref)termref, (unsigned long)canonical))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */


/*
 * Class:     jasper_SPTerm
 * Method:    spGetString
 * Signature: (Ljasper/SICStus;J)Ljava/lang/String;
 */
#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spGetString(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  char *name;
  jstring res = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  
  if (0 == SP_get_string((SP_term_ref)termref, &name))
    {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }
  else
    {
      res = (*jnienv)->NewStringUTF(jnienv, name);
    }

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SPTerm_spGetString(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  char *name;
  jstring res = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  
  if (0 == SP_get_string((SP_term_ref)termref, &name))
    {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }
  else
    {
      res = (*jnienv)->NewStringUTF(jnienv, name);
    }

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetAtom
 * Signature: (Ljasper/SICStus;J)J
 */

#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spGetAtom(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  unsigned long atmindex;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_atom((SP_term_ref)termref, &atmindex))
    {
      res = throwNCE(spobj, jnienv);
      goto cleanup;
    }
  res = (jlong)atmindex;

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}
#else  /* !NEW_WORLD_ORDER */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPTerm_spGetAtom(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  unsigned long atmindex;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_atom((SP_term_ref)termref, &atmindex))
    {
      res = throwNCE(spobj, jnienv);
      goto cleanup;
    }
  res = (jlong)atmindex;

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spConsFunctor
 * Signature: (Ljasper/SICStus;JJI)I
 */
#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spConsFunctor(JNIEnv *jnienv, jobject spobj,
                                         jlong termref, jlong atmindex, jlongArray args)
{
  jsize numargs;
  int i;
  SP_term_ref *spargs = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
     
      spargs[i] = (SP_term_ref)sparg;
    }

  if (0 == SP_cons_functor_array((SP_term_ref)termref, (unsigned long)atmindex, (int)numargs, spargs))
    {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }

 cleanup:
  if (spargs) SP_free(spargs);  /* [PM] 3.8.7 plug leak SPRM 2327 */

  SP_LEAVE_SICSTUS();
}

#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spConsFunctor(JNIEnv *jnienv, jobject term, jobject spobj, 
                                    jlong termref, jlong atmindex, jlongArray args)
{
  jsize numargs;
  int i;
  SP_term_ref *spargs;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
     
      spargs[i] = (SP_term_ref)sparg;
    }

  if (0 == SP_cons_functor_array((SP_term_ref)termref, (unsigned long)atmindex, (int)numargs, spargs))
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */



#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spErrorMessage(JNIEnv *jnienv, jobject spobj, jint eno)
{
  jstring res;
  SP_ENTER_SICSTUS();
#if DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d\n", (int) eno);
#endif
  res = (*jnienv)->NewStringUTF(jnienv, SP_error_message((int)eno));
#if DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d, msg==\"%s\"\n",
          (int) eno, SP_error_message((int)eno));
#endif

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SPException_spErrorMessage(JNIEnv *jnienv, jobject spobj, jint eno)
{
  jstring res;
  SP_ENTER_SICSTUS();
#if DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d\n", (int) eno);
#endif
  res = (*jnienv)->NewStringUTF(jnienv, SP_error_message((int)eno));
#if DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d, msg==\"%s\"\n",
          (int) eno, SP_error_message((int)eno));
#endif

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spGetErrno(JNIEnv *jnienv, jobject spobj)
{
  jint res;
  SP_ENTER_SICSTUS();
  res = (jint)SP_errno;
  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPException_spGetErrno(JNIEnv *jnienv, jobject spobj)
{
  jint res;
  SP_ENTER_SICSTUS();
  res = (jint)SP_errno;
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFloat
 * Signature: (Ljasper/SICStus;J)D
 */
#if NEW_WORLD_ORDER
JNIEXPORT jdouble JNICALL 
Java_se_sics_jasper_SICStus_spGetFloat(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  double res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_float((SP_term_ref)termref, &res))
    {
      res = throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jdouble JNICALL 
Java_se_sics_jasper_SPTerm_spGetFloat(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  double res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_float((SP_term_ref)termref, &res))
    {
      res = throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetObject
 * Signature: (Ljasper/SICStus;J)Ljava/lang/Object;
 */
#if NEW_WORLD_ORDER
JNIEXPORT jobject JNICALL 
Java_se_sics_jasper_SICStus_spGetObject(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  long lobj;
  SP_term_ref tr;
  SP_term_ref objref;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  tr = (SP_term_ref)termref;
  objref = SP_new_term_ref();

  if (! (SP_get_arg(1,tr,objref) &&
         SP_get_integer(objref,&lobj)) )
    {
      throwNCE(spobj, jnienv);
      lobj = 0;
    }
  SP_reset_term_refs(objref);

  SP_LEAVE_SICSTUS();
  return (jobject) lobj;
}
#else /* 3.8 */
JNIEXPORT jobject JNICALL 
Java_se_sics_jasper_SPTerm_spGetObject(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  long lobj;
  SP_term_ref tr;
  SP_term_ref objref;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  tr = (SP_term_ref)termref;
  objref = SP_new_term_ref();

  if (! (SP_get_arg(1,tr,objref) &&
         SP_get_integer(objref,&lobj)) )
    {
      throwNCE(spobj, jnienv);
      lobj = 0;
    }
  SP_reset_term_refs(objref);

  SP_LEAVE_SICSTUS();
  return (jobject) lobj;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spPutFloat
 * Signature: (Ljasper/SICStus;JD)I
 */
#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutFloat(JNIEnv *jnienv, jobject spobj, jlong termref, jdouble value)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_float((SP_term_ref)termref, (double)value))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else  /* !NEW_WORLD_ORDER */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutFloat(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jdouble value)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_float((SP_term_ref)termref, (double)value))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spGetInteger(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  long l;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_integer((SP_term_ref)termref, &l))
    {
      l = (long) throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return (jlong) l;
}
#else /* 3.8 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPTerm_spGetInteger(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  long l;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_integer((SP_term_ref)termref, &l))
    {
      l = (long) throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return (jlong) l;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutInteger(JNIEnv *jnienv, jobject spobj, jlong termref, jlong val)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_integer((SP_term_ref)termref, (long)val))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutInteger(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jlong val)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_integer((SP_term_ref)termref, (long)val))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spCreateGlobalRef(JNIEnv *jnienv, jobject spobj, jobject obj)
{
  long globref;
  (void) spobj;                 /* -Wunused */
  /* Does not need to call SP_ENTER/LEAVE_SICSTUS */
  /* Mean little sucker. Store an object reference in a long. */
  globref = (long)(*jnienv)->NewGlobalRef(jnienv, obj);

  return (jlong)globref;
}
#else /* 3.8 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPTerm_spCreateGlobalRef(JNIEnv *jnienv, jobject this, jobject spobj, jobject obj)
{
  long globref;
  /* Does not need to call SP_ENTER/LEAVE_SICSTUS */
  /* Mean little sucker. Store an object reference in a long. */
  globref = (long)(*jnienv)->NewGlobalRef(jnienv, obj);

  return (jlong)globref;
}
#endif /* !NEW_WORLD_ORDER */

#if 0 /* [PM] 3.8.5 not used */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spDeleteGlobalRef(JNIEnv *jnienv, jobject this, jobject spobj, jlong globref)
{
  /* Does not need to call SP_ENTER/LEAVE_SICSTUS */
  /* needs to be ifdef:d for non 32-bits platforms */
  (*jnienv)->DeleteGlobalRef(jnienv, (jobject)(long)globref);
}

JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spDeleteLocalRef(JNIEnv *jnienv, jobject this, jobject spobj, jlong ref)
{
  /* Does not need to call SP_ENTER/LEAVE_SICSTUS */
  /* needs to be ifdef:d for non 32-bits platforms */
  (*jnienv)->DeleteLocalRef(jnienv, (jobject)(long)ref);
}
#endif


#if !SPPREDICATE_DEPRECATED
/*
 * Class:     jasper_SPPredicate
 * Method:    spMakePredRef
 * Signature: (Ljasper/SICStus;Ljava/lang/String;ILjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPPredicate_spMakePredRef(JNIEnv *jnienv, jobject term, jobject spobj,  
                                      jstring name, jint arity, jstring module)
{
  char *spname, *spmodule;
  SP_pred_ref pred;

  SP_ENTER_SICSTUS();
  
  spname = (char *)(*jnienv)->GetStringUTFChars(jnienv, name, NULL);
  spmodule = (char *)(*jnienv)->GetStringUTFChars(jnienv, module, NULL);
  
  pred = SP_predicate(spname, arity, spmodule);

  if (spname)
    (*jnienv)->ReleaseStringUTFChars(jnienv, name, spname);
  
  if (spmodule)
    (*jnienv)->ReleaseStringUTFChars(jnienv, module, spmodule);
 
#if 0 
  printf("pred = %lx (%s).\n", pred, SP_error_message(SP_errno));
#endif

  SP_LEAVE_SICSTUS();

  return (jlong)(long)pred;
}
#endif /* !SPPREDICATE_DEPRECATED */


#if !SPPREDICATE_DEPRECATED
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPPredicate_spMakePredRefCanonical(JNIEnv *jnienv, jobject term, jobject spobj,  
                                      jlong name, jint arity, jlong module)
{
  jlong res;

  SP_ENTER_SICSTUS();

  res = (jlong)(long)SP_pred((unsigned long)name,arity,(unsigned long)module);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !SPPREDICATE_DEPRECATED */


/*
 * Class:     jasper_SPQuery
 * Method:    spNextSolution
 * Signature: ()I
 */


#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spNextSolution(JNIEnv *jnienv, jobject spobj, /* jobject query, */ jlong qidref)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = (jint) handleExceptions(SP_ONLY_API_ARG jnienv, SP_next_solution((SP_qid)qidref));

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPQuery_spNextSolution(JNIEnv *jnienv, jobject query, jobject spobj, jlong qidref)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = (jint) handleExceptions(SP_ONLY_API_ARG jnienv, SP_next_solution((SP_qid)qidref));

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spCutQuery(JNIEnv *jnienv, jobject spobj, /* jobject query, */jlong qidref)
{
  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_cut_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPQuery_spCutQuery(JNIEnv *jnienv, jobject query, jobject spobj, jlong qidref)
{
  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_cut_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spCloseQuery(JNIEnv *jnienv, jobject spobj, /* jobject query, */ jlong qidref)
{

  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_close_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPQuery_spCloseQuery(JNIEnv *jnienv, jobject query, jobject spobj, jlong qidref)
{

  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_close_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */


JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spExceptionTerm(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  int r;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  r = SP_exception_term((SP_term_ref)termref);

#if 0
  fprintf(stderr, "got exception term: ");
  SP_query(SP_predicate("writeq",1,""), termref);
  fprintf(stderr, "\n");
#endif

  SP_LEAVE_SICSTUS();

  return r;
}

#define LOAD_JASPER 0      /* [PD] 3.9 What it sez, maan. */

JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spInitialize(JNIEnv *jnienv, jobject spobj, jobjectArray array, jstring str)
{
  int sp_java_argc;
  int i;
  int rval = SP_SUCCESS;
  char *bootpath = NULL;

#if NEW_WORLD_ORDER
  jobject spobj_global = NULL;
#if LOAD_JASPER
  SP_term_ref tr = 0;           /* inited to silence compiler */
  int tr_set = 0;               /* true if tr need SP_reset_term_refs */
#endif

  SP_ONLY_API_ARG_PROTO_DECL0;

  SP_ONLY_API_ARG_NAME=NULL;
#endif

  DbgBreakPoint();

#if DBG
  if (getenv("__SPNATIVE_DEBUG"))
    {
      fprintf(stderr, "Java_se_sics_jasper_SICStus_spInitialize debug break\n");
      DEBUG_BREAK();
    }
#endif /* DBG */

#if DBG
  fprintf(stderr, "Java_se_sics_jasper_SICStus_spInitialize jnienv=%p\n", jnienv);
#endif /* DBG */

#if !NEW_WORLD_ORDER
  {
    {
      if (jasper_sp_global)                /* Should really not happen. */
        {
#if DBG
          fprintf(stderr, "ERROR: Re-setting jasper_sp_global from %ld to %ld\n", (long) jasper_sp_global, (long) spobj);
#endif

          (*jnienv)->DeleteGlobalRef(jnienv, jasper_sp_global);
          jasper_sp_global = NULL;
        }
  
#if DBG
      fprintf(stderr, "About to set jasper_sp_global to %ld\n", (long) obj);
#endif
      jasper_sp_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
#if DBG
      fprintf(stderr, "Set jasper_sp_global to %ld\n", (long) jasper_sp_global);
#endif

    }
    SP_ENTER_SICSTUS();           /* must be done *after* setting jasper_sp_global */
  }
#endif /* !NEW_WORLD_ORDER */

#if 0                           /* [PM] 3.9 Was no-op already in 3.8.5 */
     if (!jasperi_set_jnienv(jnienv))
       {
         rval = -3;                  /* SP_JNIENV_SET */
         goto cleanup;
       }
#endif


#if MULTI_SP_AWARE
  {
    int res;
    SICSTUS_API_STRUCT_TYPE *sprt_dispatch;
    SP_get_dispatch_type *get_dispatch;

    #if DBG>1
    fprintf(stderr, "\nCalling SP_get_dispatch() (%p())\n", SP_get_dispatch);fflush(stderr);
    #endif /* DBG */

    sprt_dispatch = SP_get_dispatch(NULL);
    if (sprt_dispatch == NULL)
      {
        #if DBG
        fprintf(stderr, "\n*** ERROR: SP_get_dispatch() (%p()) == NULL\n", SP_get_dispatch);fflush(stderr);
        #endif/* DBG */
        rval=SP_ERROR;
        goto cleanup;
      }
    #if DBG>1
    fprintf(stderr, "\nCalled SP_get_dispatch()==%p\n", sprt_dispatch);fflush(stderr);
    #endif /* DBG */

    SP_ONLY_API_ARG_NAME = 
      sprt_dispatch;
    
#if DBG>1
    fprintf(stderr, "\nCalling SP_load_sicstus_run_time(%p, NULL)\n", &get_dispatch);
#endif /* DBG */
    res = SP_load_sicstus_run_time(&get_dispatch, NULL);
#if DBG>1
    fprintf(stderr, "\nCalled SP_load_sicstus_run_time(%p, NULL) == %d\n", &get_dispatch, res);
#endif /* DBG */

    if (res < 0)
      {
#if DBG
        fprintf(stderr, "\nP_load_sicstus_run_time(%p, NULL) failed\n", &get_dispatch);
#endif  
        rval=SP_ERROR;
        goto cleanup;
      }
    SP_ONLY_API_ARG_NAME = get_dispatch(NULL);
    if (SP_ONLY_API_ARG_NAME == NULL)
      {
#if DBG
        fprintf(stderr, "\nget_dispatch failed\n");
#endif  
        rval=SP_ERROR;
        goto cleanup;
      }
  }
#endif  /* MULTI_SP_AWARE */

  /* [PM] 3.9b5 a safe default (not SBRK) */
  SP_set_memalloc_hooks(MM_USE_DEFAULT,NULL,NULL,NULL,NULL);

  if (array != NULL)
    {
      sp_java_argc = (int)(*jnienv)->GetArrayLength(jnienv, array);
/* [PD] 3.9 Can't call SP_malloc here, before glue_initialize has been called.
      sp_java_argv = (char **)SP_malloc(sizeof(char *)*(sp_java_argc+1));  */
      sp_java_argv = (char **)malloc(sizeof(char *)*(sp_java_argc+1));
      
      if (sp_java_argv == NULL) goto glue_failure;
      
      for (i = 0; i < sp_java_argc; i++)
        {
          jobject arg;
          
          arg = (*jnienv)->GetObjectArrayElement(jnienv, array, i);
#if 1                           /* leak */

#if MULTI_SP_AWARE
#if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
#error "ought to fix this leak once we start creating many sicstus instances."
#endif /* SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION */
#endif /* MULTI_SP_AWARE */

          /* NOTE: leak. should copy to SP_malloc memory */
          sp_java_argv[i] = (char *)(*jnienv)->GetStringUTFChars(jnienv, arg, NULL);
#else  /* !leak (bogus) */
          {
            char *p = (char *)(*jnienv)->GetStringUTFChars(jnienv, arg, NULL);

            if (p)
              {
#error "bogus, cannot call SP_malloc before glue_initialize"
                sp_java_argv[i] = SP_malloc(strlen(p)+1);
                strcpy(sp_java_argv[i], p);
                (*jnienv)->ReleaseStringUTFChars(jnienv, arg, p);
              }
            else
              {
                /* Java exception raised */
                rval = SP_FAILURE;
                goto cleanup;
              }
          }
#endif /* !leak */
        }
      sp_java_argv[i] = NULL;
    }
  else
    {
      sp_java_argc = 0;
      sp_java_argv = NULL;
    }
  
  if (str != NULL)
    {
      bootpath = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL);
    }

#if NEW_WORLD_ORDER
  {
    /* Done the same with and without MULTI_SP_AWARE. The difference is in
       MULTI_SP_AWARE explicitly getting the dispatch vector above */

#if DBG
    fprintf(stderr, "SP_initialize(%d, %p, \"%s\");\n", sp_java_argc, sp_java_argv, ( bootpath ? bootpath : "bootpath==NULL" ));
#endif /* DBG */

    rval = SP_initialize(sp_java_argc, sp_java_argv, bootpath);
  }
#else  /* !NEW_WORLD_ORDER */
  {
#if DBG
    fprintf(stderr, "glue_initialize(%d, %p, \"%s\", NULL, NULL, 0, 1);\n", sp_java_argc, sp_java_argv, ( bootpath ? bootpath : "bootpath==NULL" ));
#endif /* DBG */

    /* Uses glue_initialize instead of SP_initialize to avoid having
       sp_pre_linkage sp_pre_map (which would give "multiple definition"
       error if statically linking with a SICStus run-time exe. (i.e.,
       this is/was SICStus 3.8.X when spnative is part of the jasper
       foreign resource. */
    rval = glue_initialize(sp_java_argc, sp_java_argv, bootpath, NULL, NULL, 0, 1);
  }
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
  {
    if (SP_ONLY_API_ARG_NAME == NULL)   /* should have been set up by  */
      {
#if DBG
        fprintf(stderr, "ERROR: glue_initialize did not set up SP_ONLY_API_ARG_NAME");
#endif /* DBG */
        goto glue_failure;
      }
    {
      int res = set_spobj_api(jnienv, spobj, SP_ONLY_API_ARG_NAME);
      if (res != SP_SUCCESS)
        {
#if DBG
          fprintf(stderr,"Error (%d) from set_spobj_api, JNI=%p\n", res, jnienv);
#endif /* DBG */
#if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
#error "need to think this through"
#endif /* SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION */
          goto glue_failure;
        }
    }

    /* sprt is initialized, SICStus object has its dispatch API address
       Now tell prolog about the SICStus object.
#if LOAD_JASPER
       This will also load the jasper module.
#endif
    */
#if LOAD_JASPER
    {

      SP_pred_ref pred;
      
      pred = SP_predicate("jasper_load", 1, "prolog");
      if (!pred) goto glue_failure;
      tr = SP_new_term_ref();
      tr_set = 1;               /* ensure tr is reset at cleanup */
      spobj_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
      if (!SP_put_integer(tr, (long)spobj_global)) goto glue_failure;
      if (SP_query_cut_fail(pred, tr) != SP_SUCCESS) goto glue_failure;
      /* succeeded in loading library(jasper) and telling it about the
         SICStus object */
      spobj_global = NULL;      /* protect from DeleteGlobalRef in cleanup */
    }
#else
    /* Use new (semi-public) API function to save the SICStus object. */
      spobj_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
      if (!sp_set_jasper_magic(spobj_global,SICSTUS_VERSION)) goto glue_failure;
      spobj_global = NULL;      /* protect from DeleteGlobalRef in cleanup */
#endif
  }
#endif /* NEW_WORLD_ORDER */

 cleanup:
  if (bootpath) (*jnienv)->ReleaseStringUTFChars(jnienv, str, bootpath);
#if NEW_WORLD_ORDER
  if (spobj_global) (*jnienv)->DeleteGlobalRef(jnienv, spobj_global);
#if LOAD_JASPER
  if (tr_set) SP_reset_term_refs(tr);
#endif
#endif

#if !NEW_WORLD_ORDER
  SP_LEAVE_SICSTUS();
#endif

  return rval;

 glue_failure:
#if DBG
  fprintf(stderr, "ERROR: glue_failure in Java_se_sics_jasper_SICStus_spInitialize");
#endif /* DBG */
  rval = -2;              /* SP_GLUEFAILURE */
  goto cleanup;
}

JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spFinalize(JNIEnv *jnienv, jobject spobj)
{
  SP_ENTER_SICSTUS();
  if (sp_java_argv)
    SP_free(sp_java_argv);
  
  SP_deinitialize();
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spLoad(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char *qlfile;
  int rval = SP_SUCCESS;

  SP_ENTER_SICSTUS();

  qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL);

  rval = SP_load(qlfile);

  (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlfile);

  SP_LEAVE_SICSTUS();

  return (jint)rval;
}

JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spRestore(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char *qlfile;
  int rval = SP_SUCCESS;
  SP_ENTER_SICSTUS();

  qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL);

  rval = SP_restore(qlfile);

  (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlfile);

  SP_LEAVE_SICSTUS();

  return (jint)rval;
}

/* [PD] FIXME: remove this. library(jasper) should never be reloaded since it
   is not loaded by Jasper anymore. */
#if 0
JNIEXPORT jint JNICALL
Java_se_sics_jasper_SICStus_spReloadJasper(JNIEnv *jnienv, jobject spobj)
{
  int rval;
  int tr_set = 0;
  SP_term_ref tr;
  SP_pred_ref pred;
  jobject spobj_global = NULL;
  SP_ENTER_SICSTUS();

  pred = SP_predicate("jasper_load", 1, "prolog");
  if (!pred) { goto barf; }
  tr = SP_new_term_ref();
  tr_set = 1;               /* ensure tr is reset at cleanup */
  spobj_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
  if (!SP_put_integer(tr, (long)spobj_global)) { goto barf; }
  rval = SP_query_cut_fail(pred, tr);
  if (rval != SP_SUCCESS) goto cleanup;
  /* succeeded in loading library(jasper) and telling it about the
     SICStus object */
  spobj_global = NULL;      /* protect from DeleteGlobalRef in cleanup */

 cleanup:
  if (spobj_global) (*jnienv)->DeleteGlobalRef(jnienv, spobj_global);
  if (tr_set) SP_reset_term_refs(tr);

  SP_LEAVE_SICSTUS();

  return rval;

 barf:
  rval = SP_ERROR;
  goto cleanup;
}
#endif


#if 0
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spLoadLibfile(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char qlpath[1024], *qlfile;
  int rval = SP_SUCCESS;
  
  SP_ENTER_SICSTUS();
  
  /* Pick up the Prolog boot-path */
  strcpy(qlpath, sp_get_boot_path());
  qlpath[strlen(qlpath)-4] = '\0'; /* Remove trailing "/bin" */
  strcat(qlpath, "/library/");

  if ((qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL)))
    {
      strcat(qlpath, qlfile);
      
      rval = SP_load(qlpath);
      
      (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlpath);
    }
  SP_LEAVE_SICSTUS();

  return (jint)rval;
}
#endif

#if 1 /* [PM] 3.8.5 */

/*
  Exception handling:
  Calls from Prolog to Java (via Jasper) *never* leave a pending Java exception.
  Therefore if there is Java exception then it should not be ignored
 */

static jlong handleExceptions(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, int query_rval)
{
  jboolean have_jexcp;
  SP_term_ref excp_term;
  
  have_jexcp = (*jnienv)->ExceptionCheck(jnienv);

  if (query_rval == SP_ERROR)
    {
      if (have_jexcp)
        {
          /* 
           * Retract the prolog exception if there was a
           * Java exception.
           */
          excp_term = SP_new_term_ref();
          SP_exception_term(excp_term);
          SP_reset_term_refs(excp_term);
        }
    }
  else if (have_jexcp)
    {
      /* There was a java exception and no Prolog exception,
       */
      ;
    }

  return (jlong)query_rval;
}
#else /* pre 3.8.5 */
static jlong handleExceptions(JNIEnv *jnienv, int query_rval)
{
  SP_term_ref excp_term;
  jthrowable excp;
  /* [PM] 3.8.5 the following is broken as it ignores the case when
     prolog makes several calls to Java, some of which raises exceptions
     that are caught in prolog, before returning to the surrounding
     Java
  */
  
  /* Handle exceptions from recursive calls to Java */
  excp = (*jnienv)->ExceptionOccurred(jnienv);

  if (query_rval == SP_ERROR)
    {
      if (excp != NULL)
        {
          /* 
           * Retract the prolog exception if there was a
           * Java exception. This means that a java exception
           * was thrown and not caught by the Prolog exception
           * mechanism. The Java exception will stay thrown
           * when we return to the JVM.
           */
          excp_term = SP_new_term_ref();
          SP_exception_term(excp_term);
        }
    }
  /* 
   * If there was a java exception and no Prolog exception,
   * this means that the java exception was handled by the 
   * Prolog exception mechanism, and should therefore be ignored.
   */
  else if (excp != NULL)
    (*jnienv)->ExceptionClear(jnienv);

  return (jlong)query_rval;
}
#endif





/* The calling Java code should zap all SPTerm refs not older than termref. */
JNIEXPORT void JNICALL Java_se_sics_jasper_SICStus_spResetTermRefs(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  if (termref <= (jlong)SP_new_term_refs(0))
    {
      SP_reset_term_refs((SP_term_ref)termref);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jlong JNICALL Java_se_sics_jasper_SICStus_spGetTermRefs(JNIEnv *jnienv, jobject spobj)
{
  jlong mark;
  SP_ENTER_SICSTUS();
  mark = SP_new_term_refs(0);
  SP_LEAVE_SICSTUS();
  return mark;
}

JNIEXPORT jboolean JNICALL Java_se_sics_jasper_SICStus_spValidTermRef(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean result;

  SP_ENTER_SICSTUS();
  result = ((0 < termref && termref < (jlong)SP_new_term_refs(0)) ? JNI_TRUE : JNI_FALSE);
  SP_LEAVE_SICSTUS();
  return result;
}


/* should be called inside an SP_ENTER_SICSTUS() block!! */
/* zero iff Java exception raised */
/* If it returns a valid (non-zero) SP_qid then it will have allocated
   a few term-refs that must be freed by the caller. The reason for
   the leak is that the term refs are needed when calling
   SP_open_query and by then it is to late to reclaim them. ([PM]
   3.8.7 SPRM 2495) */
static SP_qid jasper_open_call(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid res = 0;
  SP_term_ref mark = SP_new_term_refs(0);
  int mark_valid = 1;           /* [PM] sprm 2495 */
  jsize numargs = (*jnienv)->GetArrayLength(jnienv, args);
  SP_term_ref *spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);
  const char *pred_name = NULL;
  const char *module_name = NULL;
  int i;


  pred_name = (*jnienv)->GetStringUTFChars(jnienv, name, NULL);
  if (!pred_name) goto cleanup; /* exception raised */
  module_name = (*jnienv)->GetStringUTFChars(jnienv, module, NULL);
  if (!module_name) goto cleanup; /* exception raised */

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
      DBG_VALIDATE_TERM_REF(sparg);
      spargs[i] = (SP_term_ref)sparg;
    }
  {
    SP_pred_ref call_pred = SP_predicate("jasper_call", 2, "prolog"); /* jasper_call(+Goal,+Module) */
    SP_term_ref goal_term = SP_new_term_ref();
    SP_term_ref module_term = SP_new_term_ref();
    unsigned long name_atom = SP_atom_from_string(pred_name);

    if (0 == (name_atom = SP_atom_from_string(pred_name))
        ||
        !SP_put_string(module_term, module_name) ) {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }

    if (0 == SP_cons_functor_array(goal_term, name_atom, (int)numargs, spargs)) {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }
    /* Goal = PRED(ARGS) */
    res = SP_open_query(call_pred, goal_term, module_term); /* [PM] This chpt remembers current term stack index! SPRM 2495 */
    if (!res) {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }
    /* [PM] sprm 2495 We cannot reset term refs to mark since that
       would make subsequent backtracking into the query *increase*
       the term stack index (to module_term). Instead we leak the term
       refs created before SP_open_query (module_term, goal_term). */
    mark_valid = 0;
    
  }
 cleanup:

  if (pred_name) (*jnienv)->ReleaseStringUTFChars(jnienv, name, pred_name);
  if (module_name) (*jnienv)->ReleaseStringUTFChars(jnienv, module, module_name);

  if (spargs) SP_free(spargs);              /* already bombed if SP_malloc returned NULL */

  if (mark_valid) SP_reset_term_refs(mark);
  return res;                   /* zero iff Java exception raised */
}

JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spOpenContext(JNIEnv *jnienv, jobject spobj)
{
  SP_qid query;
  SP_ENTER_SICSTUS();
 
  query = SP_open_query(SP_predicate("true", 0, "prolog"));

  SP_LEAVE_SICSTUS();
  return (!query ? 0 : (jlong)query);
}


JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spCall(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  int rc;
  jlong res;
  SP_term_ref mark;
  SP_term_ref mark_valid = 0;

  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCall\n");

  SP_ENTER_SICSTUS();

#if DBG
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spCall:%d, JNI=%p\n", __LINE__, jnienv);
#endif

  mark = SP_new_term_refs(0);
  mark_valid = 1;
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);
  
  if (!query)                   /* Java exception raised */
    {
      rc = SP_SUCCESS;
      goto cleanup;
    }
  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCall\n");

  if ((rc = SP_next_solution(query)) > 0)
    {
#if DBG>1
      fprintf(stderr,"Java_se_sics_jasper_SICStus_spCall:%d, JNI=%p rc=%d\n", __LINE__, jnienv, (int)rc);
#endif
      SP_cut_query(query);
    }

 cleanup:
  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);
  if (mark_valid) SP_reset_term_refs(mark);
  SP_LEAVE_SICSTUS();
  return res;
}


#if !SPPREDICATE_DEPRECATED
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spQuery(JNIEnv *jnienv, jobject spobj, jobject pred, jlong predref, jlongArray args)
{
  jsize numargs;
  int i, rc;
  SP_term_ref *spargs;
  SP_qid query;
  jlong res;

  SP_ENTER_SICSTUS();

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif

  /* pred arity validated in caller */

  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif


  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
      DBG_VALIDATE_TERM_REF(sparg);
      spargs[i] = (SP_term_ref)sparg;
    }
#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif
  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spQuery\n");

  query = SP_open_query_array((SP_pred_ref)(long)predref, spargs);
#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p, query=%p\n", __LINE__, jnienv, (void*)query);
#endif
  
  if ((rc = SP_next_solution(query)) > 0)
    {
#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p rc=%d\n", __LINE__, jnienv, (int)rc);
#endif

      SP_cut_query(query);
    }
#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif


  SP_free(spargs);              /* already bombed if SP_malloc returned NULL */

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif

  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif

  SP_LEAVE_SICSTUS();

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spQuery:%d, JNI=%p\n", __LINE__, jnienv);
#endif


  return res;
}
#endif /* !SPPREDICATE_DEPRECATED */

JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spCallCutFail(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  int rc;
  jlong res;
  SP_term_ref mark;
  SP_term_ref mark_valid = 0;

  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCallCutFail\n");

  SP_ENTER_SICSTUS();

#if DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spCallCutFail:%d, JNI=%p\n", __LINE__, jnienv);
#endif
 
  mark = SP_new_term_refs(0);
  mark_valid = 1;
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);

  if (!query)                   /* Java exception raised */
    {
      rc = SP_SUCCESS;
      goto cleanup;
    }
  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCallCutFail\n");

  if ((rc = SP_next_solution(query)) > 0)
    {
#if DBG>1
      fprintf(stderr,"Java_se_sics_jasper_SICStus_spCallCutFail:%d, JNI=%p rc=%d\n", __LINE__, jnienv, (int)rc);
#endif
      SP_close_query(query);
    }

 cleanup:
  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);
  if (mark_valid) SP_reset_term_refs(mark);

  SP_LEAVE_SICSTUS();
  return res;
}

#if !SPPREDICATE_DEPRECATED
/* NOTE: Should merge with Java_se_sics_jasper_SICStus_spQuery */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spQueryCutFail(JNIEnv *jnienv, jobject spobj, jobject pred, jlong predref, jlongArray args)
{
  jsize numargs;
  int i, rc;
  SP_term_ref *spargs;
  SP_qid query;
  jlong res;

  SP_ENTER_SICSTUS();

  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
      DBG_VALIDATE_TERM_REF(sparg);
      spargs[i] = (SP_term_ref)sparg;
    }
  
  query = SP_open_query_array((SP_pred_ref)(long)predref, spargs);

  if ((rc = SP_next_solution(query)) > 0)
    SP_close_query(query);

  SP_free(spargs);              /* already bombed if SP_malloc returned NULL */

  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !SPPREDICATE_DEPRECATED */


JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spOpenCall(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  SP_ENTER_SICSTUS();
 
  /* 3.8.7 Note that this leaks term-refs that must be freed by caller after the query is closed ([PM] 3.8.7 sprm 2495) */
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);

  SP_LEAVE_SICSTUS();
  return (!query ? 0 : (jlong)query);
}



#if !SPPREDICATE_DEPRECATED
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spOpenQuery(JNIEnv *jnienv, jobject spobj, jobject pred, jlong predref, jlongArray args)
{
  jlong res;
  jsize numargs;
  int i;
  SP_term_ref *spargs;

  SP_ENTER_SICSTUS();
  
  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
      DBG_VALIDATE_TERM_REF(sparg);     
      spargs[i] = (SP_term_ref)sparg;
    }
  
  res = SP_open_query_array((SP_pred_ref)(long)predref, spargs);

  SP_free(spargs);              /* already bombed if SP_malloc returned NULL */
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !SPPREDICATE_DEPRECATED */

JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spMakeTermRef(JNIEnv *jnienv, jobject spobj)
{
  jlong res;

  SP_ENTER_SICSTUS();

  res = (jlong)SP_new_term_ref();
  DBG_VALIDATE_TERM_REF(res);
  SP_LEAVE_SICSTUS();
  return res;
}


#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spTermType(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  res = (jint)SP_term_type((SP_term_ref)termref);

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPTerm_spTermType(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  res = (jint)SP_term_type((SP_term_ref)termref);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spGetListChars(JNIEnv *jnienv, jobject spobj, /* jobject term, */ jlong termref)
{
  jstring str = NULL;
  char *name = NULL;
  
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_get_list_chars((SP_term_ref)termref, &name))
    {
      str = (*jnienv)->NewStringUTF(jnienv, name);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  
  SP_LEAVE_SICSTUS();
  return str;
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SPTerm_spGetListChars(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  jstring str = NULL;
  char *name = NULL;
  
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_get_list_chars((SP_term_ref)termref, &name))
    {
      str = (*jnienv)->NewStringUTF(jnienv, name);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  
  SP_LEAVE_SICSTUS();
  return str;
}
#endif /* !NEW_WORLD_ORDER */

/* [PM] 3.8.7 Was declared but not defined! */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spGetNumberChars(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jstring str = NULL;
  char *name = NULL;
  
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_get_number_chars((SP_term_ref)termref, &name))
    {
      str = (*jnienv)->NewStringUTF(jnienv, name);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  
  SP_LEAVE_SICSTUS();
  return str;
}


#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spGetList(JNIEnv *jnienv, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spGetList(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */



#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutTerm(JNIEnv *jnienv, jobject spobj, jlong termref, jlong termref2)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_term((SP_term_ref)termref, (SP_term_ref)termref2))
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutTerm(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref, jlong termref2)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_term((SP_term_ref)termref, (SP_term_ref)termref2))
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFunctorCanonical
 * Signature: (Ljasper/SICStus;J)J
 */
#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spGetFunctorCanonical(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  unsigned long canonical;
  int dummy_arity;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &canonical, &dummy_arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jlong)canonical;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPTerm_spGetFunctorCanonical(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  unsigned long canonical;
  int dummy_arity;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &canonical, &dummy_arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jlong)canonical;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFunctorArity
 * Signature: (Ljasper/SICStus;J)J
 */
#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spGetFunctorArity(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  unsigned long dummy_canonical;
  int arity;
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &dummy_canonical, &arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jint) arity;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPTerm_spGetFunctorArity(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  unsigned long dummy_canonical;
  int arity;
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &dummy_canonical, &arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jint) arity;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetArg
 * Signature: (Ljasper/SICStus;JJJ)J
 */
#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spGetArg(JNIEnv *jnienv, jobject spobj, jlong i, jlong termref, jlong arg)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_arg((int)i, (SP_term_ref)termref, (SP_term_ref)arg))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spGetArg(JNIEnv *jnienv, jobject term, jobject spobj, jlong i, jlong termref, jlong arg)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_arg((int)i, (SP_term_ref)termref, (SP_term_ref)arg))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */



#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutVariable(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_variable((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutVariable(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_variable((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_list((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else  /* !NEW_WORLD_ORDER */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutList(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_list((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SICStus_spIsList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_list((SP_term_ref)termref))
    {
      res = JNI_TRUE;
    }
  else
    {
      res = JNI_FALSE;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SPTerm_spIsList(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_list((SP_term_ref)termref))
    {
      res = JNI_TRUE;
    }
  else
    {
      res = JNI_FALSE;
    }

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SICStus_spIsEmptyList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  res = JNI_FALSE;
  /* SP API really need SP_is_empty_list()/SP_is_nil() */
  if (SP_is_atom((SP_term_ref)termref))
    {
      char *pname;
      
      if (SP_get_string((SP_term_ref)termref, &pname))
        {
          if (pname[0] == '['
              && pname[1] == ']'
              && pname[2] == '\0')
            {
              res = JNI_TRUE;
            }
        }
    }

  SP_LEAVE_SICSTUS();
  return res;
}


#if NEW_WORLD_ORDER
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SICStus_spIsAtomic(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;

}
#else /* 3.8 */
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SPTerm_spIsAtomic(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;

}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SICStus_spIsNumber(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;
  
  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SPTerm_spIsNumber(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;
  
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spGetStringFromAtom
 * Signature: (Ljasper/SICStus;J)Ljava/lang/String;
 */
#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spGetStringFromAtom(JNIEnv *jnienv, jobject spobj, /* jobject term, */ jlong canonical)
{
  jstring str = NULL;
  char *name = NULL;

  SP_ENTER_SICSTUS();

  name = SP_string_from_atom((unsigned long) canonical);

  if (name)
    str = (*jnienv)->NewStringUTF(jnienv, name);
  
  SP_LEAVE_SICSTUS();
  return str;  
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SPTerm_spGetStringFromAtom(JNIEnv *jnienv, jobject term, jobject spobj, jlong canonical)
{
  jstring str = NULL;
  char *name = NULL;

  SP_ENTER_SICSTUS();

  name = SP_string_from_atom((unsigned long) canonical);

  if (name)
    str = (*jnienv)->NewStringUTF(jnienv, name);
  
  SP_LEAVE_SICSTUS();
  return str;  
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL
Java_se_sics_jasper_SICStus_spPrintVariable(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  char atom_buffer[256];
  jstring res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

#if 0
  printf("in spPrintVariable(): termref = %ld\n", (long)termref);
#endif  

  sp_variable_to_string((SP_term_ref)termref, atom_buffer);
  
  res = (*jnienv)->NewStringUTF(jnienv, atom_buffer);

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL
Java_se_sics_jasper_SPTerm_spPrintVariable(JNIEnv *jnienv, jobject term, jobject spobj, jlong termref)
{
  char atom_buffer[256];
  jstring res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

#if 0
  printf("in spPrintVariable(): termref = %ld\n", (long)termref);
#endif  

  sp_variable_to_string((SP_term_ref)termref, atom_buffer);
  
  res = (*jnienv)->NewStringUTF(jnienv, atom_buffer);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spGetAtomFromString(JNIEnv *jnienv, jobject spobj, jstring str)
{
  unsigned long canonical = 0;
  char *s;

  SP_ENTER_SICSTUS();

  if ((s = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL)))
    {
      canonical = SP_atom_from_string(s);
      
      (*jnienv)->ReleaseStringUTFChars(jnienv, str, s);
    }

  if (!canonical)
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
  return canonical;
}
#else /* 3.8 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPTerm_spGetAtomFromString(JNIEnv *jnienv, jobject term, jobject spobj, jstring str)
{
  unsigned long canonical = 0;
  char *s;

  SP_ENTER_SICSTUS();

  if ((s = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL)))
    {
      canonical = SP_atom_from_string(s);
      
      (*jnienv)->ReleaseStringUTFChars(jnienv, str, s);
    }

  if (!canonical)
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
  return canonical;
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spPutFunctor(JNIEnv *jnienv, jobject spobj, jlong termref, jlong atom, jint arity)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_functor((SP_term_ref)termref, (unsigned long)atom, arity))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else  /* !NEW_WORLD_ORDER */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spPutFunctor(JNIEnv *jnienv, jobject t, jobject spobj, jlong termref, jlong atom, jint arity)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_functor((SP_term_ref)termref, (unsigned long)atom, arity))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SICStus_spConsList(JNIEnv *jnienv, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_cons_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#else /* 3.8 */
JNIEXPORT void JNICALL 
Java_se_sics_jasper_SPTerm_spConsList(JNIEnv *jnienv, jobject t, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_cons_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}
#endif /* !NEW_WORLD_ORDER */

/*
 * Class:     jasper_SPTerm
 * Method:    spCompare
 * Signature: (Ljasper/SICStus;JJ)I
 */
#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spCompare(JNIEnv *jnienv, jobject spobj, jlong t1, jlong t2)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);
  res = SP_compare((SP_term_ref)t1, (SP_term_ref)t2);

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPTerm_spCompare(JNIEnv *jnienv, jobject t, jobject spobj, jlong t1, jlong t2)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);
  res = SP_compare((SP_term_ref)t1, (SP_term_ref)t2);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */


/*
 * Class:     jasper_SPTerm
 * Method:    spUnify
 * Signature: (Ljasper/SICStus;JJ)Z
 */
#if NEW_WORLD_ORDER
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SICStus_spUnify(JNIEnv *jnienv, jobject spobj, jlong t1, jlong t2)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);

  if (SP_unify((SP_term_ref)t1, (SP_term_ref)t2))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jboolean JNICALL 
Java_se_sics_jasper_SPTerm_spUnify(JNIEnv *jnienv, jobject t, jobject spobj, jlong t1, jlong t2)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);

  if (SP_unify((SP_term_ref)t1, (SP_term_ref)t2))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spRegisterAtom(JNIEnv *jnienv, jobject spobj, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_register_atom((unsigned long)t1);

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPCanonicalAtom_spRegisterAtom(JNIEnv *jnienv, jobject t, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_register_atom((unsigned long)t1);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */


#if NEW_WORLD_ORDER
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SICStus_spUnRegisterAtom(JNIEnv *jnienv, jobject spobj, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_unregister_atom((unsigned long)t1);

  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jint JNICALL 
Java_se_sics_jasper_SPCanonicalAtom_spUnRegisterAtom(JNIEnv *jnienv, jobject t, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_unregister_atom((unsigned long)t1);

  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spAtomFromString(JNIEnv *jnienv, jobject spobj, /* jobject t, */ jstring string)
{
  char *str;
  jlong cAtom = 0;

  SP_ENTER_SICSTUS();

  str = (char *)(*jnienv)->GetStringUTFChars(jnienv, string, NULL);
  if (str) {
    cAtom = (jlong)SP_atom_from_string(str);
    (*jnienv)->ReleaseStringUTFChars(jnienv, string, str);
  }

  if (!cAtom) {
    throwNCE(spobj, jnienv);
    cAtom = 0;
  }

  SP_LEAVE_SICSTUS();
  return cAtom;
}
#else /* 3.8 */
JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SPCanonicalAtom_spAtomFromString(JNIEnv *jnienv, jobject t, jobject spobj, jstring string)
{
  char *str;
  jlong cAtom = 0;

  SP_ENTER_SICSTUS();

  str = (char *)(*jnienv)->GetStringUTFChars(jnienv, string, NULL);
  if (str) {
    cAtom = (jlong)SP_atom_from_string(str);
    (*jnienv)->ReleaseStringUTFChars(jnienv, string, str);
  }

  if (!cAtom) {
    throwNCE(spobj, jnienv);
    cAtom = 0;
  }

  SP_LEAVE_SICSTUS();
  return cAtom;
}
#endif /* !NEW_WORLD_ORDER */

#if NEW_WORLD_ORDER
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SICStus_spStringFromAtom(JNIEnv *jnienv, jobject spobj, jlong cAtom)
{
  jstring res;

  SP_ENTER_SICSTUS();
  res =  (*jnienv)->NewStringUTF(jnienv,SP_string_from_atom((unsigned long)cAtom));
  SP_LEAVE_SICSTUS();
  return res;
}
#else /* 3.8 */
JNIEXPORT jstring JNICALL 
Java_se_sics_jasper_SPCanonicalAtom_spStringFromAtom(JNIEnv *jnienv, jobject t, jlong cAtom)
{
  jstring res;

  SP_ENTER_SICSTUS();
  res =  (*jnienv)->NewStringUTF(jnienv,SP_string_from_atom((unsigned long)cAtom));
  SP_LEAVE_SICSTUS();
  return res;
}
#endif /* !NEW_WORLD_ORDER */

#if CHARSIO_ENABLE

/*
 * CHARSIO support
 * xref library(charsio)
 */

struct open_String {
  unsigned char *chars;         /* GetStringUTFChars(string) */
  int index;
  int len;
  int size;
  jstring string;
  SP_stream *stream;
  struct open_String *next;
#if DBG
  JNIEnv *jnienv;
#endif
};

/* Linked list of all open_String structures. Access to this is
   protected by SP_ENTER_SICSTUS (?), or rather, the SICStus clazz
   monitor:

   [PM] 3.9b5 We synchronize the access to open_Strings by all SICStus
   objects using the SICStus class (in
   SICStus.openStringStream/closeStringStream). This way it will not
   matter that there may be string streams belonging to more than one
   run-time on the list.
*/

static struct open_String *open_Strings = NULL;

static int SPCDECL open_String_getc(void *handle)
{
  struct open_String *buf = (struct open_String *)handle;

  if (buf->index >= buf->len)
    {
      buf->index = buf->len+1;  /* make index>len */
      return -1;
    }
  
  return (int) buf->chars[buf->index++];
}

static int SPCDECL open_String_eof(void *handle)
{
  struct open_String *buf = (struct open_String *)handle;

  return buf->index > buf->len;
}

static int SPCDECL open_String_close(void *handle)
{
  struct open_String *buf = (struct open_String *)handle;

  /* Called in Prolog context, i.e., inside SP_ENTER_SICSTUS
     but not necessarily from same native thread as it was created in
  */
  buf->stream = NULL;           /* says that surrounding SP_streams already closed */

  return 0;
}

/* [PM] 3.9b5 must have a lock on SICStus Clazz to enter this  */
static SP_stream *open_String_stream(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, jstring string)
{
  SP_stream *stream = NULL;
  struct open_String *buf;


  jstring global_string = NULL;

  if (!(global_string = (*jnienv)->NewGlobalRef(jnienv, string))) {
    return NULL;
  }

  buf = (struct open_String *)SP_malloc(sizeof(struct open_String));
  buf->string = global_string;

  buf->chars = (unsigned char *) (*jnienv)->GetStringUTFChars(jnienv, global_string, NULL);
  buf->len = strlen(buf->chars);
  buf->size = buf->len+1;
  buf->index = 0;
  buf->next = open_Strings;
  open_Strings = buf;
  
#if DBG
  buf->jnienv = jnienv;
#endif

  if (SP_make_stream_context(buf,
                             open_String_getc, NULL, NULL, open_String_eof, NULL, open_String_close, &stream,
                             0, SP_STREAMHOOK_WCI) == SP_SUCCESS)
    {
      buf->stream = stream;
    }
  else                          /* SP_make_stream_context barfed */
    {
      buf->stream = NULL;
    }
  return stream;
}


JNIEXPORT jlong JNICALL 
Java_se_sics_jasper_SICStus_spOpenStringStream(JNIEnv *jnienv, jobject spobj, jstring string)
{
  SP_stream *stream;
  long stream_code;
  
  SP_ENTER_SICSTUS();
  stream = open_String_stream(SP_ONLY_API_ARG jnienv, string);
  
  {
    SP_term_ref tmp;
    
    tmp = SP_new_term_ref();
    
    if (! ( SP_put_address(tmp, stream)
            &&
            SP_get_integer(tmp, &stream_code) ) )
      {
        stream_code = 0;
      }
    SP_reset_term_refs(tmp);
  }
  
  SP_LEAVE_SICSTUS();

  return stream_code;
}

JNIEXPORT jboolean JNICALL
Java_se_sics_jasper_SICStus_spCloseStringStream(JNIEnv *jnienv, jobject spobj, jlong stream_code)
{
  jboolean closed = JNI_FALSE;
  SP_stream *stream;
  SP_ENTER_SICSTUS();           /* also to protect open_Strings */

  /* convert from stream code to address */
  {
    SP_term_ref tmp;
    
    tmp = SP_new_term_ref();
    if (! (SP_put_integer(tmp, (long)stream_code)
           &&
           SP_get_address(tmp, (void*)&stream) ) )
      {
        stream = NULL;
      }
    SP_reset_term_refs(tmp);
  }

  
  {
    struct open_String **pp;

    /* Go through *all* buf objects. Close stream if not already and
       free all pending bufs.
       Obviously this code assumes there are very few opened String
       streams. Unless there are multiple Java threads there will be
       only one. 
    */
    for (pp = &open_Strings; *pp != NULL;)
      {
        struct open_String *buf = *pp;
        if (buf->stream == stream)
          {                     /* stream was not already closed */
            closed = JNI_TRUE;

            (void) SP_fclose(stream); /* will set buf->stream to NULL */
#if DBG
            if (buf->stream != NULL)
              {
                fprintf(stderr, "ERROR: SP_fclose(stream) did not clear buf->stream\n");
              }
#endif
          }
        /* Both if closed here and if closed from prolog */
        if (buf->stream == NULL)
          {
            *pp = buf->next;   /* unlink buf */

#if DBG
            if (jnienv != buf->jnienv) {
              fprintf(stderr, "%% DBG: spCloseStringStream: wrong thread! jnienv==%p, buf->jnienv==%p\n", jnienv, buf->jnienv); fflush(stderr);
              abort();
            }
#endif

            (*jnienv)->ReleaseStringUTFChars(jnienv, buf->string, buf->chars);
            (*jnienv)->DeleteGlobalRef(jnienv, buf->string);
#if DBG
            buf->next = NULL;
            buf->chars = NULL;
            buf->string = NULL;
#endif
            SP_free(buf);
          }
        else
          {
            pp = &buf->next;
          }
        /* INVARIANT *pp == buf->next */
      }
  }
  
  SP_LEAVE_SICSTUS();
  return closed;
}





#endif /* CHARSIO_ENABLE */


/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 2
 *  end:
 **/

