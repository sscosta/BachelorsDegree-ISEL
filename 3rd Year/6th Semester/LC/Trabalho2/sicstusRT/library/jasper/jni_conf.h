#ifndef _JNI_CONF_H_
#define _JNI_CONF_H_

/**********************************************************
**
**  FILE
**    jni_conf.h
**
**  AUTHOR
**    Jesper Eskilson <jojo@sics.se>
** 
**  CREATED
**    Fri Jan 16 10:15:27 1998
**    
**  DESCRIPTION
**    Platform-speficic stuff for JNI
**
***********************************************************
*/

/* JNI on Win32 uses __int64, which is not present in Watcom */
#ifdef __WATCOMC__
#define __int64 long
#endif


#endif /* _JNI_CONF_H_ */
