/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

/*******************************************************************
* File: rcs_defs.hh
* Purpose: Under windows pointers passed between DLL`s and/or EXE's must be far
* and functions in an EXE or a DLL to be called by other DLL's should use
* the pascal calling convention.  Classes that can be used by other DLL's
* or EXE's need to be exported. Under other platforms these notions are
* irrelevant so I define some macros here to reduce the number of
* "#ifdef _Windows" in the rest of the rcs library code.
*******************************************************************/

#ifndef RCS_DEFS_HH
#define RCS_DEFS_HH

#ifdef HAVE_CONFIG_H

#define UNIX_LIKE_PLAT 1
#define RCS_EXPORT
#define RCS_FAR
#define RCS_PASCAL
#define RCS_HUGE

#else

#if defined(sunos5) && !defined(NO_THREADS)
#define _REENTRANT
#endif

#ifndef NO_SCCS_IDS
#if defined(__BORLANDC__) || defined(sunos5CC)
#define NO_SCCS_IDS
#endif
#endif

/* Make the symbols _MSDOS and MSDOS used by Microsoft Visual C++ and __MSDOS__
 used by Borland C++ equivalent. I can still use MSC_VER and __BORLANDC__
 to distinguish them. */
#ifdef _MSDOS
#ifndef __MSDOS__
#define __MSDOS__
#endif
#endif

#ifdef __MSDOS__
#ifndef _MSDOS
#define _MSDOS
#endif
#ifndef MSDOS
#define MSDOS
#endif
#endif

#ifdef _WIN32
#ifndef WIN32
#define WIN32
#endif
#ifndef __WIN32__
#define __WIN32__
#endif
#endif

#ifdef __WIN32__
#ifndef WIN32
#define WIN32
#endif
#ifndef _WIN32
#define _WIN32
#endif
#endif





/* Make the symbols _WINDOWS used by Microsoft Visual C++ and _Windows
 used by Borland C++ equivalent. I can still use MSC_VER and __BORLANDC__
 to distinguish them. */
#if  defined(_Windows)
#if defined(USE_PCNFS) && !defined(NO_DCE_RPC)
#if defined(__cplusplus) || defined(__CPLUSPLUS__)
extern "C" {
#endif
#include <tklib.h>              /* PC-NFS Toolkit */
#if defined(__cplusplus) || defined(__CPLUSPLUS__)
}
#endif
#endif
#ifndef _WINDOWS
#define _WINDOWS
#endif
#endif

#if defined(_WINDOWS)
#if defined(USE_PCNFS) && !defined(NO_DCE_RPC)
#if defined(__cplusplus) || defined(__CPLUSPLUS__)
extern "C" {
#endif
#include <tklib.h>              /* PC-NFS Toolkit */
#if defined(__cplusplus) || defined(__CPLUSPLUS__)
}
#endif
#endif
#ifndef _Windows
#define _Windows
#endif
#ifndef __MSDOS__
#define __MSDOS__ 1
#endif
#endif

#if defined(_Windows) && !defined(WIN32)
#ifndef WIN16
#define WIN16
#endif
#else
#ifdef WIN16
#undef WIN16
#endif
#endif

#if defined(WIN32) || defined(WIN16)
#ifndef _Windows
#define _Windows
#endif
#ifndef WINDOWS
#define WINDOWS
#endif
#ifndef _WINDOWS
#define _WINDOWS
#endif
#ifndef MSDOS
#define MSDOS
#endif
#endif


/* RCS_EXPORT */
/* Use RCS_EXPORT in front of every class or function which */
/* should be exported under windows. */
#ifdef RCSLIB_DYNAMIC
#if defined(_WINDOWS) && !defined(gnuwin32)
#ifdef _MSC_VER
#if (_MSC_VER >= 900)
#define USING_DECLSPEC
#ifdef CREATING_RCSLIB
#define RCS_EXPORT __declspec(dllexport)
#else 
/* CREATING_RCSLIB */
#define RCS_EXPORT __declspec(dllimport)
#endif
/* CREATING_RCSLIB */

#else 
/* (_MSC_VER >= 900) */
#define RCS_EXPORT _export
#endif
/* (_MSC_VER >= 900) */

#else 
/* _MSC_VER */

#define RCS_EXPORT _export
#endif 
/* _MSC_VER */

#else 
/* !defined(_WINDOWS) && !defined(gnuwin32) */

#define RCS_EXPORT
#endif 
/* !defined(_WINDOWS) && !defined(gnuwin32) */

#else
#define RCS_EXPORT
#endif

#ifndef RCS_EXPORT
#define RCS_EXPORT
#endif

/* Use RCS_PASCAL in front of any function definition that needs to */
/*  be passed back to a windows DLL. */
#ifdef WIN16
#define RCS_PASCAL _pascal
#else
#define RCS_PASCAL
#endif

#ifdef WIN16
#define RCS_FAR _far
#define RCS_HUGE _huge
#else
#define RCS_FAR
#define RCS_HUGE
#endif

#define RCS_PTR RCS_FAR
#define RCS_CALLBACK_FUNC RCS_PASCAL


#ifdef WIN32
#define RCS_WINPROC_TYPE __stdcall
#endif
#ifdef WIN16
#define RCS_WINPROC_TYPE RCS_PASCAL
#endif


#if defined(DOS) || defined(WINDOWS)  || defined(WIN32) || defined(WIN16) || defined(_Windows) || defined(_MSC_VER) || defined(_WINDOWS) || defined(__MSDOS__) || defined(_MSDOS) || defined(__BORLANDC__) || defined(__WIN32__) || defined(_WIN32)
#define DOS_WINDOWS
#endif

#if defined(M_I86) || defined(_M_I86) || defined(lynxosPC) || defined(__TURBOC__) || defined(__BORLANDC__)
#define MACHINE_I80x86
#endif

#if defined(M_I86LM) || defined(_M_I86LM) || defined(__LARGE__)
#define LARGE_MEMORY_MODEL
#endif

#if defined(DOS_WINDOWS) && !defined(WIN32) && defined(MACHINE_I80x86) && !defined(LARGE_MEMORY_MODEL)
#error  Dos and 16-bit Windows applications for Intel 80x86 Machines must be compiled with the LARGE memory model to use the RCS Library.
#endif

#if !defined(sunos4) && defined(__cplusplus) && !defined(_MSC_VER) && !defined(qnx)
#define EXTERN_C_STD_HEADERS
#endif


#ifndef PLATNAME
#ifdef DOS_WINDOWS
#ifdef WIN32
#define PLATNAME "WIN32"
#else
#ifdef WIN16
#define PLATNAME "WIN16"
#else
#define PLATNAME "MSDOS"
#endif 
/* !WIN16 */
#endif 
/* !WIN32 */
#endif 
/* !DOS_WINDOWS */
#endif


#ifdef gnuwin32
#define NO_DCE_RPC 1
#endif

#ifdef WIN32
#define NO_DCE_RPC 1
#endif


#if !defined(MSDOS) && !defined(VXWORKS) && !defined(_WINDOWS)
#define UNIX_LIKE_PLAT 1
#endif

#if defined(sunos) || defined(sunos4) || defined(sunos5) || defined(sunos4CC) || defined(sunos5CC)
#ifndef SUN
#define SUN
#endif
#endif

#ifdef WIN32
#if defined(_MT) && !defined(NO_THREADS)
#ifndef MULTITHREADED
#define MULTITHREADED
#endif
#endif
#endif

#ifdef NO_THREADS
#ifdef POSIX_THREADS
#undef POSIX_THREADS
#endif
#endif

#if defined(WIN16) && !defined(WIN32)
#ifndef USE_OLD_WINSOCK
#define USE_OLD_WINSOCK
#endif
#endif

#if  defined(WIN32)
#ifndef MS_WINDOWS_API
#define MS_WINDOWS_API 1
#endif
#ifndef HAVE_TERMINATETHREAD
#define HAVE_TERMINATETHREAD 1
#endif
#ifndef HAVE_GET_CURRENT_PROCESS_ID
#define HAVE_GET_CURRENT_PROCESS_ID 1
#endif
#endif
 
#if defined(linux_2_2_5)  || \
    defined(linux_2_2_10) || \
    defined(linux_2_2_12) || \
    defined(linux_2_2_13) || \
    defined(linux_2_2_14) || \
    defined(linux_2_2_15) || \
    defined(linux_2_2_16) || \
    defined(linux_2_2_17) || \
    defined(linux_2_2_18)
#ifndef LINUX_KERNEL_2_2
#define LINUX_KERNEL_2_2
#endif
#endif

#if defined(linux_2_4_0_test1) || \
    defined(linux_2_4_0_test9) || \
    defined(linux_2_4_0) || \
    defined(linux_2_4_4) || \
    defined(linux_2_4_6) || \
    defined(linux_2_4_7) || \
    defined(linux_2_4_8) || \
    defined(linux_2_4_9) || \
    defined(linux_2_4_10) || \
    defined(linux_2_4_13) || \
    defined(linux_2_4_14) || \
    defined(linux_2_4_16) || \
    defined(linux_2_4_17) || \
    defined(linux_2_4_18) || \
    defined(linux_2_4_19)
#ifndef LINUX_KERNEL_2_4
#define LINUX_KERNEL_2_4
#endif
#endif

#if defined(LINUX) && !defined(linux)
#define linux
#endif

#if defined(linux) && !defined(LINUX)
#define LINUX
#endif


#endif /* HAVE_CONFIG_H */



/* The ifndef was added specicically to allow rcsvers.c to include this. */
#ifndef NO_DBG_MEM
#ifdef DEBUG_MEMORY
#include "dbg_mem.h"            /* DEBUG_MALLOC, DEBUG_FREE */
#else
#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC(x) malloc(x)
#endif
#ifndef DEBUG_FREE
#define DEBUG_FREE(x) free(x)
#endif
#ifndef DEBUG_REALLOC
#define DEBUG_REALLOC(x,y) realloc(x,y)
#endif
#endif
#endif

#ifndef __unused_parameter__
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif

#ifndef __unused_parameter__
#define __unused_parameter__
#endif



#if defined(ADD_STRDUP) && defined(__cplusplus)
#include <stdlib.h>
#include <string.h>
static inline char * rcs_strdup(const char *str)
{
  if(!str)
    {
      return 0;
    }
  int len = strlen(str);
  void *ptr =malloc(len+1);
  strcpy((char *)ptr,str);
  return ((char *)ptr);
}
#define strdup rcs_strdup
#endif

#if _MSC_VER >= 1400
#ifndef HAVE_SNPRINTF_S
#define HAVE_SNPRINTF_S 1
#endif
#endif

#if !defined(SNPRINTF_FUNC) && !defined(SNPRINTF_ARGS)
# if defined(HAVE_SNPRINTF_S) && defined(_TRUNCATE)
#  define SNPRINTF_FUNC _snprintf_s
#  define SNPRINTF_ARGS(x,y) (x),(y),_TRUNCATE
# elif HAVE_SNPRINTF
#  define SNPRINTF_FUNC snprintf
#  define SNPRINTF_ARGS(x,y) (x),(y)
# elif HAVE__SNPRINTF
#  define SNPRINTF_FUNC _snprintf
#  define SNPRINTF_ARGS(x,y) (x),(y)
# else
#  define SNPRINTF_FUNC sprintf
#  define SNPRINTF_ARGS(x,y) (x)
# endif
#endif

#include <string.h>

#if 0
#ifndef strncpy
#if defined(HAVE_STRNCPY_S) && defined(_TRUNCATE) 
#define strncpy(dst,src,count) strncpy_s(dst,count,src,_TRUNCATE)
#else
#define strncpy(dst,src,count) strncpy(dst,src,count-1),((char *)dst)[count-1]=0
#endif
#endif
#endif


#if _MSC_VER >= 1400
#ifndef strdup
#define strdup _strdup
#endif
#endif

/* RCS_DEFS_HH */
#endif








