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

#ifndef RCS_PRNT_NO_CONFIG_H
#define RCS_PRNT_NO_CONFIG_H

#include "rcs_defs.hh"		/* _Windows, RCS_FAR, EXTERN_C_STD_HEADERS */

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE
#else
#define DEBUG_MALLOC(x) malloc(x)
#define DEBUG_FREE(x) free(x)
#endif

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#ifdef VXWORKS
#include "vxWorks.h"
#endif


#ifdef USING_VARARGS
#include <varargs.h>		/* va_list, va_start(), va_end() */
#else
#include <stdarg.h>		/* va_list, va_start(), va_end() */
#endif

#ifndef NO_STDIO
#include <stdio.h>		/* __printf()'s */
#endif

#include <string.h>		/* strchr(), memmove() */
#include <stdlib.h>		/* malloc(), free(), realloc() */
#include <errno.h>		// errno()
#ifdef VXWORKS
#include <logLib.h>		/* logMsg() */
#endif


#ifdef VXWORKS
#include <taskLib.h>		/* taskIdSelf() */
#else
#include <sys/types.h>
#ifdef UNIX_LIKE_PLAT
#include <unistd.h>		/* getpid() */
#endif
#endif

#include <ctype.h>

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef MS_WINDOWS_API
#include <windows.h>		/* MessageBox() */

#if defined(WIN32) && !defined(gnuwin32) && !defined(mingw32) && !defined(_WINDLL)
#ifndef HAVE_ALLOC_CONSOLE
#define HAVE_ALLOC_CONSOLE 1
#endif
#endif

/* MS_WINDOWS_API */
#endif

/* RCS_PRNT_NO_CONFIG_H */
#endif
