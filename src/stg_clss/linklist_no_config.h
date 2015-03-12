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


#ifndef LINKLIST_NO_CONFIG_H
#define LINKLIST_NO_CONFIG_H

#ifndef DO_NOT_USE_RCSLIB
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */

#else

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"

#else

#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC(x) malloc(x)
#endif

#ifndef DEBUG_FREE
#define DEBUG_FREE(x) free(x)
#endif

#endif

// DO_NOT_USE_RCSLIB
#endif


#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		/* malloc() */
#include <string.h>		/* memcpy() */
#ifndef NO_STDIO
#include <stdio.h>		/* fprintf(), stderr */
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif


/* LINKLIST_NO_CONFIG_H */
#endif
