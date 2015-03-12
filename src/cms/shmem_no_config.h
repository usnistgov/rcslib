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

#ifndef SHMEM_NO_CONFIG_H
#define SHMEM_NO_CONFIG_H

/* Include Files */
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS  */

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>		/* NULL */
#include <stddef.h>		/* size_t */

#ifndef _WINDOWS
#include <sys/types.h>		/* key_t */
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifndef KEY_T_DEFINED

#ifdef VXWORKS
typedef int key_t;		/* key_t is not defined in VxWorks or  Windows -- do
				   it here, and it should be visible
				   throughout all the shared mem code */
#endif

#ifdef WIN32
typedef long key_t;
#endif

#endif

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif


#include <stdio.h>		/* sscanf() */
#include <stddef.h>		/* size_t */
#include <sys/stat.h>		/* S_IRUSR, etc. */
#include <sys/types.h>		/* key_t */
#include <errno.h>		// errno
#include <string.h>		/* strchr(), memcpy(), memset() */
#include <stdlib.h>		/* strtod */

#ifdef VXWORKS
#include <taskLib.h>
#include <intLib.h>		// intLock()
#endif

#ifdef lynxosPC
#include <fa.h>			// struct fa_info,  fast_enable_preemption()
#include <kernel.h>		// disable(), sdisable(),restore(),srestore()
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef VXWORKS
typedef int key_t;
#endif

#ifdef lynxosPC
struct fa_info *fa_ptr = NULL;
#endif

// SHMEM_NO_CONFIG_H
#endif
