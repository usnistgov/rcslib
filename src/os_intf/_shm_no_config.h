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

#ifndef _SHM_NO_CONFIG_H
#define _SHM_NO_CONFIG_H

#include "rcs_defs.hh"

#if MS_WINDOWS_API
#if !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif

#include <windows.h>
#endif

/* Purely local shared memory functions-- converting System V shared
   memory to POSIX shared memory required something intermediate in
   System V IPC that returned a file descriptor suitable for mmap(),
   and that something doesn't exist. */

#ifndef MS_WINDOWS_API
#include <sys/types.h>		/* key_t */
#endif

#include <stddef.h>		/* size_t */

#ifndef USING_VARARGS
#include <stdarg.h>
#endif

#ifndef KEY_T_DEFINED
#define KEY_T_DEFINED
#if defined(VXWORKS)
typedef int key_t;		/* key_t is not defined in VxWorks or  Windows -- do
				   it here, and it should be visible
				   throughout all the shared mem code */
#endif

#if MS_WINDOWS_API
typedef long key_t;
#endif
#endif /* KEY_T_DEFINED */

/* _SHM_NO_CONFIG_H */
#endif
