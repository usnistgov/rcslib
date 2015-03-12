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

#ifndef HTTP_SRV_NO_CONFIG_H
#define HTTP_SRV_NO_CONFIG_H

#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */
#include "sokintrf_no_config.h"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>
#include <string.h>		/* memset(), strerror() */
#include <stdlib.h>		// malloc(), free()
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>		/* errno */
#include <signal.h>		// SIGPIPE, signal()

#ifdef USING_VARARGS
#include <varargs.h>		/* va_list, va_start(), va_end() */
#else
#include <stdarg.h>		/* va_list, va_start(), va_end() */
#endif

#if (defined(__CENTERLINE__) && !defined(VXWORKS)) || defined(sunos5) || defined(sparcworks)
#include <sys/filio.h>		/* FIONREAD */
  char *strerror (int errnum);
  char *dl_inet_ntoa (struct in_addr);
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif


#ifdef VXWORKS
#include "vxWorks.h"
#include "taskLib.h"		// taskSpawn
#endif

#ifdef UNIX_LIKE_PLAT
#include <sys/types.h>
#include <sys/wait.h>		// waitpid
#endif

#ifndef NO_THREADS
#ifdef SGI
#include <sys/resource.h>
#include <sys/prctl.h>		// sproc(), prctl()
#endif


#ifdef WIN32
#ifdef MULTITHREADED
#include <process.h>		// _beginthread
#else
#define NO_THREADS
#endif
#endif

#endif

#endif
// #ifndef HTTP_SRV_NO_CONFIG_H
