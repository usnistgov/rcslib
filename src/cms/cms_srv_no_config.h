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

#ifndef CMS_SRV_NO_CONFIG_H
#define CMS_SRV_NO_CONFIG_H

#include "rcs_defs.hh"
#include "sokintrf_no_config.h"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>		/* sscanf(),NULL */
  /* FILE, fopen(), fgets() */
#include <string.h>		/* strchr(), memcpy() */
#include <stdlib.h>		/* malloc(), free(), exit() */
#include <ctype.h>		// isgraph()
#ifndef irix6
#include <math.h>		/* fmod() */
#else
// Work around for the conflict between the gcc includes and /usr/includes
// on some of our SGI's regarding the definition of initstate()
  extern double fmod (double, double);
#endif

#if defined(WIN32)
#if defined(WIN32) && !defined(USE_OLD_WINSOCK)
// Lame problem if windows.h is included before winsock2.h many redefined
// compiler errors result.
#include <winsock2.h>
#endif
#include <windows.h>		/* InitializeSecurityDescriptor(), CreateThread() */
#ifndef HAVE_SET_CONSOLE_CTRL_HANDLER
#define HAVE_SET_CONSOLE_CTRL_HANDLER 1
#endif
#ifndef HAVE_GET_CURRENT_PROCESS_ID
#define HAVE_GET_CURRENT_PROCESS_ID
#endif
#else

#ifdef VXWORKS
#include <taskLib.h>		/* taskLock(), taskUnlock(), taskIdSelf() */
#else
#include <sys/types.h>
#include <unistd.h>		/* getpid() */
#include <sys/wait.h>		/* waitpid() */
#endif
#endif

#include <signal.h>		/* sigvec(), struct sigvec,  SIGINT */
  /* kill() */

#ifdef WIN32
#ifdef MULTITHREADED
#include <process.h>		// _beginthread
#endif
#endif


#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef VXWORKS
extern int cms_server_task_priority;
extern int cms_server_task_stack_size;
#endif

#ifdef VXWORKS
#include <semLib.h>
extern SEM_ID cms_server_list_mutex;
#endif

#ifndef NO_DCE_RPC
#define NO_DCE_RPC
#endif

#if defined(VXWORKS) || (defined(__MSDOS__) && !defined(MS_WINDOWS_API))  || defined(DARWIN) || defined(qnx)
#ifdef ENABLE_RCS_TTY
#undef ENABLE_RCS_TTY
#endif
#endif

// CMS_SRV_NO_CONFIG_H
#endif
