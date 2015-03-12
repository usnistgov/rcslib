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

#ifndef NML_SRV_NO_CONFIG_H
#define NML_SRV_NO_CONFIG_H

#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS  */

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <string.h>		/* memcpy() */

#if defined(WIN32) && !defined(gnuwin32)
#if defined(WIN32) && !defined(USE_OLD_WINSOCK)
// Lame problem if windows.h is included before winsock2.h many redefined
// compiler errors result.
#include <winsock2.h>
#endif

#include <windows.h>		/* GetCurrentProcessId(), GetCurrentThreadID() */
#else
#ifdef VXWORKS
#include <vxWorks.h>
#endif
#include <signal.h>		/* kill() */
#include <sys/types.h>
#include <unistd.h>		/* getpid() */
#include <sys/wait.h>		/* waitpid() */
#include <stdlib.h>		/* atexit() */
#ifdef VXWORKS
#include <taskLib.h>		/* taskIdSelf() */
#endif
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#endif
// NML_SRV_NO_CONFIG_H
