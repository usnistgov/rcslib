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

/* This file contains code that used to be in  nml.cc,
   It is only included by nml.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   nml.hh has the definitions of functions in nml.cc that others should
   include.


 */

#ifndef NML_NO_CONFIG_H
#define NML_NO_CONFIG_H

#ifndef HAVE_CONFIG_H

/* Include Files */
#include "rcs_defs.hh"		/* _Windows, RCS_FAR, EXTERN_C_STD_HEADERS */

#include <stdio.h>
#include <errno.h>

#ifdef _Windows
#if defined(WIN32) && !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif
#include <windows.h>		/* GetCurrentTask() */
#include <direct.h>		// _getcwd()
#endif

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <string.h>		/* memcpy() */
#include <stdlib.h>		/* atexit() */
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>


#ifdef VXWORKS
#include <taskLib.h>		/* taskIdSelf() */
#endif

#if defined(sunos5) || defined(SGI) || defined(linux)
#include <sys/param.h>		// MAXHOSTNAMELEN
#endif


#ifdef sunos5
#include <thread.h>		// thr_exit
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif



#if !defined(NO_VSNPRINTF) && !defined(HAVE_VSNPRINTF) && defined(linux)
#define HAVE_VSNPRINTF 1
#endif

#if !defined(NO_SNPRINTF) && !defined(HAVE_SNPRINTF) && defined(linux)
#define HAVE_SNPRINTF 1
#endif

#ifdef POSIX_THREADS
#include <pthread.h>
#endif

// end of ifndef HAVE_CONFIG_H
#endif

#endif
