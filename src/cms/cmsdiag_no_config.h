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

/* This file contains code that used to be in  cmsdiag.cc,
   It is only included by cmsdiag.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   cmsdiag.hh has the definitions of functions in cmsdiag.cc that others should
   include.


 */

#ifndef CMSDIAG_NO_CONFIG_H
#define CMSDIAG_NO_CONFIG_H

#ifdef WIN32
#include <windows.h>		// GetCurrentProcessId()
#else
#ifdef VXWORKS
#include <taskLib.h>		/* taskIdSelf() */
#include <sysLib.h>		// sysModel(),sysBspRev()
#else
#include <sys/types.h>
#include <unistd.h>		/* getpid() */
#endif
#endif

#if defined(SUN) &&  !defined(HAVE_SYSINFO)
#define HAVE_SYSINFO 1
#endif

#if defined(WIN32)
#ifndef HAVE_GET_CURRENT_PROCESS_ID
#define HAVE_GET_CURRENT_PROCESS_ID 1
#endif
#endif

#ifdef HAVE_SYSINFO
#include <sys/systeminfo.h>
#endif

#include <stdlib.h>		// memset()
#include <string.h>		// strncpy()
#include <time.h>		// time_t, time()
#include <math.h>		// floor()

#endif

