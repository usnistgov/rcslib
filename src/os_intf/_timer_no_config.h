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


#ifndef _TIMER_NO_CONFIG_H
#define _TIMER_NO_CONFIG_H

#define NO_SCCS_IDS

#include "rcs_defs.hh"

#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

#include <errno.h>		/* errno */
#include <string.h>		/* strerror() */

#ifdef VXWORKS

#include <vxWorks.h>
#include <taskLib.h>		/* taskDelay() */
#include <tickLib.h>		/* tickGet() */
#include <sysLib.h>		/* sysClkRateGet() */
#include <timers.h>		/* clock_gettime */

#endif /* VXWORKS */

#ifdef LYNX


#include <errno.h>		/* EINTR */
#include <unistd.h>		/* select() */
#include <time.h>		/* CLK_TCK, since no _SC_CLK_TCK */
#include <sys/time.h>		/* struct timeval, gettimeofday(),
				   struct itimerval, setitimer(),
				   ITIMER_REAL */


#endif /* LYNX */

#if  defined(SUN) || defined(SGI) || defined(sparcworks) || defined(darwin) || defined(qnx) || defined(linux)
#include <errno.h>		/*  EINTR */
#include <unistd.h>		/* select(), sysconf(), _SC_CLK_TCK */
#ifndef irix6
#include <sys/time.h>		/* struct timeval, gettimeofday(),
				   struct itimerval, setitimer(),
				   ITIMER_REAL */
#endif

#endif /* SUN */

#ifdef linux
#include <linux/version.h>

#if defined(LINUX_VERSION_CODE) && defined(KERNEL_VERSION)
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,2,0)
#define LINUX_KERNEL_2_2_OR_LATER
#include <sched.h>
#endif
#endif
#endif



#ifdef irix6
#include <time.h>
#endif

#ifdef __MSDOS__
#ifdef _Windows
#ifdef USE_TOOL_HELP
/* The timerCount function is more accurate than the GetTickCount() function
but you need toolhelp.dll which comes with Windows 3.1 but not Windows 3.0 or
Windows NT */
#include "toolhelp.h"		/* timerCount() */
#else
#if defined(WIN32) && !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif
#include <windows.h>		/* GetTickCount() */
#endif
#endif
#include <time.h>		/* clock(), CLK_TCK */
#ifndef _Windows
#include <dos.h>		/* delay() */
#endif
#endif

#if defined(mingw32)
#include <windows.h>
#endif

// _TIMER_NO_CONFIG_H
#endif
