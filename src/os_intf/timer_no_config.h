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


#ifndef TIMER_NO_CONFIG_H
#define TIMER_NO_CONFIG_H

#include "rcs_defs.hh"		// __MSDOS__
#include "dbg_mem.h"		// DEBUG_MALLOC,DEBUG_FREE

#ifdef VXWORKS

extern "C"
{
#include <vxWorks.h>

#ifndef NO_STDIO
#include <stdio.h>
#endif

#include <string.h>		// strtok(), strncmp()
#include <stdlib.h>		// atof()
#include <taskLib.h>		/* taskDelay() */
#include <tickLib.h>		/* tickGet() */
#include <sysLib.h>		/* sysClkRateGet() */
}

#endif				/* VXWORKS */

#ifdef LYNX

extern "C"
{
#include <string.h>		// strtok(), strncmp()
#include <stdlib.h>		// atof()

#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

#include <stdlib.h>		/* exit() */
#include <signal.h>		/* struct sigaction, sigaction(), SIGALRM,
				   sigset_t */
#include <errno.h>		/* perror(), EINTR */
#include <unistd.h>		/* select() */
#include <time.h>		/* CLK_TCK, since no _SC_CLK_TCK, */
  /* setitimer() */
#include <sys/time.h>		/* struct timeval, gettimeofday(),
				   struct itimerval,
				   ITIMER_REAL */

}

#endif				/* LYNX */

#if defined(SUN) || defined(LINUX)

extern "C"
{
#include <stdlib.h>		// atof()
#include <string.h>		// strtok(), strncmp()

#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

#include <stdlib.h>		/* exit() */
#include <signal.h>		/* struct sigaction, sigaction(), SIGALRM,
				   sigset_t */
#include <errno.h>		/* perror(), EINTR */
#include <unistd.h>		/* select(), sysconf(), _SC_CLK_TCK */
#include <sys/time.h>		/* struct timeval, gettimeofday(),
				   struct itimerval, setitimer(),
				   ITIMER_REAL */
#include <sys/types.h>
#include <sys/wait.h>		// waitpid()
}

#endif				/* SUN */

#ifdef __MSDOS__
#include <stdlib.h>		// atof()
#include <string.h>		// strtok(), strncmp()
#include <time.h>		/* clock(), CLK_TCK */
#ifndef _WINDOWS
#include <dos.h>		/* delay() */
#endif
#endif

#endif
// #ifndef TIMER_NO_CONFIG_H
