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

#ifndef UNIX_SEM_NO_CONFIG_H
#define UNIX_SEM_NO_CONFIG_H

#ifndef NO_STDIO
#include <stdio.h>
#endif

#include <stdlib.h>
#include <errno.h>		/* errno */
#include <string.h>		/* strerror() */
#include <unistd.h>

#ifdef __CENTERLINE__
#ifndef USING_VARARGS
#define USING_VARARGS
#endif
#include <varargs.h>		/* va_list, va_arg(), va_start(), va_end() */
#else
#include <stdarg.h>		/* va_list, va_arg(), va_start(), va_end() */
#endif
#include <sys/types.h>
#include <sys/ipc.h>		/* IPC_CREATE, IPC_NOWAIT */

/* There are two types of posix semaphores named and unnamed.
  unamed semaphores can either have the pshared flag set or not
 determining whether it can be shared between processes.  Currently (12/27/02),
Linux 
implements only unnamed posix semaphores that are not shared between
processes. This is useless to RCSLIB so on Linux System V semaphores
will be used instead.
*/

#if defined(darwin) || defined(qnx)
#include <semaphore.h>
#define POSIX_SEMAPHORES
#else
#include <sys/sem.h>		/* struct sembuf */
#endif

#ifdef qnx
#include <fcntl.h>
#endif

#ifndef irix6
#include <math.h>		/* fmod() */
#else /* irix6 */
/*
 Work around for the conflict between the gcc includes and /usr/includes
 on some of our SGI's regarding the definition of initstate()
 */
extern double fmod (double, double);
#endif /* irix6 */


#if defined(sunos5) || defined(linux) || defined(POSIX_SEMAPHORES)
#define USE_ITIMER_SIGNALS
#endif

#ifdef USE_ITIMER_SIGNALS
#include <signal.h>
#include <sys/time.h>
#endif /* USE_TIMER_SIGNALS */

/* UNIX_SEM_NO_CONFIG_H */
#endif 
