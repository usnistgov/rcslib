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


#ifndef SENDMSGT_NO_CONFIG_H
#define SENDMSGT_NO_CONFIG_H

#include "rcs_defs.hh"		/* MSDOS, _Windows */
#include "sokintrf_no_config.h"

/* This is neccessary to avoid muliple definitions of fd_set, etc when both
* RPC via PCNFS and Windows Sockets are to be available. */
#ifdef USE_PCNFS
#undef USE_PCNFS
#endif

#include <sys/types.h>		/* u_char etc needed by sys/socket.h */
				/* fd_set, FD_ZERO, FD_SET */

#if !defined(DOS_WINDOWS) || defined(gnuwin32)
#include <sys/socket.h>		/* stuct msghdr, sendmsg() */
#ifndef VXWORKS
#include <sys/time.h>		/* struct timeval */
#else
#include <sys/times.h>		/* struct timeval */
#include <sockLib.h>		/* sendmsg() */
#endif
#ifndef _Windows
#include <unistd.h>		/* select() */
#endif
#else
#if defined(MSDOS) && !defined(WINDOWS)
#include <tklib.h>
#include <sys/socket.h>		/* stuct msghdr, sendmsg() */
#include <sys/uio.h>		/* stuct iovec */
#else
#ifdef USE_OLD_WINSOCK
#include <winsock.h>		/* select(), typedef fd_set, FD_ZERO, FD_SET, struct */
#else
#include <winsock2.h>
#endif

#endif
#endif
#ifndef irix6
#include <math.h>		/* fmod() */
#else
/*
 Work around for the conflict between the gcc includes and /usr/includes
 on some of our SGI's regarding the definition of initstate()
*/
extern double fmod (double, double);
#endif

#include <errno.h>		/* errno variable */
#include <string.h>		/* strerror() */
#include <stdlib.h>		/* DEBUG_MALLOC(), free() */


#endif
// SENDMSGT_NO_CONFIG_H
