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

#ifndef RECVLINE_NO_CONFIG_H
#define RECVLINE_NO_CONFIG_H

/* This is neccessary to avoid muliple definitions of fd_set, etc when both
* RPC via PCNFS and Windows Sockets are to be available. */
#ifdef USE_PCNFS
#undef USE_PCNFS
#endif

#include "rcs_defs.hh"		/* _Windows */

#include "sokintrf_no_config.h"

#if (!defined(_Windows) && !defined(MSDOS)) || defined(gnuwin32)
#include <unistd.h>		/*  select() */
#include <sys/types.h>		/* typedef fd_set, FD_ZERO, FD_SET */
#include <sys/socket.h>		/* recv() */
#ifndef VXWORKS
#include <sys/time.h>		/* struct timeval */
#else
#include <sys/times.h>		/* struct timeval */
#endif
#else
#ifdef _Windows
#ifdef USE_OLD_WINSOCK
#include <winsock.h>		/* select(), typedef fd_set, FD_ZERO, FD_SET, struct */
#else
#include <winsock2.h>
#endif
#else
#include <tklib.h>		/* select(), recv(), typedef fd_set, FD_ZERO, FD_SET */
#include <tk_errno.h>		/* tk_geterrno(), EWOULDBLOCK */
#endif
#endif
#include <stddef.h>		/* size_t */
#include <errno.h>		/* errno */
#include <stdlib.h>		/* malloc(), free() */
#include <string.h>		/* strerror() */
#include <math.h>		/* modf(), fabs() */

#ifdef VXWORKS
#include <taskLib.h>
#endif

#endif
//  RECVLINE_NO_CONFIG_H





