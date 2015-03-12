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

/* This file contains code that used to be in  sokintrf.cc,
   It is only included by sokintrf.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   sokintrf.hh has the definitions of functions in sokintrf.cc that others should
   include.


 */



#ifndef SOKINTRF_NO_CONFIG_H
#define SOKINTRF_NO_CONFIG_H

#include "rcs_defs.hh"		/* _WINDOWS, EXTERN_C_STD_HEADERS */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>		/* errno */

#ifdef VXWORKS
#include <vxWorks.h>		/* struct fd_set */
#define FD_SET_DEFINED
#ifndef NO_NETINET_IN_H
#include <netinet/in.h>		/* struct in_addr, sockaddr_in */
#endif
#define SOCKADDR_IN_DEFINED
#define IN_ADDR_DEFINED
#include <sys/times.h>		/* struct timeval */
#define TIMEVAL_DEFINED
#include <sys/socket.h>		/* struct sockaddr, msghdr */
#define SOCKADDR_DEFINED
#define MSGHDR_DEFINED
#include <ioLib.h>		/* FIONREAD */
#include <net/uio.h>		/* struct iovec */
#define IOVEC_DEFINED
#include <hostLib.h>		/* hostGetByName() */
#include <sockLib.h>		/* miscelaneous Socket stuff */
#include <ioLib.h>		/* miscellaneous IO stuff  */
#endif

#ifdef UNIX_LIKE_PLAT
#include <sys/types.h>		/* struct fd_set */
#define FD_SET_DEFINED
#include <sys/socket.h>		/* struct sockaddr, msghdr */
#define SOCKADDR_DEFINED
#define MSGHDR_DEFINED
#ifdef sunos5
#include <sys/select.h>		/* make sure NBBY defined before in.h */
  /* (needed for Solaris 2.6) */
#endif
#include <netinet/in.h>		/* struct in_addr, sockaddr_in */
#define IN_ADDR_DEFINED
#define SOCKADDR_IN_DEFINED
#include <netdb.h>		/* struct hostent */
#define HOSTENT_DEFINED
#include <sys/time.h>		/* struct timeval */
#define TIMEVAL_DEFINED
#include <sys/uio.h>		/* struct iovec */
#define IOVEC_DEFINED
#include <sys/ioctl.h>		/* FIONREAD */
#include <unistd.h>		/* close() */

#endif


#ifdef _WINDOWS
#ifndef gnuwin32
#ifdef USE_OLD_WINSOCK
#include <winsock.h>		/* select(), typedef fd_set, FD_ZERO, FD_SET, struct */
#else
#include <winsock2.h>
#define USE_WINSOCK 1

#endif

#define SOCKET_DEFINED
#define IN_ADDR_DEFINED
#define SOCKADDR_IN_DEFINED
#define SOCKADDR_DEFINED
#define TIMEVAL_DEFINED
#define FD_SET_DEFINED
#define HOSTENT_DEFINED
#else
#include <sys/types.h>		/* struct fd_set */
#define FD_SET_DEFINED
#include <sys/socket.h>		/* struct sockaddr, msghdr */
#define SOCKADDR_DEFINED
#define MSGHDR_DEFINED
#include <netinet/in.h>		/* struct in_addr, sockaddr_in */
#define IN_ADDR_DEFINED
#define SOCKADDR_IN_DEFINED
#include <netdb.h>		/* struct hostent */
#define HOSTENT_DEFINED
#include <sys/time.h>		/* struct timeval */
#define TIMEVAL_DEFINED
#include <sys/uio.h>		/* struct iovec */
#define IOVEC_DEFINED
#include <sys/ioctl.h>		/* FIONREAD */
#include <unistd.h>		/* close() */
#endif				/* gnuwin32 */

#endif				/* _WINDOWS */


#ifdef SUN
#include <sys/filio.h>		/* FIONREAD */
#endif

#include <signal.h>

#ifndef MSGHDR_DEFINED
#include "msghdr.h"
#endif

#ifdef VXWORKS
#include <arpa/inet.h>
#endif

#ifdef linux
#include <arpa/inet.h>
#endif

#ifdef sunos5
#include <arpa/inet.h>
#endif

#endif
// #ifndef SOKINTRF_NO_CONFIG_H










