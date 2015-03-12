
// Rashmi - added this file to nmlcfgsvr.cc would compile


#ifndef NMLCFGSVR_NO_CONFIG_H
#define NMLCFGSVR_NO_CONFIG_H


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



#if !defined(SNPRINTF_FUNC) && !defined(SNPRINTF_ARGS)
#if HAVE_SNPRINTF
#define SNPRINTF_FUNC snprintf
#define SNPRINTF_ARGS(x,y) (x),(y)
#elif HAVE__SNPRINTF
#define SNPRINTF_FUNC _snprintf
#define SNPRINTF_ARGS(x,y) (x),(y)
#else
#define SNPRINTF_FUNC sprintf
#define SNPRINTF_ARGS(x,y) (x)
#endif
#endif

#ifndef strdup
static inline char * rcs_strdup(const char *str)
{
  if(!str)
    {
      return 0;
    }
  int len = (int) strlen(str);
  void *ptr =malloc(len+1);
  strcpy((char *)ptr,str);
  return ((char *)ptr);
}
#define strdup rcs_strdup
#endif

#endif
