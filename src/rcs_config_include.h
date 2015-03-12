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



#ifndef RCS_CONFIG_INCLUDE_H
#define RCS_CONFIG_INCLUDE_H

#ifdef RCS_HH
#error This file should never be included in a source that also includes rcs.hh.
#endif

#ifndef HAVE_CONFIG_H
#error This file can not be included unless HAVE_CONFIG_H is defined.
#endif

#if !defined(VERSION) && !defined(BUILD_PLATFORM)
#error rcs_config.h should have been included before this and it should have defined VERSION and BUILD_PLATFORM
#endif 

#ifdef MS_WINDOWS_API 

#include "rcs_config_win.h"

/* MS_WINDOWS_API */
#endif

#ifdef SET_POSIX_SOURCE_FLAG
#define _POSIX_SOURCE 1
#define _XOPEN_SOURCE 500
#define __EXTENSIONS__ 1
#endif

#ifndef __NO_INCLUDE_WARN__
# define __NO_INCLUDE_WARN__ 1
# define RCS_CONFIG_H_UNDEF_NO_INCLUDE_WARN 1
#endif

#ifdef STDC_HEADERS
# include <stdarg.h>
# include <stdio.h>
# include <errno.h>
# include <stdlib.h>
# include <stddef.h>
				
/* STDC_HEADERS  */
#else
# if HAVE_STDIO_H
#  include <stdio.h>
# endif
# if HAVE_ERRNO_H
#  include <errno.h>
# elif HAVE_SYS_ERRNO_H
#  include <sys/errno.h>
# endif
# if HAVE_STDARG_H
#   include <stdarg.h>
# endif
# if HAVE_STDLIB_H
#   include <stdlib.h>
# endif
# if HAVE_STDDEF_H
#   include <stddef.h>
# endif

/* STDC_HEADERS  */
#endif


#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_MATH_H
# include <math.h>
#endif

#ifdef HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif

#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif

#ifdef HAVE_CTYPE_H
# include <ctype.h>
#endif

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  if HAVE_TIME_H
#   include <time.h>
#  endif
# endif
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#ifdef HAVE_VALUES_H
# include <values.h>
#endif

#ifdef HAVE_FLOAT_H
# include <float.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# if HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
# endif
#endif

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
# include <sys/systeminfo.h>
#endif

#ifdef POSIX_THREADS
# if HAVE_PTHREAD_H
#  include <pthread.h>
# endif
#else
# ifndef MS_WINDOWS_API
#  define NO_THREADS 1
# endif

/* POSIX_THREADS */
#endif

#ifndef MS_WINDOWS_API 

#ifdef HAVE_IPC_H
# include <ipc.h>
#elif defined(HAVE_SYS_IPC_H)
# include <sys/ipc.h>
#endif

#if defined(HAVE_SEM_H) && defined(UNIX_SEM_SOURCE)
# include <sem.h>
#elif defined(HAVE_SYS_SEM_H) && defined(UNIX_SEM_SOURCE)
# include <sys/sem.h>
#endif

#if defined(HAVE_SHM_H)
# include <shm.h>
#elif defined(HAVE_SYS_SHM_H)
# include <sys/shm.h>
#endif

#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>
#endif

#if defined(HAVE_UIO_H)
# include <uio.h>
#elif defined(HAVE_SYS_UIO_H)
# include <sys/uio.h>
#elif defined(HAVE_NET_UIO_H)
# include <net/uio.h>
#endif

#if defined(CMS_NETWORK_SOURCE)

# if defined(HAVE_SOCKET_H)
#  include <socket.h>
# elif defined(HAVE_SYS_SOCKET_H)
#  include <sys/socket.h>
# endif

# ifdef HAVE_NETDB_H
#  include <netdb.h>
# endif

# ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
# endif

# ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
# endif

# ifdef HAVE_ARPA_INET_H
#  include <arpa/inet.h>
# endif

#endif

#ifdef HAVE_STANDARDS_H
# include <standards.h>
#endif

#if defined(HAVE_NETINET_TCP_H) || defined(HAVE_NETINET_TCP_H_2)
# include <netinet/tcp.h>
#endif


#ifdef HAVE_FILIO_H
# include <filio.h>
#elif defined(HAVE_SYS_FILIO_H)
# include <sys/filio.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#elif defined(HAVE_IOCTL_H)
# include <ioctl.h>
#endif

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#ifdef POSIX_SEMAPHORES
# if HAVE_SEMAPHORE_H  && UNIX_SEM_SOURCE
#  include <semaphore.h>
# endif
#endif

/* ! MS_WINDOWS_API */
#endif


#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif

#ifdef HAVE_SETITIMER
# define USE_ITIMER_SIGNALS 1
#endif

#if defined(ENABLE_RCS_XDR) && defined(XDR_SOURCE)

# if defined(HAVE_RPC_RPC_H)
#  include <rpc/rpc.h>		/* struct XDR */
# else
#  if HAVE_RPC_TYPES_H
#   include <rpc/types.h>
#  endif
#  if HAVE_RPC_XDR_H
#   include <rpc/xdr.h>		/* struct XDR */
#  else
#   if HAVE_XDR_H
#    include "xdr.h"
#   endif

/* if HAVE_RPC_XDR_H */
#  endif

/* if HAVE_RPC_RPC_H */
# endif

/* defined(ENABLE_RCS_XDR) && defined(XDR_SOURCE) */
#endif


#if !defined(ENABLE_RCS_PRNT) && !defined(ENABLE_RCS_PRINT)  && !defined(DISABLE_RCS_PRINT)
# define DISABLE_RCS_PRINT
#endif

#ifndef __unused_parameter__
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif

#ifndef __unused_parameter__
#define __unused_parameter__
#endif

#if !defined(__GNUC__) && !defined(__attribute__)
#define __attribute__(x)
#endif

/* Apple's have snprintf but it is wrapped in macros so insane, that it can't be used. */
#ifdef __APPLE__
#undef HAVE_SNPRINTF
#undef SNPRINTF_OK_IN_CPLUSPLUS
#endif

#if defined(__cplusplus) && !defined(SNPRINTF_OK_IN_CPLUSPLUS) && defined(HAVE_SNPRINTF)
# undef HAVE_SNPRINTF
#endif

#if defined(__cplusplus) && !defined(PUTENV_OK_IN_CPLUSPLUS) && defined(HAVE_PUTENV)
# undef HAVE_PUTENV
#endif

#if defined(__cplusplus) && !defined(SNPRINTF_OK_IN_CPLUSPLUS) && defined(HAVE__SNPRINTF)
# undef HAVE__SNPRINTF
#endif

#if !defined(SNPRINTF_FUNC) && !defined(SNPRINTF_ARGS)
# if defined(HAVE_SNPRINTF_S) && defined(_TRUNCATE)
#  define SNPRINTF_FUNC _snprintf_s
#  define SNPRINTF_ARGS(x,y) (x),(y),_TRUNCATE
# elif HAVE_SNPRINTF
#  define SNPRINTF_FUNC snprintf
#  define SNPRINTF_ARGS(x,y) (x),(y)
# elif HAVE__SNPRINTF
#  define SNPRINTF_FUNC _snprintf
#  define SNPRINTF_ARGS(x,y) (x),(y)
# else
#  define SNPRINTF_FUNC sprintf
#  define SNPRINTF_ARGS(x,y) (x)
# endif
#endif

#if defined(__NO_INCLUDE_WARN__) && defined(RCS_CONFIG_H_UNDEF_NO_INCLUDE_WARN)
# undef __NO_INCLUDE_WARN__
# undef RCS_CONFIG_H_UNDEF_NO_INCLUDE_WARN
#endif

/* Microsoft Visual C++ 2005, does not support several 
standard POSIX functions that previous versions did without
an extra _ in front. This code was developed/tested with
Rashmi Patel of GDRS. */
#if defined(WIN32) && (_MSC_VER >= 1400) 

#pragma warning(disable:4996) 
#define INLINE __forceinline 

static INLINE int open(const char *pathname, int flags, int mode) {
return _open(pathname, flags, mode); }

static INLINE int close(int fd) { return _close(fd); }

static INLINE int write(int fd, const void *buf, unsigned int count) {
return _write(fd, buf, count); }

//static INLINE char *strdup(const char *s) { return _strdup(s); } 

#endif
/* end of -- #if defined(WIN32) && (_MSC_VER >= 1400)  */

#if 0
#ifndef strncpy
#if defined(HAVE_STRNCPY_S) && defined(_TRUNCATE) 
#define strncpy(dst,src,count) strncpy_s(dst,count,src,_TRUNCATE)
#else
#define strncpy(dst,src,count) strncpy(dst,src,count-1),((char *)dst)[count-1]=0
#endif
#endif
#endif


#if _MSC_VER >= 1400
#ifndef strdup
#define strdup _strdup
#endif
#endif


/* RCS_CONFIG_INCLUDE_H */
#endif 










