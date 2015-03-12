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

#ifndef MSGHDR_NO_CONFIG_H						  
#define MSGHDR_NO_CONFIG_H						  

#include "rcs_defs.hh"		/* _Windows */

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif
#endif

#if  (!defined(_Windows) && !defined(WIN32)) || defined(USE_PCNFS)

#ifndef MSDOS
#include <sys/types.h>		/* caddr_t */
#ifndef irix5
#ifndef VXWORKS
#include <sys/uio.h>		/* struct iovec */
#define IOVEC_DEFINED

#else
#include <net/uio.h>		/* struct iovec */
#define IOVEC_DEFINED

#endif
#else

#ifndef IOVEC_DEFINED
#define IOVEC_DEFINED
/* /usr/include/sys/uio.h conflicts with
*  /depot/gnu/arch/lib/gcc-lib/mips-sgi-irix5.2/2.6.3/include/unistd.h
* on the definition of readv so we can not use either
*  header file for irix5. */
  typedef struct iovec
  {
    caddr_t iov_base;
    int iov_len;
  }
  iovec_t;
#endif
#endif

#include <sys/socket.h>		/*  struct msghdr */
#define MSGHDR_DEFINED

#else
#include <tklib.h>
#endif
#else
#ifndef CADDR_T_DEFINED
#ifdef gnuwin32
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#define IOVEC_DEFINED
#define CADDR_T_DEFINED
#define MSGHDR_DEFINED
#endif

#define CADDR_T_DEFINED
  typedef char *caddr_t;
#endif
#ifndef IOVEC_DEFINED
#define IOVEC_DEFINED
  struct iovec
  {
    caddr_t iov_base;
    int iov_len;
  };
#endif



#ifndef MSGHDR_DEFINED
#define MSGHDR_DEFINED
/*
 * Message header for recvmsg and sendmsg calls.
 */
  struct msghdr
  {
    caddr_t msg_name;		/* optional address */
    int msg_namelen;		/* size of address */
    struct iovec *msg_iov;	/* scatter/gather array */
    int msg_iovlen;		/* # elements in msg_iov */
    caddr_t msg_accrights;	/* access rights sent/received */
    int msg_accrightslen;
  };
#endif
#endif

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
#ifdef EXTERN_C_STD_HEADERS
}
#endif
#endif

/* MSHHDR_NO_CONFIG_H */
#endif
