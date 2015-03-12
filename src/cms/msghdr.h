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


#ifndef MSGHDR_H
#define MSGHDR_H

#ifdef HAVE_CONFIG_H

#ifndef RCS_CONFIG_INCLUDE_H
#include "rcs_config_include.h"
#endif

#if !defined(HAVE_CADDR_T_TYPE) && !defined(CADDR_T_DEFINED) && !defined(HAVE__CADDR_T)
typedef char *caddr_t;
#define CADDR_T_DEFINED
#endif

#if !defined(HAVE_STRUCT_IOVEC_TYPE) && !defined(IOVEC_DEFINED)
  struct iovec
  {
    caddr_t iov_base;
    int iov_len;
  };
  typedef struct iovec iovec_t;
#define IOVEC_DEFINED
#endif

#if !defined(HAVE_STRUCT_MSGHDR_TYPE) && !defined(MSGHDR_DEFINED)
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
#define MSGHDR_DEFINED
#endif



#else  
/* HAVE_CONFIG_H  */
#include "msghdr_no_config.h"

/* HAVE_CONFIG_H */
#endif

/* MSGHDR_H */
#endif

