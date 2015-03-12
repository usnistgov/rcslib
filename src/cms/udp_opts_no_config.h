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

#ifndef UDP_OPTS_NO_CONFIG_H
#define UDP_OPTS_NO_CONFIG_H

// This is neccessary to avoid muliple definitions of fd_set, etc when both
// RPC via PCNFS and Windows Sockets are to be available
#ifdef USE_PCNFS
#undef USE_PCNFS
#endif


#include "rcs_defs.hh"		/* _Windows, EXTERN_C_STD_HEADERS */
#include "sokintrf_no_config.h"

#ifdef UNIX_LIKE_PLAT
#if defined(qnx) || defined(irix6)
#include <fcntl.h>		// fcntl, O_NONBLOCK
#else
#include <sys/fcntl.h>		// fcntl, O_NONBLOCK
#endif
#include <errno.h>		// errno
#include <string.h>		// strerror()
#endif

#ifdef sunos5
#include <sys/types.h>
#include <fcntl.h>
#endif


// UDP_OPTS_NO_CONFIG_H
#endif
