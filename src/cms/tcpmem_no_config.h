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

/* This file contains code that used to be in  tcpmem.cc,
   It is only included by tcpmem.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   tcpmem.hh has the definitions of functions in tcpmem.cc that others should
   include.


 */


#ifndef TCPMEM_NO_CONFIG_H
#define TCPMEM_NO_CONFIG_H

// This is neccessary to avoid muliple definitions of fd_set, etc when both
// RPC via PCNFS and Windows Sockets are to be available
#ifdef USE_PCNFS
#undef USE_PCNFS
#endif

#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */
#include "sokintrf_no_config.h"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#ifdef VXWORKS
#include "vxWorks.h"
#include "taskLib.h"
#endif

#include <stdlib.h>		// strtol()
#include <string.h>		// strstr()
#include <errno.h>		// errno, strerror()
#include <signal.h>		// signal, SIG_ERR, SIGPIPE
#include <ctype.h>		// isdigit()


#ifndef irix6
#include <math.h>		/* fmod() */
#else
// Work around for the conflict between the gcc includes and /usr/includes
// on some of our SGI's regarding the definition of initstate()
  extern double fmod (double, double);
#endif


#ifdef EXTERN_C_STD_HEADERS
}
#endif

// TCPMEM_NO_CONFIG_H
#endif
