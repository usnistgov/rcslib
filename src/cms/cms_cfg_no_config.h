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

/* This file contains code that used to be in  cms_cfg.cc,
   It is only included by cms_cfg.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   cms_cfg.hh has the definitions of functions in cms_cfg.cc that others should
   include.


 */
#ifndef CMS_CFG_NO_CONFIG_H
#define CMS_CFG_NO_CONFIG_H

/* Include Files */
#include "rcs_defs.hh"		/* __MSDOS__, _Windows, EXTERN_C_STD_HEADERS */
#include "sokintrf_no_config.h"	

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>		/* sscanf(), NULL  */
  /* FILE, fopen(), fgets() */
#include <string.h>		/* strcpy(), strlen(),memcpy() */
  /* strcmp(),strchr() */

#include <errno.h>		// errno
#include <ctype.h>		// toupper(), tolower()

#ifdef UNIX_LIKE_PLAT
#include <unistd.h>		// gethostname()
#endif


#ifdef EXTERN_C_STD_HEADERS
}
#endif

#if (!defined(MSDOS) || defined(WIN32)) && !defined(linux) && !defined(VXWORKS)
#define ENABLE_RCS_INET_FILES
#ifdef ENABLE_RCS_INET_FILES
#include "inetfile.hh"		// internet file reading functions inet_file_*
#endif
#endif

#ifndef NO_DCE_RPC
#define NO_DCE_RPC
#endif

#if defined(DARWIN) ||  ( defined(__MSDOS__) && !defined(WIN32) )
#ifdef ENABLE_RCS_TTY
#undef ENABLE_RCS_TTY
#endif
#endif

#if defined(VXWORKS) || defined(USE_BIT3)
#ifndef ENABLE_RCS_GLOBMEM
#define ENABLE_RCS_GLOBMEM 1
#endif
#else
#ifdef ENABLE_RCS_GLOBMEM
#undef ENABLE_RCS_GLOBMEM
#endif
/* defined(VXWORKS) || defined(USE_BIT3) */
#endif


#if defined(VXWORKS)
#else
/* defined(VXWORKS) || defined(USE_BIT3) */
#endif

#if !defined(linux) || ( defined(NO_RTL) && !defined(HAVE_RTAI) )
#ifdef ENABLE_RCS_RTLMEM
#undef ENABLE_RCS_RTLMEM
#endif
#endif

#include "sokintrf.h"

/* CMS_CFG_NO_CONFIG_H */
#endif


