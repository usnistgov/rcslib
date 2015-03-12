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

/* This file contains code that used to be in  cms.cc,
   It is only included by cms.cc.
   It is not used at all if you have the rcs_config.h file generated
   by configure, created using autoconf and friends, so it is used
   only with the old static Makefile system.
   cms.hh has the definitions of functions in cms.cc that others should
   include.


 */

#ifndef CMS_NO_CONFIG_H
#define CMS_NO_CONFIG_H

/* Include Files */
#include "rcs_defs.hh"		/* __MSDOS__, _Windows, EXTERN_C_STD_HEADERS */

#include "rcsvers.hh"		// rcs_version_printed, print_rcs_version()

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		/* malloc(), free() */
#include <stddef.h>		/* size_t */

#include <string.h>		/* strcpy(), strlen(),memcpy() */
  /* strcmp(),strchr() */
#include <ctype.h>		// tolower(), toupper()

#include <errno.h>		/* errno, ERANGE */

#if defined(__MSDOS__) && defined(USE_PCNFS)
/* PC-NFS 5.0 Programmer`s Toolkit Header files */
#include <tklib.h>		/* tkdll_cleanup(), tkdll_init(), */
  /* rtm_install() */
#endif
#include <stdio.h>

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


#ifdef EXTERN_C_STD_HEADERS
}
#endif

#endif
