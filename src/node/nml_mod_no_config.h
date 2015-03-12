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

#ifndef NML_MOD_NO_CONFIG_H
#define NML_MOD_NO_CONFIG_H

#include "rcs_defs.hh"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>
#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy()
#include <errno.h>		// errno
#include <stdarg.h>
#include <math.h>		// fabs()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#if !defined(SNPRINTF_FUNC) && !defined(SNPRINTF_ARGS)
# if defined(HAVE_SNPRINTF_S) && defined(_TRUNCATE)
#  define SNPRINTF_FUNC snprintf_s
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


#endif
//  NML_MOD_NO_CONFIG_H
