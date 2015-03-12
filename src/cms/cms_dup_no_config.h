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

#ifndef CMS_DUP_NO_CONFIG_H
#define CMS_DUP_NO_CONFIG_H

#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <errno.h>		/* errno global variable */
#include <limits.h>		/* SHORT_MIN, SHOR_MAX, . . . */
#include <float.h>		/* FLT_MIN, FLT_MAX */
#include <string.h>		/* memcpy(), strerror() */
#include <stdlib.h>		/* strtol(), strtoul(), strtod() */
#ifdef CenterLine		// CenterLine headers messed up the defintion of these functions
  char *strerror (int errnum);
  unsigned long strtoul (const char *, char **, int);
#endif
#include <stdio.h>		/* sprintf() */
#include <ctype.h>		/* isspace() */

#ifdef EXTERN_C_STD_HEADERS
}
#endif

//  CMS_DUP_NO_CONFIG_H
#endif
