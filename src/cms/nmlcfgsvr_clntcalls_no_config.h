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

#ifndef NMLCFGSVR_CLNTCALLS_NO_CONFIG_H
#define NMLCFGSVR_CLNTCALLS_NO_CONFIG_H

// Rashmi added
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string.h>

static inline char * rcs_strdup(const char *str)
{
  if(!str)
    {
      return 0;
    }
  int len = strlen(str);
  void *ptr =malloc(len+1);
  strcpy((char *)ptr,str);
  return ((char *)ptr);
}
#define strdup rcs_strdup


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


/*   NMLCFGSVR_CLNTCALLS_NO_CONFIG_H */
#endif

