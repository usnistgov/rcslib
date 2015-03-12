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

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

/*
   sincos.c

   Modification history:

   18-Dec-2001  FMP reworked with better def'ing
   30-Jan-1997  FMP added #ifdef __CPLUSPLUS__ around math.h include
   29-Jan-1997  FMP changed added PLATFORM_WITH_ to make more obvious
*/

#ifndef HAVE_SINCOS

#include "sincos.h"

#ifndef SINCOS_SUPPORT

#include <math.h>

void sincos(double x, double *sx, double *cx)
{
  *sx = sin(x);
  *cx = cos(x);
}
#else
void sincos_c_not_empty(void);

void sincos_c_not_empty(void)
{
}
#endif

#else
void sincos_c_not_empty2(void);

void sincos_c_not_empty2(void)
{
}
#endif



