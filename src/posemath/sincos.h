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

#ifndef SINCOS_H
#define SINCOS_H

/*
  sincos.h

  support for native sincos functions
*/

/*
  for each platform that has built-in support for the sincos function,
  define SINCOS_SUPPORT here
*/

#ifdef sunos4
#define SINCOS_SUPPORT
#endif

#ifdef VXWORKS
#ifndef POWERPC
#define SINCOS_SUPPORT
#endif
#ifndef POWERPC
#define SINCOS_SUPPORT
#endif
#endif

#if defined (__GLIBC__) && defined(__USE_GNU) && defined(__FAST_MATH__) 
#define SINCOS_SUPPORT
#define sincos __sincos
#endif

/*
  all other platforms will not have SINCOS_SUPPORT defined, and will
  get the declaration for the explicit function
*/

#ifndef SINCOS_SUPPORT

extern void sincos(double x, double *sx, double *cx);

#endif

#endif /* #ifndef SINCOS_H */





