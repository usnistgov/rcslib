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

/* Prototypes for math printing functions */

/*
   Modification history:

    9-Jan-1998 WPS forces VxWorks to use C function calls io streams are
    not available.
  16-May-1997 WPS added #ifdef __cplusplus #define __CPLUSPLUS__
  all ANSI C++ compilers define __cplusplus automatically so it
  makes more sense to use than __CPLUSPLUS__ which needs to be
  defined separately.
  14-Apr-1997  FMP took out legacy math_print stuff
   10-Feb-1997  FMP added C++ stuff
   4-Nov-1996  Fred Proctor added math_printError()
*/

#ifndef MATHPRNT_H
#define MATHPRNT_H

#include "posemath.h"

#if defined(__cplusplus) && !defined(VXWORKS) && !defined(gnuwin32) && !defined(NO_IOSTREAM)

/* If we have generated config.h all .cc files that include this header must
 * first include either <iostream.h> or <iostream> */

extern std::ostream & operator << (std::ostream & stream, PM_CARTESIAN v);
extern std::ostream & operator << (std::ostream & stream, PM_SPHERICAL s);
extern std::ostream & operator << (std::ostream & stream, PM_CYLINDRICAL c);
extern std::ostream & operator << (std::ostream & stream, PM_QUATERNION q);
extern std::ostream & operator << (std::ostream & stream, PM_ROTATION_VECTOR r);
extern std::ostream & operator << (std::ostream & stream, PM_ROTATION_MATRIX m);
extern std::ostream & operator << (std::ostream & stream, PM_EULER_ZYZ zyz);
extern std::ostream & operator << (std::ostream & stream, PM_EULER_ZYX zyx);
extern std::ostream & operator << (std::ostream & stream, PM_RPY rpy);
extern std::ostream & operator << (std::ostream & stream, PM_POSE pose);
extern std::ostream & operator << (std::ostream & stream, PM_XYA pose);
extern std::ostream & operator << (std::ostream & stream, PM_HOMOGENEOUS hom);

#else  /* end of C++ */

#ifdef __cplusplus
#ifdef EXTERN_C_STD_HEADERS
extern "C" {
#endif
#endif

#include <stdio.h>

#ifdef __cplusplus
#ifdef EXTERN_C_STD_HEADERS
}
#endif
#endif


#ifdef __cplusplus
extern "C" {
#endif

  extern void pmSNprintf(char *string, size_t max_string, 
			 const char *format, ...);

  extern void pmPrintf(const char *format, ...);
  extern void pmFprintf(FILE *file, const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif /* no C++ */

#endif /* #ifndef MATHPRNT_H */
