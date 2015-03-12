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



#ifndef RCS_CE_H
#define RCS_CE_H

#if 0
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

double RCS_CE_ATOF(const char *);
int RCS_CE_VSPRINTF(char *, const char *, va_list);
void RCS_CE_UNICODE_TO_ASCII(char *str, wchar_t *wcs, int len);
void RCS_CE_ASCII_TO_UNICODE(wchar_t *wcs, const char *str, int len);


#ifdef __cplusplus
}
#endif
#endif

#endif
