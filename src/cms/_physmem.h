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


#ifndef _PHYSMEM_H
#define _PHYSMEM_H


#ifdef __cplusplus
extern "C"
{
#endif
  extern int read_physmem (unsigned long source, void *destination,
			   long bytes);
  extern int write_physmem (unsigned long destination, void *source,
			    long bytes);

#ifdef __MSDOS__
#ifdef MS_WINDOWS_API
#if !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif

#include <windows.h>

  LPBYTE create_ptr_to_physmem (unsigned long phys_addr, int bytes,
				WORD FAR * ptr_to_selector);
  extern DWORD MapPhysicalToLinear (DWORD dwPhysical, DWORD dwLength);
  extern WORD SynthSelector (DWORD dwLinearAddress, DWORD dwLength);
  extern LPSTR GetSelectorPointer (WORD selector);

#else
  int move_physmem (unsigned long target, unsigned long source, long bcount);
#endif
#endif

#ifdef __cplusplus
}
#endif


#endif
