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


#ifndef RTLRTAI_SHM_H
#define RTLRTAI_SHM_H


#ifdef __cplusplus
extern "C"
{
#endif

  extern void *rtlrtai_shm_alloc (const char *name,
				  int key, unsigned long size);

  extern void *rtlrtai_malloc (unsigned long size);

  extern void rtlrtai_shm_free (const char *name,
				int key, unsigned long size, void *addr);

  extern void rtlrtai_free (void *addr);



#ifdef __cplusplus
}
#endif

#endif
