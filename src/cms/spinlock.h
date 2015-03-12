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


#ifndef SPINLOCK_H
#define SPINLOCK_H


#ifdef __cplusplus
extern "C"
{
#endif

/* prototypes for C-language interface to 68000 assembly language functions */

/* acquire access to a semaphored data buffer */
  extern int acquire_access (short int *semaphore, double timeout);

/* release semaphore when done  */
  extern int release_access (short int *semaphore, double timeout);

  extern int increment_read_status (short int *);
  extern int decrement_read_status (short int *);
  extern int increment_write_status (short int *);
  extern int decrement_write_status (short int *);

#ifdef __cplusplus
}
#endif

#endif
