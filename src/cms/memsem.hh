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

/**********************************************************************
* File: memsem.hh
* Purpose: Provides function prototypes that let programmers
* use a block of memory to implement a mutual exclusion semaphore.
*
* With LynxOs and SunOs using semop is very inefficient if the semaphore will
* ussually be available. Other platforms may give you no semaphore operations.
*************************************************************************/


#ifndef MEMSEM_HH
#define MEMSEM_HH

#include "sem.hh"

/* Take the mutual exclusion semaphore. */
struct mem_access_object
{
  void *data;
  long connection_number;
  long total_connections;
  double timeout;
  double sem_delay;
  int read_only;
  bool split_buffer;
  char toggle_bit;
  RCS_SEMAPHORE *sem;
};


extern int mem_get_access (struct mem_access_object *mo);


/* Give up the mutual exclusion semaphore. */
extern int mem_release_access (struct mem_access_object *mo);

#endif
