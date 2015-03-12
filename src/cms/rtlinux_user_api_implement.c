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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if defined(HAVE_CONFIG_H) || !defined(NO_RTL)


#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "mbuff.h"
#include "rtl_rtai_common_api.h"



extern void *
rtlrtai_shm_alloc (const char *name, int key, unsigned long size)
{
  return mbuff_alloc (name, size);
}


extern void *
rtlrtai_malloc (unsigned long size)
{
  return malloc (size);
}


extern void
rtlrtai_shm_free (const char *name, int key, unsigned long size, void *addr)
{
  mbuff_free (name, addr);
}


extern void
rtlrtai_free (void *addr)
{
  if (0 != addr)
    free (addr);
}


#endif
// defined(HAVE_CONFIG_H) || !defined(NO_RTL)
