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

#ifdef rtai

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#ifndef __KERNEL__		/* __KERNEL__ needs to be defined or   */
#define __KERNEL__		/* rtai_shm.h barfs out with a O_RDWR  */
#endif /* not defined error.           (P.C.) */

#ifndef MODULE			/* MODULE needs to be defined so that   */
#define MODULE			/* rtai_kmalloc() and rtai_kfree() will */
#endif /* inline correctly without throwing up */
#include <linux/module.h>	/* a __this_module error.       (P.C.)  */

#include <linux/version.h>

#ifndef LINUX_VERSION_CODE
#define LINUX_VESION_CODE (0)
#endif
#ifndef KERNEL_VERSION
#define KERNEL_VERSION(a,b,c) (132097)
#endif

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,4,0)
#include <linux/slab.h>
#else
#include <linux/malloc.h>
#endif

#include "rtai_shm.h"
#include "rtl_rtai_common_api.h"



extern void *
rtlrtai_shm_alloc (const char *name, int key, unsigned long size)
{
  return rtai_kmalloc (key, size); 
}


extern void *
rtlrtai_malloc (unsigned long size)
{
  return (void *) kmalloc (size, 1);
}


extern void
rtlrtai_shm_free (const char *name, int key, unsigned long size, void *addr)
{
  rtai_kfree (key);
}


extern void
rtlrtai_free (void *addr)
{
  if (0 != addr)
    {
      kfree (addr);
    }
}

/* ifdef rtai */
#endif
