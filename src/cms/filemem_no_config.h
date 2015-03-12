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

#ifndef FILEMEM_NO_CONFIG_H
#define FILEMEM_NO_CONFIG_H

#if defined(VXWORKS) || defined(DOS_WINDOWS) || defined(lynxosPC)
#ifdef USE_FCNTL_FILE_LOCKING
#undef USE_FCNTL_FILE_LOCKING
#endif
#endif

#if !defined(USE_FCNTL_FILE_LOCKING) && !defined(MSDOS)
#define FILEMEM_USE_SEMAPHORES
#endif

#include "rcs_defs.hh"		// DOS_WINDOWS
#include <stdio.h>		// FILE *, fpos_t

#ifdef FILEMEM_USE_SEMAPHORES
#include "sem.hh"		// class RCS_SEMAPHORE

#if  !defined(DOS_WINDOWS) && !defined(VXWORKS)
#include <sys/types.h>		/* key_t */
#include <sys/ipc.h>
#include <sys/sem.h>
#endif

#include <stddef.h>		/* size_t */

#ifndef KEY_T_DEFINED

#ifdef VXWORKS
typedef int key_t;		/* key_t is not defined in VxWorks or  Windows -- do
				   it here, and it should be visible
				   throughout all the shared mem code */
#endif

#ifdef WIN32
typedef long key_t;
#endif
#endif

#endif


#ifdef USE_FCNTL_FILE_LOCKING
#include <sys/fcntl.h>
#endif

#ifdef sunos4
// The sunos4 header files say that fgetpos and fsetpos take a pointer to a
// a long even though ANSI says they should take an fpos_t.
typedef long fpos_t;
#endif


#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy(), strcmp()
#include <errno.h>		// errno


#ifdef  USE_FCNTL_FILE_LOCKING
#include <fcntl.h>
#endif

#if defined(USE_FCNTL_FILE_LOCKING) && defined(FILEMEM_USE_SEMAPHORES)
#error Can not compile with both defined(USE_FCNTL_FILE_LOCKING && FILEMEM_USE_SEMAPHORES)
#endif

#if defined(lynxosPC) || defined(sunos4)

// no prototypes for these in LynxOS-- how lame. Let's do them here

#include <stdio.h>		/* ftell(), fpos_t */
#include <unistd.h>		// SEEK_SET

  static int fgetpos (FILE * stream, fpos_t * ptr);
  static int fsetpos (FILE * stream, const fpos_t * ptr);

// FIXME-- test these!

  int fgetpos (FILE * stream, fpos_t * ptr)
  {
    long retval;

      retval = ftell (stream);

    if (retval == -1L)
      {
	return -1;
      }
    else
      {
	*ptr = (fpos_t) retval;
	return 0;
      }
  }

  int fsetpos (FILE * stream, const fpos_t * ptr)
  {
    return fseek (stream, (long) *ptr, SEEK_SET);
  }

#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif

// Sunos4 header files are missing SEEK_SET
#if defined(sunos4) && !defined(SEEK_SET)
#define SEEK_SET 0
#endif

#endif
//  FILEMEM_NO_CONFIG_H
