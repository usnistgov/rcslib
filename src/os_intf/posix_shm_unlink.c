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

#include <stdio.h>
#include <stdlib.h>

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#if HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif

#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif


int
main (int argc, const char **argv)
{
  int i;

#if defined(HAVE_SHM_UNLINK) || defined(POSIX_SHAREDMEM_AVAILABLE)
  for (i = 1; i < argc; i++)
    {
      if(!argv[i])
	{
	  break;
	}
      if (-1 == shm_unlink (argv[i]))
	{
	  if(!argv[i])
	    {
	      break;
	    }
	  fprintf (stderr, "shm_unlink(%s) failed\n",
		   argv[i]);
	  int errno_copy=errno;
	  char *s_err = strerror(errno_copy);
	  if(s_err)
	    {
	      fprintf(stderr,"shm_unlink: errno=%d -- %s\n", 
		      errno_copy, s_err); 
	    }
	}
    }
#else
  fprintf (stderr,
	   "shm_unlink is unimplemented on this system so this program is useless.\n");
  exit (-1);
#endif

  exit (0);
}
