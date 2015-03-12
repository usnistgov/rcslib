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
#include <string.h>

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SEMAPHORE_H
#include <semaphore.h>
#endif

#if HAVE_ERRNO_H
#include <errno.h>
#endif


int
main (int argc, const char **argv)
{
  int i;
  int exitval = 0;

#if HAVE_SEM_UNLINK || POSIX_SEMAPHORES
  for (i = 1; i < argc; i++)
    {
      if (-1 == sem_unlink (argv[i]))
	{
	  fprintf (stderr, "sem_unlink(%s) failed (%d) -- %s.\n",
		   argv[i], 
		   errno, 
		   strerror (errno));
	  exitval++;
	}
    }
#else
  fprintf (stderr,
	   "sem_unlink is unimplemented on this system so this program is useless.\n");
  exit (255);
#endif

  exit (exitval);
}
