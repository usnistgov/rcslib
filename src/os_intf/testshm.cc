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


extern "C"
{
#include <stdio.h>		/* printf(), scanf(), NULL */
#include <string.h>		/* strcpy() */
}
#include "sharedmem.hh"

#ifdef VXWORKS
extern "C" int testshm ();	/* make it easily shell-accessible */
int
testshm (int create)		/* run this in the foreground */
#else
int
main (int argc, char **argv)
#endif
{
  RCS_SHAREDMEM *shm;

#if defined (VXWORKS)
  shm =
    new RCS_SHAREDMEM (0x100, 256,
		       create ? RCS_SHAREDMEM_CREATE : RCS_SHAREDMEM_NOCREATE,
		       0664);
#else
  if (argc > 1)
    {
      /* any cmd line args mean this is the sharedmem creator */
      shm = new RCS_SHAREDMEM (0x100, 256, RCS_SHAREDMEM_CREATE, 0664);
    }
  else
    {
      shm = new RCS_SHAREDMEM (0x100, 256, RCS_SHAREDMEM_NOCREATE);
    }
#endif

  if (shm->addr == NULL)
    {
      printf ("can't create shared memory-- exiting\n");
      return 1;
    }

  /* run give and take, interactively */
  while (!feof (stdin))
    {
#define BUFFERSIZE 256
      char buf[BUFFERSIZE];
      fputs ("r, w, or q? ", stdout);
      fgets (buf, BUFFERSIZE, stdin);

      if (buf[0] == 'q')
	break;
      if (buf[0] == 'r')
	{
	  printf ("%s\n", shm->addr);
	}
      if (buf[0] == 'w')
	{
	  printf ("what string? ");
	  scanf ("%s", buf);
	  strcpy ((char *) shm->addr, buf);
	}
    }

  delete shm;
  return 0;
}
