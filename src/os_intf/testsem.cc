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


#ifdef VXWORKS
extern "C"
{
#include <vxWorks.h>
#include <stdio.h>		/* feof(), stdin, fputs(), etc. */
#include <errno.h>		/* errno */
#include <taskLib.h>		/* taskSpawn */
}
#else
extern "C"
{
#include <stdio.h>		/* feof(), stdin, fputs(), etc. */
#include <errno.h>		/* perror() */
}
#endif
#include "semaphore.hh"
#include "timer.hh"		/* esleep() */

#ifdef VXWORKS
static int semtaker_done = 0;
static void
semtaker ()
{
  RCS_SEMAPHORE sem (0x100, RCS_SEMAPHORE_NOCREATE);

  semtaker_done = 0;
  while (!semtaker_done)
    {
      sem.wait ();
      printf ("semtaker got semaphore\n");
      esleep (2.0);		/* wait 2 seconds */
      sem.post ();
      printf ("semtaker gave semaphore\n");
      esleep (2.0);
    }

  printf ("semtaker quit\n");
}
#endif

#ifdef VXWORKS
extern "C" int testsem ();	/* make it easily shell-accessible */
int
testsem ()			/* run this in the foreground */
#else
int
main (int argc, char **argv)
#endif
{
  RCS_SEMAPHORE *sem;

#if defined (VXWORKS)
  sem = new RCS_SEMAPHORE (0x100, RCS_SEMAPHORE_CREATE, 0664, 0);
  taskSpawn ("semtaker", 100, VX_FP_TASK, 8000, (FUNCPTR) semtaker);
#else
  if (argc > 1)
    {
      /* any cmd line args mean this is the semaphore creator */
      sem = new RCS_SEMAPHORE (0x100, RCS_SEMAPHORE_CREATE, 0664, 0);
    }
  else
    {
      sem = new RCS_SEMAPHORE (0x100, RCS_SEMAPHORE_NOCREATE);
    }
#endif

  /* run give and take, interactively */
  while (!feof (stdin))
    {
#define BUFFERSIZE 80
      char buf[BUFFERSIZE];
      fputs ("give, take, or quit (g/t/q)? ", stdout);
      fgets (buf, BUFFERSIZE, stdin);

      if (buf[0] == 'q')
	break;
      if (buf[0] == 'g')
	{
	  /* give it */
	  if (sem->post () == -1)
	    {
	      fputs ("can't give it\n", stdout);
	    }
	  else
	    {
	      fputs ("gave it\n", stdout);
	      printf ("val = %d\n", sem->getvalue ());
	    }
	}
      if (buf[0] == 't')
	{
	  /* take it */
	  if (sem->wait () == -1)
	    {
	      fputs ("can't give it\n", stdout);
	    }
	  else
	    {
	      fputs ("took it\n", stdout);
	      printf ("val = %d\n", sem->getvalue ());
	    }
	}
    }

#if defined (VXWORKS)
  semtaker_done = 1;
#endif

  delete sem;
}
