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
#include <stdioLib.h>		/* fprintf(), stderr */
#include <sysLib.h>		/* sysClkRateGet() */
#include <taskLib.h>		/* taskLock(), taskUnlock(), taskDelay() */
#include <tickLib.h>		/* tickGet() */

#if 0
/* FIXME: can't include taskLib.h (Where is REG_SET?) */
#include <taskLib.h>		/* taskLock(), taskUnlock(), taskDelay() */
#endif
#else
#include <stdio.h>		/* fprintf(), stderr */
#endif

#include "spinlock.h"
#include "rcs_prnt.hh"		/* rcs_print_error() */

extern int increment_read_status (short int *);
extern int decrement_read_status (short int *);

/* acquire access to a semaphored data buffer */
int
acquire_access (short int *sem, double timeout)
{
  ULONG timeout_ticks=0;
  ULONG start_ticks =0;
  ULONG current_ticks=0;
  ULONG elapsed_ticks=0;
  if (timeout == 0.0)
    {
      timeout_ticks = 1;
      start_ticks = tickGet ();
      elapsed_ticks = 0;
    }
  else if (timeout > 0.0)
    {
      timeout_ticks = (int) (timeout * sysClkRateGet ());
      start_ticks = tickGet ();
      elapsed_ticks = 0;
    }

  /* loop until timeout_count expires */
  while (elapsed_ticks <= timeout_ticks && (timeout >= 0.0))
    {
      taskLock ();
      if (!increment_read_status (sem))
	{
	  return 0;		/* success */
	}
      else
	{
	  taskUnlock ();
	  taskDelay (1);
	  if (timeout >= 0.0)
	    {
	      current_ticks = tickGet ();
	      elapsed_ticks = current_ticks - start_ticks;
	    }
	}
    }

  taskUnlock ();
  rcs_print_error ("timed out while acquiring access on semaphore 0x%x\n",
		   (int) sem);
  return -1;			/* indicate timeout failure */
}

/* release semaphore when done  */
int
release_access (short int *sem, double timeout)
{
  int tst;
  ULONG timeout_ticks, start_ticks, current_ticks, elapsed_ticks;
  timeout_ticks = (ULONG) (timeout * sysClkRateGet ());
  start_ticks = tickGet ();
  elapsed_ticks = 0;

  while (elapsed_ticks <= timeout_ticks)
    {
      tst = decrement_read_status (sem);
      if (tst == 0)
	{
	  taskUnlock ();
	  return 0;		/* success */
	}
      else
	{
	  if (tst < 0)
	    {
	      taskUnlock ();
	      rcs_print_error ("invalid semaphore on 0x%x", (int) sem);
	      return -1;	/* invalid semaphore  */
	    }
	}
      current_ticks = tickGet ();
      elapsed_ticks = current_ticks - start_ticks;
    }

  taskUnlock ();
  rcs_print_error ("timed out while releasing access on semphore 0x%x",
		   (int) sem);
  return -1;			/* timeout failure */
}
