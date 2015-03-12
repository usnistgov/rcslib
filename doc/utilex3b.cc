/* utilex3b.cc */
#include "utilex3.hh"
#include "rcs.hh"

/* Process B */
main()
{
	RCS_SEMAPHORE my_sem(MY_SEM_ID, /* Both processes must agree on id. */
		RCS_SEMAPHORE_NOCREATE,   /* Do not Create the semaphore. */ 
		0.100); /* timeout = 100 milliseconds */

	if(-1 != my_sem.wait())
	{
	    /* Access shared resource. */
	   my_sem.post();
	}
} 
