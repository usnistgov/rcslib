/* utilex3a.cc */
#include "utilex3.hh"
#include "rcs.hh"

 /* Process A */
main()
{
	RCS_SEMAPHORE my_sem(MY_SEM_ID, /* Both processes must agree on id. */
		RCS_SEMAPHORE_CREATE,   /* Create the semaphore. */ 
		0.100,    /* timeout = 100 milliseconds */
		MY_SEM_MODE,   /* Set permissions for semaphore */
 		1);          /* Initial State */

	 if(-1 != my_sem.wait())
	{
		/* Access shared resource. */
		my_sem.post();
	} 
}
