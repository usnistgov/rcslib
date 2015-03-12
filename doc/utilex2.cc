#include "rcs.hh"
#include <stdio.h> 

int my_sync_func(void *arg)
{
	 getchar();
	 return(0);
}

main()
{
	RCS_TIMER timer(0.02, my_sync_func, NULL);

 	while(1)
	{
		 /* Do some processing. */

		timer.wait();
	}
}
