#include "rcs.hh"


main()
{
	RCS_TIMER timer(0.02);/* Initialize timer for 20 millisecond cycles. */

 	while(1)
 	{
		/* Do some processing. */

		timer.wait();        /* Wait for the end of cycle. */
	}
}

