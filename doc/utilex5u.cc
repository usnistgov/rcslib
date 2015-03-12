#include "rcs.hh"
#include "roots.hh"

/* A UNIX or DOS Application which uses compute_roots. */
main()
{
	double r1, r2;
	/* Send all the rcs_print messages to the standard output. */
	set_rcs_print_destination(RCS_PRINT_TO_STDOUT);
	
	/* Try to compute the roots of a quadratic that will cause an error.*/
	compute_roots(1, 2, -1, r1, r2);
}
