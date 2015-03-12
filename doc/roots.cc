#include "roots.hh"
#include "rcs.hh"
#include <math.h>

/* Function for computing the roots of a quadratic. */
void compute_roots( double A, double B, double C, double &root1, double &root2)
{
	if(B*B-4*A*C < 0)
	{
		rcs_print("compute_roots: Can't compute square root of %lf\n",
			  B*B-4*A*C);
		 return;
	}
	root1 = (-B+sqrt(B*B-4*A*C))/(2*A);
	root2 = (-B-sqrt(B*B-4*A*C))/(2*A);
}
