#include <stdio.h>

void output(int i)
{
	printf("output int: %d\n",i);
}

void output(float f)
{
	printf("output float: %f\n",f);
}
main()
{
	int integer;
	float floating_point;
	integer = 501;
	floating_point = 1.41;
	output(integer);
	output(floating_point);
}
