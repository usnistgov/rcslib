#include <stdio.h>

class BASE_CLASS_A {
  public:
	int ia;
	void print_ia();
};

class DERIVED_CLASS_B: public BASE_CLASS_A {
  public:
	float fb;
	void print_fb();
};

void BASE_CLASS_A::print_ia()
{
	printf("ia = %d\n", ia);
}

void DERIVED_CLASS_B::print_fb()
{
	printf("fb = %f\n", fb);
}

main()
{
	DERIVED_CLASS_B b;
	b.ia = 1;
	b.fb = 2.0;
	b.print_ia();
	b.print_fb();
}
