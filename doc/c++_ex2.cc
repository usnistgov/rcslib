#include <stdio.h>

class CLASS_B {
  public:
	CLASS_B(char *name);
};

CLASS_B::CLASS_B(char *name)
{
	printf("Constructor for CLASS_B called with name = %s.\n",name);
}

CLASS_B outside_B("outside");

void my_function();

main()
{	
	CLASS_B main_B("main");
	my_function();
}

void my_function()
{
	CLASS_B my_fuction_B("my_function");
	CLASS_B *ptr_to_B;

	ptr_to_B = new CLASS_B("new");

	delete ptr_to_B; // See <A HREF="quickC++.html#Destructors_Header">Destructors</a>
}
