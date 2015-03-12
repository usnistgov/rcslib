#include <stdio.h>

class CLASS_C {
  public:
	CLASS_C(char *_name);  // This is the constructor declaration.
	~CLASS_C();            // This is the constructor declaration.
	char *name;
};

CLASS_C::CLASS_C(char *_name) 
{	
	name = _name;
	printf("Constructor for CLASS_C called with name = %s.\n",name);
}

CLASS_C::~CLASS_C()
{	
	printf("Destructor for CLASS_C called with name = %s.\n",name);
}

// VxWorks users should probably not 
// declare objects with constructors here.
CLASS_C outside_C("outside"); 

void my_function();

main()
{	
	CLASS_C main_C("main");
	my_function();
	//exit(0);
}

void my_function()
{
	CLASS_C my_function_C("my_function");
	CLASS_C *ptr_to_C;

	ptr_to_C = new CLASS_C("new");

	delete ptr_to_C; // See <A HREF="quickC++.html#Destructors_Header">Destructors</a>
}

