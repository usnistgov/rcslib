#include <stdio.h>

class BASE_CLASS_C {
  public:
	BASE_CLASS_C(char *_name) {name = _name;} ;
	char *name;
	virtual void virtual_function();
	void non_virtual_function();
};

void BASE_CLASS_C::virtual_function()
{
	printf("BASE_CLASS_C::virtual_function called with name = %s\n",name);
}

void BASE_CLASS_C::non_virtual_function()
{
	printf("BASE_CLASS_C::non_virtual_function called with name = %s\n",name);
}

class DERIVED_CLASS_D: public BASE_CLASS_C {
  public:
	DERIVED_CLASS_D(char *_name): BASE_CLASS_C(_name) {};
	virtual void virtual_function();
	void non_virtual_function();
};
	
void DERIVED_CLASS_D::virtual_function()
{
	printf("DERIVED_CLASS_D::virtual_function called with name = %s\n",name);
}

void DERIVED_CLASS_D::non_virtual_function()
{
	printf("DERIVED_CLASS_D::non_virtual_function called with name = %s\n",name);
}

int function_using_ptr_to_base_class(BASE_CLASS_C *ptr_to_c)
{
	ptr_to_c->virtual_function();
	ptr_to_c->non_virtual_function();
}

main()
{
	BASE_CLASS_C c("c");
	DERIVED_CLASS_D d("d");

	printf("calling c.virtual_function()\n");
	c.virtual_function();
	printf("calling c.non_virtual_function()\n");
	c.non_virtual_function();
	printf("calling d.virtual_function()\n");
	d.virtual_function();
	printf("calling d.non_virtual_function()\n");
	d.non_virtual_function();
	
	printf("calling function_using_ptr_to_base_class(&c)\n");
	function_using_ptr_to_base_class(&c);	
	printf("calling function_using_ptr_to_base_class(&d)\n");
	function_using_ptr_to_base_class(&d);
}
