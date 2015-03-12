#include <stdio.h>

class CLASS_A {
	public:	// Defines the access for the following members.
	int first_member;
	float second_member;
	void function_member();
};

void CLASS_A::function_member()
{
	printf("first_member = %d, second_member = %lf\n", 
	first_member, second_member);
}

CLASS_A first_instance_of_A;
CLASS_A second_instance_of_A;

main()
{
	first_instance_of_A.first_member = 123;
	first_instance_of_A.second_member = 3.14;
	second_instance_of_A.first_member = 501;
	second_instance_of_A.second_member = 2.718;
	first_instance_of_A.function_member();
	second_instance_of_A.function_member();
}
