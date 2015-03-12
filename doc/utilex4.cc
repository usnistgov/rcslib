#include "rcs.hh"

class MY_STRUCT {
 public:
	 int count;
};

RCS_LINKED_LIST *my_list;
int compute_total_count(RCS_LINKED_LIST *my_list);

main()
{
	int totalCount;
	MY_STRUCT S1, S2, S3;
	my_list = new RCS_LINKED_LIST();   /* Initialize and create linked list. */
	
	/* Store S1 on the end of list. */
	my_list->store_at_tail(&S1, sizeof(MY_STRUCT), 0); 

	/* Store S2 on the end of list. */
	my_list->store_at_tail(&S2, sizeof(MY_STRUCT), 0); 

	/* Store S3 on the end of list. */
	my_list->store_at_tail(&S3, sizeof(MY_STRUCT), 0); 

	totalCount = compute_total_count(my_list);
}

int compute_total_count(RCS_LINKED_LIST *my_list)
{
	MY_STRUCT *ptr_to_struct;
	int total_count = 0;

	/* Get first object and initialize the internal pointer to start of list. */
	ptr_to_struct = (MY_STRUCT *) my_list->get_head(); 

	while(NULL != ptr_to_struct)
	{
		total_count += ptr_to_struct->count;
		ptr_to_struct = (MY_STRUCT *) my_list->get_next();
	}
	return(total_count);	
}
