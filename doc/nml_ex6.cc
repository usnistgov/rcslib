/* nml_ex6.cc */

#include "rcs.hh"
#include "nml_ex1.hh"

#include <stdio.h>

/* This example prompts the user when NML times out to see if it should try again. */

main()
{
	NML example_nml(ex_format, "ex_buf1", "ex6_proc", "ex_cfg.nml");
	EXAMPLE_MSG *example_msg_ptr;
	char input_array[10];

	TRY_AGAIN:

	switch(example_nml.read())
	{
	case -1:
		if(example_nml.error_type == NML_TIMED_OUT)
		{
			rcs_print("NML timed out\n"); 
			rcs_print("Do you want to try again? (y/n)");
			fgets(input_array,sizeof(input_array),stdin);
			if(input_array[0] == 'y')
				goto TRY_AGAIN;
		}
		break;

	case 0:
		/* The buffer contains the same message you read last time. */
		break;

	case EXAMPLE_MSG_TYPE:
		example_msg_ptr = (EXAMPLE_MSG *)example_nml.get_address();
		/* We have a new example message. */
		break;

	}
}






