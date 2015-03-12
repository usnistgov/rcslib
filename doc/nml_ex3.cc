/* nml_ex3.cc */ 
#include "rcs.hh" 
#include "nml_ex1.hh"

int main(int argc, const char **argv) 
{
	RCS_TIMER timer(0.1);
	NML example_nml(ex_format, "ex_buf1","ex3_proc", "ex_cfg.nml");
	EXAMPLE_MSG *example_msg_ptr;
	int quit = 0;

	while(!quit)
	{
		switch(example_nml.read())
		{
		case -1:
			rcs_print( "A communications error occurred.\n");
			quit = 1;
			break;

		case 0:
			/* The buffer contains the same message */
			/* you read last time. */
			break;

		case EXAMPLE_MSG_TYPE:
			example_msg_ptr = (EXAMPLE_MSG *)example_nml.get_address();
			rcs_print(" We have a new example message. \n");
			rcs_print(" The value of its members are:\n ");
			rcs_print(" f=%f, c=%c, i=%d\n ",
				example_msg_ptr->f,
				example_msg_ptr->c,
				example_msg_ptr->i);
			quit = 1;
			break;
		}
		timer.wait();
	} 
} 
