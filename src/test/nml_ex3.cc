/* cgtest_read.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

#include <signal.h>
static int quit=0;
static void cgtest_read_sigint_handler(int sig)
{
  quit = 1;
}

#ifdef VXWORKS 
extern "C" void cgtest_read();

void cgtest_read()
#else
main()
#endif
{
	RCS_TIMER timer(2.0);
	NML example_nml(ex_format, "ex_buf1", "ex3_proc", "ex_cfg.nml");
	EXAMPLE_MSG *example_msg_ptr;
	//signal(SIGINT,cgtest_read_sigint_handler);
	set_rcs_print_flag(PRINT_EVERYTHING);
	set_rcs_print_file("something.log");
	set_rcs_print_destination(RCS_PRINT_TO_FILE);
	printf("cgtest_read: Press ^C to exit.\n");

	while(!quit)
	{
		switch(example_nml.read())
		{
		case -1:
		  printf( "A communications error occurred.\n");
			break;

		case 0:
			/* The buffer contains the same message */
			/* you read last time. */
		  printf("no new data.\n");
			break;

		case EXAMPLE_MSG_TYPE:
			example_msg_ptr = (EXAMPLE_MSG *)example_nml.get_address();
			printf(" We have a new example message. \n");
			printf(" The value of its members are:\n ");
			printf(" f=%f, c=%c, i=%d\n ", 
				example_msg_ptr->f,
				example_msg_ptr->c,
				example_msg_ptr->i);
			break;
		}
		timer.wait();
	}
}
