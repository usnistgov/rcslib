/* nml_ex5.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

/* This example uses the PHANTOM writes to catch writes of a specific type of msg. */

int my_write(NMLmsg *msg)
{
	if(msg->type == EXAMPLE_MSG_TYPE)
	{
		rcs_print("Wrote EXAMPLE_MSG.\n");
	}
}


main()
{
	NML example_nml(ex_format, "ex_buf1", "ex5_proc", "ex_cfg.nml");
	example_nml.phantom_write = my_write;
	EXAMPLE_MSG example_msg;
	/* . . .  */
	example_nml.write(example_msg);
}
