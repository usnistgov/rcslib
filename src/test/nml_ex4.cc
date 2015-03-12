/* nml_ex4.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

#include <stdlib.h>

#ifdef VXWORKS
extern "C" void example_nml_write();

void example_nml_write()
#else
main()
#endif
{
	NML example_nml(ex_format, "ex_buf1", "ex4_proc", "ex_cfg.nml");
	EXAMPLE_MSG example_msg;
	example_msg.f = 123.456;
	example_msg.c = 'c';
	example_msg.i = 99;
	example_nml.write(example_msg);
	exit(0);
}
