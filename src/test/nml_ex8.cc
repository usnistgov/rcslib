/* nml_ex8.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

main()
{
	char string[40];
	NML_SERVER srv1(ex_format,"ex_buf1","ex8_svr","ex_cfg.nml");
	NML_SERVER srv2(ex_format,"ex_buf3","ex8_svr","ex_cfg.nml");

	srv1.spawn();
	srv2.spawn();

	rcs_print("Press <ENTER> to kill servers.\n");
	gets(string);

	/* Destructors for srv1, srv2 called implicitly. */
}
