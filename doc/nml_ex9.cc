#include "rcs.hh"
#include "nml_ex1.hh"


int main(int argc,const char **argv)
{
  NML nml1(ex_format, "ex_buf1","ex9_svr","ex_cfg.nml");
  NML nml2(ex_format, "ex_buf2","ex9_svr","ex_cfg.nml");
  
  run_nml_servers(); /* This never returns. */
}
