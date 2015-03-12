#include "rcs.hh"
#include "nml_ex1.hh"

#ifdef VXWORKS
extern "C" void nml_ex9();

void nml_ex9()
#else
main()
#endif
{
	NML nml1(ex_format, "ex_buf1","ex9_svr","ex_cfg.nml");


  NML example02_nml(ex_format, "ex_buf02", "ex9_svr", "ex_cfg.nml");
  NML example03_nml(ex_format, "ex_buf03", "ex9_svr", "ex_cfg.nml");
  NML example04_nml(ex_format, "ex_buf04", "ex9_svr", "ex_cfg.nml");
  NML example05_nml(ex_format, "ex_buf05", "ex9_svr", "ex_cfg.nml");
  NML example06_nml(ex_format, "ex_buf06", "ex9_svr", "ex_cfg.nml");
  NML example07_nml(ex_format, "ex_buf07", "ex9_svr", "ex_cfg.nml");
  NML example08_nml(ex_format, "ex_buf08", "ex9_svr", "ex_cfg.nml");
  NML example09_nml(ex_format, "ex_buf09", "ex9_svr", "ex_cfg.nml");


  NML example11_nml(ex_format, "ex_buf11", "ex9_svr", "ex_cfg.nml");
  NML example12_nml(ex_format, "ex_buf12", "ex9_svr", "ex_cfg.nml");
  NML example13_nml(ex_format, "ex_buf13", "ex9_svr", "ex_cfg.nml");
  NML example14_nml(ex_format, "ex_buf14", "ex9_svr", "ex_cfg.nml");
  NML example15_nml(ex_format, "ex_buf15", "ex9_svr", "ex_cfg.nml");
  NML example16_nml(ex_format, "ex_buf16", "ex9_svr", "ex_cfg.nml");
  NML example17_nml(ex_format, "ex_buf17", "ex9_svr", "ex_cfg.nml");
  NML example18_nml(ex_format, "ex_buf18", "ex9_svr", "ex_cfg.nml");
  NML example19_nml(ex_format, "ex_buf19", "ex9_svr", "ex_cfg.nml");


  NML example21_nml(ex_format, "ex_buf21", "ex9_svr", "ex_cfg.nml");
  NML example22_nml(ex_format, "ex_buf22", "ex9_svr", "ex_cfg.nml");
  NML example23_nml(ex_format, "ex_buf23", "ex9_svr", "ex_cfg.nml");
  NML example24_nml(ex_format, "ex_buf24", "ex9_svr", "ex_cfg.nml");
  NML example25_nml(ex_format, "ex_buf25", "ex9_svr", "ex_cfg.nml");
  NML example26_nml(ex_format, "ex_buf26", "ex9_svr", "ex_cfg.nml");
  NML example27_nml(ex_format, "ex_buf27", "ex9_svr", "ex_cfg.nml");
  NML example28_nml(ex_format, "ex_buf28", "ex9_svr", "ex_cfg.nml");
  NML example29_nml(ex_format, "ex_buf29", "ex9_svr", "ex_cfg.nml");

	run_nml_servers(); /* This never returns. */
}
