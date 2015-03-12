#include "rcs.hh"
#include "nml_ex1.hh"

#ifdef VXWORKS
#include <taskLib.h>
#include <unistd.h>

int bServer_tid = 0;

extern "C" void bServer();

void bServer()
#else
main()
#endif
{
#ifdef VXWORKS
  bServer_tid = taskIdSelf();
#endif

	NML *nml1 = new NML(ex_format, "ex_buf1","bServer",TEST_CFG);
	rcs_print("Starting bServer . . .\n");

	run_nml_servers(); /* This never returns. */
}

#ifdef VXWORKS
void killBServer()
{
  if(bServer_tid > 0)
    {
      kill(bServer_tid, 2);
      bServer_tid = 0;
    }
}
#endif
