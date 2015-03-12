#include <stdlib.h>

#include "rcs.hh"
#include "cgtester.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#ifdef VXWORKS
extern "C" void cgtestsvr();

void cgtestsvr()
#else
main()
#endif
{
  char *MY_LOG_FILE=0;
  char *RCS_DEBUG_FLAG = 0;


  RCS_DEBUG_FLAG = getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  MY_LOG_FILE = getenv("CGTESTSVR1_LOG_FILE");
  if(MY_LOG_FILE)
    {
      set_rcs_print_file(MY_LOG_FILE);
      set_rcs_print_destination(RCS_PRINT_TO_FILE);
    }
#ifdef DEBUG_MEMORY
  if(getenv("DEBUG_MEMORY"))
    {
      enable_debug_memory();
    }
#endif
  NML nml1(cgtester_format, "cgtest_buf1","cgtestsvr","cgtest.nml");
  run_nml_servers(); /* This never returns. */
}
