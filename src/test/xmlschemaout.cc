/* nml_ex4.cc */
#include "rcs.hh"
#include "cgtester.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#include <stdlib.h>

#ifdef VXWORKS
extern "C" void example_nml_write();

void example_nml_write()
#else
main()
#endif
{
  char *MY_LOG_FILE=0;
  char *RCS_DEBUG_FLAG=0;
  const char *xmlout=0;
  NML *example_nml1l=0;
  EXAMPLE_MESSAGE *example_msg=0;

  RCS_DEBUG_FLAG=getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  MY_LOG_FILE=getenv("XMLSCHEMAOUT_LOG_FILE");
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
  example_nml1l = new NML(cgtester_format, 0,0,0);
  example_msg = new EXAMPLE_MESSAGE();

  example_msg->f = 123.456;
  example_msg->c = 'c';
  example_msg->i = 99;
  example_msg->d = etime();
  example_msg->lastvar = ((int) example_msg->d)%3600000;
  //printf("cgtestwrite : example_msg->lastvar = %d\n",example_msg->lastvar); 
  example_nml1l->xmlSchemaSaveAs("xmlschemaout.xsd");
  delete example_nml1l;
  delete example_msg;
#ifdef DEBUG_MEMORY
  print_dbg_mem_list();
#endif
  exit(0);
}
