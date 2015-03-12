/* nml_ex4.cc */
#include "rcs.hh"
#include "cgtester.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VXWORKS
extern "C" void example_nml_write();

void example_nml_write()
#else
main(int argc, const char **argv)
#endif
{
  NML *example_nml1l = 0;
  EXAMPLE_MESSAGE *example_msg=0;
  const char *xmlout=0;
  char *MY_LOG_FILE=0;
  char *RCS_DEBUG_FLAG = 0;


  RCS_DEBUG_FLAG = getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  MY_LOG_FILE = getenv("XMLOUT_LOG_FILE");
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
  //  example_nml1l->xmlSetStyleProperty("NS_PREFIX=cgt");
  //example_nml1l->xmlSetStyleProperty("NS_HREF=xmlschemaout.xsd");

  example_msg= new EXAMPLE_MESSAGE();

  example_msg->f = 123.456;
  example_msg->c = 'c';
  example_msg->i = 99;
  example_msg->d = etime();
  example_msg->hexdata[0] = 0x0;
  example_msg->hexdata[1] = 0xFF;
  example_msg->hexdata[2] = 0x1;
  example_msg->hexdata[3] = 0xFE;
  example_msg->hexdata[4] = 0x0;
  example_msg->hexdata[5] = 0xFF;
  example_msg->hexdata[6] = 0x1;
  example_msg->hexdata[7] = 0xFE;
  strcpy(example_msg->name,"Will");
  example_msg->name[4] = 0xDD;
  strcpy(example_msg->name+5,"newline(\n),cr(\r)&\"\'");
  //  example_msg->enumtestvar = d;
#ifndef NO_NML_UNBOUNDED
  example_msg->cda_unbounded = strdup("string_for_cda_unbounded");
  example_msg->cda_unbounded_length =
    example_msg->cda_unbounded_size_allocated = strlen(example_msg->cda_unbounded) +1;
  
  example_msg->ida_unbounded = new int [10];
  example_msg->ida_unbounded_length = example_msg->ida_unbounded_size_allocated = 10;
  for(int jj=0; jj< 10; jj++)
    {
      example_msg->ida_unbounded[jj] = jj;
    }
#endif

  example_msg->lastvar = ((int) example_msg->d)%3600000;
  if(argc > 1 )
    {
      NMLmsg *read_msg = example_nml1l->readMsgFromXmlFile(argv[1]);
      if(0 != read_msg)
	{
	  if(read_msg->type == EXAMPLE_MESSAGE_TYPE)
	    {
	      *example_msg = *((EXAMPLE_MESSAGE *)read_msg);
	    }
	  else
	    {
	      fprintf(stderr,"Wrong type of message in xml file.\n");
	      exit(254);
	    }
	}
      else
	{
	  fprintf(stderr,"Couln't read xml file.\n");
	  exit(253);
	}
    }

  //printf("cgtestwrite : example_msg->lastvar = %d\n",example_msg->lastvar); 
  example_nml1l->xmlMsgSaveAs(example_msg, "xmlout.xml");
  delete example_nml1l;
  delete example_msg;
#ifdef DEBUG_MEMORY
  print_dbg_mem_list();
#endif
  exit(0);
}
