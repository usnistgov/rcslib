/* nml_ex4.cc */
#include <stdlib.h>

#include "rcs.hh"
#include "cgtester.hh"

#include "cgtester_n_codegen_protos.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef VXWORKS
extern "C" void example_nml_write();

void example_nml_write()
#else
main(int argc, const char **argv)
#endif
{
char *MY_LOG_FILE=0;
  char *RCS_DEBUG_FLAG = 0;
  double t1=0;
  double t2=0;
  const char *selflags = "XXX";
  if(argc > 1)
    {
      if(strlen(argv[1]) >= 3)
      {
	selflags=argv[1];
      }
    }


  RCS_DEBUG_FLAG = getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  MY_LOG_FILE = getenv("CGTESTWRITE_LOG_FILE");
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
  NML *example_nml1l = new NML(cgtester_format, "cgtest_buf1", "cgtestwrite_local", "cgtest.nml");
#if 0
  NML example_nml2l(cgtester_format, "cgtest_buf2", "cgtestwrite_local", "cgtest.nml");
  NML example_nml1r(cgtester_format, "cgtest_buf1", "cgtestwrite_remote", "cgtest.nml");
#endif
  NML *example_nml2r = new NML(cgtester_format, "cgtest_buf2", "cgtestwrite_remote", "cgtest.nml");
  EXAMPLE_MESSAGE *example_msg = new EXAMPLE_MESSAGE();
  example_msg->f = 123.456;
  example_msg->c = 'c';
  example_msg->i = 99;
  example_msg->d = etime();

#ifndef NO_NML_UNBOUNDED;
  example_msg->cda_unbounded = strdup("unbounded_string");
  example_msg->cda_unbounded_length = example_msg->cda_unbounded_size_allocated=
    strlen(example_msg->cda_unbounded)+1;
#endif

  example_msg->hexdata[0] = 0x0;
  example_msg->hexdata[1] = 0xFF;
  example_msg->hexdata[2] = 0x1;
  example_msg->hexdata[3] = 0xFE;
  example_msg->hexdata[4] = 0x0;
  example_msg->hexdata[5] = 0xFF;
  example_msg->hexdata[6] = 0x1;
  example_msg->hexdata[7] = 0xFE;
  const char *badstring = "Willnl(\n)cr(\r)lt(<)gt(>)q(\")a1(\')a2(&)";
  int badstringlen = strlen(badstring);
  strcpy(example_msg->name,badstring);
  memset(example_msg->name+badstringlen,0xBB,sizeof(example_msg->name)-badstringlen);
  
  example_msg->enumtestvar = d;
  printf("example_msg->enumtestvar=%d %s\n",
	 example_msg->enumtestvar,
	 cgtester_enum_enumtest_symbol_lookup(example_msg->enumtestvar));
  example_msg->lastvar = ((int) example_msg->d)%3600000;
  if(selflags[0] == 'X')
    {
      printf("cgtestwrite : example_msg->lastvar = %d\n",example_msg->lastvar);
      example_nml1l->xmlMsgSaveAs(example_msg,"cgtestwrite1l.xml");
      t1=etime();
      if( example_nml1l->write(example_msg) < 0) 
	{
	  fprintf(stderr, "example_nml1l->write() failed\n");
	  delete example_nml1l;
	  delete example_nml2r;
	  delete example_msg;
#ifdef DEBUG_MEMORY
	  print_dbg_mem_list();
#endif
	  exit(255);
	}
      t2 = etime();
      printf("example_nml1l->write(example_msg) took %f seconds.\n",(t2-t1));
    }
  //  example_nml2l->write(example_msg);


  if(selflags[1] == 'X')
    {
      example_msg->d = etime();
      example_msg->lastvar = ((int) example_msg->d)%3600000;
      printf("cgtestwrite : example_msg->lastvar = %d\n",example_msg->lastvar);
      //example_nml1r->write(example_msg);
      example_nml2r->xmlMsgSaveAs(example_msg,"cgtestwrite2r.xml");
      t1=etime();
      if( example_nml2r->write(example_msg) < 0)
	{
	  fprintf(stderr, "example_nml2r->write() failed\n");
	  delete example_nml1l;
	  delete example_nml2r;
	  delete example_msg;
#ifdef DEBUG_MEMORY
	  print_dbg_mem_list();
#endif
	  exit(255);
	}
      t2 = etime();
      printf("example_nml2r->write(example_msg) took %f seconds.\n",(t2-t1));
    }


    if(selflags[2] == 'X')
    {      
      for(int i= 0 ; i < 5 ; i++)
	{
	  example_msg->d = etime();
	  example_msg->lastvar = ((int) example_msg->d)%3600000;
	  printf("cgtestwrite : example_msg->lastvar = %d\n",example_msg->lastvar);
	  t1=etime();
	  if( example_nml2r->write(example_msg) < 0)
	    {
	      fprintf(stderr, "example_nml2r->write() failed\n");
	      delete example_nml1l;
	      delete example_nml2r;
	      delete example_msg;
#ifdef DEBUG_MEMORY
	      print_dbg_mem_list();
#endif
	      exit(255);
	    }
	  t2 = etime();
	  printf("example_nml2r->write(example_msg) took %f seconds.\n",(t2-t1));
	}
    }

  delete example_nml1l;
  delete example_nml2r;
  delete example_msg;
#ifdef DEBUG_MEMORY
  print_dbg_mem_list();
#endif
  exit(0);
}
