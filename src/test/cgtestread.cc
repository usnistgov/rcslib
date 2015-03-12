/* cgtest_read.cc */
#include <stdlib.h>
#include <string.h>

#include "rcs.hh"
#include "cgtester.hh"
#include "cgtester_n_codegen_protos.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#include <stdio.h>
#include <signal.h>

static int quit=0;
static void cgtest_read_sigint_handler(int sig)
{
  quit = 1;
}

#ifdef VXWORKS 
extern "C" void cgtest_read();

void cgtest_read()
#else
main(int argc, const char **argv)
#endif
{
  char *MY_LOG_FILE=0;
  char *RCS_DEBUG_FLAG = 0;
  double t1=0;
  double t2=0;


  RCS_DEBUG_FLAG = getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  MY_LOG_FILE = getenv("CGTESTREAD_LOG_FILE");
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
  NML *example_nml1l = new NML(cgtester_format, "cgtest_buf1", "cgtestread_local", "cgtest.nml");
  NML *example_nml2l = new NML(cgtester_format, "cgtest_buf2", "cgtestread_local", "cgtest.nml");
  NML *example_nml1r = new NML(cgtester_format, "cgtest_buf1", "cgtestread_remote", "cgtest.nml");
  NML *example_nml2r = new NML(cgtester_format, "cgtest_buf2", "cgtestread_remote", "cgtest.nml");

  EXAMPLE_MESSAGE *example_msg_ptr;
  //signal(SIGINT,cgtest_read_sigint_handler);
  set_rcs_print_destination(RCS_PRINT_TO_FILE);

  printf("reading 1st buffer locally\n");
  t1 = etime();
  switch(example_nml1l->read())
    {
    case -1:
      printf( "A communications error occurred.\n");
        delete example_nml1l;
	delete example_nml2l;
	delete example_nml1r;
	delete example_nml2r;
	exit(255);
	break;

    case 0:
      /* The buffer contains the same message */
      /* you read last time. */
      printf("no new data.\n");
      break;

    case EXAMPLE_MESSAGE_TYPE:
      t2 = etime();
      printf("example_nml1l->read() took %f seconds\n",(t2-t1));
      example_msg_ptr = (EXAMPLE_MESSAGE *)example_nml1l->get_address();
      example_nml1l->xmlMsgSaveAs(example_msg_ptr,"cgtestread1l.xml");
      printf(" We have a new example message. \n");
      printf(" The value of its members are:\n ");
      printf("enumtestvar=%d %s\n",
	     example_msg_ptr->enumtestvar,
	     cgtester_enum_enumtest_symbol_lookup(example_msg_ptr->enumtestvar));
      printf(" f=%f, c=%c, i=%d, d=%f, lastvar=%d\n ", 
	     example_msg_ptr->f,
	     example_msg_ptr->c,
	     example_msg_ptr->i,
	     example_msg_ptr->d, 
	     example_msg_ptr->lastvar);
      printf("hexdata: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X\n",
	     example_msg_ptr->hexdata[0],
	     example_msg_ptr->hexdata[1],
	     example_msg_ptr->hexdata[2],
	     example_msg_ptr->hexdata[3],
	     example_msg_ptr->hexdata[4],
	     example_msg_ptr->hexdata[5],
	     example_msg_ptr->hexdata[6],
	     example_msg_ptr->hexdata[7],
	     example_msg_ptr->hexdata[8],
	     example_msg_ptr->hexdata[9]);
      printf("name:%s\n",example_msg_ptr->name);
#ifndef NO_NML_UNBOUNDED
      printf("cda_unbounded:%s\n",example_msg_ptr->cda_unbounded);
#endif
      break;
    }

  printf("reading 2nd buffer locally\n");
  t1 = etime();
  switch(example_nml2l->read())
    {
    case -1:
      printf( "A communications error occurred.\n");
      delete example_nml1l;
      delete example_nml2l;
      delete example_nml1r;
      delete example_nml2r;
      exit(255);
      break;

    case 0:
      /* The buffer contains the same message */
      /* you read last time. */
       printf("no new data.\n");
      break;

    case EXAMPLE_MESSAGE_TYPE:
      t2 = etime();
      printf("example_nml2l->read() took %f seconds\n",(t2-t1));
      example_msg_ptr = (EXAMPLE_MESSAGE *)example_nml2l->get_address();
      example_nml2l->xmlMsgSaveAs(example_msg_ptr,"cgtestread2l.xml");
      printf(" We have a new example message. \n");
      printf(" The value of its members are:\n ");
      printf("enumtestvar=%d %s\n",
	     example_msg_ptr->enumtestvar,
	     cgtester_enum_enumtest_symbol_lookup(example_msg_ptr->enumtestvar));
      printf(" f=%f, c=%c, i=%d, d=%f, lastvar=%d\n ", 
	     example_msg_ptr->f,
	     example_msg_ptr->c,
	     example_msg_ptr->i,
	     example_msg_ptr->d, 
	     example_msg_ptr->lastvar);
      printf("hexdata: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X\n",
	     example_msg_ptr->hexdata[0],
	     example_msg_ptr->hexdata[1],
	     example_msg_ptr->hexdata[2],
	     example_msg_ptr->hexdata[3],
	     example_msg_ptr->hexdata[4],
	     example_msg_ptr->hexdata[5],
	     example_msg_ptr->hexdata[6],
	     example_msg_ptr->hexdata[7],
	     example_msg_ptr->hexdata[8],
	     example_msg_ptr->hexdata[9]);
      printf("name:%s\n",example_msg_ptr->name);
#ifndef NO_NML_UNBOUNDED
      printf("cda_unbounded:%s\n",example_msg_ptr->cda_unbounded);
#endif
      break;
    }


  printf("reading 1st buffer remotely\n");
  t1 = etime();
  switch(example_nml1r->read())
    {
    case -1:
      printf( "A communications error occurred.\n");
      delete example_nml1l;
      delete example_nml2l;
      delete example_nml1r;
      delete example_nml2r;
      exit(255);
      break;

    case 0:
      /* The buffer contains the same message */
      /* you read last time. */
      printf("no new data.\n");
      break;

    case EXAMPLE_MESSAGE_TYPE:
      t2 = etime();
      printf("example_nml1r->read() took %f seconds\n",(t2-t1));
      example_msg_ptr = (EXAMPLE_MESSAGE *)example_nml1r->get_address();
      example_nml1r->xmlMsgSaveAs(example_msg_ptr,"cgtestread1r.xml");
      printf(" We have a new example message. \n");
      printf(" The value of its members are:\n ");
      printf("enumtestvar=%d %s\n",
	     example_msg_ptr->enumtestvar,
	     cgtester_enum_enumtest_symbol_lookup(example_msg_ptr->enumtestvar));
      printf(" f=%f, c=%c, i=%d, d=%f, lastvar=%d\n ", 
	     example_msg_ptr->f,
	     example_msg_ptr->c,
	     example_msg_ptr->i,
	     example_msg_ptr->d, 
	     example_msg_ptr->lastvar);
      printf("hexdata: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X\n",
	     example_msg_ptr->hexdata[0],
	     example_msg_ptr->hexdata[1],
	     example_msg_ptr->hexdata[2],
	     example_msg_ptr->hexdata[3],
	     example_msg_ptr->hexdata[4],
	     example_msg_ptr->hexdata[5],
	     example_msg_ptr->hexdata[6],
	     example_msg_ptr->hexdata[7],
	     example_msg_ptr->hexdata[8],
	     example_msg_ptr->hexdata[9]);
      printf("name:%s\n",example_msg_ptr->name);
#ifndef NO_NML_UNBOUNDED
      printf("cda_unbounded:%s\n",example_msg_ptr->cda_unbounded);
#endif
      break;
    }

  printf("reading 2nd buffer remotely\n");
  t1 = etime();
  switch(example_nml2r->read())
    {
    case -1:
      printf( "A communications error occurred.\n");
      delete example_nml1l;
      delete example_nml2l;
      delete example_nml1r;
      delete example_nml2r;
      exit(255);
      break;

    case 0:
      /* The buffer contains the same message */
      /* you read last time. */
      printf("no new data.\n");
      break;

    case EXAMPLE_MESSAGE_TYPE:
      t2 = etime();
      printf("example_nml2r->read() took %f seconds\n",(t2-t1));
      example_msg_ptr = (EXAMPLE_MESSAGE *)example_nml2r->get_address();
      example_nml2r->xmlMsgSaveAs(example_msg_ptr,"cgtestread2r.xml");
      printf(" We have a new example message. \n");
      printf(" The value of its members are:\n ");
      printf("enumtestvar=%d %s\n",
	     example_msg_ptr->enumtestvar,
	     cgtester_enum_enumtest_symbol_lookup(example_msg_ptr->enumtestvar));
      printf(" f=%f, c=%c, i=%d, d=%f, lastvar=%d\n ", 
	     example_msg_ptr->f,
	     example_msg_ptr->c,
	     example_msg_ptr->i,
	     example_msg_ptr->d, 
	     example_msg_ptr->lastvar);
      printf("hexdata: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X\n",
	     example_msg_ptr->hexdata[0],
	     example_msg_ptr->hexdata[1],
	     example_msg_ptr->hexdata[2],
	     example_msg_ptr->hexdata[3],
	     example_msg_ptr->hexdata[4],
	     example_msg_ptr->hexdata[5],
	     example_msg_ptr->hexdata[6],
	     example_msg_ptr->hexdata[7],
	     example_msg_ptr->hexdata[8],
	     example_msg_ptr->hexdata[9]);
      printf("name:%s\n",example_msg_ptr->name);
#ifndef NO_NML_UNBOUNDED
      printf("cda_unbounded:%s\n",example_msg_ptr->cda_unbounded);
#endif
      break;
    }


  printf("argv[0]=%s\n",argv[0]);
  int arg0len = strlen(argv[0]);
  printf("arg0len=%d\n",arg0len);
#ifdef DEBUG_MEMORY
  char *cgwritecommand = (char*) DEBUG_MALLOC(arg0len + 14);
#else
  char *cgwritecommand = (char*) malloc(arg0len + 14);
#endif
  strcpy(cgwritecommand,argv[0]);
#ifndef mingw32
  strcpy(cgwritecommand+arg0len-4,"write 0X0");
  printf("cgwritecommand=%s\n",cgwritecommand);
#else
  strcpy(cgwritecommand+arg0len-8,"write.exe 0X0");
  printf("cgwritecommand=%s\n",cgwritecommand);
#endif
  quit=1;
  for(int i=0; i < 5; i++)
    {
      if(system(cgwritecommand))
	{
	  delete example_nml1l;
	  delete example_nml2l;
	  delete example_nml1r;
	  delete example_nml2r;
	  exit(255);
	}
      printf("reading 2nd buffer remotely\n");
      t1 = etime();
      switch(example_nml2r->read())
	{
	case -1:
	  printf( "A communications error occurred.\n");
	  delete example_nml1l;
	  delete example_nml2l;
	  delete example_nml1r;
	  delete example_nml2r;
	  exit(255);
	  break;

	case 0:
	  /* The buffer contains the same message */
	  /* you read last time. */
	  printf("no new data.\n");
	  break;

	case EXAMPLE_MESSAGE_TYPE:
	  t2 = etime();
	  printf("example_nml2r->read() took %f seconds\n",(t2-t1));
	  example_msg_ptr = (EXAMPLE_MESSAGE *)example_nml2r->get_address();
	  example_nml2r->xmlMsgSaveAs(example_msg_ptr,"cgtestread2r.xml");
	  printf(" We have a new example message. \n");
	  printf(" The value of its members are:\n ");
	  printf("enumtestvar=%d %s\n",
		 example_msg_ptr->enumtestvar,
		 cgtester_enum_enumtest_symbol_lookup(example_msg_ptr->enumtestvar));
	  printf(" f=%f, c=%c, i=%d, d=%f, lastvar=%d\n ", 
		 example_msg_ptr->f,
		 example_msg_ptr->c,
		 example_msg_ptr->i,
		 example_msg_ptr->d, 
		 example_msg_ptr->lastvar);
	  printf("hexdata: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X\n",
		 example_msg_ptr->hexdata[0],
		 example_msg_ptr->hexdata[1],
		 example_msg_ptr->hexdata[2],
		 example_msg_ptr->hexdata[3],
		 example_msg_ptr->hexdata[4],
		 example_msg_ptr->hexdata[5],
		 example_msg_ptr->hexdata[6],
		 example_msg_ptr->hexdata[7],
		 example_msg_ptr->hexdata[8],
		 example_msg_ptr->hexdata[9]);
	  printf("name:%s\n",example_msg_ptr->name);
#ifndef NO_NML_UNBOUNDED
	  printf("cda_unbounded:%s\n",example_msg_ptr->cda_unbounded);
#endif
	  break;
	}
    }

#ifdef DEBUG_MEMORY
  DEBUG_FREE(cgwritecommand);
#else
  free(cgwritecommand);
#endif

  delete example_nml1l;
  delete example_nml2l;
  delete example_nml1r;
  delete example_nml2r;
#ifdef DEBUG_MEMORY
  print_dbg_mem_list();
#endif
}
