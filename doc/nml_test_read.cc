
#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

static bool sigint_occured=false;

static void my_sigint_handler(int sig)
{
  sigint_occured=true;
}

int 
main(int argc, char **argv)
{
  int repeat_count;
  double repeat_delay;
  repeat_count=0;
  repeat_delay=1.0;
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);

  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile  lastvar\n");
      exit(127);
    }
  if(argc >= 6)
    {
      repeat_count=strtol(argv[5],0,0);
    }
  if(argc >= 7)
    {
      repeat_delay =strtod(argv[6],0);
    }
  
  NML *nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  if(0 == nml)
    {
      exit(126);
    }
  if(!nml->valid())
    {
      fprintf(stderr,"nml->valid() check failed.\n");
      delete nml;
      exit(125);
    }
  int t;
  if(TEST_MESSAGE_TYPE != (t = nml->read()))
    {
      fprintf(stderr,"nml->read() returned %d when %d was expected.\n", t,TEST_MESSAGE_TYPE);
      delete nml;
      exit(124);
    }
  TEST_MESSAGE *tst_msg = (TEST_MESSAGE *) nml->get_address();
  if(0 == tst_msg)
    {
      fprintf(stderr,"nml->get_address() returned NULL\n");
      delete nml;
      exit(123);
    }
  int expected_last_var=strtol(argv[4],0,0);
  if(tst_msg->lastvar != expected_last_var)
    {
      fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
	      expected_last_var, tst_msg->lastvar);
      delete nml;
      exit(122);
    }
  int i=0;
  void (*old_handler)(int)=0;
  if(repeat_count != 0)
    {
      old_handler = signal(SIGINT,my_sigint_handler);
    }
  for(i=0;
      ((i < repeat_count || repeat_count < 0) && !sigint_occured) ; i++)
    {      
      esleep(repeat_delay);
      t = nml->read();
      rcs_print("nml->read() returned %d\n",t);
      switch(t)
	{
	case TEST_MESSAGE_TYPE:
	  tst_msg = (TEST_MESSAGE *) nml->get_address();
	  rcs_print("tst_msg->i=%d\n",tst_msg->i);
	  if(tst_msg->lastvar != expected_last_var)
	    {
	      fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
		      expected_last_var, tst_msg->lastvar);
	      signal(SIGINT,old_handler);
	      delete nml;
	      exit(122);
	    }
	  break;
	  
	case 0:
	  break;
	  
	case -1:
	default:
	  fprintf(stderr,"Bad return value from read.\n");
	  signal(SIGINT,old_handler);
	  delete nml;
	  exit(122);
	}
    }
  if(repeat_count != 0)
    {
      signal(SIGINT,old_handler);
    }
  delete nml;
  exit(0);
}
