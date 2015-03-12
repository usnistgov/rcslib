
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
  NML *nml;
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  nml=0;
  repeat_count=0;
  repeat_delay=1.0;
  
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
  void (*old_handler)(int)=0;
  if(repeat_count != 0)
    {
      old_handler = signal(SIGINT,my_sigint_handler);
    }

  nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  if(nml == 0)
    {
      exit(126);
    }
  if(!nml->valid())
    {
      fprintf(stderr,"nml->valid() check failed.\n");
      delete nml;
      nml=0;
      exit(125);
    }
  int t;
  TEST_MESSAGE tst_msg;
  tst_msg.lastvar=strtol(argv[4],0,0);
  if((t = nml->write(tst_msg)))
    {
      fprintf(stderr,"nml->write() returned %d\n", t);
      delete nml;
      nml=0;
      exit(124);
    }
  int i=0;
  for(i = 0 ;
      ((i < repeat_count || repeat_count  < 0) && !sigint_occured) ; i++)
    {
      tst_msg.i=i;
      rcs_print("tst_msg.i=%d\n",tst_msg.i);
      esleep(repeat_delay);
      if((t = nml->write(tst_msg)))
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  signal(SIGINT,old_handler);
	  delete nml;
	  nml=0;
	  exit(124);
	}
    }
  if(repeat_count != 0)
    {
      signal(SIGINT,old_handler);
    }

  if(nml)
    {
      delete nml;
      nml=0;
    }
  exit(0);
}
