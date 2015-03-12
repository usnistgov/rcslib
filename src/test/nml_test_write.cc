/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.

*/ 


#include "rcs.hh"
#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

static bool sigint_occured=false;
static void (*old_sigint_handler)(int)=0;

static void my_sigint_handler(int sig)
{
  printf("signal %d caught.\n",sig);
  fflush(stdout);
  sigint_occured=true;
#if 0
  if(old_sigint_handler != 0 && 
     old_sigint_handler != SIG_IGN &&
     old_sigint_handler != SIG_DFL &&
     old_sigint_handler != SIG_ERR &&
     old_sigint_handler != my_sigint_handler)
    {
      (*old_sigint_handler)(sig);
    }
#endif

  sigint_occured=true;
}

static const char *Id="$Id: nml_test_write.cc 1893 2012-01-11 15:23:08Z shackle $ " __FILE__ " compiled on " __DATE__ " at " __TIME__ ;

int 
main(int argc, const char ** const argv)
{
  double repeat_delay;
  class NML *nml=0;
  int repeat_count;
  long last_var;
  char *simpler_msg_env = getenv("SIMPLER_MSG");
  cms_print_queue_free_space=(getenv("PRINT_QUEUE_FREE_SPACE") != 0);
  
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  nml=0;
  repeat_count=0;
  repeat_delay=1.0;
  printf("%s\n",Id);

  
  if(argc < 5)
    {
      fprintf(stderr,"nml_test_write usage : bufname procname configfile lastvar [repeat_count] [repeat_delay]\n");
      fprintf(stderr,"\n\t\t (set the environment variable SIMPLER_MSG to 1 to use the simpler NML msg.)\n");
      fprintf(stderr,"\n\t\t (set the environment variable DEBUG_RCSLIB to 1 for verbose diagnotsics output, --enable-debug-print must have been used when the RCS library was configured to be effective.)\n");
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
  last_var=strtol(argv[4],0,0);
  bool do_nml_start=(getenv("DO_NML_START") != 0);
  if(do_nml_start)
    {
      nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
      nml_start();
    }
  else
    {
      nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);

    }
  if(!nml ||  !nml->cms)
    {
      exit(126);
    }

  printf("BufferSize:%ld\n",nml->cms->size);
  printf("nml->cms->BufferLine=%s\n",nml->cms->BufferLine);
  printf("nml->cms->ProcessLine=%s\n",nml->cms->ProcessLine);
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

#if RCS_MAJOR_VERSION >= 2010
  strcpy(tst_msg.message,"tst_msg.message");
  tst_msg.message_length = strlen(tst_msg.message);
  reset_time_tracker(&tst_msg.tt);
#endif

  /* 
   * FIXME: long long are more trouble to support than they are worth.
   tst_msg.ll = -1LL*0x123456789LL;
   tst_msg.ull = 0x123456789ULL;
   tst_msg.lla[0] = -1LL*0x123456789LL;
   tst_msg.ulla[0] = 0x123456789ULL;
  */

  printf("tst_msg.size=%ld\n",tst_msg.size);
  SIMPLER_MSG smsg;
  printf("smsg.size=%ld\n",smsg.size);
  printf("argv[0]=%s\n",argv[0]);
  double write_start_time=etime();
  double max_time_diff=0.0;
  int num_writes=0;
  double total_time_diff = 0.0;

  if(simpler_msg_env)
    {
      write_start_time=etime();
      if((t = nml->write(smsg)))
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  delete nml;
	  nml=0;
	  exit(124);
	}
    }
  else
    {
      tst_msg.lastvar= last_var;
      tst_msg.first_count = tst_msg.last_count = last_var;
      write_start_time=etime();
      if((t = nml->write(tst_msg)))
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  delete nml;
	  nml=0;
	  exit(124);
	}
    }
  double write_end_time=etime();
  double time_diff = (write_end_time-write_start_time);
  max_time_diff=time_diff;
  total_time_diff += time_diff;
  num_writes++;
  printf("time_to_write=%f\n", time_diff);

  int i=0;
  if(repeat_count != 0)
    {
      old_sigint_handler = signal(SIGINT,my_sigint_handler);
      fflush(stderr);
      printf("\n\nStarting repeating loop : repeat_count=%d\n",repeat_count);
      fflush(stdout);
    }
  bool fail_on_error= (getenv("COUNT_ERRORS") == 0);
  FILE *ftimes=0;
  if(getenv("CREATE_WRITE_TIMES_CSV") != 0)
    {
      ftimes = fopen("write_times.csv","w");
    }
  if(ftimes)
    {
      fprintf(ftimes,"time,diff,max,avg,tst_msg.i,errors,error_type,space_available,queue_length\n");
    }
  double start_time = etime();
  int num_errors = 0;
  int last_space_available = 0;
  int last_queue_length = -1;
  bool last_write_failed = false;
  int consecutive_errors = 0;
  int consecutive_oks = 0;
  double avg = 0.0;
  int sa = -1;
  int ql = -1;
  for(i = 0 ;
      ((i < repeat_count || repeat_count  < 0) && !sigint_occured) ; i++)
    {
      if(!last_write_failed) {
	tst_msg.i++;
      }
      tst_msg.d = (etime() - start_time);
      tst_msg.first_count = tst_msg.last_count = i;
      if(repeat_delay > 0)
	{
	  esleep(repeat_delay);
	}
#if RCS_MAJOR_VERSION >= 2010
      cycle_time_tracker(&tst_msg.tt);
#endif
      write_start_time=etime();
      bool write_failed = false;
      if((t = nml->write(tst_msg)))
	{
	  write_failed=true;
	  consecutive_errors++;
	  if(!last_write_failed) {
	    fprintf(stderr,"consecutive_oks=%d\n",consecutive_oks);
	    consecutive_oks=0;
	    fprintf(stderr,"nml->write() returned %d\n", t);
	  }
	  if(fail_on_error) {
	    signal(SIGINT,SIG_DFL);
	    delete nml;
	    nml=0;
	    exit(124);
	  }
	  else {
	    num_errors++;
	  }
	}
      else {
	consecutive_oks++;
	if(last_write_failed) {
	  fprintf(stderr,"consecutive_errors=%d\n",consecutive_errors);
	  consecutive_errors=0;
	  fprintf(stderr,"nml->write() succeeded.\n");
	}
      }
      if(write_failed != last_write_failed || (i%10000) == 0 || repeat_delay > 2.0) {
	rcs_print("%s %s %s %s i=%d, tst_msg.i=%d, \ttst_msg.lastvar=%ld\n",
		  argv[0],argv[1],argv[2],argv[3],i,tst_msg.i,tst_msg.lastvar);
      }
      last_write_failed = write_failed;
      write_end_time=etime();
      time_diff = (write_end_time-write_start_time);
      if(time_diff > max_time_diff)
	{
	  max_time_diff=time_diff;
	}
      total_time_diff += time_diff;
      num_writes++;
      avg = (total_time_diff/num_writes);
      sa = nml->get_space_available();
      if(sa != last_space_available) {
	printf("nml->get_space_available() returned %d\n",sa);
	printf("sizeof(tst_msg) = %lu, (nml->get_space_available()/sizeof(tst_msg) = %lu\n",
	       ((unsigned long) sizeof(tst_msg)),
	       ((unsigned long) (sa/sizeof(tst_msg))));
	last_space_available = sa;
      }
      ql = nml->get_queue_length();
      if(ql != last_queue_length) {
	printf("nml->get_queue_length() returned %d\n",ql);
	last_queue_length = ql;
      }
      if(ftimes)
	{
	  fprintf(ftimes,"%+9.3f,%+9.3f,%+9.3f,%+9.3f,%6d,%6d,%6d,%6d,%6d\n",
		  write_end_time,
		  time_diff,
		  max_time_diff,
		  avg,
		  tst_msg.i,
		  num_errors,
		  nml->error_type,
		  sa,
		  ql
		  );
	}
    }
  if(ftimes)
    {
      fclose(ftimes);
    }
  fflush(stderr);
  esleep(0.2);
  printf("%s %s %s %s stats:\n",
	 argv[0],argv[1],argv[2],argv[3]);
  printf("time,diff,max,avg,tst_msg.i,errors,error_type,space_available,queue_length\n");
  printf("%+9.3f,%+9.3f,%+9.3f,%+9.3f,%6d,%6d,%6d,%6d,%6d\n",
	 write_end_time,
	 time_diff,
	 max_time_diff,
	 avg,
	 tst_msg.i,
	 num_errors,
	 nml->error_type,
	 sa,
	 ql
	 );
  printf("\n");
  fflush(stdout);

  if(repeat_count != 0)
    {
      signal(SIGINT,SIG_DFL);
    }

  if(nml)
    {
      printf("Calling delete nml.\n");
      fflush(stdout);
      delete nml;
      nml=0;
    }
  if(do_nml_start)
    {
      printf("Calling nml_cleanup().\n");
      fflush(stdout);
      nml_cleanup();
    }
  printf("%s done.\n",Id);
  fflush(stdout);

  exit(0);
}
