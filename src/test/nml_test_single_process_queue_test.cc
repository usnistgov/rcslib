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

static const char *Id="$Id: nml_test_write.cc 1171 2008-05-07 14:59:18Z shackle $ " __FILE__ " compiled on " __DATE__ " at " __TIME__ ;

int 
main(int argc, const char ** const argv)
{
  double repeat_delay;
  class NML *nml=0;
  int repeat_count;
  long last_var;
  char *simpler_msg_env = getenv("SIMPLER_MSG");

  cms_print_queue_free_space=1;
  
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  nml=0;
  repeat_count=0;
  repeat_delay=-1.0;
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
  bool do_nml_start=(getenv("DO_NML_START"));
  if(do_nml_start)
    {
      nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
      nml_start();
    }
  else
    {
      nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
    }
  if(!nml)
    {
      rcs_print_error("Out of memory? (nml==0) \n");
      exit(126);
    }

  if(!nml->cms)
    {
      rcs_print_error("Out of memory? (nml->cms==0) \n");
      exit(126);
    }

  printf("BufferSize:%ld\n",nml->cms->size);
  printf("nml->cms->BufferLine=%s\n",nml->cms->BufferLine);
  printf("nml->cms->ProcessLine=%s\n",nml->cms->ProcessLine);
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
  FILE *ftimes=0;
  if(getenv("CREATE_WRITE_TIMES_CSV") != 0)
    {
      ftimes = fopen("write_times.csv","w");
    }
  if(ftimes)
    {
      fprintf(ftimes,"time,diff,max,avg\n");
    }
  nml->clear();
  double start_time = etime();
  int last_read_i=-1;
  for(i = 0 ;
      ((i < repeat_count || repeat_count  < 0) && !sigint_occured) ; i++)
    {
      tst_msg.i=i;
      tst_msg.d = (etime() - start_time);
      tst_msg.first_count = tst_msg.last_count = i;
      rcs_print("%s %s %s %s tst_msg.i=%d, \ttst_msg.lastvar=%ld\n",
		argv[0],argv[1],argv[2],argv[3],tst_msg.i,tst_msg.lastvar);
      if(repeat_delay > 0)
	{
	  esleep(repeat_delay);
	}
#if RCS_MAJOR_VERSION >= 2010
      cycle_time_tracker(&tst_msg.tt);
#endif
      write_start_time=etime();
      int before_write_queue_length= nml->get_queue_length();
      rcs_print("%s %s %s %s \tbefore_write_queue_length=%d\n",
		argv[0],argv[1],argv[2],argv[3],before_write_queue_length);
      int before_write_space_available= nml->get_space_available();
      rcs_print("%s %s %s %s \tbefore_write_space_available=%d\n",
		argv[0],argv[1],argv[2],argv[3],before_write_space_available);
      if((t = nml->write(tst_msg)))
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  if(nml->error_type == NML_QUEUE_FULL_ERROR)
	    {
	      int after_write_queue_length= nml->get_queue_length();
	      rcs_print("%s %s %s %s\tafter_write_queue_length=%d\n",
			argv[0],argv[1],argv[2],argv[3],after_write_queue_length);
	      if(after_write_queue_length != before_write_queue_length ||
		 after_write_queue_length < 1)
		{
		  rcs_print_error("%s %s %s %s invalid after_write_queue_length=%d\n",
				  argv[0],argv[1],argv[2],argv[3],
				  after_write_queue_length);
		  signal(SIGINT,SIG_DFL);
		  nml->clear();
		  delete nml;
		  nml=0;
		  exit(124);
		}
	      rcs_print("%s %s %s %s QUEUE_FULL calling read(). . .\n",
			argv[0],argv[1],argv[2],argv[3]);
	      rcs_print("%s %s %s %s last_read_i=%d\n",
			argv[0],argv[1],argv[2],argv[3],last_read_i);
	      NMLTYPE read_ret = nml->read();
	      rcs_print("%s %s %s %s read returned %ld\n",
			argv[0],argv[1],argv[2],argv[3],read_ret);
	      int after_read_queue_length= nml->get_queue_length();
	      rcs_print("%s %s %s %s after_read_queue_length=%d\n",
			argv[0],argv[1],argv[2],argv[3],after_read_queue_length);
	      if(after_read_queue_length != (before_write_queue_length-1))
		{
		  rcs_print_error("%s %s %s %s invalid after_read_queue_length=%d\n",
				  argv[0],argv[1],argv[2],argv[3],
				  after_read_queue_length);
		  signal(SIGINT,SIG_DFL);
		  nml->clear();
		  delete nml;
		  nml=0;
		  exit(124);
		}
	      int after_read_space_available= nml->get_space_available();
	      rcs_print("%s %s %s %s after_read_space_available=%d\n",
			argv[0],argv[1],argv[2],argv[3],
			after_read_space_available);
	      if(read_ret != tst_msg.type)
		{
		  rcs_print_error("%s %s %s %s read returned %ld != %ld\n",
				  argv[0],argv[1],argv[2],argv[3],
				  read_ret,
				  tst_msg.type);
		  signal(SIGINT,SIG_DFL);
		  nml->clear();
		  delete nml;
		  nml=0;
		  if(ftimes)
		    {
		      fclose(ftimes);
		      ftimes=0;
		    }
		  exit(124);
		}
	      else 
		{
		  TEST_MESSAGE *tst_msgP = (TEST_MESSAGE *)
		    nml->get_address();
		  rcs_print("%s %s %s %s tst_msgP->i=%d, \ttst_msgP->lastvar=%ld\n",
			    argv[0],argv[1],argv[2],argv[3],
			    tst_msgP->i,
			    tst_msgP->lastvar);
		  if((last_read_i+1) != tst_msgP->i)
		    {
		      rcs_print_error("%s %s %s %s read (tst_msgP->i=%d) != (last_read_i+1=%d)\n",
				  argv[0],argv[1],argv[2],argv[3],
				      tst_msgP->i,
				      (last_read_i+1));
		      signal(SIGINT,SIG_DFL);
		      nml->clear();
		      delete nml;
		      nml=0;
		      if(ftimes)
			{
			  fclose(ftimes);
			  ftimes=0;
			}
		      exit(125);
		    }
		  last_read_i = tst_msgP->i;
		}
	      i--;
	    }
	  else
	    {
	      signal(SIGINT,SIG_DFL);
	      nml->clear();
	      delete nml;
	      nml=0;
	      if(ftimes)
		{
		  fclose(ftimes);
		  ftimes=0;
		}
	      exit(125);
	    }
	}
      else
	{
	  write_end_time=etime();
	  time_diff = (write_end_time-write_start_time);
	  if(time_diff > max_time_diff)
	    {
	      max_time_diff=time_diff;
	    }
	  total_time_diff += time_diff;
	  num_writes++;
	  double avg = (total_time_diff/num_writes);
	  if(ftimes)
	    {
	      fprintf(ftimes,"%f,%f,%f,%f\n",
		      write_end_time,
		      time_diff,
		      max_time_diff,
		      avg);
	    }

	  // This block executed if nml->write returned 0:
	  int after_write_queue_length= nml->get_queue_length();
	  rcs_print("%s %s %s %s\tafter_write_queue_length=%d\n",
		argv[0],argv[1],argv[2],argv[3],after_write_queue_length);
	  if(after_write_queue_length != (before_write_queue_length+1))
	    {
	      rcs_print("%s %s %s %s\t(after_write_queue_length=%d) != ((before_write_queue_length+1)=%d)\n",
			argv[0],argv[1],argv[2],argv[3],
			after_write_queue_length,
			(before_write_queue_length+1));
	      nml->clear();
	      delete nml;
	      nml=0;
	      if(ftimes)
		{
		  fclose(ftimes);
		  ftimes=0;
		}
	      exit(125);
	    }
	  int after_write_space_available= nml->get_space_available();
	  rcs_print("%s %s %s %s\tafter_write_space_available=%d\n",
		argv[0],argv[1],argv[2],argv[3],
		    after_write_space_available);
	  if(after_write_space_available > before_write_space_available)
	    {
	      rcs_print("%s %s %s %s\t(after_write_space_available=%d) <= (before_write_space_available=%d)\n",
			argv[0],argv[1],argv[2],argv[3],
			after_write_space_available,
			(before_write_space_available+1));
	      nml->clear();
	      delete nml;
	      nml=0;
	      if(ftimes)
		{
		  fclose(ftimes);
		  ftimes=0;
		}
	      exit(125);
	    }
	}
    }
  nml->clear();
  if(ftimes)
    {
      fclose(ftimes);
    }
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
