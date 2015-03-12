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


#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>

static bool sigint_occured=false;

static void my_sigint_handler(int /*sig*/)
{
  sigint_occured=true;
}

int 
main(int argc, char **argv)
{
  int repeat_count;
  double repeat_delay;
  bool use_read_with_args = false;
  NML *nml = 0;
  repeat_count=0;
  repeat_delay=1.0;
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);

  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile  lastvar [repeat_count>  [repeat_delay]\n");
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

  if(argc >= 8)
    {
      use_read_with_args = (!strcmp("use_read_with_args",argv[8]));
    }
  TEST_MESSAGE tst_msg_for_read_with_args;

  nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
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
  nml_start();

  int t;
  double max_time_diff = 0.0;
  double total_time_diff=0.0;
  int num_reads=0;
  int new_reads=0;
  int expected_last_var=strtol(argv[4],0,0);
  double start_read_time=etime();
#if RCS_MAJOR_VERSION >= 2010
  nml->cms->do_not_print_timeout_errors=true;
#endif

  if(!use_read_with_args) {
    t = nml->read();
  } else {
    t = nml->read(&tst_msg_for_read_with_args,
		  sizeof(tst_msg_for_read_with_args));
  }
    
  double end_time=etime();
  double time_diff = end_time-start_read_time;
  max_time_diff=time_diff;
  total_time_diff += time_diff;
  num_reads++;
  if(t>0)
    {
      new_reads++;
    }
  printf("time to read=%f\n",time_diff);
  switch(t)
    {
    case TEST_MESSAGE_TYPE:
      {
	TEST_MESSAGE *tst_msg = 0;
	  if(!use_read_with_args) {
	    tst_msg = (TEST_MESSAGE *) nml->get_address();
	  } else {
	    tst_msg = &tst_msg_for_read_with_args;
	  }
	if(0 == tst_msg)
	  {
	    fprintf(stderr,"nml->get_address() returned NULL\n");
	    delete nml;
	    exit(123);
	  }
  /* 
   * FIXME: long long are more trouble to support than they are worth.
	    if(tst_msg->ll != -1LL*0x123456789LL) {
	      printf("bad ll=%Lx\n", tst_msg->ll);
	    }
	    if(tst_msg->ull != 0x123456789ULL) {
	      printf("bad ull=%Lx\n", tst_msg->ull);
	    } 
	    if(tst_msg->lla[0] != -1LL*0x123456789LL) {
	      printf("bad lla[0]=%Lx\n", tst_msg->lla[0]);
	    }
	    if(tst_msg->ulla[0] != 0x123456789ULL) {
	      printf("bad ulla[0]=%Lx\n", tst_msg->ulla[0]);
	    }
  */
	if(tst_msg->first_count != tst_msg->last_count)
	  {
	    fprintf(stderr,"tst_msg->first_count(%ld) != tst_msg->last_count(%ld)\n", tst_msg->first_count, tst_msg->last_count);
	    delete nml;
	    exit(122);
	  }
	if(tst_msg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
		    expected_last_var, tst_msg->lastvar);
	    delete nml;
	    exit(121);
	  }
      }
      break;

    case SIMPLER_MSG_TYPE:
      {
	SIMPLER_MSG *smsg = (SIMPLER_MSG *) nml->get_address();
	if(0 == smsg)
	  {
	    fprintf(stderr,"nml->get_address() returned NULL\n");
	    delete nml;
	    exit(120);
	  }
	if(smsg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != smsg->last_var(%ld)\n",
		    expected_last_var, smsg->lastvar);
	    delete nml;
	    exit(119);
	  }
      }
      break;

    case 0:
      if(repeat_count >= 0 && repeat_count <= 1)
	{
	  fprintf(stderr,"nml->read() returned %d when %d was expected.\n", t,TEST_MESSAGE_TYPE);
	  delete nml;
	  exit(118);
	}
      break;

    default:
      fprintf(stderr,"nml->read() returned %d when %d was expected.\n", t,TEST_MESSAGE_TYPE);
      delete nml;
      exit(118);
    }
  int i=0;
  void (*old_handler)(int)=0;
  if(repeat_count != 0)
    {
      old_handler = signal(SIGINT,my_sigint_handler);
#if RCS_MAJOR_VERSION >= 2010
      if(repeat_delay > 0 && getenv("NML_TEST_READ_USE_SUBSCRIPTION") != 0)
	{
	  nml->setup_subscription(repeat_delay);
	}
#endif
    }
  FILE *ftimes=0;
  if(getenv("CREATE_READ_TIMES_CSV") != 0)
    {
      ftimes = fopen("read_times.csv","w");
    }
  if(ftimes)
    {
      fprintf(ftimes,"time,diff,max,avg,i,errors,error_type,space_available,queue_length\n");
    }
  bool fail_on_error= (getenv("COUNT_ERRORS") == 0);
  int num_errors = 0;
  int msg_i = -1;


  int new_count = 0;
  double new_min = 1e99;;
  double new_max = 0;
  double new_total = 0;
  double new_total2 = 0;
  
  int old_count = 0;
  double old_min = 1e99;;
  double old_max = 0;
  double old_total = 0;
  double old_total2 = 0;

  int delay_count = 0;
  double delay_total = 0;
  double delay_total2 = 0;
  double delay_min = 1e99;;
  double delay_max = 0;

  for(i=0;
      ((i < repeat_count || repeat_count < 0) && !sigint_occured) ; i++)
    {      
      double start_delay_time = etime();
      if(repeat_delay > 0)
	{
	  esleep(repeat_delay);
	}
      start_read_time=etime();
      time_diff = start_read_time - start_delay_time;
      delay_total += time_diff;
      delay_total2 += time_diff*time_diff;
      delay_count++;
      if(delay_min > time_diff) {
	delay_min = time_diff;
      }
      if(delay_max < time_diff) {
	delay_max = time_diff;
      }

      t = nml->read();
      end_time=etime();
      time_diff = end_time-start_read_time;
      if(time_diff > max_time_diff)
	{
	  max_time_diff=time_diff;
	}
      total_time_diff += time_diff;
      num_reads++;
      double avg = (total_time_diff/num_reads);
      if(t>0)
	{
	  new_reads++;
	  new_count++;
	  new_total += time_diff;
	  new_total2 += time_diff*time_diff;
	  if(new_min > time_diff) {
	    new_min = time_diff;
	  }
	  if(new_max < time_diff) {
	    new_max = time_diff;
	  }
	}
      else 
	{
	  old_count++;
	  old_total += time_diff;
	  old_total2 += time_diff*time_diff;
	  if(old_min > time_diff) {
	    old_min = time_diff;
	  }
	  if(old_max < time_diff) {
	    old_max = time_diff;
	  }
	}
      if(repeat_count == i - 1 || i == 0) {
	printf("time to read=%f (max=%f,#=%d,avg=%f,new=%d,percent_new=%f%%)\n",
	       time_diff,
	       max_time_diff,
	       num_reads,
	       avg,
	       new_reads,
	       ((double) (new_reads*100.0)/num_reads));
	rcs_print("nml->read() returned %d\n",t);
      }
      switch(t)
	{
	case TEST_MESSAGE_TYPE:
	  {
	    TEST_MESSAGE *tst_msg2 = (TEST_MESSAGE *) nml->get_address();
	    msg_i = tst_msg2->i;
	    if(repeat_count == i - 1 || i == 0) {
	      rcs_print("tst_msg2->i=%d\n",tst_msg2->i);
	    }
	    if(tst_msg2->first_count != tst_msg2->last_count)
	      {
		fprintf(stderr,"tst_msg->first_count(%ld) != tst_msg->last_count(%ld)\n", tst_msg2->first_count, tst_msg2->last_count);
		delete nml;
		exit(117);
	      }
	    if(tst_msg2->lastvar != expected_last_var)
	      {
		fprintf(stderr,"expected_last_var(%d) != tst_msg2->last_var(%ld)\n",
			expected_last_var, tst_msg2->lastvar);
		signal(SIGINT,old_handler);
		delete nml;
		exit(116);
	      }
	  }
	  break;
	  
	case 0:
	  break;
	  
	case -1:
	default:
	  fprintf(stderr,"Bad return value from read.\n");
	  if(fail_on_error) {
	    signal(SIGINT,old_handler);
	    delete nml;
	    exit(115);
	  }
	  else {
	    num_errors++;
	  }
	}
      if(ftimes)
	{
	  fprintf(ftimes,"%+9.3f,%+.4f,%+.4f,%+.4f,%6d,%6d,%6d,%6d,%6d\n",
		  end_time,
		  time_diff,
		  max_time_diff,
		  avg,
		  msg_i,
		  num_errors,
		  nml->error_type,
		  nml->get_space_available(),
		  nml->get_queue_length()
		  );
	}
    }
  if(ftimes)
    {
      fclose(ftimes);
    }
  if(new_count > 0) {
    double new_sigma = sqrt(new_total2-new_total*new_total/new_count)/new_count;
    printf("Timing info new : count=%d, min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
		      new_count,new_min,new_max,new_total/new_count,
		      new_sigma);

  }
  if(old_count > 0) {
    double old_sigma = sqrt(old_total2-old_total*old_total/old_count)/old_count;
    printf("Timing info old : count=%d,min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
		      old_count,old_min,old_max,old_total/old_count,
		      old_sigma);
  }
  if(delay_count > 0) {
    double delay_sigma = sqrt(delay_total2-delay_total*delay_total/delay_count)/delay_count;

    printf("Timing info delay : count=%d,min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
		      delay_count,delay_min,delay_max,delay_total/delay_count,
		      delay_sigma);
  }

  if(repeat_count != 0)
    {
      signal(SIGINT,old_handler);
    }
  delete nml;
  nml_cleanup();
  exit(0);
}
