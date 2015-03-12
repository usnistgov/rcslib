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

// This file is essentially the same as nml_test_read.cc except that
// since vxworks doesn't use main the main is replaced with a different function

#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <vxWorks.h>
#include <taskLib.h>

static bool sigint_occured=false;

static void my_sigint_handler(int sig)
{
  sigint_occured=true;
}

extern volatile int vx_nml_test_read_quit_flag;
volatile int vx_nml_test_read_quit_flag=0;

extern "C" int set_vx_nml_test_read_quit_flag(int new_flag);
extern "C" int get_vx_nml_test_read_quit_flag(void);

int vx_test_read_tid=0;

int
set_vx_nml_test_read_quit_flag(int new_flag)
{
  vx_nml_test_read_quit_flag=new_flag;
  return(0);
}

int
get_vx_nml_test_read_quit_flag(void)
{
  return vx_nml_test_read_quit_flag;
}


extern int vx_nml_test_read_priority;
int vx_nml_test_read_priority=60;

extern "C"  int set_vx_nml_test_read_priority(int new_priority);
extern "C"  int get_vx_nml_test_read_priority(void);

int set_vx_nml_test_priority(int new_priority)
{
  vx_nml_test_read_priority=new_priority;
  return 0;
}

int get_vx_nml_test_read_priority(void)
{
  return vx_nml_test_read_priority;
}


extern NML *vx_test_read_nml_ptr;

NML *vx_test_read_nml_ptr=0;


extern "C" int 
vx_nml_test_read(const char *buf, 
		 const char *proc, 
		 const char * cfg, 
		 long expected_last_var, 
		 int repeat_count, 
		 int repeat_delay_ms);

extern "C" int
vx_test_nml_read_second_part(
			     NML *_nml_ptr,
			     int repeat_count,
			     long expected_last_var, 
			     int repeat_delay_ms);


int 
vx_nml_test_read(const char *buf, 
		 const char *proc, 
		 const char * cfg, 
		 long expected_last_var, 
		 int repeat_count, 
		 int repeat_delay_ms)
{
  if(get_vx_nml_test_read_quit_flag())
    {
      return 1;
    }
  //set_rcs_print_destination(RCS_PRINT_TO_STDOUT); 
  //set_rcs_print_flag(PRINT_EVERYTHING);
  rcs_print("Starting vx_nml_test_read(%s,%s,%s,%ld,%d,%d)\n",
	 buf,proc,cfg,expected_last_var,repeat_count,repeat_delay_ms);
  vx_test_read_nml_ptr = new NML(nml_test_format,buf,proc,cfg);
  rcs_print_debug(PRINT_MISC,"vx_test_read_nml_ptr=%p\n",vx_test_read_nml_ptr);
  if(0 == vx_test_read_nml_ptr)
    {
      return(126);
    }
  rcs_print_debug(PRINT_MISC,"vx_test_read_nml_ptr->valid() =%d\n",vx_test_read_nml_ptr->valid());
  if(!vx_test_read_nml_ptr->valid())
    {
      fprintf(stderr,"vx_test_read_nml_ptr->valid() check failed.\n");
      return(125);
    }
  int t;
  rcs_print_debug(PRINT_MISC,"calling vx_test_read_nml_ptr->read()\n");
  fflush(stdout);
  t = vx_test_read_nml_ptr->read();
  rcs_print_debug(PRINT_MISC,"vx_test_read_nml_ptr->read() returned %d\n",t);
  fflush(stdout);
  switch(t)
    {
    case TEST_MESSAGE_TYPE:
      {
	TEST_MESSAGE *tst_msg = (TEST_MESSAGE *) vx_test_read_nml_ptr->get_address();
	rcs_print_debug(PRINT_MISC,"tst_msg->first_count=%d,tst_msg->last_count=%d,tst_msg->lastvar=%d,tst_msg->i=%d\n",
		  tst_msg->first_count,tst_msg->last_count,tst_msg->lastvar,tst_msg->i);
	fflush(stdout);
	if(0 == tst_msg)
	  {
	    fprintf(stderr,"vx_test_read_nml_ptr->get_address() returned NULL\n");
	    return(123);
	  }
	if(tst_msg->first_count != tst_msg->last_count)
	  {
	    fprintf(stderr,"tst_msg->first_count(%d) != tst_msg->last_count(%d)\n", tst_msg->first_count, tst_msg->last_count);
	    return(122);
	  }
	if(tst_msg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
		    expected_last_var, tst_msg->lastvar);
	    return(121);
	  }
      }
      break;

    case SIMPLER_MSG_TYPE:
      {
	SIMPLER_MSG *smsg = (SIMPLER_MSG *) vx_test_read_nml_ptr->get_address();
	if(0 == smsg)
	  {
	    fprintf(stderr,"vx_test_read_nml_ptr->get_address() returned NULL\n");
	    return(120);
	  }
	if(smsg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != smsg->last_var(%ld)\n",
		    expected_last_var, smsg->lastvar);
	    return(119);
	  }
      }
      break;

    default:
      fprintf(stderr,"vx_test_read_nml_ptr->read() returned %d when %d was expected.\n", t,TEST_MESSAGE_TYPE);
      return(118);
    }
   if(repeat_count != 0 && ! get_vx_nml_test_read_quit_flag())
    {
      rcs_print_debug(PRINT_MISC,"calling taskSpawn(%s, . . .)\n","tRead");
      fflush(stdout);
      vx_test_read_tid=
	taskSpawn("tRead",	// name
		  vx_nml_test_read_priority,		// priority
		  VX_FP_TASK,	// options
		  0x2000,		// stack size
		  (FUNCPTR) vx_test_nml_read_second_part, // entry point
		  ((int) vx_test_read_nml_ptr), // arg 1
		  ((int)repeat_count), // arg 2
		  ((int) expected_last_var), // arg 3
		  (int)repeat_delay_ms, // arg 4
		  0,0,0,0,0,0);
#if 0
      vx_test_nml_read_second_part(vx_test_read_nml_ptr,repeat_count,expected_last_var,repeat_delay_ms);
#endif
    }
   rcs_print_debug(PRINT_MISC,"returning from vx_nml_test_read(%s,%s,%s,%ld,%d,%d)\n",
	     buf,proc,cfg,expected_last_var,repeat_count,repeat_delay_ms);
   return(0);
}

int vx_test_nml_read_second_part(NML *_nml_ptr,int repeat_count,long expected_last_var, int repeat_delay_ms)
{
  static  TEST_MESSAGE tst_msg;
  static  SIMPLER_MSG smsg;
  static  int i=0;
  static  int t=0;
  static  double repeat_delay;
  static  double start_time;
  static  double elapsed_time;
  static  double diffr;
  static  double maxr=-1e14;
  static  double minr=1e14;
  static  double total_rtime=0;
  static  double avg_diffr;
  static  double diffc;
  static  double maxc=-1e14;
  static  double minc=1e14;
  static  double avg_diffc;
  static  int new_msgs=0;
  static  double t1, t2, last_t1;
  //  RCS_TIMER timer(repeat_delay);
  static int print_count;

  i=0;
  t=-1;
  maxr=-1e14;
  minr=1e14;
  total_rtime=0;
  maxc=-1e14;
  minc=1e14;
  new_msgs=0;

  rcs_print_debug(PRINT_MISC,"vx_test_nml_read_second_part(NML *_nml_ptr=%p,int repeat_count=%d,long expected_last_var=%d, int repeat_delay_ms=%d) called.\n",
	    _nml_ptr,repeat_count,expected_last_var,repeat_delay_ms);

  repeat_delay = 0.001*repeat_delay_ms;

  if(repeat_delay_ms < 1)
    {
      print_count = 2000;
    }
  else if(repeat_delay_ms >= 2000)
    {
      print_count =1;
    }
  else
    {
      print_count = 2000/repeat_delay_ms;
    }

  esleep(0.5);
  start_time=etime();

  for(i=0;
      ((i < repeat_count || repeat_count < 0) && !sigint_occured  && ! get_vx_nml_test_read_quit_flag()) ; i++)
    {      
      t1 = etime();
      t = vx_test_read_nml_ptr->read();
      t2 = etime();
      diffr = t2 -t1;
      if(maxr < diffr)
	{
	  maxr=diffr;
	}
      if(minr > diffr)
	{
	  minr=diffr;
	}
      total_rtime+=diffr;
      if(i > 1)
	{
	  diffc = t1 -last_t1;
	  if(maxc < diffc)
	    {
	      maxc = diffc;
	    }
	  if(minc > diffc)
	    {
	      minc = diffc;
	    }
	}
      last_t1 = t1;
      elapsed_time = t2 - start_time;
      avg_diffc = elapsed_time/i;
      avg_diffr = total_rtime/i;
      if((i % print_count) == 2)
	{
	  rcs_print("i=%d,elapsed_time=%f,total_rtime=%f,diffr=%f,avg_diffr=%f,maxr=%f,minr=%f,diffc=%f,avg_diffc=%f,maxc=%f,minc=%f,new_msgs=%d,vx_test_read_nml_ptr->read() returned %d\n",i,elapsed_time,total_rtime,diffr,avg_diffr,maxr,minr,diffc,avg_diffc,maxc,minc,new_msgs,t);
	}
      switch(t)
	{
	case TEST_MESSAGE_TYPE:
	  {
	    new_msgs++;
	    TEST_MESSAGE *tst_msg2 = (TEST_MESSAGE *) vx_test_read_nml_ptr->get_address();
	    if(new_msgs % 10 == 0)
	      {
		rcs_print_debug(PRINT_MISC,"new_msgs=%d,tst_msg2->i=%d,tst_msg2->lastvar=%d\n",new_msgs,tst_msg2->i,tst_msg2->lastvar);
	      }
	    if(tst_msg2->first_count != tst_msg2->last_count)
	      {
		fprintf(stderr,"tst_msg2->first_count(%d) != tst_msg2->last_count(%d)\n", tst_msg2->first_count, tst_msg2->last_count);
		return(117);
	      }
	    if(tst_msg2->lastvar != expected_last_var)
	      {
		fprintf(stderr,"expected_last_var(%d) != tst_msg2->last_var(%ld)\n",
			expected_last_var, tst_msg2->lastvar);
		return(116);
	      }
	  }
	break;
	  
	case 0:
	  break;
	  
	case -1:
	default:
	  fprintf(stderr,"Bad return value from read.\n");
	  return(115);
	}
      if(repeat_delay_ms >1)
	{
	  esleep(repeat_delay);
	}
      else if(i % 100 == 0)
	{
	  taskDelay(1);
	}
    }
  rcs_print("BufferLine=%s\n",
	    _nml_ptr->cms->BufferLine);
  rcs_print("ProcessLine=%s\n",
	    _nml_ptr->cms->ProcessLine);
  rcs_print("i=%d,elapsed_time=%f,total_rtime=%f,diffr=%f,avg_diffr=%f,maxr=%f,minr=%f,diffc=%f,avg_diffc=%f,maxc=%f,minc=%f,new_msgs=%d,vx_test_read_nml_ptr->read() returned %d\n",i,elapsed_time,total_rtime,diffr,avg_diffr,maxr,minr,diffc,avg_diffc,maxc,minc,new_msgs,t);
  rcs_print("vx_test_nml_read_second_part(NML *_nml_ptr=%p,int repeat_count=%d,long expected_last_var=%d, int repeat_delay_ms=%d) returning.\n",
	    _nml_ptr,repeat_count,expected_last_var,repeat_delay_ms);
  vx_test_read_tid=0;
  return(0);
}

extern "C" int vx_test_nml_read_cleanup(void);

int vx_test_nml_read_cleanup(void)
{
  if(vx_test_read_tid)
    {
      rcs_print("vx_test_read_tid=%d\n",vx_test_read_tid);
      taskDelete(vx_test_read_tid);
      vx_test_read_tid=0;
    }
  NML *_nml = vx_test_read_nml_ptr;
  rcs_print("%s:%d %s vx_test_read_nml_ptr=%p\n",
	    __FILE__,__LINE__, __FUNCTION__, vx_test_read_nml_ptr);
  vx_test_read_nml_ptr=0;
  if(_nml)
    {
      delete _nml;
    }
  return(0);
}





