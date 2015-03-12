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

#include <semLib.h>
#include "globmem.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <taskLib.h>

static bool sigint_occured=false;
static void (*old_sigint_handler)(int)=0;

int vx_test_write_tid=0;

static void my_sigint_handler(int sig)
{
  sigint_occured=true;
  if(old_sigint_handler && old_sigint_handler != SIG_IGN &&
     old_sigint_handler != SIG_DFL)
    {
      (*old_sigint_handler)(sig);
    }
  sigint_occured=true;
}

extern "C" int
vx_nml_test_write(const char *buf, 
		  const char *proc, 
		  const char *cfg,
		  long last_var,
		  int repeat_count,
		  int repeat_delay_ms);

class NML *vx_test_write_nml_ptr=0;

extern int vx_test_write_use_simpler_msg;
int vx_test_write_use_simpler_msg=0;

extern "C" int vx_test_nml_write_second_part(NML *_nml_ptr,
					     int repeat_count,
					     long last_var,
					     int repeat_delay_ms);


extern volatile int vx_nml_test_write_quit_flag;
volatile int vx_nml_test_write_quit_flag=0;

extern "C" int set_vx_nml_test_write_quit_flag(int new_flag);
extern "C" int get_vx_nml_test_write_quit_flag(void);

int
set_vx_nml_test_write_quit_flag(int new_flag)
{
  vx_nml_test_write_quit_flag=new_flag;
  return(0);
}

int
get_vx_nml_test_write_quit_flag(void)
{
  return vx_nml_test_write_quit_flag;
}

extern int vx_nml_test_write_priority;
int vx_nml_test_write_priority=70;

extern "C"  int set_vx_nml_test_write_priority(int new_priority);
extern "C"  int get_vx_nml_test_write_priority(void);

int set_vx_nml_test_priority(int new_priority)
{
  vx_nml_test_write_priority=new_priority;
  return 0;
}

int get_vx_nml_test_write_priority(void)
{
  return vx_nml_test_write_priority;
}


int
vx_nml_test_write(const char *buf, 
		  const char *proc, 
		  const char *cfg,
		  long last_var,
		  int repeat_count,
		  int repeat_delay_ms)
{
  if(get_vx_nml_test_write_quit_flag())
    {
      return 1;
    }

  //  set_rcs_print_destination(RCS_PRINT_TO_STDOUT); 
  // set_rcs_print_flag(PRINT_EVERYTHING);

  rcs_print("vx_nml_test_write(const char *buf=%s, const char *proc=%s, const char *cfg=%s, long last_var=%d, int repeat_count=%d, int repeat_delay_ms=%d) called.\n",
	    buf,proc,cfg,last_var,repeat_count,repeat_delay_ms);

  vx_test_write_nml_ptr = new NML(nml_test_format,buf,proc,cfg);
  rcs_print_debug(PRINT_MISC,"vx_test_write_nml_ptr=%p\n",
	    vx_test_write_nml_ptr);
  rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);
  if(vx_test_write_nml_ptr == 0)
    {
      return(126);
    }
  rcs_print_debug(PRINT_MISC,"vx_test_write_nml_ptr->valid()=%d\n",
	    vx_test_write_nml_ptr->valid());
  rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);
  if(!vx_test_write_nml_ptr->valid())
    {
      fprintf(stderr,"vx_test_write_nml_ptr->valid() check failed.\n");
      return(125);
    }
  int t;
  TEST_MESSAGE tst_msg;
  rcs_print_debug(PRINT_MISC,"tst_msg.size=%ld\n",tst_msg.size);
  SIMPLER_MSG smsg;
  rcs_print_debug(PRINT_MISC,"smsg.size=%ld\n",smsg.size);
  rcs_print_debug(PRINT_MISC,"vx_test_write_use_simpler_msg=%d\n",
	    vx_test_write_use_simpler_msg);
  rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);
  fflush(stdout);
  if(vx_test_write_use_simpler_msg != 0)
    {
      if((t = vx_test_write_nml_ptr->write(smsg)))
	{
	  rcs_print_debug(PRINT_MISC,"vx_test_write_nml_ptr->write() returned %d\n", t);
	  fflush(stdout);
	  return(124);
	}
    }
  else
    {
      tst_msg.lastvar= last_var;
      tst_msg.first_count = tst_msg.last_count = last_var;
      rcs_print_debug(PRINT_MISC,"calling vx_test_write_nml_ptr->write(&tst_msg);\n");
      fflush(stdout);
      t = vx_test_write_nml_ptr->write(&tst_msg);
      rcs_print_debug(PRINT_MISC,"t=%d\n",t);
      fflush(stdout);
      if(t)
	{
	  rcs_print_debug(PRINT_MISC,"vx_test_write_nml_ptr->write() returned %d\n", t);
	  return(123);
	}
    }
  rcs_print_debug(PRINT_MISC,"&use_dma=%p\n", ((void*)  &(((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma)));
  rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);
  rcs_print_debug(PRINT_MISC,"repeat_count=%d\n",repeat_count);
  fflush(stdout);
  if(repeat_count != 0)
    {
      rcs_print_debug(PRINT_MISC,"calling taskSpawn\n"); 
      vx_test_write_tid= taskSpawn(0, //name
		vx_nml_test_write_priority,		// priority
		VX_FP_TASK,	// options
		0x2000,		// stack size
		(FUNCPTR) vx_test_nml_write_second_part, // entry point
		((int) vx_test_write_nml_ptr), // arg 1
		((int)repeat_count), // arg 2
		((int) last_var), // arg 3
		(int)repeat_delay_ms, // arg 4
		0,0,0,0,0,0);
      rcs_print_debug(PRINT_MISC,"vx_nml_test_write waiting for quit flag.\n");
      while(!get_vx_nml_test_write_quit_flag())
	{
	  esleep(0.5);
	}
#if 0
      vx_test_nml_write_second_part(vx_test_write_nml_ptr,repeat_count,last_var,repeat_delay_ms);
#endif      
    }
  rcs_print_debug(PRINT_MISC,"vx_nml_test_write returning.\n");
  return(0);
}

int vx_test_nml_write_second_part(NML *_nml_ptr,int repeat_count,long last_var, int repeat_delay_ms)
{
  static  TEST_MESSAGE tst_msg;
  static  SIMPLER_MSG smsg;
  static  int i=0;
  static  int t=0;
  static  double repeat_delay;
  static  double start_time;
  static  double diffw=0;
  static  double maxw=-1e14;
  static  double minw=1e14;
  static  double diffc=0;
  static  double maxc=-1e14;
  static  double minc=1e14;
  static  double t1,last_t1,t2;
  static  double elapsed_time=0;
  static  double total_wtime=0;
  static  double avg_diffw;
  static  double avg_diffc;
  static int print_count;

  rcs_print_debug(PRINT_MISC,"vx_test_nml_write_second_part(NML *_nml_ptr=%p,int repeat_count=%d,long last_var=%d, int repeat_delay_ms=%d) called.\n",
	    _nml_ptr,repeat_count,last_var,repeat_delay_ms);
  rcs_print_debug(PRINT_MISC,"&use_dma=%p\n", ((void*)  &(((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma)));
  rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);

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
  repeat_delay= 0.001*repeat_delay_ms;
  tst_msg.lastvar=last_var;
  sigint_occured=0;
  start_time=etime();
  i=0;
  t=0;
  diffw=0;
  maxw=-1e14;
  minw=1e14;
  diffc=0;
  maxc=-1e14;
  minc=1e14;
  t1,last_t1,t2;
  elapsed_time=0;
  total_wtime=0;
  
  for(i = 0 ;
      ((i < repeat_count || repeat_count  < 0) && !sigint_occured && ! get_vx_nml_test_write_quit_flag()) ; i++)
    {
      tst_msg.i=i;
      tst_msg.first_count = tst_msg.last_count = i;
      t1=etime();
      //      rcs_print_debug(PRINT_MISC,"use_dma=%d\n", ((GLOBMEM *) (vx_test_write_nml_ptr->cms))->use_dma);
      //rcs_print_debug(PRINT_MISC,"t1=%f\n",t1);
      if((t = _nml_ptr->write(tst_msg)))
	{
	  fprintf(stderr,"_nml_ptr->write() returned %d\n", t);
	  return(124);
	}
      t2=etime();
      //rcs_print_debug(PRINT_MISC,"t2=%f\n",t2);
      diffw = t2 - t1;
      total_wtime += diffw;
      if(maxw < diffw)
	{
	  maxw = diffw;
	}
      if(minw> diffw)
	{
	  minw = diffw;
	}
      avg_diffw = total_wtime/(i+1);
      elapsed_time=t2 - start_time;
      if(i > 0)
	{
	  diffc = t1 - last_t1;
	  if(maxc < diffc)
	    {
	      maxc = diffc;
	    }
	  if(minc> diffc)
	    {
	      minc = diffc;
	    }
	  avg_diffc = elapsed_time/i;
	}
      last_t1=t1;
      if((i% print_count) == 2)
	{
	  rcs_print("t1=%f,tst_msg.i=%d, \ttst_msg.lastvar=%ld\n",
		    t1,tst_msg.i,tst_msg.lastvar);
	  rcs_print("i=%d,elapsed_time=%f,total_wtime=%f,diffw=%f,avg_diffw=%f,maxw=%f,minw=%f,diffc=%f,avg_diffc=%f,maxc=%f,minc=%f\n",
		    i,elapsed_time,total_wtime,diffw,avg_diffw,maxw,minw,diffc,avg_diffc,maxc,minc);
	}
      if(repeat_delay_ms > 1)
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
  rcs_print("t1=%f,tst_msg.i=%d, \ttst_msg.lastvar=%ld\n",
	    t1,tst_msg.i,tst_msg.lastvar);
  rcs_print("i=%d,elapsed_time=%f,total_wtime=%f,diffw=%f,avg_diffw=%f,maxw=%f,minw=%f,diffc=%f,avg_diffc=%f,maxc=%f,minc=%f\n",
	    i,elapsed_time,total_wtime,diffw,avg_diffw,maxw,minw,diffc,avg_diffc,maxc,minc);
  rcs_print("vx_test_nml_write_second_part(NML *_nml_ptr=%p,int repeat_count=%d,long last_var=%d, int repeat_delay_ms=%d) returning.\n",
	    _nml_ptr,repeat_count,last_var,repeat_delay_ms);
  fflush(stdout);
  return(0);
}


extern "C" int vx_test_nml_write_cleanup(void);

int 
vx_test_nml_write_cleanup(void)
  {
    rcs_print("%s:%d %s vx_test_write_nml_ptr=%p\n",
	      __FILE__,__LINE__,__FUNCTION__,vx_test_write_nml_ptr);
    NML *_nml = vx_test_write_nml_ptr;
    if(vx_test_write_nml_ptr)
      {
	vx_test_write_nml_ptr=0;
	delete _nml;
      }
    if(vx_test_write_tid)
      {
	rcs_print("vx_test_write_tid=%d\n",vx_test_write_tid);
	taskDelete(vx_test_write_tid);
	vx_test_write_tid=0;
      }
    return(0);
  }





