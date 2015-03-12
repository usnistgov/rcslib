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


#include <vxWorks.h>
#include <taskLib.h>
#include <logLib.h>
#include <tickLib.h>

int vxh_id=0;
int vxh_stop_flag=0;

int vxhStop()
{
  if(!vxh_stop_flag)
    {
      vxh_stop_flag=1;
      taskDelay(10);
    }
  if(vxh_id)
    {
      taskDelete(vxh_id);
      vxh_id=0;
    }
}

int first_vxh_tick=0;
int vxh_count=0;
int vxh_tick1=0;
int last_vxh_tick1=0;
int vxh_tick_diff;
int max_vxh_tick_diff=-1000;
int min_vxh_tick_diff=100000;
int vxh_total_ticks=0;

int vxh_task()
{
  vxh_count=0;
  max_vxh_tick_diff=-1000;
  min_vxh_tick_diff=100000;
  vxh_total_ticks=0;
  taskDelay(10);
  first_vxh_tick=tickGet();
  while(!vxh_stop_flag)
    {
      vxh_tick1 =tickGet();
      if(vxh_count > 0)
	{
	  vxh_tick_diff = vxh_tick1 - last_vxh_tick1;
	  if(min_vxh_tick_diff > vxh_tick_diff)
	    {
	      min_vxh_tick_diff = vxh_tick_diff;
	    }
	  if(max_vxh_tick_diff < vxh_tick_diff)
	    {
	      max_vxh_tick_diff = vxh_tick_diff;
	    }
	}
      last_vxh_tick1=vxh_tick1;
      if(vxh_count % 2000 == 2)
	{
	  vxh_total_ticks=vxh_tick1 - first_vxh_tick;
	  logMsg("vxh_count=%d,vxh_total_ticks=%d,max_vxh_tick_diff=%d,min_vxh_tick_diff=%d\n",
		 vxh_count,vxh_total_ticks,max_vxh_tick_diff,min_vxh_tick_diff,0,0);
	}
      taskDelay(1);
      vxh_count++;
    }
  vxh_total_ticks=vxh_tick1 - first_vxh_tick;
  logMsg("vxh_count=%d,vxh_total_ticks=%d,max_vxh_tick_diff=%d,min_vxh_tick_diff=%d\n",
	 vxh_count,vxh_total_ticks,max_vxh_tick_diff,min_vxh_tick_diff,0,0);
  vxh_id=0;
  return 0;
}
      
int tid;

int vxhGo()
{
  vxhStop();
  tid = taskNameToId("tShell");
  taskPrioritySet(tid,4);
  tid = taskNameToId("tLogTask");
  taskPrioritySet(tid,3);
  tid = taskNameToId("tExcTask");
  taskPrioritySet(tid,3);
  tid = taskNameToId("tTelnetd");
  taskPrioritySet(tid,5);
  tid = taskNameToId("tTelnetOutTask");
  taskPrioritySet(tid,6);
  tid = taskNameToId("tTelnetInTask");
  taskPrioritySet(tid,6);
  tid = taskNameToId("tWdbTask");
  taskPrioritySet(tid,7);

  vxh_stop_flag=0;
  vxh_count=0;
  vxh_id =  taskSpawn(0, //name
		1,		// priority
		VX_FP_TASK,	// options
		0x2000,		// stack size
		(FUNCPTR) vxh_task, // entry point
		      0,0,0,0,0,
		      0,0,0,0,0);
  return(0);
}













