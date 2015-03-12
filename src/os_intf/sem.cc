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

/*
  This software was produced by the National Institute of Standards and
  Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this
  software assume all responsibility associated with its operation,
  modification, maintenance, and subsequent redistribution.
  */

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
extern "C"
{
#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif
#include "_sem.h"
}
#endif

#include "_sem.h"
#include "sem.hh"
#include "rcs_prnt.hh"		// rcs_print_debug(), PRINT_SEMAPHORE_ACTIVITY
#include "timer.hh"		// etime()


RCS_SEMAPHORE::RCS_SEMAPHORE (unsigned long int _id, int _oflag, double _time,
			      int _mode, int _state, bool _blocking_type):
  id(_id),timeout(_time),oflag(_oflag),mode(_mode),state(_state),sem(0),sval(0),leave_resource(false),
  blocking_type(_blocking_type),interrupting_operation(false),restart_after_interrupt(1)
{
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
		   "Constructing RCS_SEMAPHORE(%lu,0x%X,%f,0x%X,0x%X,%d):  this=%p\n",
		   _id, _oflag, _time, _mode, _state,_blocking_type,(void*) this);

  /* save the constructor args */
  id = _id;
  oflag = _oflag;
  mode = _mode;
  state = _state;
  timeout = _time;
  blocking_type=_blocking_type;
  interrupting_operation=false;
  restart_after_interrupt=1;
  leave_resource=false;

  if (oflag & RCS_SEMAPHORE_CREATE)
    {
      sem = rcs_sem_create_w_ulong_key (id, mode, state,(int) blocking_type);
    }
  else
    {
      sem = rcs_sem_open_w_ulong_key (id, 0,mode, (int)blocking_type);
    }

  if (sem == NULL)
    {
      rcs_print_error
	("can't create semaphore (id = %lu, oflag = %d, timeout = %f, mode = 0x%X, state = %d)\n",
	 id, oflag, timeout, mode, state);
    }
}

int
RCS_SEMAPHORE::valid ()
{
  return (sem != NULL);
}

RCS_SEMAPHORE::~RCS_SEMAPHORE ()
{
  rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		   "Destructing RCS_SEMAPHORE:  this=%p\n", (void *) this);
  if (sem == NULL)
    return;

  /* need to destroy the semaphore before closing our copy */
  if(!leave_resource)
    {
      if (oflag & RCS_SEMAPHORE_CREATE && !leave_resource)
	{
#ifdef LYNX
	  char filename[32];
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(filename,sizeof(filename)),
			  "/tmp/sem%d", id);
	  unlink (filename);
#else
	  rcs_sem_destroy (sem);
#endif
	}
      rcs_sem_close (sem);
    }
  else
    {
      rcs_sem_close_leaving_resource(sem);
    }
  sem = NULL;
}

int
RCS_SEMAPHORE::wait ()
{
  int retval;
#ifdef DEBUG
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "semaphore wait started  at %lf\n", etime ());
#endif
  if (sem == NULL)
    {
      rcs_print_error("RCS_SEMAPHORE::wait() -- sem == NULL\n");
      return -1;
    }
  if (interrupting_operation)
    {
      rcs_print_error("RCS_SEMAPHORE::wait() interrupting_operation=%d\n",
		      interrupting_operation);
      return -1;
    }
  restart_after_interrupt=1;
  retval = rcs_sem_wait (sem, timeout,&restart_after_interrupt);
#ifdef DEBUG
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "semaphore wait finished  at %lf\n", etime ());
#endif
  return retval;
}

int
RCS_SEMAPHORE::trywait ()
{
  if (sem == NULL || interrupting_operation)
    return -1;
  return rcs_sem_trywait (sem);
}

int
RCS_SEMAPHORE::post ()
{
#ifdef DEBUG
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY, "semaphore posted at %lf\n",
		   etime ());
#endif
  if (sem == NULL)
    return -1;
  return rcs_sem_post (sem);
}

int
RCS_SEMAPHORE::flush ()
{      
#ifdef DEBUG
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY, "semaphore flushed at %lf\n",
		   etime ());
#endif
  if (sem == NULL)
    return -1;
  return rcs_sem_flush (sem);
}

int
RCS_SEMAPHORE::getvalue ()
{
  if (sem == NULL)
    return -1;
  return rcs_sem_getvalue (sem, &sval);
}

int
RCS_SEMAPHORE::setflag (int _oflag)
{
  oflag = _oflag;		/* we can reset whether this was the
				   one who created the semaphore */
  return (0);
}

int
RCS_SEMAPHORE::clear ()
{
  return rcs_sem_clear (sem);
}

void RCS_SEMAPHORE::interrupt_operation()
{
  flush();
  interrupting_operation=true;
  restart_after_interrupt=0;
}


void RCS_SEMAPHORE::clear_interrupt_operation()
{
  interrupting_operation=false;
  restart_after_interrupt=1;
}

void
RCS_SEMAPHORE::inc_waiting()
{
  rcs_sem_inc_waiting(sem);
}

void
RCS_SEMAPHORE::dec_waiting()
{
  rcs_sem_dec_waiting(sem);
}



