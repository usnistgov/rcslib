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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#include <vxWorks.h>		/* ERROR */
#include <stdio.h>		/* NULL */
#include <stddef.h>		/* sizeof() */
#include <stdlib.h>		/* malloc() */
#include <semLib.h>		/* generic semaphore interface */
#include <sysLib.h>		/* sysClkRateGet() */
#include <taskLib.h>		/* taskLock(),taskUnlock(), taskIdSelf() */
#include "_table.h"		/* _RCS_TABLE, C lang version */
#include "_timer.h"		/* convertSecondsToTicks() */
#include "_sem.h"
#include "dbg_mem.h"		/* DEBUG_MALLOC,DEBUG_CALLOC, DEBUG_FREE */
#include "rcs_prnt.hh"		/* rcs_print_error */

int task_lock_sem_stuff = 1;

/* OS table of semaphore keys and rcs_sem_t's */
_RCS_TABLE rcs_sem_table;
int rcs_sem_table_inited = 0;
int print_rcs_sem_warnings = 1;

/* shell debug functions */

void
rcs_sem_table_print ()
{
  if (!rcs_sem_table_inited)
    {
      printf ("RCS Semaphore table NOT initialized.\n");
      return;
    }
  rcs_print("RCS Semaphore Table\n");
  table_print (&rcs_sem_table);
  printf ("\tid\tkey\t(struct semaphore *)\n");
}

void
rcs_sem_table_clear ()
{
  if (task_lock_sem_stuff)
    taskLock ();
  rcs_sem_table_inited = 0;
  table_clearall (&rcs_sem_table);
  if (task_lock_sem_stuff)
    taskUnlock ();
}

/* POSIX semaphore functions */



#if 0
  // sem_init is for unnamed posix semaphores  which should never be used.
  // We need to communicate between unrelated processes.
int
rcs_sem_init (rcs_sem_t * sem, int pshared, unsigned int value)
{
  if (NULL == sem)
    {
      return ERROR;
    }
  return -1;			/* can't do it */
}

#endif

int
rcs_sem_destroy (rcs_sem_t * sem)
{
  int retval = 0;
  if (task_lock_sem_stuff)
    taskLock ();
  if (NULL == sem)
    {
      if (task_lock_sem_stuff)
	taskUnlock ();
      return ERROR;
    }
  if (((int) sem->sem_id) <= 0 || ((int) sem->sem_id) == ERROR)
    {
      if (task_lock_sem_stuff)
	taskUnlock ();
      return ERROR;
    }
  retval = semDelete (sem->sem_id) == ERROR ? -1 : 0;
  table_clear (&rcs_sem_table, sem->key);
  sem->sem_id = NULL;

  if (task_lock_sem_stuff)
    taskUnlock ();
  sem->sem_id = NULL;
  return retval;
}


int
rcs_sem_clear (rcs_sem_t * sem)
{
  /* Un implimented. */
  if (NULL == sem)
    {
      return ERROR;
    }
  return (0);
}



rcs_sem_t *
rcs_sem_open (const char *name, int oflag, /* int mode */ ...)
{
  int tid = taskIdSelf ();
  rcs_sem_t *sem = NULL;
  sem = (rcs_sem_t *) DEBUG_MALLOC (sizeof (rcs_sem_t));
  sem->sem_id = (SEM_ID) ERROR;

  if (task_lock_sem_stuff)
    taskLock ();

  /* now need to register the sem key with a server, so that subsequent
     open's can get the same key */
  if (!rcs_sem_table_inited)
    {
      /* create a new table of rcs_sem_t *'s */
      table_new (&rcs_sem_table, sizeof (rcs_sem_t));
      rcs_print_error ("Semaphore table not initialized.\n");
      rcs_sem_table_inited = 1;
      if (task_lock_sem_stuff)
	taskUnlock ();
      return NULL;
    }


  if (table_get (&rcs_sem_table, (unsigned long int) name, sem) < 0)
    {
      if (task_lock_sem_stuff)
	taskUnlock ();
      rcs_print_error
	("Error: RCS Semaphore table contains no entry for (%ld(0x%lX)).\n",
	 sem->key, sem->key);
      rcs_print_error
	("Error: Make this process(%d(0x%X)) the master or start the master first to avoid this Error.\n",
	 tid, tid);
      DEBUG_FREE (sem);
      sem = NULL;
      return NULL;
    }

  if (task_lock_sem_stuff)
    taskUnlock ();

  if (sem->key != (int) name)
    {
      rcs_print_error
	("Semaphore retrieved from table does not have the correct key. (%ld) != (%d).\n", sem->key, ((int) name));
      DEBUG_FREE (sem);
      sem = NULL;
      return NULL;
    }

  if (((int) sem->sem_id) <= 0 || ((int) sem->sem_id) == ERROR)
    {
      rcs_print_error ("Bad id for the semaphore in the table.\n");
      table_clear (&rcs_sem_table, sem->key);
      DEBUG_FREE (sem);
      sem = NULL;
      return NULL;
    }
  return sem;
}

rcs_sem_t *rcs_sem_open_w_ulong_key (unsigned long int key, 
				     int oflag,int mode, int blocking_type)
{
  return rcs_sem_open((const char *)key,oflag,mode);
}

int
rcs_sem_close (rcs_sem_t * sem)
{
  sem->sem_id = NULL;
  DEBUG_FREE (sem);
  return (0);
}

int
rcs_sem_flush (rcs_sem_t * sem)
{
  if (NULL == sem)
    {
      return -1;
    }
  if (((SEM_ID) NULL) == sem->sem_id || ((SEM_ID) ERROR) == sem->sem_id)
    {
      return -1;
    }
  return (semFlush (sem->sem_id) == ERROR ? -1 : 0);
}

int
rcs_sem_unlink (const char *name)
{
  return 0;			/* no unlinking, since no file sys sems */
}

int
rcs_sem_wait (rcs_sem_t * sem, double sem_timeout, int *restart_int_ptr)
{
  int ticks = 0;
  int semTakeReturnValue = 0;


  /* Make sure the sem is valid */
  if (NULL == sem)
    {
      return -1;
    }
  if (((int) sem->sem_id) <= 0 || ERROR == ((int) sem->sem_id)
      || sem->key < 1)
    {
      return -1;
    }


  ticks = covertSecondsToTicks (sem_timeout);

  /* Call the VxWorks specific semaphore function. */
  semTakeReturnValue = semTake (sem->sem_id, ticks);

  if (semTakeReturnValue == ERROR)
    {
      if (ticks == NO_WAIT)
	{
	  return -1;
	}
      else
	{
	  return -2;		/* Assume the error was a timeout. */
	}
    }
  return 0;
}

int
rcs_sem_trywait (rcs_sem_t * sem)
{
  if (NULL == sem)
    {
      return -1;
    }
  if (sem->sem_id <= 0 || ERROR == ((int) sem->sem_id) || sem->key < 1)
    {
      return -1;
    }
  return semTake (sem->sem_id, NO_WAIT) == ERROR ? -1 : 0;
}

int
rcs_sem_post (rcs_sem_t * sem)
{
  if (NULL == sem)
    {
      return -1;
    }
  if (((int) sem->sem_id) <= 0 || ERROR == ((int) sem->sem_id)
      || sem->key < 1)
    {
      return -1;
    }
  return semGive (sem->sem_id) == ERROR ? -1 : 0;
}

/* #include <private/semLibP.h> */
int
rcs_sem_getvalue (rcs_sem_t * sem, unsigned int *sval)
{
  return -1;			/* Function not implemented. */
#if 0
  if (NULL == sem)
    {
      return -1;
    }
  if (((int) sem->sem_id) <= 0 || ERROR == sem->sem_id || sem->key < 1)
    {
      return -1;
    }
  return sem->sem->semCount;
#endif
}

rcs_sem_t *
rcs_sem_create (unsigned long int key, int mode, int state)
{
  rcs_sem_t *sem = NULL;
  int tid = taskIdSelf ();
  if (task_lock_sem_stuff)
    taskLock ();
  sem = (rcs_sem_t *) DEBUG_MALLOC (sizeof (rcs_sem_t));
  sem->sem_id = NULL;

  if (table_get (&rcs_sem_table, (unsigned long int) key, sem) >= 0)
    {
      if (sem->key != (int) key)
	{
	  if (task_lock_sem_stuff)
	    taskUnlock ();
	  rcs_print_error
	    ("Semaphore retrieved from table does not have the correct key. (%ld) != (%d).\n", sem->key, ((int)key));
	  return NULL;
	}
      if (((int) sem->sem_id) == ERROR || ((int) sem->sem_id) <= 0)
	{
	  table_clear (&rcs_sem_table, sem->key);
	  DEBUG_FREE (sem);
	  return NULL;
	}
      if (task_lock_sem_stuff)
	taskUnlock ();
      if (print_rcs_sem_warnings)
	{
	  rcs_print_error ("\n");
	  rcs_print_error
	    ("Warning: This process(%d(0x%X)) was configured to create a semaphore with key = (%ld(0x%lX)),\n",
	     tid, tid, sem->key, sem->key);
	  rcs_print_error
	    ("Warning: but a semaphore with this key has already been created.\n");
	  rcs_print_error ("Warning: The existing semaphore will be used.");
	  rcs_print_error
	    ("Warning: Make this process a non-master or start it first to avoid this warning.\n");
	  rcs_print_error
	    ("Warning: You may also set print_rcs_sem_warnings=0 to avoid this warning.");
	  rcs_print_error ("\n");
	}
      return sem;
    }


  /* ignore oflag and mode, since no ownership; state is set
     to full in semMCreate, so state arg has no effect */
  sem->key = key;
  if (state)
    {
      sem->sem_id = semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE);
      if (NULL == sem->sem_id)
	{
	  rcs_print_error
	    ("semMCreate(SEM_Q_PRIORITY | SEM_INVERSION_SAFE) returned NULL.\n");
	  return NULL;
	}
    }
  else
    {
      sem->sem_id = semBCreate (SEM_Q_PRIORITY, SEM_EMPTY);
      if (NULL == sem->sem_id)
	{
	  rcs_print_error
	    ("semBCreate(SEM_Q_PRIORITY, SEM_EMPTY) returned NULL.\n");
	  return NULL;
	}
    }
  /* now need to register the sem key with a server, so that subsequent
     open's can get the same key */
  if (!rcs_sem_table_inited)
    {
      /* create a new table of rcs_sem_t *'s */
      table_new (&rcs_sem_table, sizeof (rcs_sem_t));
      rcs_sem_table_inited = 1;
    }
  table_add (&rcs_sem_table, key, sem);
  if (task_lock_sem_stuff)
    taskUnlock ();
  return sem;
}

rcs_sem_t *rcs_sem_create_w_ulong_key (unsigned long int key, int mode, int state, int blocking_type)
{
  return rcs_sem_create(key,mode,state);
}

int rcs_sem_inc_waiting(volatile rcs_sem_t * sem)
{
  return 0;
}

int rcs_sem_dec_waiting(volatile rcs_sem_t * sem)
{
  return 0;
}

double rcs_sem_get_default_bsem_wait(void)
{
  return 0.0;
}

int
rcs_sem_close_leaving_resource (rcs_sem_t * sem)
{
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "rcs_sem_close_leaving_resource(%p)\n",
		   (void*)sem);
  return(0);
}
