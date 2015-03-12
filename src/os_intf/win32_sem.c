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

/* Semaphore functions for MS_WINDOWS_API  (Windows NT/95/98 and CE) */

#ifdef HAVE_CONFIG_H
#ifndef PACKAGE
#include "rcs_config.h"
#endif
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"

#if !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 *  compiler errors result. */
#include <winsock2.h>
#endif

#include <windows.h>
#include <winbase.h>

#ifndef NO_STDIO
#include <stdio.h>
#endif

/* HAVE_CONFIG_H */
#endif

#include "_sem.h"
#include "dbg_mem.h"
#include "rcs_prnt.hh"

int
rcs_sem_clear (rcs_sem_t * sem)
{
  /* Un implimented. */
  return (0);
}

rcs_sem_t *
rcs_sem_open_w_ulong_key (unsigned long _key, int oflag, int mode,int blocking_type)
{
  return rcs_sem_create_w_ulong_key(_key,oflag,mode,blocking_type);
#if 0
  unsigned long int id = _key;
  rcs_sem_t *sem;
  int last_error;
  int tries = 0;
  sem = (rcs_sem_t *) DEBUG_MALLOC (sizeof (rcs_sem_t));
  if (NULL == sem)
    {
      rcs_print_error ("Out of memory. (Error = %ld\n)", GetLastError ());
      return NULL;
    }

#ifndef NO_STDIO
  SNPRINTF_FUNC ( SNPRINTF_ARGS(sem->name,sizeof(sem->name)), 
		  "sem%d", (int) id);
#else
  strcpy (sem->name, "sem");
  _itoa (id, sem->name + 3, 10);
#endif
  sem->handle = NULL;
  last_error = 2;
  while ( NULL == sem->handle && tries < 10 && last_error == 2)
    {
      sem->handle = OpenMutex (MUTEX_ALL_ACCESS, TRUE, sem->name);
      last_error = GetLastError();
      if (NULL == sem->handle)
	{
	    SleepEx (((unsigned long) (1000)), FALSE);
	}
      tries++;
    }
  if (NULL == sem->handle)
    {
      sem->handle = OpenMutex (MUTEX_ALL_ACCESS, TRUE, sem->name);
    }
  if (NULL == sem->handle)
    {  
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,"OpenMutex(MUTEX_ALL_ACCESS, TRUE,%s)  failed.\n",sem->name);
      free (sem);
      return NULL;
    }
  return sem;
#endif
}

rcs_sem_t *
rcs_sem_create_w_ulong_key (unsigned long int id, int mode, int state, int blocking_type)
{
  rcs_sem_t *sem;
  SECURITY_ATTRIBUTES sa;
  SECURITY_DESCRIPTOR sd;
  if (FALSE ==
      InitializeSecurityDescriptor (&sd, SECURITY_DESCRIPTOR_REVISION))
    {
      rcs_print_error
	("Can not initailize security descriptor.(Error = %ld)\n",
	 GetLastError ());
      return NULL;
    }
  sa.nLength = sizeof (SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = &sd;
  sa.bInheritHandle = TRUE;
  sem = (rcs_sem_t *) DEBUG_MALLOC (sizeof (rcs_sem_t));
  if (NULL == sem)
    {
      rcs_print_error ("Out of memory. (Error = %ld\n)", GetLastError ());
      return NULL;
    }
#ifndef NO_STDIO
  SNPRINTF_FUNC ( SNPRINTF_ARGS(sem->name,sizeof(sem->name)),
		  "sem%d", (int) id);
#else
  strcpy (sem->name, "sem");
  _itoa (id, sem->name + 3, 10);
#endif
  sem->handle = CreateMutex (&sa, FALSE, sem->name);

  if (NULL == sem->handle)
    {
      rcs_print_error ("Can not create semaphore. (Error = %ld)\n",
		       GetLastError ());
      free (sem);
      return NULL;
    }
  return sem;
}

#if 0
/* sem_init is for unnamed posix semaphores  which should never be used.
 * We need to communicate between unrelated processes. */

int
rcs_sem_init (rcs_sem_t * sem, int pshared, unsigned int value)
{
  return 0;
}
#endif

int
rcs_sem_destroy (rcs_sem_t * sem)
{
  if (NULL != sem)
    {
      if(sem->name[0] == 's' && 
	 sem->name[1] == 'e' &&
	 sem->name[2] == 'm')
	{
	  if (NULL != sem->handle)
	    {
	      CloseHandle (sem->handle);
	      sem->handle = NULL;
	    }
	}
    }
  return 0;
}

int
rcs_sem_close (rcs_sem_t * sem)
{
  if (NULL != sem)
    {
      if(sem->name[0] == 's' && 
	 sem->name[1] == 'e' &&
	 sem->name[2] == 'm')
	{
	  if (NULL != sem->handle)
	    {
	      CloseHandle (sem->handle);
	      sem->handle = NULL;
	    }
	  memset(sem,0,sizeof(rcs_sem_t));
	  DEBUG_FREE(sem);
	}
    }
  return 0;
}

int
rcs_sem_close_leaving_resource (rcs_sem_t * sem)
{
  return rcs_sem_close(sem);
}

int
rcs_sem_unlink (const char *name)
{
  rcs_sem_t *sem = (rcs_sem_t *) name;
  return rcs_sem_close (sem);
}

int
rcs_sem_wait (rcs_sem_t * sem, double timeout, int *restart_after_int)
{
  int error;
  if (sem == NULL)
    {
      return -1;
    }
  if (sem->handle == NULL)
    {
      return -1;
    }
  if (WAIT_FAILED == WaitForSingleObject (sem->handle,
					  ((unsigned long) (timeout * 1000))))
    {
      if (WAIT_TIMEOUT == (error = GetLastError ()))
	{
	  return -2;
	}
      rcs_print_error ("WaitForSingleObject failed. (Error = %d)\n", error);
      return -1;
    }
  return 0;
}


int
rcs_sem_trywait (rcs_sem_t * sem)
{
  return rcs_sem_wait (sem, 0,0);
}

int
rcs_sem_post (rcs_sem_t * sem)
{
  if (FALSE == ReleaseMutex (sem->handle))
    {
      rcs_print_error ("ReleaseMutex failed. (Error = %ld)\n",
		       GetLastError ());
      return -1;
    }
  return 0;
}

int
rcs_sem_flush (rcs_sem_t * sem)
{
  if (FALSE == ReleaseMutex (sem->handle))
    {
      rcs_print_error ("ReleaseMutex failed. (Error = %ld)\n",
		       GetLastError ());
      return -1;
    }
  return 0;
}


int
rcs_sem_getvalue (rcs_sem_t * sem, unsigned int *sval)
{
  if (WAIT_FAILED == WaitForSingleObject (sem->handle, 0))
    {
      return 1;
    }
  return 0;
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
  return(0.01);
}
