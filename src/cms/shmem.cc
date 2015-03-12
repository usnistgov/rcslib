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

/*************************************************************************
* File: shmem.cc                                                         *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for the Communication Management System (CMS).       *
*          Includes:                                                     *
*                    1. Member Functions for class SHMEM.                *
* Notes: The class SHMEM should be used by procedures accessing a shared *
*  memory buffer on the same processor.                                  *
* The RCS_SEMAPHORE is no longer used. Instead a section of the shared   *
* memory buffer itself is used to guarantee mutual exclusion.            *
*************************************************************************/

/* Include Files */
#define CMS_DERIVED_CLASS 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_SHMEM)

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#ifndef HAVE_KEY_T_TYPE
typedef long key_t;
#endif
#define KEY_T_DEFINED
#else
#include "shmem_no_config.h"
#endif

#include "rcs_prnt.hh"
#include "cms.hh"		/* class CMS */
#include "shmem.hh"		/* class SHMEM */
#include "shm.hh"		/* class RCS_SHAREDMEM */
//#include "sem.hh"             /* class RCS_SEMAPHORE */
#include "memsem.hh"		/* mem_get_access(), mem_release_access() */
#include "timer.hh"		/* etime(), esleep() */
#include "physmem.hh"		// class PHYSMEM_HANDLE

#if ENABLE_RCS_CMS_MRPQ
#include "cms_mrpq.hh" 		//  class CMS_MULTIREADER_PRIORITY_QUEUE
#endif

/* Common Definitions. */

#include "autokey.h"

/* rw-rw-r-- permissions */
#ifndef MS_WINDOWS_API
#define MODE (0666)
#else
#define MODE (0)
#endif

static double last_non_zero_x;
static double last_x;

struct shmem_blocking_sem
{
#ifdef MS_WINDOWS_API
  HANDLE bsem;
#else
  RCS_SEMAPHORE *bsem;		// blocking semaphore
#endif
};

static int
not_zero (volatile double x)
{
  last_x = x;
  if (x < -1E-6 && last_x < -1E-6)
    {
      last_non_zero_x = x;
      return 1;
    }
  if (x > 1E-6 && last_x > 1E-6)
    {
      last_non_zero_x = x;
      return 1;
    }
  return 0;
}

/* SHMEM Member Functions. */

/* Constructor for use with cms_config. */
SHMEM::SHMEM (const char *bufline, 
	      const char *procline,
	      int set_to_server,
	      int set_to_master):
  CMS (bufline, procline, set_to_server),
  lkey(0),lbsem_key(-1),second_read(0),shm(0),sem(0),master(0),sem_delay(0),min_bsem_wait(0),mao_ptr(0),
  mutex_type(OS_SEM_MUTEX),shm_addr_offset(0),
  bsem_ptr(0),autokey_table_size(0),last_bsem_wait_time(0.0),
  i_have_mutex(false),always_flush_bsem(false), repeat_main_access(false),
  use_os_sem(true),use_os_sem_only(true)
{
  /* Set pointers to null so only properly opened pointers are closed. */
  char *semdelay_equation=0;
  char *min_bsem_wait_equation=0;

  shm = NULL;
  sem = NULL;
  sem_delay = 0.00001;
  min_bsem_wait=rcs_sem_get_default_bsem_wait();

  use_os_sem = 1;
  use_os_sem_only = 1;
  mutex_type = OS_SEM_MUTEX;
  lbsem_key = -1;
  second_read = 0;
  mao_ptr=0;
  i_have_mutex=false;
  always_flush_bsem=false;
  repeat_main_access=false;

  if (status < 0)
    {
      cms_print_error ("SHMEM: status = %d\n", status);
      return;
    }

  /* Save parameters from configuration file. */
  if (sscanf (bufline, "%*s %*s %*s %*s %*s %*s %*s %*s %*s %ld", &lkey) != 1)
    {
      cms_print_error ("SHMEM: Invalid configuration file format.\n");
      return;
    }

  master = is_local_master;
  if (1 == set_to_master)
    {
      master = 1;
    }
  else if (-1 == set_to_master)
    {
      master = 0;
    }

  if (NULL != (semdelay_equation = strstr (proclineupper, "SEMDELAY=")))
    {
      sem_delay = strtod (semdelay_equation + 9, (char **) NULL);
    }
  else if (NULL != (semdelay_equation = strstr (buflineupper, "SEMDELAY=")))
    {
      sem_delay = strtod (semdelay_equation + 9, (char **) NULL);
    }

  if (NULL != (min_bsem_wait_equation = strstr (proclineupper, "MIN_BSEM_WAIT=")))
    {
      min_bsem_wait = strtod (min_bsem_wait_equation + 14, (char **) NULL);
    }
  else if (NULL != (min_bsem_wait_equation = strstr (buflineupper, "MIN_BSEM_WAIT=")))
    {
      min_bsem_wait = strtod (min_bsem_wait_equation + 14, (char **) NULL);
    }

  if (NULL != (semdelay_equation = strstr (buflineupper, "BSEM=")))
    {
      lbsem_key = strtol (semdelay_equation + 5, (char **) NULL, 0);
    }

  if (NULL != strstr (buflineupper, "MUTEX=NONE"))
    {
      mutex_type = NO_MUTEX;
      use_os_sem = 0;
      use_os_sem_only = 0;
    }
  if(NULL != strstr(buflineupper,"ALWAYS_FLUSH_BSEM"))
    {
      always_flush_bsem=true;
    }

  if (NULL != strstr (buflineupper, "MUTEX=OS_SEM"))
    {
      mutex_type = OS_SEM_MUTEX;
      use_os_sem = 1;
      use_os_sem_only = 1;
    }

  if (NULL != strstr (buflineupper, "MUTEX=NO_INTERRUPTS"))
    {
      mutex_type = NO_INTERRUPTS_MUTEX;
      use_os_sem = 0;
      use_os_sem_only = 0;
    }

  if (NULL != strstr (buflineupper, "MUTEX=NO_SWITCHING"))
    {
      mutex_type = NO_SWITCHING_MUTEX;
#ifdef lynxosPC
      if (fa_ptr == NULL)
	{
	  fa_ptr = fast_info_attach ((char *) 0xC0000000);
	}
#endif
      use_os_sem = 0;
      use_os_sem_only = 0;
    }

  if (NULL != strstr (buflineupper, "MUTEX=MAO"))
    {
      mao_ptr = new struct mem_access_object;
      mutex_type = MAO_MUTEX;
      use_os_sem = 0;
      use_os_sem_only = 0;
    }

  if (NULL != strstr (buflineupper, "MAO_W_OS_SEM"))
    {
      mao_ptr = new struct mem_access_object;
      mutex_type = MAO_MUTEX_W_OS_SEM;
      use_os_sem = 1;
      use_os_sem_only = 0;
    }

  /* Open the shared memory buffer and create mutual exclusion semaphore. */
  open ();
}

SHMEM::~SHMEM ()
{
  /* detach from shared memory and semaphores */
  close ();
}

/*
  Open the SHMEM buffer
  */
int
SHMEM::open ()
{
  /* Set pointers to NULL incase error occurs. */
  sem = NULL;
  shm = NULL;
  i_have_mutex=false;
  bsem_ptr = NULL;
  shm_addr_offset = NULL;
  second_read = 0;
  autokey_table_size = 0;
  blocking_support_enabled=false;
  key_t bsem_key = (key_t) lbsem_key;
  key_t key = (key_t) lkey;
  bool  return_minus_1_skipped=false;

  if(total_connections <= 0)
    {
      use_autokey_for_connection_number=false;
    }
  if (use_autokey_for_connection_number)
    {
      autokey_table_size = sizeof (AUTOKEY_TABLE_ENTRY) * total_connections;
    }

  /* set up the shared memory address and semaphore, in given state */
  if (master)
    {
      shm = new RCS_SHAREDMEM (key, size, RCS_SHAREDMEM_CREATE, (int) MODE);
      if (shm->addr == NULL)
	{
	  switch (shm->create_errno)
	    {
	    case EACCES:
	      status = CMS_PERMISSIONS_ERROR;
	      break;

	    case EEXIST:
	      status = CMS_RESOURCE_CONFLICT_ERROR;
	      break;

	    case ENOMEM:
	    case ENOSPC:
	      status = CMS_CREATE_ERROR;
	      break;

	    default:
	      status = CMS_MISC_ERROR;
	    }
	  if(!cleaning_flag)
	    {
	      delete shm;
	      shm = NULL;
	      return -1;
	    }
	  return_minus_1_skipped=true;
	}
      if (use_os_sem)
	{
	  key_t sem_key = (key_t) lkey;
	  sem =
	    new RCS_SEMAPHORE (sem_key, RCS_SEMAPHORE_CREATE, timeout, (int) MODE,
			       (use_os_sem_only != 0));
	  if (NULL == sem)
	    {
	      cms_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	      cms_print_error (" Possibly out of memory?\n");
	      status = CMS_CREATE_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	  if (!sem->valid ())
	    {
	      cms_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	      status = CMS_MISC_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	}
      if ( bsem_key > 0)
	{
	  blocking_support_enabled=true;
	  bsem_ptr = new struct shmem_blocking_sem;
#ifndef MS_WINDOWS_API
	  bsem_ptr->bsem = new RCS_SEMAPHORE (bsem_key, RCS_SEMAPHORE_CREATE,
				    timeout, (int) MODE, 0,true);
	  if (NULL == bsem_ptr->bsem)
	    {
	      cms_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	      cms_print_error (" Possibly out of memory?\n");
	      status = CMS_CREATE_ERROR;
	      blocking_support_enabled=false;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	  if (!bsem_ptr->bsem->valid ())
	    {
	      cms_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	      status = CMS_MISC_ERROR;
	      blocking_support_enabled=false;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
#else
	  char bsem_event_name[80];
#ifndef NO_STDIO
	  int bsem_key_int = (int) bsem_key;
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(bsem_event_name,sizeof(bsem_event_name)),
			  "event%d", bsem_key_int);
#else
	  strcpy (bsem_event_name, "event");
	  _itoa (bsem_key, bsem_event_name + 5, 10);
#endif
	  SECURITY_ATTRIBUTES sa;
	  SECURITY_DESCRIPTOR sd;
	  if (FALSE ==
	      InitializeSecurityDescriptor (&sd,
					    SECURITY_DESCRIPTOR_REVISION))
	    {
	      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
				   "Can not initailize security descriptor.\n");
	      status = CMS_MISC_ERROR;
	      blocking_support_enabled=false;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	  sa.nLength = sizeof (SECURITY_ATTRIBUTES);
	  sa.lpSecurityDescriptor = &sd;
	  sa.bInheritHandle = TRUE;
	  bsem_ptr->bsem = CreateEvent (&sa, TRUE, FALSE, bsem_event_name);
	  if (NULL == bsem_ptr->bsem)
	    {
	      blocking_support_enabled=false;
	      status = CMS_MISC_ERROR;
	      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
				   "CreateEvent error:");
	    }
#endif
	}
      in_buffer_id = 0;
    }
  else
    {
      if(wait_for_master)
	{
	  shm = new RCS_SHAREDMEM (key, size, RCS_SHAREDMEM_NOCREATE,0,1);
	}
      else
	{
	  shm = new RCS_SHAREDMEM (key, size, RCS_SHAREDMEM_NOCREATE);
	}
      if (NULL == shm)
	{
	  cms_print_error
	    ("CMS: couldn't create RCS_SHAREDMEM(%d(0x%X), %ld(0x%X), RCS_SHAREDMEM_NOCREATE).\n",
	     (int) key, (unsigned)key, size, (unsigned)size);
	  status = CMS_CREATE_ERROR;
	  if(!cleaning_flag)
	    {
	      return -1;
	    }
	  return_minus_1_skipped=true;
	}
      if (shm->addr == NULL)
	{
	  switch (shm->create_errno)
	    {
	    case EACCES:
	      status = CMS_PERMISSIONS_ERROR;
	      break;

	    case EEXIST:
	      status = CMS_RESOURCE_CONFLICT_ERROR;
	      break;

	    case ENOENT:
	      status = CMS_NO_MASTER_ERROR;
	      break;

	    case ENOMEM:
	    case ENOSPC:
	      status = CMS_CREATE_ERROR;
	      break;

	    default:
	      status = CMS_MISC_ERROR;
	    }
	  if(!cleaning_flag)
	    {
	      delete shm;
	      shm = NULL;
	      return -1;
	    }
	  return_minus_1_skipped=true;
	}
      if (use_os_sem)
	{
	  sem = new RCS_SEMAPHORE (key, RCS_SEMAPHORE_NOCREATE, timeout);
	  if (NULL == sem)
	    {
	      cms_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	      cms_print_error (" Possibly out of memory?\n");
	      status = CMS_CREATE_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	  if (!sem->valid ())
	    {
	      cms_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	      status = CMS_MISC_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	}
      if (bsem_key > 0)
	{
	  blocking_support_enabled=true;
	  bsem_ptr = new struct shmem_blocking_sem;
#ifndef MS_WINDOWS_API
	  bsem_ptr->bsem =
	    new RCS_SEMAPHORE (bsem_key, RCS_SEMAPHORE_NOCREATE, timeout,0,0,true);
	  if (NULL == bsem_ptr->bsem)
	    {
	      blocking_support_enabled=false;
	      cms_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	      cms_print_error (" Possibly out of memory?\n");
	      status = CMS_CREATE_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
	  if (!bsem_ptr->bsem->valid ())
	    {
	      blocking_support_enabled=false;
	      cms_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	      status = CMS_MISC_ERROR;
	      if(!cleaning_flag)
		{
		  return -1;
		}
	      return_minus_1_skipped=true;
	    }
#else
	  char bsem_event_name[80];
#ifndef NO_STDIO
	  int bsem_key_int2 = (int) bsem_key;
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(bsem_event_name,sizeof(bsem_event_name)),
			  "event%d", bsem_key_int2);
#else
	  strcpy (bsem_event_name, "event");
	  _itoa (bsem_key, bsem_event_name + 5, 10);
#endif
	  bsem_ptr->bsem = OpenEvent (EVENT_ALL_ACCESS, TRUE, bsem_event_name);
	  if (NULL == bsem_ptr->bsem)
	    {
	      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
				   "OpenEvent error:");
	      rcs_print_error("Buffer=%s,Proc=%s,bsem_event_name=%s\n",
			      BufferName,
			      ProcessName,
			      bsem_event_name);
	      rcs_print_error("BufferLine=%s\n",BufferLine);
	      rcs_print_error("ProcessLine=%s\n",ProcessLine);
	    }
#endif
	}
    }

  if(total_connections < 0)
    {
      total_connections = 0;
    }

  if (min_compatible_version < 3.44 && min_compatible_version > 0)
    {
      total_subdivisions = 1;
    }

  if (min_compatible_version > 2.57 || min_compatible_version <= 0)
    {
      if (!shm->created)
	{
	  char *cptr = (char *) shm->addr;
	  if(cptr)
	    {
	      cptr[31] = 0;
	      if(!*cptr)
		{
		  rcs_print_warning("The buffer name was not set in buffer %s (key=%d(0x%X))\n",BufferName, (int) key,(unsigned) key);
		}
	      else if (strncmp (cptr, BufferName, 31))
		{
		  cms_print_error
		    ("Shared memory buffers %s and %s conflict. (key=%d(0x%X))\n",
		     BufferName, cptr, (int) key,(unsigned) key);
		  status = CMS_RESOURCE_CONFLICT_ERROR;
		  if(!cleaning_flag)
		    {
		      return -1;
		    }
		  return_minus_1_skipped=true;
		}
	    }
	}
      if (master)
	{
	  if (shm->created && shm && shm->addr)
	    {
	      memset (shm->addr, 0, size);
	    }
	  if (use_autokey_for_connection_number)
	    {
	      void *autokey_table_end =
		(void *) (((char *) shm->addr) + 32 + autokey_table_size);
	      memset (autokey_table_end, 0, size - 32 - autokey_table_size);
	    }
	  strncpy ((char *) shm->addr, BufferName, 32);
	}
      if(shm && shm->addr)
	{
	  if (use_autokey_for_connection_number)
	    {
	      void *autokey_table = (void *) (((char *) shm->addr) + 32);
	      connection_number =
		autokey_getkey (autokey_table, total_connections, ProcessName);
	      shm_addr_offset =
		(void *) ((char *) (shm->addr) + 32 + autokey_table_size);
	      if(!this->queuing_enabled || 
		 !this->max_message_size_set_on_buffer_line) {
		max_message_size -= (32 + autokey_table_size);	/* size of cms buffer available for user */
	      }
	    }
	  else
	    {
	      shm_addr_offset = (void *) ((char *) (shm->addr) + 32);
	      if(!this->queuing_enabled || 
		 !this->max_message_size_set_on_buffer_line) {
		max_message_size -= 32;	/* size of cms buffer available for user */
	      }
	    }
	}
      else
	{
	  shm_addr_offset=0;
	}
      /* messages = size - CMS Header space */
      if (enc_max_size <= 0 || enc_max_size > size)
	{
	  if (neutral)
	    {
	      max_encoded_message_size -= 32;
	    }
	  else
	    {
	      max_encoded_message_size -=
		(cms_encoded_data_explosion_factor * 32);
	    }
	}
      /* Maximum size of message after being encoded. */
      guaranteed_message_space -= 32;	/* Largest size message before being encoded
					   that can be guaranteed to fit after xdr. */
      //      size -= 32;
      size_without_diagnostics -= 32;
      subdiv_size =
	(size_without_diagnostics - total_connections) / total_subdivisions;
      subdiv_size -= (subdiv_size % 4);
    }
  else
    {
      if (master && shm && shm->addr)
	{
	  memset (shm->addr, 0, size);
	}
      shm_addr_offset = shm->addr;
    }
  skip_area = 32 + total_connections + autokey_table_size;
  if(0 != mao_ptr)
    {
      mao_ptr->data = shm_addr_offset;
      mao_ptr->timeout = timeout;
      mao_ptr->total_connections = total_connections;
      mao_ptr->sem_delay = sem_delay;
      mao_ptr->connection_number = connection_number;
      mao_ptr->split_buffer = split_buffer;
      mao_ptr->read_only = 0;
      mao_ptr->sem = sem;
    }
  if (min_compatible_version > 5.0 || min_compatible_version < 1E-6)
    {
      if (neutral && master && shm->created &&
	  encoded_header_size < size - skip_area)
	{
	  header.write_id = 0;
	  header.was_read = 0;
	  header.in_buffer_size = 0;
	  encode_header ();
	  char *header_loc = ((char *) shm->addr) + skip_area;
	  memcpy (header_loc, encoded_header, encoded_header_size);
	}
    }

  fast_mode = !queuing_enabled && !split_buffer && !neutral &&
    (mutex_type == NO_SWITCHING_MUTEX);
  handle_to_global_data = dummy_handle = new PHYSMEM_HANDLE;
  long diff = (long) (((char *)shm_addr_offset) - ((char *) shm->addr));
  if(diff < 0 || diff >= size) {
    // if this code is ever executed it is probably a very strange bug or memory corruption problem
    cms_print_error ("Bad shared memory pointer difference = %ld, between %p and %p \n",
		     diff,
		     ((char *) shm_addr_offset), 
		     ((char *) shm->addr));
    status = CMS_MISC_ERROR;
    return -1;
  }
  long htgb_size = size-diff;
  handle_to_global_data->set_to_ptr (shm_addr_offset, htgb_size);
  if(total_connections <= 0 &&
     (mutex_type == MAO_MUTEX || mutex_type == MAO_MUTEX_W_OS_SEM))
    {
      cms_print_error ("Can not use this mutex type with total_connections or max_procs = %ld\n", total_connections);
      status = CMS_MISC_ERROR;
      if(!cleaning_flag)
	{
	  return -1;
	}
      return_minus_1_skipped=true;
    } 
  if ((connection_number < 0 || connection_number >= total_connections)
      && (mutex_type == MAO_MUTEX || mutex_type == MAO_MUTEX_W_OS_SEM))
    {
      cms_print_error ("Bad connection number %ld\n", connection_number);
      status = CMS_MISC_ERROR;
      if(!cleaning_flag)
	{
	  return -1;
	}
      return_minus_1_skipped=true;
    }
#if ENABLE_RCS_CMS_MRPQ
  if(multireader_priority_queue_enabled)
    {
      mrpq = new CMS_MULTIREADER_PRIORITY_QUEUE(this,(master && shm->created));
    }
#endif
  last_bsem_wait_time=etime();
  if(return_minus_1_skipped)
    {
      return -1;
    }
  return 0;
}

/* Closes the  shared memory and mutual exclusion semaphore  descriptors. */
int
SHMEM::close ()
{
  int nattch = 0;
  second_read = 0;
  if(cleaning_flag)
    {
      delete_totally=true;
    }

  if(i_have_mutex)
    {
      clear();
      i_have_mutex=false;
    }
#if ENABLE_RCS_CMS_MRPQ
  if(mrpq)
    {
      if(!preserve_mrpq_reader_id)
	{
	  remove_current_mrpq_reader_id();
	}
      delete mrpq;
      mrpq=0;
    }
#else
  mrpq=0;
#endif

  if (use_autokey_for_connection_number)
    {
      void *autokey_table = (void *) (((char *) shm->addr) + 32);
      autokey_releasekey (autokey_table, total_connections, ProcessName,
			  connection_number);
    }
  if (NULL != shm)
    {
      /* see if we're the last one */
      nattch = shm->nattch ();
      shm->delete_totally = delete_totally;
      delete shm;
      shm = NULL;
    }
  if (NULL != sem)
    {
      /* if we're the last one, then make us the master so that the
         semaphore will go away */
      if ((nattch <= 1 && nattch > -1) || delete_totally)
	{
	  sem->setflag (RCS_SEMAPHORE_CREATE);
	}
      else
	{
	  sem->setflag (RCS_SEMAPHORE_NOCREATE);
	}
      delete sem;
    }
    if(NULL != bsem_ptr)
      {
#ifndef MS_WINDOWS_API
	if (NULL != bsem_ptr->bsem)
	  {
	    //bsem_ptr->bsem->flush();

	    /* if we're the last one, then make us the master so that the
	       semaphore will go away */
	    if ((nattch <= 1 && nattch > -1) || delete_totally)
	      {
		bsem_ptr->bsem->setflag (RCS_SEMAPHORE_CREATE);
	      }
	    else
	      {
		bsem_ptr->bsem->setflag (RCS_SEMAPHORE_NOCREATE);
	      }
	    delete bsem_ptr->bsem;
	  }
#endif
	delete bsem_ptr;
	bsem_ptr=NULL;
      }

  if(0 != mao_ptr)
    {
      delete mao_ptr;
      mao_ptr=0;
    }
  return 0;
}

/* Access the shared memory buffer. */
CMS_STATUS SHMEM::main_access (void *_local)
{
#ifndef MS_WINDOWS_API
  if(bsem_ptr && bsem_ptr->bsem)
    {
      bsem_ptr->bsem->inc_waiting();
    }
#endif
  do
    {
      repeat_main_access=false;
      main_access_to_repeat(_local);
    }
  while(repeat_main_access && !interrupting_operation);
#ifndef MS_WINDOWS_API
  if(bsem_ptr && bsem_ptr->bsem)
    {
      bsem_ptr->bsem->dec_waiting();
    }
#endif

  return(status);
}

int
SHMEM::wait_for_anything(double _timeout)
{
  double curtime=etime();
  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
      return -1;
    }
  if(curtime - last_bsem_wait_time < min_bsem_wait && min_bsem_wait > 0)
    {
      esleep(min_bsem_wait);
      return(0);
    }
  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
      return -1;
    }
#ifndef MS_WINDOWS_API
  if(bsem_ptr && bsem_ptr->bsem)
    {
      bsem_ptr->bsem->inc_waiting();
    }
#endif
  blocking_timeout = _timeout;
  last_bsem_wait_time = etime();
#ifndef MS_WINDOWS_API
  if(!bsem_ptr || !bsem_ptr->bsem)
    {
      cms_print_error("bsem_ptr == NULL\n");
      status = CMS_MISC_ERROR;
      esleep(0.01);
      return -1;
    }
  bsem_ptr->bsem->timeout = blocking_timeout;
  bsem_ptr->bsem->wait ();
#else
  DWORD
    timeoutMillis = (DWORD) (blocking_timeout * 1000.0);
  if (blocking_timeout < 0)
    {
      timeoutMillis = INFINITE;
    }
  switch (WaitForSingleObject (bsem_ptr->bsem, timeoutMillis))
    {
    case WAIT_TIMEOUT:
      status = CMS_TIMED_OUT;
      second_read = 0;
      return (status);
		  
    case WAIT_OBJECT_0:
      second_read++;
      break;
      
    default:
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "WaitForSingleObject error");
      status = CMS_MISC_ERROR;
      second_read = 0;
      return (status);
    }
#endif
#ifndef MS_WINDOWS_API
  if(bsem_ptr && bsem_ptr->bsem)
    {
      bsem_ptr->bsem->dec_waiting();
    }
#endif
  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
      return -1;
    }
  return(0);
}


CMS_STATUS SHMEM::main_access_to_repeat (void *_local)
{
  int bsem_ret;
  bsem_ret=0;

  if(second_read == 0 && blocking_timeout > 1e-6 )
    {
      orig_blocking_timeout = blocking_timeout;
      blocking_read_start = etime();
    }

#if defined(LYNX) && !defined(lynxosPC)
  int
    interrupt_disable_number =
    0;
  int
    switching_disable_number =
    1;
#endif
  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
      return status;
    }

  /* Check pointers. */
  if (shm == NULL)
    {
      second_read = 0;
      return (status = CMS_MISC_ERROR);
    }

  if(bsem_ptr == NULL && not_zero(blocking_timeout))
    {
      cms_print_error
	("No blocking semaphore available. Can not call blocking_read(%f).\n",
	 blocking_timeout);
      second_read = 0;
      return (status = CMS_NO_BLOCKING_SEM_ERROR);
    }
  if(bsem_ptr != NULL)
    {
      if (bsem_ptr->bsem == NULL && not_zero (blocking_timeout))
	{
	  cms_print_error
	    ("No blocking semaphore available. Can not call blocking_read(%f).\n",
	     blocking_timeout);
	  second_read = 0;
	  return (status = CMS_NO_BLOCKING_SEM_ERROR);
	}
    }


#ifdef VXWORKS
  int
    intLockKey =
    0;
#endif

  switch (mutex_type)
    {
    case NO_MUTEX:
      break;

    case MAO_MUTEX:
    case MAO_MUTEX_W_OS_SEM:
      mao_ptr->read_only = ((internal_access_type == CMS_CHECK_IF_READ_ACCESS) ||
			    (internal_access_type == CMS_PEEK_ACCESS) ||
			    (internal_access_type == CMS_READ_ACCESS));
      switch (mem_get_access (mao_ptr))
	{
	case -1:
	  cms_print_error ("SHMEM: Can't take semaphore\n");
	  second_read = 0;
	  return (status = CMS_MISC_ERROR);
	case -2:
	  if (timeout > 0)
	    {
	      cms_print_error ("SHMEM: Timed out waiting for semaphore.\n");
#ifndef MS_WINDOWS_API
	      cms_print_error ("buffer = %s, timeout = %f sec.\n",
			       BufferName, timeout);
#endif
	    }
	  second_read = 0;
	  return (status = CMS_TIMED_OUT);
	default:
	  break;
	}
      toggle_bit = mao_ptr->toggle_bit;
      break;

    case OS_SEM_MUTEX:
      if (sem == NULL)
	{
	  second_read = 0;
	  return (status = CMS_MISC_ERROR);
	}
      switch (sem->wait ())
	{
	case -1:
	  if(interrupting_operation)
	    {
	      status = CMS_INTERRUPTED_OPERATION;
	      return(status);
	    }
	  cms_print_error ("SHMEM: Can't take semaphore\n");
	  second_read = 0;
	  return (status = CMS_MISC_ERROR);
	case -2:
	  if (timeout > 0)
	    {
	      cms_print_error ("SHMEM: Timed out waiting for semaphore.\n");
#ifndef MS_WINDOWS_API
	      cms_print_error ("buffer = %s, timeout = %f sec.\n",
			       BufferName, timeout);
#endif
	    }
	  second_read = 0;
	  return (status = CMS_TIMED_OUT);
	  
	default:
	  break;
	}
      break;

    case NO_INTERRUPTS_MUTEX:
#ifdef LYNX
      disable (interrupt_disable_number);
      break;
#else
#ifdef VXWORKS
      intLockKey = intLock ();
      break;
#else
      cms_print_error ("Interrupts can not be disabled.\n");
      second_read = 0;
      return (status = CMS_MISC_ERROR);
      //break;
#endif
#endif

    case NO_SWITCHING_MUTEX:
#ifdef lynxosPC
      fa_ptr->preempt++;
      break;
#else
#if 0
// The  LYNX documentation says this should work but the
// symbol sdisable is not available.
      sdisable (switching_disable_number);
      break;
#else
#ifdef VXWORKS
      taskLock ();
      break;
#else
      cms_print_error ("Interrupts can not be disabled.\n");
      return (status = CMS_MISC_ERROR);
      //break;
#endif
#endif
#endif
    default:
      cms_print_error ("SHMEM: Invalid mutex type.(%d)\n", mutex_type);
      second_read = 0;
      return (status = CMS_MISC_ERROR);
      //break;
    }
  i_have_mutex=true;

#ifdef ENABLE_RCS_DIAG
  if (second_read > 0 && enable_diagnostics)
    {
      disable_diag_store = 1;
    }
#endif

  /* Perform access function. */
  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
    }
  else
    {
      handle_to_global_data->set_offset(0);
      internal_access (handle_to_global_data, _local);
    }

  disable_diag_store = 0;

  if(bsem_ptr != NULL)
    {
      if(interrupting_operation)
	{
	  status = CMS_INTERRUPTED_OPERATION;
	  return status;
	}
      if (NULL != bsem_ptr->bsem &&
	  ((internal_access_type == CMS_WRITE_ACCESS
	   || internal_access_type == CMS_WRITE_IF_READ_ACCESS) ||
	   (always_flush_bsem && 
	    ( status != CMS_READ_OLD 
	      && status != CMS_WAIT_FOR_READ_INCOMPLETE 
	      && status != CMS_WAIT_FOR_WRITE_INCOMPLETE 
	      && status != CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE 
	      && status != CMS_WAIT_FOR_CLEAR_INCOMPLETE 
	      && status != CMS_WAIT_FOR_READ_OK 
	      && status != CMS_WAIT_FOR_WRITE_OK 
	      && status != CMS_WAIT_FOR_QUEUE_LENGTH_OK 
	      && status != CMS_WAIT_FOR_CLEAR_OK ) && status > 0 )))
	{
#ifndef MS_WINDOWS_API
	  bsem_ptr->bsem->flush ();
#else
	  if (!PulseEvent (bsem_ptr->bsem))
	    {
	      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE, "PulseEvent error");
	    }
#endif
	}
    }
  switch (mutex_type)
    {
    case NO_MUTEX:
      break;
      
    case MAO_MUTEX:
    case MAO_MUTEX_W_OS_SEM:
      if(mao_ptr)
	{
	  mem_release_access (mao_ptr);
	}
      break;

    case OS_SEM_MUTEX:
      sem->post ();
      break;
    case NO_INTERRUPTS_MUTEX:
#ifdef LYNX
      restore (interrupt_disable_number);
      break;
#else
#ifdef VXWORKS
      intUnlock (intLockKey);
#else
      cms_print_error ("Can not restore interrupts.\n");
      break;
#endif
#endif

    case NO_SWITCHING_MUTEX:
#ifdef lynxosPC
      if ((!(--(fa_ptr->preempt)) || fa_ptr->preempt > 0x40) && fa_ptr->flag)
	{
	  fast_enable_preemption ();
	}
      break;
#else
#if 0
      // The  LYNX documentation says this should work but
      // the symbol srestore is not available.
      srestore (switching_disable_number);
      break;
#else
#ifdef VXWORKS
      taskUnlock ();
      break;
#else
      cms_print_error ("Can not restore interrupts.\n");
      break;
#endif
#endif
#endif
    }
  i_have_mutex=false;

  if(interrupting_operation)
    {
      status = CMS_INTERRUPTED_OPERATION;
      return status;
    }

  switch (internal_access_type)
    {
    case CMS_WAIT_FOR_READ_ACCESS:
    case CMS_WAIT_FOR_CLEAR_ACCESS:
    case CMS_WAIT_FOR_WRITE_ACCESS:
    case CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS:
    case CMS_READ_ACCESS:
      if(NULL != bsem_ptr)
	{
	  if (NULL != bsem_ptr->bsem && 
	      ( status == CMS_READ_OLD 
		|| status == CMS_WAIT_FOR_READ_INCOMPLETE 
		|| status == CMS_WAIT_FOR_WRITE_INCOMPLETE 
		|| status == CMS_WAIT_FOR_CLEAR_INCOMPLETE 
		|| status == CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE) &&
	      (blocking_timeout > 1e-6 || blocking_timeout < -1E-6))
	    {
	      if(blocking_timeout > 1e-6)
		{
		  blocking_timeout = orig_blocking_timeout - (etime()-blocking_read_start);
		  if(blocking_timeout < 0)
		    {
		      status = CMS_TIMED_OUT;
		      second_read=0;
		      return status;
		    }
		}
	      if (max_repeat_blocking_reads > 0 
		  && second_read > max_repeat_blocking_reads 
		  && total_subdivisions <= 1)
		{
		  status = CMS_MISC_ERROR;
		  cms_print_error
		    ("CMS: Blocking semaphore error. The semaphore wait has returned %d times but there is still no new data.\n",
		     second_read);
		  second_read = 0;
		  return (status);
		}
#ifndef MS_WINDOWS_API
	      second_read++;
	      bsem_ptr->bsem->timeout = blocking_timeout;
#if !defined(VXWORKS) && !defined(MS_WINDOWS_API)
	      sem_force_fifo = 1;
#endif
	      double curtime=etime();
	      if(curtime - last_bsem_wait_time < min_bsem_wait && min_bsem_wait > 0 
		 && (second_read > 1 || 
		     !queuing_enabled || multireader_priority_queue_enabled
		     || internal_access_type != CMS_READ_ACCESS))
		{
		  esleep(min_bsem_wait);
		  bsem_ret=0;
		}
	      else
		{
		  last_bsem_wait_time = etime();
		  bsem_ret =
		    bsem_ptr->bsem->wait ();
		}
	      if(interrupting_operation)
		{
		  status = CMS_INTERRUPTED_OPERATION;
		  return(status);
		}
		  
#if !defined(VXWORKS) && !defined(MS_WINDOWS_API)
	      sem_force_fifo = 0;
#endif
	      if (bsem_ret == -2)
		{
		  status = CMS_TIMED_OUT;
		  second_read = 0;
		  return (status);
		}
	      if (bsem_ret == -1)
		{
		  if(bsem_ptr && bsem_ptr->bsem)
		    {
		      cms_print_error ("CMS: Blocking semaphore error. bsem_ptr->timeout=%f\n",
				       bsem_ptr->bsem->timeout);
		    }
		  else
		    {
		      cms_print_error ("CMS: Blocking semaphore error.\n");
		    }
		  status = CMS_MISC_ERROR;
		  second_read = 0;
		  return (status);
		}
#else
	      DWORD
		timeoutMillis = (DWORD) (blocking_timeout * 1000.0);
	      if (blocking_timeout < 0)
		{
		  timeoutMillis = INFINITE;
		}
	      switch (WaitForSingleObject (bsem_ptr->bsem, timeoutMillis))
		{
		case WAIT_TIMEOUT:
		  status = CMS_TIMED_OUT;
		  second_read = 0;
		  return (status);
		  
		case WAIT_OBJECT_0:
		  second_read++;
		  break;

		default:
		  rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
				       "WaitForSingleObject error");
		  status = CMS_MISC_ERROR;
		  second_read = 0;
		  return (status);
		}
#endif
	      if(interrupting_operation)
		{
		  status = CMS_INTERRUPTED_OPERATION;
		  return status;
		}
	      repeat_main_access=true;
	      //main_access (_local);
	    }
	}
      break;

    default:
      break;

    }
  if(!repeat_main_access)
    {
      second_read = 0;
    }
  return (status);
}

void SHMEM::interrupt_operation(void)
{
  interrupting_operation=true;
  if(NULL != bsem_ptr && NULL != bsem_ptr->bsem)
    {
#ifndef MS_WINDOWS_API
      bsem_ptr->bsem->interrupt_operation ();
#else
      if (!PulseEvent (bsem_ptr->bsem))
	{
	  rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE, "PulseEvent error");
	}
#endif
    }
}

void SHMEM::clear_interrupt_operation(void)
{
  interrupting_operation=false;
  if(NULL != bsem_ptr && NULL != bsem_ptr->bsem)
    {
#ifndef MS_WINDOWS_API
      bsem_ptr->bsem->clear_interrupt_operation();
#endif
    }
}

void SHMEM::set_leave_resource(bool b)
{
#ifndef MS_WINDOWS_API
  if(bsem_ptr && bsem_ptr->bsem)
    {
      bsem_ptr->bsem->leave_resource =b;
    }
#endif
  if(shm)
    {
      shm->leave_resource=b;
    }
  if(sem)
    {
      sem->leave_resource=b;
    }
  leave_resource=b;
}

#if 0 
//def VXWORKS
// Those offsets look wrong, so if 0 this out
CMS_STATUS SHMEM::read ()
{
  if (fast_mode)
    {
      handle_to_global_data->set_offset(total_connections);
      taskLock ();
      read_raw ();
      taskUnlock ();
      return (status);
    }
  internal_access_type = CMS_READ_ACCESS;
  main_access (data);
  return (status);
}

CMS_STATUS SHMEM::peek ()
{
  if (fast_mode)
    {
      handle_to_global_data->set_offset(total_connections);
      taskLock ();
      peek_raw ();
      taskUnlock ();
      return (status);
    }
  internal_access_type = CMS_PEEK_ACCESS;
  main_access (data);
  return (status);
}

CMS_STATUS SHMEM::write (void *user_data)
{
  if (fast_mode)
    {
      handle_to_global_data->set_offset(total_connections);
      taskLock ();
      write_raw (user_data);
      taskUnlock ();
      return (status);
    }
  internal_access_type = CMS_WRITE_ACCESS;
  main_access (user_data);
  return (status);
}

CMS_STATUS SHMEM::write_if_read (void *user_data)
{
  if (fast_mode)
    {
      handle_to_global_data->set_offset(total_connections);
      taskLock ();
      write_if_read_raw (user_data);
      taskUnlock ();
      return (status);
    }
  internal_access_type = CMS_WRITE_IF_READ_ACCESS;
  main_access (user_data);
  return (status);
}


// VXWORKS
#endif

//  defined(ENABLE_RCS_SHMEM)
#else
#include "rcs_empty_source"
#endif
