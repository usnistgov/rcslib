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

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_GLOBMEM)

/*************************************************************************
* File: globmem.cc                                                       *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for the Communication Management System(CMS).        *
* Includes:                                                              *
*          1. Member functions for class GLOBMEM.                        *
* Note: GLOBMEM is used for communication between multiple VxWorks       *
* boards in a single backplane.                                          *
*************************************************************************/

/* Include Files */
#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <string.h>		/* memcpy(), memset(), strtol() */
#include <stddef.h>		/* size_t */
#include <stdlib.h>		/* calloc() */
#include <ctype.h>		/* isdigit(), toupper(), tolower() */
#include <math.h>		/* fabs() */

#ifdef VXWORKS
#include "vxWorks.h"
#include "sysLib.h"		/* sysBusToLocalAdrs() */
#include "taskLib.h"		/* taskLock() */
#include "semLib.h"
#include "semSmLib.h"
#include "objLib.h"
#include "vme.h"		/* VME_AM_STD_USR_DATA */
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif



#ifdef VXWORKS
extern "C"
{
#include "bd_types.h"
#ifndef NO_BUS_LOCK_SUPPORT
#include "bus_lock.h"
#endif
#ifndef NO_DMA_SUPPORT
#include "dma.h"
#endif
}
#endif

#include "cms.hh"		/* class CMS */
#include "globmem.hh"		/* class GLOBMEM */
#include "timer.hh"		/* class RCS_TIMER */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "physmem.hh"		// class PHYSMEM_HANDLE
#include "sem.hh"		// class RCS_SEMAPHORE 


// USE_SHAREDMEM_TO_TEST_GLOBMEM --  silly option used for doing some debugging on a linux laptop when a real GLOBMEM system not
// available.
#ifdef BUILD_PLATFORM_FOR_TEST
#if BUILD_PLATFORM_FOR_TEST == autoconf-i686-pc-linux-gnu
#define USE_SHAREDMEM_TO_TEST_GLOBMEM 1
#endif
#endif



#ifdef USE_SHAREDMEM_TO_TEST_GLOBMEM
#include "shm.hh"		// class RCS_SHAREDMEM 
#endif

#define SEM_DELAY_EPSILON  (1.0E-6)
#define DEFAULT_SEM_DELAY (1.0E-3)
#define SEM_CHECK_TIME_EPSILON (1.0E-7)

static const bool use_physmem_handle=false;

struct GMEM_PRIVATE_INFO
{
  class RCS_SEMAPHORE *sem;
  unsigned long local_mutex_key;
  bool local_mutex_master;

#ifdef USE_SHAREDMEM_TO_TEST_GLOBMEM
  class RCS_SHAREDMEM *shm;
  unsigned long shm_key;
#endif
};

static unsigned long toggle_bit_changes=0;
static unsigned long simultaneous_rw_count=0;
static unsigned long access_sleep_count=0;


/* External data */
unsigned long (*get_physical_address_func) (char *)
  = (unsigned long (*)(char *)) NULL;


/* Global Definitions. */
static int
convert2upper (char *dest, char *src, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      if (src[i] == 0)
	{
	  dest[i] = 0;
	  return i;
	}
      dest[i] = toupper (src[i]);
    }
  return i;
}

/* GLOBMEM Member Functions */

#ifdef VXWORKS
int RCS_BOARD_TYPE = UNKNOWN_BOARD_TYPE;
int usrSmObjInitInitialized = 0;
int VxMP_Option_Installed = 0;
int dma_being_used = 0;
#endif


/* Constructor used by cms_config. */
GLOBMEM::GLOBMEM (const char *bufline, const char *procline, int set_to_server,
		  int set_to_master):
CMS (bufline, procline, set_to_server)
{
  int address_space_code = 0;
  RCS_TIMER timer (1.0);
  char *address_equation;
  char *semdelay_equation;
  char *sem_spin_delay_equation;
  physical_address = 0;
  skip_area = 0;
  local_locks = (char *) NULL;
  physmem_handle = (PHYSMEM_HANDLE *) NULL;
  sem_delay = DEFAULT_SEM_DELAY;
  half_size = size / 2;
  half_offset = size / 2;
  gmem_private_info=0;
  no_mutex=false;
  toggle_split_buffer_toggle_bit_needed=false;
  //use_physmem_handle=false;

  if (NULL != strstr (buflineupper, "MUTEX=NONE"))
    {
      no_mutex=true;
    }

#ifdef VXWORKS
  board_type = RCS_BOARD_TYPE;
  use_dma = 0;
  bl_info = 0;
  dma_info = 0;
#endif
  lock_bus = 0;

#ifdef VXWORKS
  char *board_type_eq = NULL;
  if (NULL != (board_type_eq = sysModel ()))
    {
      if (!strncmp (board_type_eq, "MVME162", 7))
	{
	  board_type = VX_MVME162_BOARD_TYPE;
	}
    }
  if (NULL != (board_type_eq = strstr (buflineupper, "BD_TYPE=")))
    {
      board_type_eq += 8;
      if (!strncmp (board_type_eq, "MVME162", 7))
	{
	  board_type = VX_MVME162_BOARD_TYPE;
	}
    }

  if (NULL != (board_type_eq = strstr (proclineupper, "BD_TYPE=")))
    {
      board_type_eq += 8;
      if (!strncmp (board_type_eq, "MVME162", 7))
	{
	  board_type = VX_MVME162_BOARD_TYPE;
	}
    }
#endif


#ifdef VXWORKS
#ifndef NO_BUS_LOCK_SUPPORT
  if (NULL != strstr (buflineupper, "LOCK_BUS"))
    {
      if (board_type == UNKNOWN_BOARD_TYPE)
	{
	  rcs_print_error
	    ("Can not use lock_bus option with unknown board type.\n");
	}
      else
	{
	  lock_bus = 1;
	  total_connections = 0;
	}
    }
#endif
  lock_task = 1;
  if (NULL != strstr (buflineupper, "NO_TASK_LOCK"))
    {
      lock_task = 0;
    }
  if (NULL != strstr (proclineupper, "NO_TASK_LOCK"))
    {
      lock_task = 0;
    }

#if HAVE_VX_TAS
  use_test_and_set = 1;
  if (NULL != strstr (buflineupper, "NO_TEST_AND_SET"))
    {
      use_test_and_set = 0;
    }
#else
  use_test_and_set = 0;
#endif


#ifndef NO_DMA_SUPPORT
  if (NULL != strstr (proclineupper, "USE_DMA"))
    {
      if (board_type == UNKNOWN_BOARD_TYPE)
	{
	  rcs_print_error
	    ("Can not use lock_bus option with unknown board type.\n");
	}
      else
	{
	  use_dma = 1;
	}
    }
#endif
#endif

  if (total_connections <= connection_number && !lock_bus)
    {
      rcs_print_error
	("GLOBMEM: connection number(%ld) must be less than total connections (%ld).\n",
	 connection_number, total_connections);
      status = CMS_CONFIG_ERROR;
      return;
    }

  address_type = INVALID_ADDRESS_TYPE;
  convert2upper (address_type_name, ProcessName, CMS_CONFIG_LINELEN);

  if (1 == set_to_master)
    {
      is_local_master = true;
    }
  else if (-1 == set_to_master)
    {
      is_local_master = false;
    }


#ifdef USE_SHAREDMEM_TO_TEST_GLOBMEM
  if(NULL != strstr(buflineupper,"USE_SHMEM_FOR_TESTING_GMEM"))
    {
      if(0 == gmem_private_info)
	{
	  gmem_private_info = new GMEM_PRIVATE_INFO;
	}
      char *smk= strstr(buflineupper,"SHM_KEY=");
      if(smk)
	{
	  gmem_private_info->shm_key =strtol(smk+8,0,0);
	}
      if (is_local_master)
	{
	  gmem_private_info->shm = new RCS_SHAREDMEM (gmem_private_info->shm_key, 
						      size, 
						      RCS_SHAREDMEM_CREATE, 
						      (int) 0777);
	}
      else
	{
	  gmem_private_info->shm = new RCS_SHAREDMEM (gmem_private_info->shm_key, 
						      size, 
						      RCS_SHAREDMEM_NOCREATE);
	}
      physical_address = (unsigned long) (gmem_private_info->shm->addr);
      address_type = GENERIC_ADDRESS;
    }
#endif

  if (address_type == INVALID_ADDRESS_TYPE)
    {
      strcat (address_type_name, "_ADDR=");
      if (NULL != (address_equation = strstr (buflineupper, address_type_name)))
	{
	  physical_address =
	    get_physical_address (address_equation + strlen (address_type_name));
	  address_type = PROCESS_SPECIFIC_ADDRESS;
	}
    }
  if (address_type == INVALID_ADDRESS_TYPE)
    {
      convert2upper (address_type_name, ProcessHost, CMS_CONFIG_LINELEN);
      strcat (address_type_name, "_ADDR=");
      if (NULL !=
	  (address_equation = strstr (buflineupper, address_type_name)))
	{
	  physical_address =
	    get_physical_address (address_equation +
				  strlen (address_type_name));
	  address_type = HOST_SPECIFIC_ADDRESS;
	}
    }
#if defined(VXWORKS) || defined(linuxVME)
  if (address_type == INVALID_ADDRESS_TYPE)
    {
      strcpy (address_type_name, "VME_ADDR=");
      if (NULL !=
	  (address_equation = strstr (buflineupper, address_type_name)))
	{
	  physical_address =
	    get_physical_address (address_equation +
				  strlen (address_type_name));
	  address_type = VME_ADDRESS;
	  strcpy (address_type_name, "VME_CODE=");
	  address_space_code = 0;
	  if (NULL !=
	      (address_equation = strstr (buflineupper, address_type_name)))
	    {
	      address_space_code = (unsigned long)
		strtol (address_equation + strlen (address_type_name),
			(char **) NULL, 0);
	    }
	}
    }
#endif
  if (address_type == INVALID_ADDRESS_TYPE)
    {
      strcpy (address_type_name, "GENERIC_ADDR=");
      if (NULL !=
	  (address_equation = strstr (buflineupper, address_type_name)))
	{
	  physical_address =
	    get_physical_address (address_equation +
				  strlen (address_type_name));
	  address_type = GENERIC_ADDRESS;
	}
    }

  if (address_type == INVALID_ADDRESS_TYPE)
    {
      rcs_print_error ("GLOBMEM: No applicable address specified.\n");
      status = CMS_CONFIG_ERROR;
      return;
    }

#ifndef USE_BIT3
  if (0 == physical_address)
    {
      rcs_print_error ("GLOBMEM: physical address is zero.\n");
      status = CMS_CONFIG_ERROR;
      return;
    }
  // end ifndef USE_BIT3
#endif


#ifdef VXWORKS
#ifndef NO_BUS_LOCK_SUPPORT
  if (lock_bus)
    {
      bl_info = getBusLockInfo (board_type, physical_address);
      if (NULL == bl_info)
	{
	  rcs_print_error
	    ("Can't get bus lock info for board type %d, and address 0x%lX\n",
	     board_type, 
	     ((unsigned long)physical_address));
	  status = CMS_CONFIG_ERROR;
	  return;
	}
    }
#endif

#ifndef NO_DMA_SUPPORT
  if (use_dma)
    {
      dma_info = getDMAInfo (board_type, physical_address);
      if (NULL == dma_info)
	{
	  rcs_print_error
	    ("Can't get DMA info for board type %d, and address 0x%lX\n",
	     board_type, 
	     ((unsigned long)physical_address) );
	  status = CMS_CONFIG_ERROR;
	  use_dma = 0;
	  return;
	}
    }
#endif

  if (use_dma && lock_bus)
    {
      rcs_print_error
	("GLOBMEM: Can't use DMA and use the bus lock for mutual exclusion.\n");
      status = CMS_CONFIG_ERROR;
      use_dma = 0;
      return;
    }

  if (use_dma && neutral)
    {
      rcs_print_error ("GLOBMEM:: Can't use DMA with neutral buffers.\n");
      status = CMS_CONFIG_ERROR;
      use_dma = 0;
      return;
    }

  if (use_dma && queuing_enabled)
    {
      rcs_print_error ("GLOBMEM: Can't use DMA with queued buffers.\n");
      status = CMS_CONFIG_ERROR;
      use_dma = 0;
      return;
    }

  /* if(use_dma && dma_being_used)
     {
     rcs_print_error("DMA already used by another process/buffer.\n");
     rcs_print_error("Only 1 NMLConnection can use DMA per board.\n");
     status = CMS_CONFIG_ERROR;
     use_dma = 0;
     return;
     }
   */
  if (use_dma)
    {
      dma_being_used = 1;
    }
  // end ifdef VXWORKS
#endif


#if !defined(USE_BIT3) || !defined(WIN32)
  physmem_handle = new PHYSMEM_HANDLE (physical_address,
				       address_space_code, size);
  if(!physmem_handle || !physmem_handle->valid())
    {
      rcs_print_error("Bad physmem_handle.");
      status = CMS_MISC_ERROR;
      return;
    }
  // end #if !defined(USE_BIT3) || !defined(WIN32)
#endif

  use_sem_spin_delay=false;
  sem_spin_delay=0.0;
  if(strstr(proclineupper,"USE_SEM_SPIN_DELAY"))
    {
      use_sem_spin_delay=true;
    }
  else if(strstr(buflineupper,"USE_SEM_SPIN_DELAY"))
    {
      use_sem_spin_delay=true;
    }
  sem_spin_delay=((connection_number%50)+1) * 1e-5;
  

  if (NULL != (semdelay_equation = strstr (proclineupper, "SEMDELAY=")))
    {
      sem_delay = strtod (semdelay_equation + 9, (char **) NULL);
    }
  else if (NULL != (semdelay_equation = strstr (buflineupper, "SEMDELAY=")))
    {
      sem_delay = strtod (semdelay_equation + 9, (char **) NULL);
    }

  if (NULL != (sem_spin_delay_equation = strstr (proclineupper, "SEM_SPIN_DELAY=")))
    {
      sem_spin_delay = strtod (sem_spin_delay_equation + 15, (char **) NULL);
    }
  else if (NULL != (sem_spin_delay_equation = strstr (buflineupper, "SEM_SPIN_DELAY=")))
    {
      sem_spin_delay = strtod (sem_spin_delay_equation + 15, (char **) NULL);
    }

  /* if this is the master, clear the buffer */
  if (is_local_master)
    {
      physmem_handle->memsetf (0, 0, size);
    }
  local_locks = (char *) NULL;
  int remainder;
  if (split_buffer)
    {
      remainder = (physical_address + total_connections + 2) % 4;
      skip_area = total_connections + 2;
    }
  else
    {
      remainder = (physical_address + total_connections) % 4;
      skip_area = total_connections;
    }
  if (remainder)
    {
      skip_area += (4 - remainder);
    }
  local_locks = (char *) malloc ((long) total_connections);

#ifdef VXWORKS
  bsem = 0;
#endif

  if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
    {
      if (is_local_master)
	{
	  physmem_handle->write (BufferName, 31);
#ifdef VXWORKS
	  if (NULL != strstr (buflineupper, "BSEM"))
	    {
	      if (VxMP_Option_Installed)
		{
#if 0
		  if (!usrSmObjInitInitialized)
		    {
		      if (OK == usrSmObjInit (NULL))
			{
			  usrSmObjInitInitialized = 1;
			}
		      else
			{
			  rcs_print_error ("usrSmObjInit error %d %s",
					   errno, strerror (errno));
			}
		    }
		  if (usrSmObjInitInitialized)
		    {
		      physmem_handle->offset = 32;
		      bsem = semBSmCreate (0, SEM_EMPTY);
		      void *glob_bsem = smObjLocalToGlobal ((void *) bsem);
		      physmem_handle->write (&glob_bsem, sizeof (void *));
		    }
		  // end if 0
#endif
		}
	    }
	  // end #ifdef VXWORKS
#endif
	}
      else
	{
	  char buf_name_check[32];
	  physmem_handle->read (buf_name_check, 31);
	  buf_name_check[31] = 0;
	  if (buf_name_check[0] != BufferName[0]
	      && !isalnum (buf_name_check[0]))
	    {
	      rcs_print_error
		("GLOBMEM: buffer not initialized, start master.\n");
	      status = CMS_NO_MASTER_ERROR;
	      return;
	    }
	  if (strncmp (buf_name_check, BufferName, 31))
	    {
	      rcs_print_error
		("GLOBMEM: Buffers %s and %s may have conflicting addresses..\n",
		 buf_name_check, BufferName);
	      status = CMS_MISC_ERROR;
	      return;
	    }
#ifdef VXWORKS
	  if (NULL != strstr (buflineupper, "BSEM"))
	    {
	      if (VxMP_Option_Installed)
		{
#if 0
		  if (!usrSmObjInitInitialized)
		    {
		      if (OK == usrSmObjInit (NULL))
			{
			  usrSmObjInitInitialized = 1;
			}
		      else
			{
			  rcs_print_error ("usrSmObjInit error %d %s",
					   errno, strerror (errno));
			}
		    }
		  if (usrSmObjInitInitialized)
		    {
		      physmem_handle->offset = 32;
		      void *glob_bsem = NULL;
		      physmem_handle->read (&glob_bsem, sizeof (void *));
		      bsem = (SEM_ID) smObjGlobalToLocal (glob_bsem);
		    }
		  // end #if 0
#endif
		}
	    }
	  // end #ifdef VXWORKS
#endif
	}
      skip_area += 36;
      physical_address += 36;
    }

  if (total_subdivisions <= 0)
    {
      total_subdivisions = 1;
    }

  if (split_buffer)
    {
      if (neutral)
	{
	  max_message_size =
	    (size_without_diagnostics / 2) - skip_area - encoded_header_size;
	  max_encoded_message_size =
	    size_without_diagnostics - skip_area - total_connections -
	    encoded_header_size;
	  guaranteed_message_space = max_message_size / 4;
	}
      else
	{
	  max_message_size =
	    (size_without_diagnostics / 2) - skip_area - sizeof (CMS_HEADER);
	  max_encoded_message_size = 4 * max_message_size;
	  guaranteed_message_space = max_message_size;
	}
    }
  else
    {
      subdiv_size =
	(size_without_diagnostics - skip_area) / total_subdivisions;
      subdiv_size -= (subdiv_size % 4);
      if (neutral)
	{
	  max_message_size = subdiv_size - encoded_header_size;
	  max_encoded_message_size = subdiv_size - encoded_header_size;
	  guaranteed_message_space = max_message_size / 4;
	}
      else
	{
	  max_message_size = subdiv_size - sizeof (CMS_HEADER);
	  max_encoded_message_size = 4 * max_message_size;
	  guaranteed_message_space = max_message_size;
	}
    }

  if (enc_max_size > 0 && enc_max_size < max_encoded_message_size)
    {
      max_encoded_message_size = enc_max_size;
    }

  handle_to_global_data = physmem_handle;
  fast_mode = split_buffer && !neutral && !queuing_enabled &&
    (timeout < SEM_CHECK_TIME_EPSILON && timeout >= 0.0) && 
    (read_timeout < SEM_CHECK_TIME_EPSILON && read_timeout >= 0.0) && 
    (write_timeout < SEM_CHECK_TIME_EPSILON && write_timeout >= 0.0);
#ifdef VXWORKS
  my_lock = (char *) physical_address + connection_number;
  toggle_bit_address = (char *) physical_address + total_connections;
  was_read_address = (char *) physical_address + total_connections + 1;
#endif


  if(NULL != strstr(buflineupper,"USE_LOCAL_MUTEX"))
    {
      if(0 == gmem_private_info)
	{
	  gmem_private_info = new GMEM_PRIVATE_INFO;
	}
      gmem_private_info->local_mutex_master = (NULL != strstr(proclineupper,"LOCAL_MUTEX_MASTER")) || is_local_master;
      gmem_private_info->local_mutex_key = (unsigned long) physical_address;
      char *lmk= strstr(buflineupper,"LOCAL_MUTEX_KEY");
      if(lmk)
	{
	  gmem_private_info->local_mutex_key =strtol(lmk+15,0,0);
	}
      if(gmem_private_info->local_mutex_master)
	{
	  gmem_private_info->sem = 
	    new RCS_SEMAPHORE (gmem_private_info->local_mutex_key,
			       RCS_SEMAPHORE_CREATE, timeout, (int)0,1);
	}
      else
	{
	  gmem_private_info->sem = 
	    new RCS_SEMAPHORE (gmem_private_info->local_mutex_key,
			       RCS_SEMAPHORE_NOCREATE, timeout);
	}	  
      lock_task=0;
    }

  rcs_print_debug (PRINT_CMS_CONSTRUCTORS, "GLOBMEM::GLOBMEM() returning. this=%p,gmem_private_info=%p,lock_task=%d,lock_bus=%d,use_dma=%d,fast_mode=%d,split_buffer=%d,physical_address=0x%lX,connection_number=%ld,my_lock=%p,toggle_bit_address=%p,was_read_address=%p,timeout=%f\n",
		   ((void *)this),
		   ((void *)gmem_private_info),
		   lock_task,lock_bus,
		   use_dma,fast_mode,
		   split_buffer,
		   physical_address,
		   connection_number,
		   ((void *) my_lock),
		   ((void *) toggle_bit_address),
		   ((void *) was_read_address),
		   timeout);
  if(gmem_private_info)
    {
      rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"gmem_private_info->sem=%p,gmem_private_info->local_mutex_key=%lu(0x%lX),gmem_private_info->local_mutex_master=%d\n",
		      ((void *)gmem_private_info->sem),
		      gmem_private_info->local_mutex_key,
		      gmem_private_info->local_mutex_key,
		      gmem_private_info->local_mutex_master);
    }
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: size = %ld\n", size);
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: total_connections = %ld\n", total_connections);
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: skip_area = %d\n", skip_area);
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: max_message_size = %ld\n", max_message_size);
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: max_encoded_message_size = %ld\n",
	     max_encoded_message_size);
  rcs_print_debug(PRINT_CMS_CONSTRUCTORS,"GLOBMEM: guaranteed_message_space = %ld\n", max_message_size);

}

/* Destructor. */
GLOBMEM::~GLOBMEM ()
{
  if (NULL != physmem_handle)
    {
      if (delete_totally)
	{
	  physmem_handle->memsetf (0, 0, size);
	}
      delete physmem_handle;
      physmem_handle = (PHYSMEM_HANDLE *) NULL;
    }
  if (NULL != local_locks)
    {
      free (local_locks);
      local_locks = (char *) NULL;
    }
#ifdef VXWORKS
#ifndef NO_BUS_LOCK_SUPPORT
  if (NULL != bl_info)
    {
      freeBusLockInfo (bl_info);
    }
#endif
#ifndef NO_DMA_SUPPORT
  if (NULL != dma_info)
    {
      freeDMAInfo (dma_info);
      dma_being_used = 0;
    }
#endif

  // end #ifdef VXWORKS
#endif
  GMEM_PRIVATE_INFO *gpi = gmem_private_info;
  gmem_private_info=0;
  if(gpi)
    {
        RCS_SEMAPHORE *sem= gpi->sem;
	gpi->sem=0;
	if(sem)
	  {
	    delete sem;
	  }
	delete gpi;
    }
#if 0
  printf("GLOBMEM toggle_bit_changes=%lu\n",toggle_bit_changes);
  printf("GLOBMEM simultaneous_rw_count=%lu\n",simultaneous_rw_count);
  printf("GLOBMEM access_sleep_count=%lu\n",access_sleep_count);
#endif

}

unsigned long
GLOBMEM::get_physical_address (char *_address_string)
{
  if (NULL != get_physical_address_func)
    {
      return (get_physical_address_func (_address_string));
    }
  if (NULL == _address_string)
    {
      return (0);
    }
  if (isdigit (_address_string[0]))
    {
      return ((unsigned long) strtoul (_address_string, (char **) NULL, 0));
    }

  /* There used to be several other cards and operating system weird ways ways of getting an address especially Bit3 + Windows stuff,
     which has been eliminated. */

  /* No Way Found. */
  return (0);
}


/* Access the global memory buffer. */
CMS_STATUS GLOBMEM::main_access (void *_user_data)
{
  if ((blocking_timeout > 1e-6 || blocking_timeout < -1e-6))
    {
      rcs_print_error (" Can not call blocking_read when using GLOBMEM.\n");
      return (status = CMS_NO_BLOCKING_SEM_ERROR);
    }

  /* check pointers */
  char was_read_byte;

  write_just_completed = false;
  read_only = ((internal_access_type == CMS_CHECK_IF_READ_ACCESS) ||
	       (internal_access_type == CMS_READ_ACCESS) ||
	       (internal_access_type == CMS_PEEK_ACCESS));

#ifdef VXWORKS
#ifndef NO_DMA_SUPPORT
  if (use_dma)
    {
      int dma_status = checkForDMADone (dma_info);
      double time_diff =0;
      double start_time =0;

      if (timeout > 1e-6)
	{
	  start_time = etime ();
	}
      while (1 != dma_status)
	{
	  if (dma_status < 0)
	    {
	      status = CMS_MISC_ERROR;
	      return (status);
	    }
	  time_diff = etime () - start_time;
	  if (time_diff >= (timeout / 2))
	    {
	      rcs_print_error ("Timed out waiting for DMA Ready!\n");
	      status = CMS_TIMED_OUT;
	      return (status);
	    }
	  dma_status = checkForDMADone (dma_info);
	}
    }
#endif
#endif

  /* acquire  access */
  if (get_access () == -1)
    {
      return (status);
    }

  if (split_buffer && !read_only)
    {
      if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
	{
	  physmem_handle->set_offset(total_connections + 36);
	}
      else
	{
	  physmem_handle->set_offset(total_connections);
	}
      if (-1 == physmem_handle->write (&toggle_bit, 1))
	{
	  rcs_print_error
	    ("GLOBMEM: Error occured while reading physical memory.\n");
	  status = CMS_MISC_ERROR;
	  return (status);
	}
    }

  physmem_handle->set_offset(0);
  physmem_handle->set_size(size);
  
  /* Perform access function. */
  internal_access (physmem_handle, _user_data);


  if (!read_only && status > 0)
    {
      write_just_completed = true;
    }

  /* give up  access */
  if (release_access () == -1)
    {
      return (status);
    }

  if (split_buffer && !read_only)
    {
      was_read_byte = 0;
      if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
	{
	  physmem_handle->set_offset(total_connections + 37);
	}
      else
	{
	  physmem_handle->set_offset(total_connections + 1);
	}
      if (-1 == physmem_handle->write (&was_read_byte, 1))
	{
	  rcs_print_error ("GLOBMEM: can not set was read flag.\n");
	  release_access ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  return (status);
}


void
GLOBMEM::toggle_split_buffer_toggle_bit()
{
  register char *ptr;
  register char *base = (char *) physical_address;

  toggle_split_buffer_toggle_bit_needed=false;
  toggle_bit = !toggle_bit;
  if(!use_physmem_handle)
    {
      ptr = base+total_connections;
      *ptr = toggle_bit;
    }
  else
    {
      if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
	{
	  physmem_handle->set_offset(total_connections + 36);
	}
      else
	{
	  physmem_handle->set_offset(total_connections);
	}
      if (-1 == physmem_handle->write (&toggle_bit, 1))
	{
	  rcs_print_error
	    ("GLOBMEM: Error occured while reading physical memory.\n");
	  status = CMS_MISC_ERROR;
	}  
    }
}

bool  
GLOBMEM::check_locks_clear()
{
  register char *ptr;
  register char *base = (char *) physical_address;

  toggle_split_buffer_toggle_bit_needed=false;

  if(split_buffer)
    {
      if(!use_physmem_handle)
	{
	  ptr = base+total_connections;
	  toggle_bit = *ptr;
	}
      else
	{
	  physmem_handle->set_offset(total_connections + 36);
	  if (-1 == physmem_handle->read (&toggle_bit, 1))
	    {
	      status = CMS_MISC_ERROR;
	      rcs_print_error
		("GLOBMEM: Error occured while reading physical memory.\n");
	      return (-1);
	    }
	}
    }

  if(!use_physmem_handle)
    {
      memcpy (local_locks, base, total_connections);
    }
  else
    {
      physmem_handle->set_offset(36);
      if (-1 == physmem_handle->read (local_locks, 
				      total_connections))
	{
	  status = CMS_MISC_ERROR;
	  release_access ();
	  rcs_print_error
	    ("GLOBMEM: Error occured while reading physical memory.\n");
	  return (-1);
	}
    }
  if(split_buffer)
    {
      bool side0_clear=true;
      bool side1_clear=true;
      for (int i = 0; i < total_connections; i++)
	{
	  if (0 != local_locks[i])
	    {
	      if (i != connection_number)
		{
		  if(local_locks[i] == 2)
		    {
		      side1_clear = false;
		      if(!side0_clear)
			{
			  return(0);
			}
		    }
		  else if(local_locks[i] == 3)
		    {
		      side0_clear = false;
		      if(!side1_clear)
			{
			  return(0);
			}
		    }
		  else
		    {
		      return(0);
		    }
		}
	    }
	}
      if(side0_clear && side1_clear)
	{
	  return 1;
	}
      if(side0_clear)
	{
	  if(toggle_bit != 0)
	    {
	      toggle_bit_changes++;
	      toggle_split_buffer_toggle_bit_needed=true;
	      return 0;
	    }
	  simultaneous_rw_count++;
	  return 1;
	}
      else if(side1_clear)
	{
	  if(toggle_bit !=  1)
	    {
	      toggle_bit_changes++;
	      toggle_split_buffer_toggle_bit_needed=true;
	      return 0;
	    }
	  simultaneous_rw_count++;
	  return 1;
	}
      return 0;
    }
  else
    {
      for (int i = 0; i < total_connections; i++)
	{
	  if (0 != local_locks[i])
	    {
	      if (i != connection_number && 
		  !(read_only && local_locks[i] > 1))
		{
		  return(0);
		}
	    }
	}
    }
  return(1);
}


int
GLOBMEM::get_access ()
{
  register char *ptr;
  register char *base = (char *) physical_address;

  if(no_mutex)
    {
      return 0;
    }


  char write_char;
  double start_time, time;

  if (physmem_handle == NULL)
    {
      rcs_print_error ("GLOBMEM: No handle to physical memory.\n");
      status = CMS_MISC_ERROR;
      return (-1);
    }

#ifdef VXWORKS
#if HAVE_VX_TAS
  if (use_test_and_set)
    {
      char *tas_address = ((char *)physmem_handle->get_physical_address()) + 36;
      if (timeout > 0)
	{
	  start_time = etime ();
	}
      while (1)
	{
	  if (vxTas (tas_address) == TRUE)
	    {
	      // got it
	      return 0;
	    }
	  if (timeout > 0)
	    {
	      if (etime () - start_time > timeout)
		{
		  status = CMS_TIMED_OUT;
		  return (-1);
		}
	    }
	  else if (timeout == 0)
	    {
	      status = CMS_TIMED_OUT;
	      return -1;
	    }
	  access_sleep_count++;
	  if (sem_delay > SEM_DELAY_EPSILON)
	    {
	      esleep (sem_delay);
	    }
	}
    }
  // HAVE_VX_TAS
#endif
  // VXWORKS
#endif


  RCS_SEMAPHORE *sem= 0;
  if(gmem_private_info)
    {
      sem = gmem_private_info->sem;
    }

  
  if(sem && sem->wait() < 0)
    {
      return -1;
    }


#ifdef VXWORKS
  if (lock_task)
    {
      taskLock ();
    }
#endif

#ifdef VXWORKS
#ifndef NO_BUS_LOCK_SUPPORT
  if (lock_bus)
    {
      return enableBusLock (bl_info);
    }
#endif
#endif


  if (split_buffer)
    {
#if 0
      char four = 4;
      physmem_handle->set_offset(connection_number + 36);
      if (-1 == physmem_handle->write (&four, 1))
	{
	  status = CMS_MISC_ERROR;
	  rcs_print_error
	    ("GLOBMEM: Error occured while reading physical memory.\n");
	  return (-1);
	}
#endif
      physmem_handle->set_offset(total_connections + 36);
      if (-1 == physmem_handle->read (&toggle_bit, 1))
	{
	  status = CMS_MISC_ERROR;
	  rcs_print_error
	    ("GLOBMEM: Error occured while reading physical memory.\n");
	  return (-1);
	}
    }

  if (read_only)
    {
      if(split_buffer)
	{
	  write_char = 2 + toggle_bit;
	}
      else
	{
	  write_char = 2;
	}
    }
  else
    {
      write_char = 1;
    }

  if(!use_physmem_handle)
    {
      ptr = base + connection_number;
      *ptr = write_char;
    }
  else
    {
      physmem_handle->set_offset(connection_number + 36);
      if (-1 == physmem_handle->write (&write_char, 1))
	{
	  status = CMS_MISC_ERROR;
	  release_access ();
	  rcs_print_error
	("GLOBMEM: Error occured while writing to physical memory.\n");
	  return (-1);
	}
    }

  if (split_buffer && read_only)
    {
      return 0;
    }

  if (check_locks_clear())
    {
      return 0;
    }

  if(toggle_split_buffer_toggle_bit_needed)
    {
      toggle_split_buffer_toggle_bit();
      if (check_locks_clear())
	{
	  return 0;
	}
    }
      
  time = start_time = etime ();
  do
    {
      if (split_buffer)
	{
	  physmem_handle->set_offset(total_connections + 36);
	  if (-1 == physmem_handle->read (&toggle_bit, 1))
	    {
	      status = CMS_MISC_ERROR;
	      release_access ();
	      rcs_print_error
		("GLOBMEM: Error occured while reading physical memory.\n");
	      return (-1);
	    }
	}

      if (check_locks_clear())
	{
	  return 0;
	}
      
      if(toggle_split_buffer_toggle_bit_needed)
	{
	  toggle_split_buffer_toggle_bit();
	  if (check_locks_clear())
	    {
	      return 0;
	    }
	}
      if (fabs (timeout) < SEM_CHECK_TIME_EPSILON)
	{
	  break;
	}
      if (sem_delay > SEM_DELAY_EPSILON)
	{
	  release_access ();
	  access_sleep_count++;
	  esleep (sem_delay);
	  if(sem && sem->wait() < 0)
	    {
	      return -1;
	    }
#ifdef VXWORKS
	  if (lock_task)
	    {
	      taskLock ();
	    }
#endif
	  if(!use_physmem_handle)
	    {
	      ptr = base + connection_number;
	      *ptr = write_char;
	    }
	  else
	    {
	      physmem_handle->set_offset(connection_number + 36);
	      if (-1 == physmem_handle->write (&write_char, 1))
		{
		  status = CMS_MISC_ERROR;
		  release_access ();
		  rcs_print_error
		    ("GLOBMEM: Error occured while writing to physical memory.\n");
		  return (-1);
		}
	    }
	}
      else if(use_sem_spin_delay)
	{
	  release_access ();
	  double sem_spin_start_time = etime();
	  double sem_spin_end_time = sem_spin_start_time + sem_spin_delay;
	  if(sem_spin_end_time > (start_time + timeout) && timeout >= 0)
	    {
	      sem_spin_end_time = start_time + timeout;
	    }
	  // spin waiting for clear, but since we do not have the lock if it is clear we
	  // just break and try again.
	  while(etime() < sem_spin_end_time)
	    {
	      if (check_locks_clear())
		{
		  return 0;
		}
	      
	      if(toggle_split_buffer_toggle_bit_needed)
		{
		  toggle_split_buffer_toggle_bit();
		  if (check_locks_clear())
		    {
		      return 0;
		    }
		}

	    }
	  if(sem && sem->wait() < 0)
	    {
	      return -1;
	    }
#ifdef VXWORKS
	  if (lock_task)
	    {
	      taskLock ();
	    }
#endif
	  if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
	    {
	      physmem_handle->set_offset(connection_number + 36);
	    }
	  else
	    {
	      physmem_handle->set_offset(connection_number);
	    }
	  if (-1 == physmem_handle->write (&write_char, 1))
	    {
	      status = CMS_MISC_ERROR;
	      release_access ();
	      rcs_print_error
		("GLOBMEM: Error occured while writing to physical memory.\n");
	      return (-1);
	    }
	}
      else
	{
	  release_access();
	  // 	  for(volatile int stupid_count = 0; 
	  // 	      stupid_count < (connection_number+1)*(total_connections+1);
	  // 	      stupid_count++)
	  // 	    {
	  // 	      volatile double x  = stupid_count*1.1;
	  // 	    }
	  if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
	    {
	      physmem_handle->set_offset(connection_number + 36);
	    }
	  else
	    {
	      physmem_handle->set_offset(connection_number);
	    }
	  if(sem && sem->wait() < 0)
	    {
	      return -1;
	    }
#ifdef VXWORKS
	  if (lock_task)
	    {
	      taskLock ();
	    }
#endif
	  if (-1 == physmem_handle->write (&write_char, 1))
	    {
	      status = CMS_MISC_ERROR;
	      release_access ();
	      rcs_print_error
		("GLOBMEM: Error occured while writing to physical memory.\n");
	      return (-1);
	    }
	}
      time = etime ();
    }
  while ((time - start_time) < timeout || timeout < 0);

  status = CMS_TIMED_OUT;
  release_access ();
  rcs_print_error
    ("GLOBMEM: Timed out after %f seconds waiting for access to %s at %f.\n",
     time - start_time, BufferName, time);
  return (-1);
}

int
GLOBMEM::release_access ()
{

  if (split_buffer &&
      (internal_access_type == CMS_WRITE_ACCESS ||
       internal_access_type == CMS_WRITE_IF_READ_ACCESS) &&
      status > 0 && write_just_completed)
    {
      toggle_split_buffer_toggle_bit();
    }

  if(no_mutex)
    {
      return 0;
    }

  char zero = 0;
  if (NULL == physmem_handle)
    {
      status = CMS_MISC_ERROR;
      return (-1);
    }

#ifdef VXWORKS
#ifndef NO_BUS_LOCK_SUPPORT
  if (lock_bus)
    {
      return disableBusLock (bl_info);
    }
#endif
#endif

#ifdef VXWORKS
#if HAVE_VX_TAS
  if (use_test_and_set)
    {
      long *tas_address = (long *)
	((char *) physmem_handle->get_physical_address() + 36);
      *tas_address = 0;
      return 0;
    }
#endif
#endif


  if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
    {
      physmem_handle->set_offset(connection_number + 36);
    }
  else
    {
      physmem_handle->set_offset(connection_number);
    }
  if (-1 == physmem_handle->write (&zero, 1))
    {
      rcs_print_error
	("GLOBMEM: Error occured while writing to physical memory.\n");
      rcs_print_error ("GLOBMEM: Can not release physical memory.\n");
      status = CMS_MISC_ERROR;
      return (-1);
    }
#ifdef VXWORKS
  if (lock_task)
    {
      taskUnlock ();
    }
#endif
  RCS_SEMAPHORE *sem= 0;
  if(gmem_private_info)
    {
      sem = gmem_private_info->sem;
    }
  if(sem && sem->post() < 0)
    {
      return -1;
    }

  return (0);
}

static bool bread_not_implemented_error_printed=false;

CMS_STATUS
GLOBMEM::blocking_read (double btimeout)
{
  if (btimeout < 1e-6 && btimeout > -1e-6)
    {
      return read ();
    }
  if (!bread_not_implemented_error_printed)
    {
      rcs_print_error ("BLOCKING READ NOT IMPLEMENTED FOR GLOBMEM.\n");
      bread_not_implemented_error_printed = true;
    }
  status = CMS_NO_IMPLEMENTATION_ERROR;
  return (status);
}

int
GLOBMEM::check_if_transfers_complete ()
{
#ifdef VXWORKS
  if (!use_dma)
    {
      return 1;
    }
  else
    {
#ifndef NO_DMA_SUPPORT
      return checkForDMADone (dma_info);
#endif
    }
#else
  return 1;
#endif
}

//  defined(ENABLE_RCS_GLOBMEM)

#else
#include "rcs_empty_source"
#endif

