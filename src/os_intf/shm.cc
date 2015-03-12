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


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "shm_no_config.h"

// HAVE_CONFIG_H
#endif

#include "shm.hh"
#include "_shm.h"		/* rcs_shm_open(), rcs_shm_close() */
#include "rcs_prnt.hh"		// rcs_print_error()

RCS_SHAREDMEM::RCS_SHAREDMEM (long key, size_t size, int oflag, int mode,
			      int wait_for_master, double wfm_delay, double wfm_period):
  create_errno(0),addr(0),delete_totally(0),leave_resource(false),shm(0),created(0)
{
  shm = NULL;
  addr = NULL;
  delete_totally = 0;
  create_errno = 0;
  created = 0;
  leave_resource=false;

  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
      	   "Constructing RCS_SHAREDMEM (%ld,%lu,0x%X,0x%X) this=%p\n",
		   key, (unsigned long)size, oflag, mode, (void *) this);
  if (oflag & RCS_SHAREDMEM_CREATE)
    {
      /* create shared memory */
#if defined(VXWORKS) || defined(MS_WINDOWS_API)
      shm = rcs_shm_open (key, size, 1);
#else
#ifdef USE_POSIX_SHAREDMEM
      shm = rcs_shm_open (key, size, O_CREAT, mode);
#else
      shm = rcs_shm_open (key, size, IPC_CREAT, mode);
#endif
#endif
      if (shm == NULL)
	{
	  create_errno = errno;
	  rcs_print_error ("can't create shared memory\n");
	  return;
	}
    }
  else
    {
      /* attach to existing shared memory */
      if(wait_for_master)
	{
	  shm = rcs_shm_open_extended (key, size, 0,0,1,wfm_delay,wfm_period);
	}
      else
	{
	  shm = rcs_shm_open(key,size,0);
	}
      if (shm == NULL)
	{
	  create_errno = errno;
	  rcs_print_error
	    ("can't attach to shared memory-- is master started?\n");
	  return;
	}
    }
  create_errno = rcs_shm_get_create_errno(shm);
  created = rcs_shm_get_created(shm);
  /* duplicate the pointer, so users don't
     have to dig into shm->addr */
  addr = rcs_shm_get_addr(shm);
}

RCS_SHAREDMEM::~RCS_SHAREDMEM ()
{
  rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		   "Destructing RCS_SHAREDMEM this=%p, leave_resource=%d,delete_totally=%d,shm=%p\n", (void*) this,leave_resource,delete_totally,(void *)shm);
  if (shm == NULL)
    {
      return;
    }
  else
    {
      if(!leave_resource)
	{
	  if (delete_totally)
	    {
	      rcs_shm_delete (shm);
	    }
	  else
	    {
	      rcs_shm_close (shm);
	    }
	}
      else
	{
	  rcs_shm_close_leaving_resource(shm);
	}
      shm = NULL;
    }
}

int
RCS_SHAREDMEM::nattch ()
{
  int nattch_ret = -1;
  if (shm)
    {
      nattch_ret = rcs_shm_nattch (shm);
    }
  rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		   "RCS_SHAREDMEM::nattch() returning %d, (this=%p,shm=%p)\n", 
		   nattch_ret,(void *) this,(void *)shm);
  return nattch_ret;
}


  
