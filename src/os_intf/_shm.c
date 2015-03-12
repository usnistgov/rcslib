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


#define SHAREDMEMORY_SOURCE

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#ifndef HAVE_KEY_T_TYPE
typedef long key_t;
#endif
#define KEY_T_DEFINED

#else
#include "_shm_no_config.h"

/* HAVE_CONFIG_H */
#endif

#include "_shm.h"
#include "_timer.h"
#include "rcs_prnt.hh"
#include "dbg_mem.h"		/* DEBUG_MALLOC, DEBUG_CALLOC, DEBUG_FREE */

struct shm_t_struct
{
  int id;
  void *addr;
  int create_errno;
  size_t size;
  int count;
  int created;
  int key;
  char name[64];
#ifdef MS_WINDOWS_API
  HANDLE hFileMap;
#endif
};

 void *rcs_shm_get_addr(struct shm_t_struct *shm)
{
  if(!shm)
    {
      return 0;
    }
  return shm->addr;
}

int rcs_shm_get_created(struct shm_t_struct *shm)
{
  if(!shm)
    {
      return 0;
    }
  return shm->created;
}

int rcs_shm_get_create_errno(struct shm_t_struct *shm)
{
  if(!shm)
    {
      return 0;
    }
  return shm->create_errno;
}

struct shm_t_struct *
rcs_shm_open(long lkey, size_t size, int oflag, .../* master */)
{
  va_list ap;
  int mode;
#ifdef USING_VARARGS
  va_start (ap);
#else
  va_start (ap, oflag);
#endif
  mode=0;
  if(oflag)
    {
      mode = va_arg (ap, int);
    }
  va_end (ap);
  return rcs_shm_open_extended(lkey,size,oflag,mode,0,1.0,-1.0);
}

/*
  _shm.c

  C implementation of rcslib shared memory API

  Modification history:

  30-Mar-1998  FMP added this comment
  */

#ifdef MS_WINDOWS_API

#include <windows.h>

#ifndef NO_STDIO
#include <stdio.h>
#endif

struct shm_t_struct *
rcs_shm_open_extended (long lkey, size_t size, int oflag, 
		       int mode, int wait_for_master, 
		       double wfm_delay, double wfm_period)
{    
  key_t key = (key_t) lkey;
  struct shm_t_struct *shm;
  char name[64];
#ifdef UNICODE
  wchar_t wname[64];
#endif
  SECURITY_ATTRIBUTES sa;
  SECURITY_DESCRIPTOR sd;

  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_open(key=%d(0x%X),size=%d(0x%X),oflag=%d)\n",
		   (int)key, (unsigned) key, size, size, oflag);

  if (key < 1)
    {
      rcs_print_error ("rcs_shm_open(%d(0x%X), %d(0x%X), %d(0x%X)): error\n",
		       (int) key, (unsigned)key, size, size, oflag, oflag);
      rcs_print_error ("RCS Shared Memory key may not be zero.\n");
      return NULL;
    }
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


#ifndef NO_STDIO
  SNPRINTF_FUNC ( SNPRINTF_ARGS(name,sizeof(name)),
		  "shm%d", (int)key);
#else
  strcpy (name, "shm");
  _itoa (key, name + 3, 10);
#endif


  shm = (struct shm_t_struct *) DEBUG_MALLOC (sizeof (struct shm_t_struct));
  if (NULL == shm)
    {
      rcs_print_error ("Out of memory.\n");
      return NULL;
    }
  memset (shm, 0, sizeof (struct shm_t_struct));
  if (oflag)
    {
      shm->created = 1;
      shm->hFileMap =
	CreateFileMapping ((HANDLE) (0xFFFFFFFF), &sa, PAGE_READWRITE, 0,
			   (DWORD) size, name);
      if (NULL == shm->hFileMap)
	{
	  rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,"CreateFileMapping(0xFFFFFFFF,&sa,PAGE_READWRITE,%d(0x%X),%s) failed!!\n", size,size,name);
	  DEBUG_FREE (shm);
	  return NULL;
	}
#if 0
      if ((shm->hFileMap != NULL) &&
	  (GetLastError () == ERROR_ALREADY_EXISTS))
	{
	  rcs_print_error ("File mapping name conflict, name = %s\n", name);;
	  CloseHandle (shm->hFileMap);
	  DEBUG_FREE (shm);
	  return NULL;
	}
#endif
    }
  else
    {
      shm->hFileMap =
	CreateFileMapping ((HANDLE) (0xFFFFFFFF), NULL, PAGE_READWRITE, 0,
			  (DWORD) size, name);
      if (NULL == shm->hFileMap)
	{
	  rcs_print_error ("CreateFileMapping( failed!! (Error = %ld)\n",
			   GetLastError ());
	  DEBUG_FREE (shm);
	  return NULL;
	}
    }

  shm->addr = MapViewOfFile (shm->hFileMap, FILE_MAP_ALL_ACCESS, 0, 0, size);
  if (NULL == shm->addr)
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,"MapViewOfFile(shm->hFileMap,FILE_MAP_ALL_ACCESS,0,0,%d(0x%X) failed!! \n",size,size);
      CloseHandle (shm->hFileMap);
      DEBUG_FREE (shm);
      return NULL;
    }
  return shm;
}


int
rcs_shm_close_leaving_resource (struct shm_t_struct * shm)
{
  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close_leaving_resource(shm->key=%d(0x%X),shm->size=%d(0x%X),shm->addr=%p)\n",
		   shm->key, shm->key, shm->size, shm->size, shm->addr);
  return rcs_shm_close(shm);
}


int
rcs_shm_close (struct shm_t_struct * shm)
{
  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close(shm->key=%d(0x%X),shm->size=%d(0x%X),shm->addr=%p)\n",
		   shm->key, shm->key, shm->size, shm->size, shm->addr);

  if (shm != NULL)
    {
      if (shm->hFileMap != NULL)
	{
	  UnmapViewOfFile (shm->addr);
	  CloseHandle (shm->hFileMap);
	  shm->addr = NULL;
	  shm->hFileMap = NULL;
	}
      DEBUG_FREE(shm);
    }
  return 0;
}

int
rcs_shm_delete (struct shm_t_struct * shm)
{
  return rcs_shm_close (shm);
}



int
rcs_shm_nattch (struct shm_t_struct * shm)
{
  /* FIXME: I don't know how to get this number but it doesn't seem to
   * be neccessary since rcs_shm_delete and rcs_shm_close are identical
   * on this platform. */
  return 1;
}
#endif


#if defined (VXWORKS)

/* VxWorks shared memory */

#include <vxWorks.h>
#include <taskLib.h>

#ifndef NO_STDIO
#include <stdio.h>		/* stderr, etc */
#endif
#include <stdlib.h>		/* malloc, realloc, calloc, _itoa() */
#include "_table.h"


/* OS table of semaphore keys and sem_t's */
_RCS_TABLE rcs_shm_table;
int rcs_shm_table_inited = 0;
int task_lock_shm_stuff = 1;
int print_rcs_shm_warnings = 0;



/* shell debug functions */

void
rcs_shm_table_print ()
{
  if (!rcs_shm_table_inited)
    {
      printf ("RCS Shared Memory table NOT initialized.\n");
      return;
    }
  table_print (&rcs_shm_table);
  printf ("\t\tid\tkey\taddr\terrno\tsize\tcount\n");
}

void
rcs_shm_table_clear ()
{
  if (task_lock_shm_stuff)
    taskLock ();
  rcs_shm_table_inited = 0;
  table_clearall (&rcs_shm_table);
  if (task_lock_shm_stuff)
    taskUnlock ();
}

/* interface functions */

#ifdef VXWORKS

struct shm_t_struct *
rcs_shm_open_extended (long lkey, size_t size, int oflag, 
		       int mode, int wait_for_master, 
		       double wfm_delay, double wfm_period)
{    
  key_t key = (key_t) lkey;
  int tid = taskIdSelf ();
  struct shm_t_struct *shm;

  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_open(key=%d(0x%X),size=%d(0x%X),oflag=%d)\n",
		   key, key, size, size, oflag);

  if (task_lock_shm_stuff)
    taskLock ();

  /* ignore mode-- we always run privileged.  oflag non-zero means
     create it; 0 means it must exist */
  if (key == 0)
    {
      if (task_lock_shm_stuff)
	taskUnlock ();
      rcs_print_error ("rcs_shm_open(%d(0x%X), %d(0x%X), %d(0x%X)): error\n",
		       key, key, size, size, oflag, oflag);
      rcs_print_error ("RCS Shared Memory key is zero.\n");
      return NULL;
    }

  /* get shared mem data structure */
  shm = (struct shm_t_struct *) DEBUG_CALLOC (sizeof (struct shm_t_struct), 1);
  shm->id = (int) key;
  shm->addr = NULL;
  shm->size = -1;
  shm->created = 0;

  /* fill in address by consulting table */
  if (oflag)
    {
      if (rcs_shm_table_inited)
	{
	  if (table_get (&rcs_shm_table, key, shm) >= 0)
	    {
	      /* got the id already-- just copy out the address */
	      if (shm->id != key)
		{
		  if (task_lock_shm_stuff)
		    taskUnlock ();
		  rcs_print_error
		    ("Shared Memory Key retrieved from table does not match. (%d) != (%d).\n",
		     key, shm->id);
		  return NULL;
		}
	      if (shm->addr <= 0)
		{
		  if (task_lock_shm_stuff)
		    taskUnlock ();
		  return NULL;
		}
	      shm->count++;
	      table_add (&rcs_shm_table, key, shm);
	      if (task_lock_shm_stuff)
		taskUnlock ();
	      if (shm->size < size)
		{
		  rcs_print_error
		    ("This process(%d(0x%X)) was configured to create a shared memory area with key = (%d(0x%X)) and size=(%d(0x%X)\n",
		     tid, tid, key, key, size, size);
		  rcs_print_error
		    (" but a shared memory area with this key and a size of only (%d(0x%X)) already exists.",
		     shm->size, shm->size);
		  return NULL;
		}
	      if (print_rcs_shm_warnings)
		{
		  rcs_print_error
		    ("Warning: This process(%d(0x%X)) was configured to create a shared memory area with key = (%d(0x%X)), and size=(%d(0x%X)\n",
		     tid, tid, key, key, size, size);
		  rcs_print_error
		    ("Warning: but a shared memory area with this key has already been created.\n");
		  rcs_print_error ("\n");
		}
	      return shm;
	    }
	}
      shm->addr = (void *) DEBUG_MALLOC (size);
      memset(shm->addr,0,size);
      shm->size = size;
      shm->id = key;
      shm->count = 1;
      shm->created = 1;
      if (!rcs_shm_table_inited)
	{
	  /* get a new table of rcs_shm_table_t's */
	  table_new (&rcs_shm_table, sizeof (struct shm_t_struct));
	  rcs_shm_table_inited = 1;
	}
      table_add (&rcs_shm_table, key, shm);
    }
  else
    {
      /* it's connecting to created memory-- increment the reference count */
      if (!rcs_shm_table_inited)
	{
#if 0
	  shm->addr = (void *) DEBUG_CALLOC (size, 1);
	  shm->size = size;
	  shm->count = 1;
	  shm->id = key;
	  if (!rcs_shm_table_inited)
	    {
	      /* get a new table of rcs_shm_t's */
	      table_new (&rcs_shm_table, sizeof (struct shm_t_struct));
	      rcs_shm_table_inited = 1;
	    }
	  table_add (&rcs_shm_table, key, shm);
	  if (task_lock_shm_stuff)
	    taskUnlock ();

	  if (print_rcs_shm_warnings)
	    {
	      rcs_print_error
		("Warning: RCS Shared Memory Area table contains no entry for (%d(0x%X)), creating one.\n",
		 key, key);
	    }
#endif
	  shm->create_errno = ENOENT;
	  return shm;
	}
      else
	{
	  if (table_get (&rcs_shm_table, key, shm) < 0)
	    {
#if 0
	      shm->addr = (void *) DEBUG_CALLOC (size, 1);
	      shm->size = size;
	      shm->count = 1;
	      shm->id = key;
	      if (!rcs_shm_table_inited)
		{
		  /* get a new table of rcs_shm_t's */
		  table_new (&rcs_shm_table, sizeof (struct shm_t_struct));
		  rcs_shm_table_inited = 1;
		}
	      table_add (&rcs_shm_table, key, shm);
	      if (task_lock_shm_stuff)
		taskUnlock ();

	      if (print_rcs_shm_warnings)
		{
		  rcs_print_error
		    ("Warning: RCS Shared Memory Area table contains no entry for (%d(0x%X)), creating one.\n",
		     key, key);
		}
#endif
	      shm->create_errno = ENOENT;
	      return shm;
	    }
	  if (shm->id != key)
	    {
	      if (task_lock_shm_stuff)
		taskUnlock ();
	      rcs_print_error
		("Shared Memory Key retrieved from table does not match. (%d) != (%d).\n",
		 key, shm->id);
	      return NULL;
	    }
	  if (shm->addr <= 0)
	    {
	      if (task_lock_shm_stuff)
		taskUnlock ();
	      return NULL;
	    }
	  if (shm->size < size)
	    {
	      if (task_lock_shm_stuff)
		taskUnlock ();
	      rcs_print_error
		("This process(%d(0x%X)) was configured to connect a shared memory area with key = (%d(0x%X)) and size=(%d(0x%X)\n",
		 tid, tid, key, key, size, size);
	      rcs_print_error
		(" but a shared memory area with this key and a size of only (%d(0x%X)) already exists.",
		 shm->size, shm->size);
	      return NULL;
	    }
	  shm->count++;
	  table_add (&rcs_shm_table, key, shm);
	  /* got the id already-- just copy out the address */
	}
    }

  if (task_lock_shm_stuff)
    taskUnlock ();

  return shm;
}
#endif


int
rcs_shm_close_leaving_resource (struct shm_t_struct * shm)
{
  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close_leaving_resource(shm =%p)\n",shm);
  if(!shm)
    {
      return -1;
    }
  DEBUG_FREE (shm);
  return 0;
}
  
int
rcs_shm_close (struct shm_t_struct * shm)
{
  struct shm_t_struct entry;

  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close(shm->key=%d(0x%X),shm->size=%d(0x%X),shm->addr=%p)\n",
		   shm->key, shm->key, shm->size, shm->size, 
		   ((void*)shm->addr));

  if (task_lock_shm_stuff)
    taskLock ();


  /* decrement the reference count, and if it's zero remove the
     shared mem */
  table_get (&rcs_shm_table, shm->id, &entry);
  entry.count--;
  if (entry.count == 0 && entry.addr != NULL)
    {
      DEBUG_FREE (entry.addr);
      entry.addr = NULL;
      shm->addr = NULL;
      table_clear (&rcs_shm_table, shm->id);
    }
  else
    {
      /* rewrite the entry to reflect decremented ref count */
      table_add (&rcs_shm_table, shm->id, &entry);
    }

  if (task_lock_shm_stuff)
    taskUnlock ();
  DEBUG_FREE (shm);
  return 0;
}

int
rcs_shm_delete (struct shm_t_struct * shm)
{
  struct shm_t_struct entry;

  if (task_lock_shm_stuff)
    taskLock ();

  /* decrement the reference count, and if it's zero remove the
     shared mem */
  table_get (&rcs_shm_table, shm->id, &entry);
  entry.count--;
  if (entry.addr != NULL)
    {
      DEBUG_FREE (entry.addr);
      entry.addr = NULL;
      shm->addr = NULL;
      table_clear (&rcs_shm_table, shm->id);
    }
  else
    {
      /* rewrite the entry to reflect decremented ref count */
      table_add (&rcs_shm_table, shm->id, &entry);
    }

  if (task_lock_shm_stuff)
    taskUnlock ();

  DEBUG_FREE (shm);
  return 0;
}

int
rcs_shm_nattch (struct shm_t_struct * shm)
{
  struct shm_t_struct entry;

  if (task_lock_shm_stuff)
    taskLock ();

  table_get (&rcs_shm_table, shm->id, &entry);
  shm->count = entry.count;
  if (task_lock_shm_stuff)
    taskUnlock ();
  return shm->count;
}

#endif

#if !defined(MS_WINDOWS_API) && !defined(VXWORKS)

#ifndef HAVE_CONFIG_H

/* Unix shared memory */

#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

#ifdef linux_2_4
#include <linux/posix_types.h>
#endif

#include <stdlib.h>
#ifdef __CENTERLINE__
#define USING_VARARGS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include <errno.h>
#include <stddef.h>

#include <sys/types.h>
#include <unistd.h>
#include <sys/ipc.h>
#if defined(qnx) && !defined(USE_POSIX_SHAREDMEM)
#define USE_POSIX_SHAREDMEM 1
#endif
#ifdef USE_POSIX_SHAREDMEM
#include <fcntl.h>
#include <sys/mman.h>
#else
#include <sys/shm.h>
#endif
#include <string.h>

#endif /* ! HAVE_CONFIG_H */

#ifndef USE_POSIX_SHAREDMEM

struct ipc_perm2
{
  unsigned int key;
  unsigned short uid;			/* owner euid and egid */
  unsigned short gid;
  unsigned short cuid;			/* creator euid and egid */
  unsigned short cgid;
  unsigned short mode;			/* lower 9 bits of access modes */
  unsigned short seq;			/* sequence number */
};

struct shmid_ds2
{
  struct ipc_perm2 shm_perm;	/* operation perms */
  int shm_segsz;		/* size of segment (bytes) */
  long shm_a_time_int;		/* last attach time */
  long shm_d_time_int;		/* last detach time */
  long shm_c_time_int;		/* last change time */
  unsigned short shm_cpid;	/* pid of creator */
  unsigned short shm_lpid;	/* pid of last operator */
  short shm_nattch;		/* no. of current attaches */
  char bigpad[256];
};


struct ipc_perm3
{
  unsigned int key;
  unsigned short uid;			/* owner euid and egid */
  unsigned short gid;
  unsigned short cuid;			/* creator euid and egid */
  unsigned short cgid;
  unsigned short mode;			/* lower 9 bits of access modes */
  unsigned short seq;			/* sequence number */
};

struct shmid_ds3
{
  struct ipc_perm3 shm_perm;	/* operation perms */
  int shm_segsz;		/* size of segment (bytes) */
  int shm_a_time;		/* last attach time */
  int shm_d_time;		/* last detach time */
  int shm_c_time;		/* last change time */
  unsigned short shm_cpid;	/* pid of creator */
  unsigned short shm_lpid;	/* pid of last operator */
  short shm_nattch;		/* no. of current attaches */
  char bigpad[256];
};

#endif

struct local_shmem_info_struct
{
  int refcount;
  struct shm_t_struct *shm;
};
 
static struct local_shmem_info_struct shmems_created_list[512];
static int shmems_created_list_initialized = 0;

struct shm_t_struct *
rcs_shm_open_extended (long lkey, size_t size, int oflag, 
	      int mode, int wait_for_master, 
	      double wfm_delay, double wfm_period)
{
  int wait_for_master_too_long_warning_given=0;
  int shmflg = 0;
  struct shm_t_struct *shm;
  key_t key = (key_t) lkey;
  double start_time;
  double cur_time;
#ifdef USE_POSIX_SHAREDMEM
  int existed_before = 0;
#if HAVE_FSTAT
  struct stat statbuf;
#endif
#else
  union
  {
    struct shmid_ds shared_mem_info1;
    struct shmid_ds2 shared_mem_info2;
    struct shmid_ds3 shared_mem_info3;
  }
  shared_mem_info;

  void *shmat_ret;
  int pid;
  int i;
#endif

  if (oflag)
    {
      shmflg |= mode;
    }
  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
	    "rcs_shm_open(key=%d(0x%X),size=%lu(0x%lX),oflag=%d,mode=0%o)\n",
		   (int) key, (unsigned int) key, 
		   (unsigned long)size, (unsigned long)size, 
		   oflag,mode);
  if (key == 0)
    {
      rcs_print_error ("rcs_shm_open(%d(0x%X), %lu(0x%lX), %d(0x%X)): error\n",
		       (int) key, (unsigned int) key, 
		       (unsigned long)size, (unsigned long)size, oflag, oflag);
      rcs_print_error ("RCS Shared Memory key may not be zero.\n");
      return NULL;
    }

#ifdef USE_POSIX_SHAREDMEM

  shm = (struct shm_t_struct *) DEBUG_CALLOC (sizeof (struct shm_t_struct), 1);
  if (NULL == shm)
    {
      rcs_print_error ("rcs_shm_open: calloc failed\n");
      return NULL;
    }
  shm->create_errno = 0;
  shm->addr = NULL;
  shm->key = key;
  shm->size = size;
  shm->created=0;
#ifdef POSIX_SHMEM_NAME_PREFIX
  strncpy (shm->name, POSIX_SHMEM_NAME_PREFIX, 64);
  SNPRINTF_FUNC ( SNPRINTF_ARGS(shm->name+strlen(shm->name),sizeof(shm->name)-strlen(shm->name)-1),    
				"/_%d.shm", key);
#else
  SNPRINTF_FUNC ( SNPRINTF_ARGS(shm->name,sizeof(shm->name)),
		  "/_%d.shm", key);
#endif

  shm->id = 0;
  errno = 0;


  shm->id = shm_open (shm->name, O_RDWR, 0777);
  if(shm->id < 0 && wait_for_master && !oflag)
    {
      shm->create_errno=errno;
      start_time=etime();
      cur_time=start_time;
      while(shm->id < 0 && shm->create_errno == ENOENT &&
	    (wfm_period < 0 || cur_time - start_time < wfm_period)
	    )
	{
	  if(cur_time - start_time > 10.0 && wfm_period < 0 &&
	     !wait_for_master_too_long_warning_given)
	    {
	      rcs_print_warning("waited for master for shared memory with key=%d(0x%X) for longer than 10 seconds. This process is configured to wait forever.\n",
				(int) key, (unsigned int) key);
	      wait_for_master_too_long_warning_given=1;
	    }
	  if(wfm_delay > 0)
	    {
	      esleep(wfm_delay);
	    }
	  shm->id = shm_open (shm->name, O_RDWR, 0777);
	  if(shm->id < 0)
	    {
	      shm->create_errno=errno;
	    }
	  cur_time = etime();
	}
    }
  
  if(shm->id < 0 &&  !oflag)
    {
      rcs_print_error ("shm_open(%s,%d(0x%X),%d(0x%X)) failed:%s %d\n",
		       shm->name, oflag | O_RDWR, oflag | O_RDWR,
		       mode, mode, strerror (errno), errno);
      shm->create_errno = errno;
      return shm;
    }

  if (oflag)
    {
      oflag = O_CREAT;
    }

  /* Create a new memory object */
  if (shm->id <= 0)
    {
      shm->id = shm_open (shm->name, oflag | O_RDWR, 0777);
      if (shm->id == -1)
	{
	  rcs_print_error ("shm_open(%s,%d(0x%X),%d(0x%X)) failed:%s %d\n",
			   shm->name, oflag | O_RDWR, oflag | O_RDWR,
			   mode, mode, strerror (errno), errno);
	  shm->create_errno = errno;
	  return shm;
	}
      shm->created = 1;
      existed_before = 0;
    }
  else
    {
      shm->created = 0;
      existed_before = 1;
    }


  /* Set the memory object's size */
  if (!existed_before)
    {
      if (ftruncate (shm->id, size + 16) == -1)
	{
	  rcs_print_error ("ftruncate(%d,%d): %s %d\n",
			   shm->id, (size + 16), strerror (errno), errno);
	  shm->create_errno = errno;
	  return shm;
	}
    }
#if HAVE_FSTAT
  else
    {
      if (-1 == fstat (shm->id, &statbuf))
	{
	  rcs_print_error ("fstat failed. (errno=%d) %s\n",
			   errno, strerror (errno));
	  shm->create_errno = errno;
	  return shm;
	}
      if (statbuf.st_size < size + 16)
	{
	  rcs_print_error ("Shared memory buffer %s already exists but has the wrong size of %lu instead of the expected size of %d.\n", 
			   shm->name, ((unsigned long) statbuf.st_size), size + 16);
	  shm->create_errno = -1;
	  return shm;
	}
    }
#endif



  /* Map the memory object */
  shm->addr = mmap (0, size + 16,
		    PROT_READ | PROT_WRITE, MAP_SHARED, shm->id, 0);
  if (shm->addr == MAP_FAILED)
    {
      rcs_print_error
	("mmap(0,%d,PROT_READ | PROT_WRITE, MAP_SHARED,%d,0) failed: %s %d\n",
	 shm->id, size, strerror (errno), errno);
      shm->create_errno = errno;
    }
  shm->size = size;
  if (oflag & O_CREAT && !existed_before)
    {
      *((int *) ((char *) shm->addr + size)) = 0;
    }
  else
    {
      (*((int *) ((char *) shm->addr + size)))++;
    }
#else


#if defined(O_CREAT)
#if O_CREAT != IPC_CREAT
  if ((oflag & O_CREAT) && !(oflag & IPC_CREAT))
    {
      oflag &= ~(O_CREAT);
      oflag |= IPC_CREAT;
    }
#endif
#endif

  if (oflag)
    {
      shmflg |= IPC_CREAT;
    }

  shm = (struct shm_t_struct *) DEBUG_CALLOC (sizeof (struct shm_t_struct), 1);
  if (NULL == shm)
    {
      rcs_print_error ("rcs_shm_open: calloc failed\n");
      return NULL;
    }
  shm->create_errno = 0;
  shm->addr = NULL;
  shm->key = key;
  errno = 0;

  shm->size = size;

  
  shm->id = shmget (key, size, shmflg);
  if(shm->id < 0 && wait_for_master && !oflag)
    {
      shm->create_errno=errno;
      start_time=etime();
      cur_time=start_time;
      while(shm->id < 0 && shm->create_errno == ENOENT &&
	    (wfm_period < 0 || cur_time - start_time < wfm_period)
	    )
	{
	  if(cur_time - start_time > 10.0 && wfm_period < 0 &&
	     !wait_for_master_too_long_warning_given)
	    {
	      rcs_print_warning("waited for master for shared memory with key=%d(0x%X) for longer than 10 seconds. This process is configured to wait forever.\n",
				(int) key, (unsigned int) key);
	      wait_for_master_too_long_warning_given=1;
	    }
	  if(wfm_delay > 0)
	    {
	      esleep(wfm_delay);
	    }
	  shm->id = shmget (key, size, shmflg);
	  if(shm->id < 0)
	    {
	      shm->create_errno=errno;
	    }
	  cur_time = etime();
	}
    }

  if (shm->id == -1)
    {
      shm->create_errno = errno;
      rcs_print_error ("shmget(%d(0x%X),%lu,%d) failed: (errno = %d): %s\n",
		       (int) key, (unsigned int) key, 
		       (unsigned long)size, 
		       shmflg, errno, strerror (errno));
      switch (errno)
	{
	case EEXIST:
	  rcs_print_error
	    ("A shared memory buffer for this key already exists.\n");
	  break;

	case EINVAL:
	  rcs_print_error
	    ("Either the size is too big or the shared memory buffer already exists but is of the wrong size.\n");
	  break;

	case ENOSPC:
	  rcs_print_error
	    ("The system imposed limit on the maximum number of shared memory segments has been exceeded.\n");
	  break;
	}
      return (shm);
    }

  /* map shmem area into local address space */
  shmflg = 0;
  /* shmflg &= ~SHM_RDONLY; */
  shmat_ret = (void *) shmat (shm->id, 0, shmflg);
  if ( ((void *) -1) == shmat_ret || ((void *) 0) == shmat_ret)
    {
      /* Darwin aka Mac OS X returns with errno = 24(EMFILE) "Too Many Open Files" the second time 
	 the same process tries to attach to the same buffer. (This ussually only happens in the server.) */
      shm->create_errno = errno;
      if(shm->create_errno == EMFILE || shm->create_errno == 0)
	{
	  for (i = 0; i < 512; i++)
	    {
	      if (shmems_created_list[i].shm  && shmems_created_list[i].shm->key == key)
		{
		  *shm = *(shmems_created_list[i].shm);
		  shm->created=0;
		  shmems_created_list[i].refcount++;
		  return(shm);
		}
	    }
	}
      rcs_print_error ("shmat(%d,0,%d) failed returning %p:(shm->create_errno = %d): %s\n", 
		       shm->id,
		       shmflg, 
		       shmat_ret,
		       shm->create_errno, 
		       strerror (shm->create_errno));
      rcs_print_error ("key = %d (0x%X)\n", 
		       (int) key, (unsigned int) key);
      shm->addr = NULL;
      return (shm);
    }
  shm->addr=shmat_ret;

  /* Check to see if I am the creator of this shared memory buffer. */
  if (shmctl (shm->id, IPC_STAT, ((struct shmid_ds *) &shared_mem_info)) < 0)
    {
      rcs_print_error ("shmctl error: %d %s\n", errno, strerror (errno));
      return shm;
    }

  /* If oflag was not set this process couldn't be the creator. */
  if (!oflag)
    {
      if(shm)
	{
	  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
			   "rcs_shm_open(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
			   (int) shm->key, (unsigned int) shm->key,
			   shm->id,shm->id,
			   (unsigned long) shm->size,
			   (unsigned long) shm->size,
			   shm->addr,shm->name);
	}
      return shm;
    }

  if (!shmems_created_list_initialized)
    {
      memset (shmems_created_list, 0, sizeof (shmems_created_list));
      shmems_created_list_initialized = 1;
    }
  else
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm && shmems_created_list[i].shm->key == key)
	    {
	        if(shm)
		  {
		    rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
				     "rcs_shm_open(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
				     (int) shm->key, (unsigned int) shm->key,
				     shm->id,shm->id,
				     (unsigned long)shm->size, (unsigned long)shm->size,
				     shm->addr,shm->name);
		  }
	      return shm;
	    }
	}
    }

  pid = (int) getpid ();
  if (pid <= 0)
    {
      rcs_print_error ("getpid error: %d %s\n", errno, strerror (errno));
      return shm;
    }
  if (((unsigned long)shared_mem_info.shared_mem_info2.shm_segsz) == ((unsigned long) shm->size) &&
      ((unsigned long)shared_mem_info.shared_mem_info1.shm_segsz) != ((unsigned long)shm->size))
    {
      shm->created = (shared_mem_info.shared_mem_info2.shm_cpid == pid);
    }
  else
    {
      shm->created = (shared_mem_info.shared_mem_info1.shm_cpid == pid);
    }
#if defined(darwin) || defined(linux_2_4_0)
  shm->created = 1;
#endif

  if (shm->created)
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm == 0)
	    {
	      shmems_created_list[i].shm = shm;
	      shmems_created_list[i].refcount = 0;
	      break;
	    }
	}
    }
#endif
  if(shm)
    {
      rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
		       "rcs_shm_open(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
		       (int) shm->key, (unsigned int) shm->key,
		       shm->id,shm->id,
		       (unsigned long) shm->size, (unsigned long)shm->size,
		       shm->addr,shm->name);
    }
  return shm;
}

#ifndef USE_POSIX_SHAREDMEM
union shared_mem_info_union
{
  struct shmid_ds shared_mem_info1;
  struct shmid_ds2 shared_mem_info2;
  struct shmid_ds3 shared_mem_info3;
};
#endif

int
rcs_shm_close (struct shm_t_struct * shm)
{
#ifdef USE_POSIX_SHAREDMEM
  int nattch;
#else
  union shared_mem_info_union shared_mem_info;
  int i;
#endif
  
  if(shm)
    {
      rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
		       "rcs_shm_close(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
		       (int) shm->key, (unsigned int) shm->key,
		       shm->id,shm->id,
		       (unsigned long)shm->size,(unsigned long)shm->size,
		       shm->addr,shm->name);
    }
#ifdef USE_POSIX_SHAREDMEM
  if (shm == 0)
    {
      return -1;
    }
  if (shm->addr > 0)
    {
      nattch = rcs_shm_nattch (shm);
      (*((int *) ((char *) shm->addr + shm->size)))--;
      if (munmap (shm->addr, shm->size + 16) == -1)
	{
	  rcs_print_error ("munmap(%p,%d) failed. %s %d\n",
			   shm->addr, shm->size, strerror (errno), errno);
	  return -1;
	}
      shm->addr = NULL;
      if (shm->id > 0)
	{
	  if (close (shm->id) == -1)
	    {
	      rcs_print_error ("close(%d) failed. %s %d\n",
			       shm->id, strerror (errno), errno);
	    }
	}
      if (nattch <= 1)
	{
	  shm_unlink (shm->name);
	}
      shm->id = 0;
    }
#else
  /* check for invalid ptr */
  if (shm == NULL)
    return -1;

  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close(shm->key=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=0x%p)\n",
		   (int) shm->key, (unsigned int) shm->key, 
		   (unsigned long)shm->size, (unsigned long)shm->size, 
		   shm->addr);

  /* detach from shmem */
  if(shm->addr &&
     ((long) shm->addr) != -1)
    {
      shmdt ((char *) shm->addr);
      shm->addr=0;
    }

  /* remove OS shmem if there are no attached processes */
  if (rcs_shm_nattch (shm) == 0)
    {
      shmctl (shm->id, IPC_RMID, ((struct shmid_ds *) &shared_mem_info));
    }

  if (shmems_created_list_initialized)
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm && shmems_created_list[i].shm->key == shm->key)
	    {
	      shmems_created_list[i].refcount--;
	      if(shmems_created_list[i].refcount < 0)
		{
		  shmems_created_list[i].refcount = 0;
		}
	      if(shmems_created_list[i].shm == shm ||
		 shmems_created_list[i].refcount == 0)
		{
		  shmems_created_list[i].shm = 0;
		  shmems_created_list[i].refcount = 0;
		}
	      break;
	    }
	}
    }

#endif


  /* DEBUG_FREE the struct shm_t_struct data struct */
  DEBUG_FREE (shm);
  shm = 0;
  return 0;
}

int
rcs_shm_close_leaving_resource (struct shm_t_struct * shm)
{
#ifndef USE_POSIX_SHAREDMEM
  int i;
#endif

  if(shm)
    {
      rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
		       "rcs_shm_close_leaving_resource(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
		       (int) shm->key, (unsigned int) shm->key,
		       shm->id,shm->id,
		       (unsigned long)shm->size,
		       (unsigned long)shm->size,
		       shm->addr,shm->name);
    }
#ifdef USE_POSIX_SHAREDMEM
  if (shm == 0)
    {
      return -1;
    }
  if (shm->addr > 0)
    {
      if (munmap (shm->addr, shm->size + 16) == -1)
	{
	  rcs_print_error ("munmap(%p,%d) failed. %s %d\n",
			   shm->addr, shm->size, strerror (errno), errno);
	  return -1;
	}
      shm->addr = NULL;
      if (shm->id > 0)
	{
	  if (close (shm->id) == -1)
	    {           
	      rcs_print_error ("close(%d) failed. %s %d\n",
			       shm->id, strerror (errno), errno);
	    }
	}
      shm->id = 0;
    }
#else
  /* check for invalid ptr */
  if (shm == NULL)
    return -1;

  rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY,
		   "rcs_shm_close(shm->key=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=0x%p)\n",
		   (int) shm->key, (unsigned int) shm->key, 
		   (unsigned long)shm->size, 
		   (unsigned long)shm->size, 
		   shm->addr);

  /* detach from shmem */
  if(shm->addr &&
     ((long) shm->addr) != -1)
    {
      shmdt ((char *) shm->addr);
      shm->addr=0;
    }

  /* remove OS shmem if there are no attached processes */
  if (shmems_created_list_initialized)
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm && shmems_created_list[i].shm->key == shm->key)
	    {
	      shmems_created_list[i].refcount--;
	      if(shmems_created_list[i].refcount < 0)
		{
		  shmems_created_list[i].refcount = 0;
		}
	      if(shmems_created_list[i].shm == shm ||
		 shmems_created_list[i].refcount == 0)
		{
		  shmems_created_list[i].shm = 0;
		  shmems_created_list[i].refcount = 0;
		}
	      break;
	    }
	}
    }
#endif

  /* DEBUG_FREE the struct shm_t_struct data struct */
  DEBUG_FREE (shm);
  shm = 0;
  return 0;
}

int
rcs_shm_delete (struct shm_t_struct * shm)
{
#ifndef USE_POSIX_SHAREDMEM
  union shared_mem_info_union shared_mem_info;
#endif
  int i;

  if(shm)
    {
      rcs_print_debug (PRINT_SHARED_MEMORY_ACTIVITY, 
		       "rcs_shm_delete(shm->key=%d(0x%X),shm->id=%d(0x%X),shm->size=%lu(0x%lX),shm->addr=%p,shm->name=%s)\n",
		       (int) shm->key, (unsigned int) shm->key,
		       shm->id,shm->id,
		       (unsigned long)shm->size,(unsigned long)shm->size,
		       shm->addr,shm->name);
    }
#ifdef USE_POSIX_SHAREDMEM
  if (shm == 0)
    {
      return -1;
    }
  if (shm->addr > 0)
    {
      (*((int *) ((char *) shm->addr + shm->size)))--;
      if (munmap (shm->addr, shm->size + 16) == -1)
	{
	  rcs_print_error ("munmap(%p,%d) failed. %s %d\n",
			   shm->addr, shm->size, strerror (errno), errno);
	  return -1;
	}
      shm->addr = NULL;
      if (shm->id > 0)
	{
	  if (close (shm->id) == -1)
	    {
	      rcs_print_error ("close(%d) failed. %s %d\n",
			       shm->id, strerror (errno), errno);
	    }
	}
      shm->id = 0;
    }
  shm_unlink (shm->name);
#else
  /* check for invalid ptr */
  if (shm == NULL)
    return -1;

  /* detach from shmem */
  if(shm->addr &&
     ((long)shm->addr) != -1)
    {
      shmdt ((char *) shm->addr);
      shm->addr=0;
    }

#ifndef NEVER_FORCE_IPC_RM
  /* remove OS shmem regardless of whether there are attached processes */
  shmctl (shm->id, IPC_RMID, ((struct shmid_ds *) &shared_mem_info));

#else
  /* remove OS shmem if there are no attached processes */
  if (rcs_shm_nattch (shm) == 0)
    {
      shmctl (shm->id, IPC_RMID, ((struct shmid_ds *) &shared_mem_info));
    }  
#endif

#endif

  if (shmems_created_list_initialized)
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm && shmems_created_list[i].shm->key == shm->key)
	    {
	      shmems_created_list[i].refcount = 0;
	      shmems_created_list[i].shm = 0;
	      break;
	    }
	}
    }

  /* DEBUG_FREE the struct shm_t_struct data struct */
  DEBUG_FREE (shm);
  shm = 0;

  return 0;
}

int
rcs_shm_nattch (struct shm_t_struct * shm)
{
#ifdef USE_POSIX_SHAREDMEM
  if (shm == 0)
    {
      return -1;
    }
  if (shm->addr == 0)
    {
      return -1;
    }
  return *((int *) (((char *) shm->addr) + shm->size)) + 1;
#else
  int i;
  int local_refcount;
  union
  {
    struct shmid_ds shared_mem_info1;
    struct shmid_ds2 shared_mem_info2;
    struct shmid_ds3 shared_mem_info3;
  }
  shared_mem_info;

  local_refcount = 0;
  
  if (shmems_created_list_initialized)
    {
      for (i = 0; i < 512; i++)
	{
	  if (shmems_created_list[i].shm && shmems_created_list[i].shm->key == shm->key)
	    {
	      local_refcount = shmems_created_list[i].refcount;
	      break;
	    }
	}
    }
  if(local_refcount < 0)
    {
      local_refcount = 0;
    }

  /* check for invalid ptr */
  if (shm == NULL)
    return -1;

  memset(&shared_mem_info,0,sizeof(shared_mem_info));

  /* get the status of shared memory */
  shmctl (shm->id, IPC_STAT, ((struct shmid_ds *) &shared_mem_info));

  if (((unsigned long)shared_mem_info.shared_mem_info2.shm_segsz) == ((unsigned long)shm->size) &&
      ((unsigned long)shared_mem_info.shared_mem_info1.shm_segsz) != ((unsigned long)shm->size))
    {
      return shared_mem_info.shared_mem_info2.shm_nattch + local_refcount;
    }
  return shared_mem_info.shared_mem_info1.shm_nattch + local_refcount;
#endif
}

#endif /* !defined(WIN32) && !defined(VXWORKS) */
