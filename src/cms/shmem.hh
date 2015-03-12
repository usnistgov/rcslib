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
* File: shmem.hh                                                         *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the Communication Management System (CMS).*
*          Includes:                                                     *
*                    1. class SHMEM.                                     *
* Notes: The class SHMEM should be used by procedures accessing a shared *
*  memory buffer on the same processor.                                  *
*************************************************************************/

#ifndef SHMEM_HH
#define SHMEM_HH

#include "cms.hh"		/* class CMS */

class RCS_SEMAPHORE;
class RCS_SHAREDMEM;
struct mem_access_object;
struct shmem_blocking_sem;

/* Class Definitions */

enum SHMEM_MUTEX_TYPE
  {
    NO_MUTEX,
    MAO_MUTEX,
    MAO_MUTEX_W_OS_SEM,
    OS_SEM_MUTEX,
    NO_INTERRUPTS_MUTEX,
    NO_SWITCHING_MUTEX
  };

class SHMEM:public CMS
{
public:
#if 0
  SHMEM (char *name, long size, int neutral, key_t key, int m = 0);
#endif

  SHMEM ( const char *bufline, 
	  const char *procline, 
	  int set_to_server = 0,
	  int set_to_master = 0);

  virtual ~ SHMEM ();
  
  virtual int wait_for_anything(double timeout);
  CMS_STATUS main_access (void *_local);
  CMS_STATUS main_access_to_repeat(void *_local);
  virtual void interrupt_operation(void);
  virtual void clear_interrupt_operation(void);
  virtual void set_leave_resource(bool);

private:

  /* data buffer stuff */
#if 0
  // def VXWORKS, this was if 0'd out because the offsets looked wrong.
  virtual CMS_STATUS read ();	/* Read from  buffer. */
  virtual CMS_STATUS peek ();	/* Read without setting flag. */
  virtual CMS_STATUS write (void *user_data);	/* Write to buffer. */
  virtual CMS_STATUS write_if_read (void *user_data);	/* Write to buffer. */
#endif
  int open ();			/* get shared mem and sem */
  int close ();			/* detach from shared mem and sem */
  long lkey;			/* key for shared mem and sem */
  long lbsem_key;		// key for blocking semaphore
  int second_read;		// true only if the first read returned no new data
  RCS_SHAREDMEM *shm;		/* shared memory */
  RCS_SEMAPHORE *sem;		/* semaphore */
  int master;			/* Is this process responsible for */
  /* clearing memory & semaphores? */
  double sem_delay;		/* Time to wait between polling the semaphore. */
  double min_bsem_wait;		// time to wait if calls to bsem wait would otherwise occur to closely 
  struct mem_access_object *mao_ptr;	/* passed to mem_get_access()  */


  enum SHMEM_MUTEX_TYPE mutex_type;
  void *shm_addr_offset;

  struct shmem_blocking_sem *bsem_ptr;
  int autokey_table_size;
  double last_bsem_wait_time;
  bool i_have_mutex;
  bool always_flush_bsem;
  bool repeat_main_access;
  bool use_os_sem;
  bool use_os_sem_only;

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  SHMEM(const SHMEM &);
  SHMEM &operator=(const SHMEM &);

};

#endif /* !SHMEM_HH */
