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


#ifndef _SHAREDMEM_H
#define _SHAREDMEM_H

#include <stddef.h>


#ifdef __cplusplus
extern "C"
{
#endif

  struct shm_t_struct;

  typedef struct shm_t_struct shm_t;

  extern struct shm_t_struct *
  rcs_shm_open(long key, 
			 size_t size, 
			 int oflag,
			 ...); 	/* mode */

  extern struct shm_t_struct *
  rcs_shm_open_extended (long key, 
		size_t size, 
		int oflag,
		int mode, 	/* permission, ignored if oflag == 0 */
		int wait_for_master, /* boolean, ignored if oflag != 0 */
		double wfm_delay, /* time between polls */
		double wfm_period); /* period before timeout */

  extern int rcs_shm_close (struct shm_t_struct * shm);
  extern int rcs_shm_close_leaving_resource (struct shm_t_struct * shm);
  extern int rcs_shm_delete (struct shm_t_struct * shm);
  extern int rcs_shm_nattch (struct shm_t_struct * shm);
  extern void *rcs_shm_get_addr(struct shm_t_struct *shm);
  extern int rcs_shm_get_created(struct shm_t_struct *shm);
  extern int rcs_shm_get_create_errno(struct shm_t_struct *shm);

#ifndef USE_RCS_SHM_GET_ADDR
#define USE_RCS_SHM_GET_ADDR 1
#endif

#ifdef VXWORKS
  void rcs_shm_table_print (void);

  extern int rcs_shm_table_inited;
  extern int task_lock_shm_stuff;
  extern int print_rcs_shm_warnings;

#endif

#ifdef __cplusplus
}
#endif

#endif


