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


#ifndef _RCS_SEMAPHORE_H
#define _RCS_SEMAPHORE_H

#include "rcs_defs.hh"

#if  defined(MS_WINDOWS_API)
#if !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif
#include <windows.h>

typedef struct
{
  char name[64];
  unsigned long int id;
  HANDLE handle;
}
rcs_sem_t;

/* MS_WINDOWS_API */
#endif 

#if defined VXWORKS
#include <semLib.h>		/* SEM_ID */

typedef struct
{
  /* Modified  4-May-1998 by WPS, match VxWorks calls and make variable names clearer. */
  /* unsigned long int id; */
  /*  struct semaphore *sem; */
  unsigned long int key;
  SEM_ID sem_id;
}
rcs_sem_t;

#endif

#if !defined(MS_WINDOWS_API) && !defined(VXWORKS)
#ifndef rcs_sem_t_defined
typedef void rcs_sem_t;
#define rcs_sem_t_defined
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif


#ifdef VXWORKS
  void rcs_sem_table_print (void);

  extern int rcs_sem_table_inited;
  extern int print_rcs_sem_warnings;
  /* VXWORKS */
#endif

#if 0
  /* sem_init is for unnamed posix semaphores  which should never be used.
   *  We need to communicate between unrelated processes. */
  int rcs_sem_init (rcs_sem_t * sem, int pshared, unsigned int value);
#endif

  int rcs_sem_destroy (rcs_sem_t * sem);
  rcs_sem_t *rcs_sem_open_w_ulong_key (unsigned long int key, 
					 int oflag,int mode, int blocking_type);
  int rcs_sem_close (rcs_sem_t * sem);
  int rcs_sem_close_leaving_resource (rcs_sem_t * sem);
  int rcs_sem_wait (rcs_sem_t * sem, double timeout, int *restart_interrupt_ptr);
  int rcs_sem_trywait (rcs_sem_t * sem);
  int rcs_sem_flush (rcs_sem_t * sem);
  int rcs_sem_post (rcs_sem_t * sem);
  int rcs_sem_getvalue (rcs_sem_t * sem, unsigned int *sval);
  int rcs_sem_clear (rcs_sem_t * sem);
  int rcs_sem_inc_waiting(volatile rcs_sem_t * sem);
  int rcs_sem_dec_waiting(volatile rcs_sem_t * sem);
  double rcs_sem_get_default_bsem_wait(void);

/* additions */
  rcs_sem_t *rcs_sem_create_w_ulong_key (unsigned long int key, int mode, int state, int blocking_type);

#ifdef __cplusplus
}
#endif

#if !defined(VXWORKS) && !defined(MS_WINDOWS_API)
extern int sem_force_fifo;
#endif

/* _RCS_SEMAPHORE_H  */
#endif
