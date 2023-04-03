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


ACKNOWLEDGEMENTS & NOTES:

   Jeremy Nett of PercepTek -- discovored that the flushing_thread and flushing_thread_created variables  were not always inititialized.  (As a result the flushing_thread code which had been almost if 0'd out already was removed. The remaining pthread_cancel call still caused segmentation faults.) April 17, 2006.


*/ 


#define UNIX_SEM_SOURCE 1

#define MAX_SEM_BLOCKING_COUNT 31

#ifdef HAVE_CONFIG_H
#ifndef PACKAGE
#include "rcs_config.h"
#endif
#include "rcs_config_include.h"

#else
#include "unix_sem_no_config.h"

#endif /* ! HAVE_CONFIG_H */

#ifdef POSIX_SEMAPHORES

#include "_shm.h"

struct rcs_sem_t_struct
{
  sem_t *sem;
  char name[64];
  int key;
  int flushcount;
  int postcount;
  int waitcount;
  int blocking_type;
  int blocking_count;
  struct shm_t_struct *waiting_shm_struct;
  volatile int *waiting_count_ptr;
  int removed;
};

typedef struct rcs_sem_t_struct rcs_sem_t;

#else
struct noposix_sem_struct
{
  int semid;
  int key;
  int flushcount;
  int postcount;
  int waitcount;
  int blocking_type;
  int blocking_count;
  int removed;
};

typedef struct noposix_sem_struct rcs_sem_t;
#endif
#define rcs_sem_t_defined

#include "_sem.h"
#include "_timer.h"		/* etime() */
#include "dbg_mem.h"		/* DEBUG_MALLOC,DEBUG_FREE */
#include "rcs_prnt.hh"

int rcs_sem_unlink (const char *name);

int sem_force_fifo = 0;

#if !defined(POSIX_SEMAPHORES)

#define SEM_TAKE (-1)		/* decrement sembuf.sem_op */
#define SEM_GIVE (1)		/* increment sembuf.sem_op */


#if (defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED) ) || defined(HAVE_UNION_SEMUN)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun
{
  int val;			/* value for SETVAL */
  struct semid_ds *buf;		/* buffer for IPC_STAT, IPC_SET */
  unsigned short int *array;	/* array for GETALL, SETALL */
  struct seminfo *__buf;	/* buffer for IPC_INFO */
};
#define UNION_SEMUN_DEFINED
#endif

#if defined(os5) && defined(sparc) && !defined(UNION_SEMUN_DEFINED)
union semun
{
  int val;
  struct semid_ds *buf;
  ushort *array;
};
#endif

 /* POSIX_SEMAPHORES */
#endif

#ifdef POSIX_SEMAPHORES
static pthread_mutex_t unix_sem_pmutex = PTHREAD_MUTEX_INITIALIZER;
struct semlist_item;
struct semlist_item{
  sem_t *sem;
  int refs;
  struct semlist_item *next;
};
static struct semlist_item *firstsemlistitem=0;

static int
release_ref(sem_t *s)
{
  struct semlist_item *si =0;
  struct semlist_item *last_si =0;
  struct semlist_item *si_to_free=0;
  int retval=0;
  pthread_mutex_lock(&unix_sem_pmutex);
  si =firstsemlistitem;
  while(si)
    {
      if(si->sem == s)
	{
	  if(si->refs > 1)
	    {
	      si->refs--;
	      retval=si->refs;
	    }
	  else
	    {
	      if(last_si)
		{
		  last_si->next = si->next;
		}
	      si->next=0;
	      si_to_free=si;
	    }
	  break;
	}
      last_si=si;
      si=si->next;
    }
  if(si_to_free == firstsemlistitem)
    {
      firstsemlistitem=0;
    }
  pthread_mutex_unlock(&unix_sem_pmutex);
  if(si_to_free)
    {
      free(si_to_free);
      si_to_free=0;
    }
  return 0;
}  

static int
increment_ref(sem_t *s)
{
  struct semlist_item *si =0;
  struct semlist_item *last_si =0;
  struct semlist_item *new_si =0;
  int retval=0;
  pthread_mutex_lock(&unix_sem_pmutex);
  if(!firstsemlistitem)
    {
      new_si = (struct semlist_item *) malloc(sizeof(struct semlist_item)); 
      new_si->sem=s;
      new_si->refs=1;
      new_si->next=0; 
      firstsemlistitem=new_si;
      pthread_mutex_unlock(&unix_sem_pmutex);
      return 1;
    }
  si =firstsemlistitem;
  while(si)
    {
      if(si->sem == s)
	{
	  si->refs++;
	  retval=si->refs;
	  pthread_mutex_unlock(&unix_sem_pmutex);
	  return retval;
	}
      last_si=si;
      si=si->next;
    }
  new_si =  (struct semlist_item *) malloc(sizeof(struct semlist_item));
  new_si->sem=s;
  new_si->refs=1;
  new_si->next=0; 
  last_si->next = new_si;
  pthread_mutex_unlock(&unix_sem_pmutex);
  return 1;
}  

/* POSIX_SEMAPHORES */
#endif

/* remove semaphore from OS-- this must be done *before* sem_close,
   since rcs_sem_close frees the storage allocated for the rcs_sem_t */
int
rcs_sem_destroy (rcs_sem_t * sem)
{
#ifndef POSIX_SEMAPHORES
  union semun sem_arg;
#endif

  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_destroy(%p) key=%d(0x%X)\n",
		   (void*)sem, (int) sem->key, (unsigned int) sem->key);

#ifdef POSIX_SEMAPHORES
  if(sem)
    {
      rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_destroy(%p) : sem->name=%s, sem->key=%d(0x%X),sem->sem=%p, sem->removed=%d\n",
		       (void*)sem,sem->name,sem->key,sem->key,(void*)sem->sem,
		       sem->removed);
    }
  if (0 != sem)
    {
      if (0 != sem->sem && ((sem_t *)-1) != sem->sem)
	{
	  sem_close (sem->sem);
	  sem->sem = 0;
	}
      sem_unlink (sem->name);
    }
#else
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 1;

  /* remove OS semaphore */
  if(!sem->removed)
    {
      if (semctl (sem->semid, 0, IPC_RMID, sem_arg) == -1)
	{
	  rcs_print_error ("semctl(%d,0,IPC_RMID, %d) failed: (errno = %d) %s\n",
			   sem->semid, IPC_RMID, errno, strerror (errno));
	  return -1;
	}
      sem->removed=1;
    }
#endif
  return 0;
}


int
rcs_sem_clear (rcs_sem_t * sem)
{
#ifdef POSIX_SEMAPHORES
  /* I don't see how to implement this with strict posix semaphores. */
  return(-1);
#else
  union semun sem_arg;
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 1;
  semctl (sem->semid, 1, SETVAL, sem_arg);
#endif
  return(0);
}

static int rcs_sem_open_val = 0;

/* create a named binary392
 semaphore */
rcs_sem_t *
rcs_sem_open_w_ulong_key (unsigned long int _key, int oflag, int mode, int blocking_type)
{
#ifdef POSIX_SEMAPHORES
  rcs_sem_t *retval;
#else
  key_t key;			/* name converted to a key */
  int semid;
  rcs_sem_t *retval;	/* semaphore id returned */
  int semflg = 0;		/* flag for perms, create, etc. */
#endif

  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_open(%lu,%d,...)\n",
		   _key,oflag);
#ifdef POSIX_SEMAPHORES
  retval = DEBUG_MALLOC (sizeof (rcs_sem_t));
  if (NULL == retval)
    {
      return NULL;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(retval->name,sizeof(retval->name)),
		  "/_%lu.sem", _key);
  retval->key = (int) _key;
  retval->sem = SEM_FAILED;
  retval->waitcount=0;
  retval->flushcount=0;
  retval->postcount=0;
  retval->blocking_type=blocking_type;
  retval->blocking_count=1;
  retval->waiting_shm_struct =0;
  retval->waiting_count_ptr = 0;
  retval->removed=0;
  if (oflag)
    {
      retval->sem = sem_open (retval->name, O_CREAT, 0777, 1);
    }
  else
    {
      retval->sem = sem_open (retval->name, 0);
    }
  if (((long)retval->sem) == 0 || retval->sem == SEM_FAILED)
    {
      rcs_print_error ("sem_open(%s,%d(0x%X),%d(0x%X),%d): retval->sem=%p(%ld), ERROR - %s %d\n",
		       retval->name, oflag, oflag, mode, mode,
		       rcs_sem_open_val, (void*)retval->sem,((long)retval->sem), strerror (errno), errno);
      DEBUG_FREE (retval);
      return NULL;
    }
  else if(retval->blocking_type)
    {
      retval->waiting_shm_struct=rcs_shm_open(_key,8,oflag,mode);
      if(retval->waiting_shm_struct)
	{
	  retval->waiting_count_ptr = (int *) rcs_shm_get_addr(retval->waiting_shm_struct);
	  if(retval->waiting_count_ptr && rcs_shm_get_created(retval->waiting_shm_struct))
	    {
	      *retval->waiting_count_ptr=0;
	    }
	}
    }
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"sem_open(%s,%d,...) returned %p\n",
		   retval->name,oflag,(void*)retval->sem);
  increment_ref(retval->sem);
  return retval;
#else /* POSIX_SEMAPHORES */

  /* if IPC_CREAT is specified for creating the sempahore, then
     the optional arg is the mode */
  if (oflag & IPC_CREAT)
    {
      semflg |= mode;
      semflg |= IPC_CREAT;
    }
  else
    {
      semflg &= ~IPC_CREAT;
    }

  /* with itaniums and the fact that pointers were 64 bits and given 
     we lost strict posix compliance for semaphores a long time ago
     anyway. name is no longer a char * -- WPS May-25-2004 */
  key = (key_t) _key;
  if (key < 1)
    {
      rcs_print_error ("rcs_sem_open: invalid key %d\n", (int) key);
      return NULL;
    }


  if ((semid = semget ((key_t) key, 1, semflg)) == -1)
    {
      rcs_print_error ("semget(key=%d(0x%x),nsems=1,semflg=%d(0x%X)) failed. %d -- %s\n",
		       (int) key, (unsigned int) key,semflg,semflg,errno,strerror(errno));
      return NULL;
    }


  /* we have a valid semid-- semantics say we return a pointer
     to the id, so we need to allocate space that users will
     free later with rcs_sem_close */
  retval = (rcs_sem_t *) DEBUG_MALLOC (sizeof (rcs_sem_t));
  retval->semid = semid;
  retval->key=key;
  retval->blocking_count=1;
  retval->waitcount=0;
  retval->flushcount=0;
  retval->postcount=0;
  retval->blocking_type=blocking_type;
  retval->removed=0;

  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "rcs_sem_open(%lu,%d,...) returning %p {semid=%d(0x%X),key=%d(0x%X)}\n",
		   _key,oflag,
		   (void*)retval,retval->semid,retval->semid,
		   retval->key,retval->key);
  return retval;

#endif /* POSIX_SEMAPHORES */
}

int
rcs_sem_close_leaving_resource (rcs_sem_t * sem)
{
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "rcs_sem_close_leaving_resource(%p)\n",
		   (void*)sem);
#ifdef POSIX_SEMAPHORES
  if(sem)
    {
      rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		       "rcs_sem_close_leaving_resource(%p) : sem->name=%s, sem->key=%d(0x%X),sem->sem=%p\n",
		       (void*)sem,sem->name,sem->key,sem->key,(void*)sem->sem);
    }
  if (sem != 0)
    {
      DEBUG_FREE (sem);
      sem=0;
    }
  return(0);
#else
  return rcs_sem_close(sem);
#endif
}

int
rcs_sem_close (rcs_sem_t * sem)
{
  int retval=0;
#ifdef POSIX_SEMAPHORES
  int refs;
#endif
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_close(%p) key=%d(0x%X)\n",
		   (void*)sem,sem->key,sem->key);
#ifdef POSIX_SEMAPHORES
  if(sem)
    {
      rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_close(%p) : sem->name=%s, sem->key=%d(0x%X),sem->sem=%p\n",
		       (void*)sem,sem->name,sem->key,sem->key,(void*)sem->sem);
    }
  if (sem != 0)
    {
      if (sem->sem != 0 && ((long) sem->sem) != -1)
	{
	  refs=release_ref(sem->sem);
	  if(refs)
	    {
	      retval = sem_close (sem->sem);
	    }
	  sem->sem = 0;
	}
    }
#endif
  if (sem != 0)
    {
      DEBUG_FREE (sem);
      sem=0;
    }
  return retval;
}

int
rcs_sem_unlink (
#ifndef POSIX_SEMAPHORES
		__unused_parameter__ 
#endif
		const char *name)
{
#ifdef POSIX_SEMAPHORES
  return sem_unlink (name);
#else
  return 0;			/* we didn't create anything */
#endif
}

#ifdef USE_ITIMER_SIGNALS
static int semwait_alarm_count = 0;
static int *semwait_restart_ptr=0;
static void
semwait_alarm_handler (__unused_parameter__ int sig)
{
  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		   "semwait_alarm_handler:: alarm_count=%d\n",
		   semwait_alarm_count);
  semwait_alarm_count++;
  if(semwait_restart_ptr)
    {
      *semwait_restart_ptr = 0;
    }
  /* Hope this isn't necessary. */
  signal (SIGALRM, semwait_alarm_handler);
}
#endif

static int eintr_error_message_printed=0;
static int timeout_set=0;

static void
print_eintr_error_message()
{
  if(timeout_set != 0) {
    return;
  }
  if(eintr_error_message_printed == 0 )
    {
      rcs_print_error(" Semaphore operation interrupted by signal.\n");
    }
  eintr_error_message_printed=1;
}

static int
rcs_sem_wait_internal (rcs_sem_t * sem, int *restart_interrupt_ptr, const struct timespec *timeout_timespec_ptr)
{
  int retval = -1;

#if defined(POSIX_SEMAPHORES)
  if(!sem || !sem->sem)
    {
      return -1;
    }
  retval = sem_wait (sem->sem);
  if (errno == EINTR)
    {
      print_eintr_error_message();
      return retval;
    }
  if (retval == -1)
    {
      rcs_print_error ("sem_wait(%p{key=%d,sem=%p}): ERROR: %s %d\n",
		       (void*)sem,
		       sem->key, 
		       ((void *) sem->sem), 
		       strerror (errno), 
		       errno);
    }
  return retval;
#else

  struct sembuf sops[2];
  union semun sem_arg;
  int errno_copy=EINTR;
  int semval;
  int semval_orig;
  int ncount;
  semval=-1;
  semval_orig=-1;
  ncount =-1;
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 0;

  sops[0].sem_num = 0;		/* only one semaphore in set */
  sops[0].sem_op = SEM_TAKE;
  sops[0].sem_flg = 0;		/* wait forever */
  if(sem->blocking_type)
    {
      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
      semval_orig=semval;
      if(sem->blocking_count > semval+1)
	{
	  sem->blocking_count=semval+1;
	}
      if(sem->blocking_count > MAX_SEM_BLOCKING_COUNT)
	{
	  while(1)
	    {
	      ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);
	      if(ncount == 0)
		{
		  semval = semctl (sem->semid, 0, GETVAL, sem_arg);
		  if(semval >= MAX_SEM_BLOCKING_COUNT)
		    {
		      sem_arg.val=1;
		      semctl(sem->semid,0,SETVAL,sem_arg);
		      sops[0].sem_op=1;
		      retval = semop (sem->semid,sops, 1);
		      sem->blocking_count=2;
		      return(0);
		    }
		  else
		    {
		      sem->blocking_count = semval-1;
		      if(sem->blocking_count < 1)
			{
			  sem->blocking_count=1;
			}
		    }
		  break;
		}
	      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
	      if(semval != semval_orig)
		{
		  return 0;
		}
	      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_wait_internal() sem->key=%d(0x%X),sem->semid=%d(0x%X),semval=%d,ncount=%d, sem->blocking_count=%d,retval=%d\n",
			      sem->key,sem->key,sem->semid,sem->semid,semval,ncount,sem->blocking_count,retval);
	      usleep(10000);
	    }
	}
      if(sem->blocking_count < 1)
	{
	  /* This should never happen. */
	  sem->blocking_count =1;
	}
      sops[0].sem_op = -sem->blocking_count;
      sops[1].sem_op = sem->blocking_count;
      sops[1].sem_num=0;
      sops[1].sem_flg=0;
      sem->blocking_count++;
      
#if HAVE_SEMTIMEDOP
      if(timeout_timespec_ptr != NULL) 
      {
      	retval = semtimedop (sem->semid,sops, 2,timeout_timespec_ptr);
      } else {
      	retval = semop (sem->semid,sops, 2);
      }
#else
      retval = semop (sem->semid,sops, 2);
#endif
      if(retval == -1)
	{
	  errno_copy = errno;
	}
      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
      ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);
      if(semval+1 > sem->blocking_count)
	{
	  sem->blocking_count = semval+1;
	}
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_wait_internal() sem->key=%d(0x%X),sem->semid=%d(0x%X),semval=%d,ncount=%d, sem->blocking_count=%d,retval=%d\n",
		      sem->key,sem->key,sem->semid,sem->semid,semval,ncount,sem->blocking_count,retval);
      
    }
  else
    {
      retval = semop (sem->semid, &sops[0], 1);
      while(restart_interrupt_ptr && *restart_interrupt_ptr && retval == -1 &&
	    errno_copy == EINTR)
	{
	  retval = semop (sem->semid, &sops[0], 1);
	  errno_copy=errno;
	}
    }
  if (retval == -1)
    {
      if(errno_copy == EINTR)
	{
	  print_eintr_error_message();
	}
      else if(errno_copy != 0)
	{
#ifdef EIDRM
	  if(errno_copy == EIDRM)
	    {
	      sem->removed=1;
	    }
#endif
	  rcs_print_error
	    ("semop(semid=%d, {sem_num=%d,sem_op=%d,sem_flg=%d},nsops=1): ERROR: %s %d\n",
	     sem->semid, sops[0].sem_num, sops[0].sem_op, sops[0].sem_flg, strerror (errno_copy),
	     errno_copy);
	}
    }

  return retval;
#endif /* POSIX_SEMAPHORES */
}

int
rcs_sem_trywait (rcs_sem_t * sem)
{
#ifdef POSIX_SEMAPHORES
  int retval;
  retval = sem_trywait (sem->sem);
  if (retval == -1)
    {
      rcs_print_error ("sem_trywait: ERROR -- %s %d\n", strerror (errno),
		       errno);
    }
  return retval;
#else
  struct sembuf sops;
  union semun sem_arg;
  int semval;
  sops.sem_num = 0;		/* only one semaphore in set */
  sops.sem_op = SEM_TAKE;
  sops.sem_flg = IPC_NOWAIT;	/* wait forever */
  memset(&sem_arg,0,sizeof(sem_arg));

  if(sem->blocking_type)
    {
      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
      if(semval >= sem->blocking_count)
	{
	  return 0;
	}
      else
	{
	  return -1;
	}
    }
  return semop (sem->semid, &sops, 1);
#endif

}

int
rcs_sem_wait (rcs_sem_t * sem, 
	      double timeout, 
	      int *restart_interrupt_ptr)
{
  const char *errstring=0;
  int errnum=0;
 

#ifdef POSIX_SEMAPHORES
  //  int sem_getvalue_ret=0;
#else
  int semval;
  int ncount;
  union semun sem_arg;
#endif
#if defined( USE_ITIMER_SIGNALS)
  int last_semwait_alarm_count = semwait_alarm_count;
  int retval = -1;
  double start_time = 0.0;
  double elapsed_time = 0.0;
  double time_left = 0.0;
  double current_time = 0.0;
  struct itimerval sem_itimer;
  struct itimerval sem_itimer_backup;
  void (*old_sigalarm_handler) (int);
  old_sigalarm_handler = SIG_ERR;
  time_left = timeout;
  if (0 == sem)
    {
      rcs_print_error("sem == NULL");
      return -1;
    }
  if(sem->removed)
    {
      rcs_print_error("rcs_sem_wait() : sem->key=%d, sem->removed=%d\n",
		      sem->key,
		      sem->removed);
      return -1;
    }

#ifdef POSIX_SEMAPHORES
  if (SEM_FAILED == sem->sem)
    {
      rcs_print_error("sem->sem == SEM_FAILED");
      return -1;
    }
#if 0
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_wait() : sem->sem=%p\n",
		  (void*)sem->sem);
  sem_getvalue_ret = sem_getvalue(sem->sem,&semval);
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait(): sem_getvalue_ret=%d, semval=%d\n",
		  sem_getvalue_ret,semval);
#endif

  if(sem_trywait(sem->sem) ==0)
    {
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_wait() : sem->sem=%p returning.\n",
		      (void*)sem->sem);
      return 0;
    }
  else
    {
      if(errno != EAGAIN)
	{
	  rcs_print_error("sem_trywait failed %d %s\n",errno,strerror(errno));
	}
    }
#else
  if(sem->removed)
    {
      rcs_print_error("rcs_sem_wait() : sem->key=%d, sem->removed=%d\n",
		      sem->key,
		      sem->removed);
      return -1;
    }
  memset(&sem_arg,0,sizeof(sem_arg));
  semval = semctl (sem->semid, 0, GETVAL, sem_arg);
  ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);  
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait() : called sem=%p,sem->sem_id=%d(0x%X),sem->key=%d(0x%X),semval=%d,ncount=%d  . . . called\n",
		  (void*)sem,
		  sem->semid,(unsigned)sem->semid,
		  sem->key,(unsigned)sem->key,
		  semval,ncount);
#endif

  if(sem->removed)
    {
      rcs_print_error("rcs_sem_wait() : sem->key=%d, sem->removed=%d\n",
		      sem->key,
		      sem->removed);
      return -1;
    }
  if (timeout < 0)
    {
      retval = rcs_sem_wait_internal (sem,restart_interrupt_ptr,NULL);
      if (retval == -1)
	{
	  errstring=strerror(errno);
	  errnum=errno;
	  if(errno == EINTR)
	    {
	      print_eintr_error_message();
	    }
	  else 
	    {
	      rcs_print_error ("semwait: ERROR -- %s %d\n",errstring,errnum);
	    }
	}
#ifdef POSIX_SEMAPHORES
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_wait() : sem=%p, sem->sem=%p sem->key=%d(0x%X) returning.\n",
		      (void*)sem,(void*)sem->sem,sem->key,sem->key);
#else
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_wait() : sem=%p, sem->id%d(0x%X) sem->key=%d(0x%X) returning.\n",
		      (void*)sem,sem->semid,sem->semid,sem->key,sem->key);
#endif
      return retval;
    }
  if (timeout < clk_tck () / 2)

    {
      retval = rcs_sem_trywait (sem);
      if (retval == -1)
	{
	  rcs_print_error ("semifwait: ERROR -- %s %d\n", strerror (errno),
			   errno);
	}
#ifdef POSIX_SEMAPHORES
#if 0
      sem_getvalue_ret = sem_getvalue(sem->sem,&semval);
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_wait() : sem=%p,sem->sem=%p,sem->key=%d(0x%X),semval=%d,  . . . returning %d\n",
		      (void*)sem,(void*)sem->sem,
		      sem->key,sem->key,semval,retval);
#endif

#else
      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
      ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);      
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_wait() : sem=%p,sem->sem_id=%d(0x%X),sem->key=%d(0x%X),semval=%d,ncount=%d  . . . returning %d\n",
		      (void*)sem,sem->semid,sem->semid,
		      sem->key,sem->key,semval,ncount,retval);
#endif
      return retval;
    }
  start_time = etime ();
  
#ifndef HAVE_SEMTIMEDOP

  old_sigalarm_handler = signal (SIGALRM, semwait_alarm_handler);

  if (old_sigalarm_handler == SIG_ERR)
    {
      rcs_print_error ("Can't setup SIGALRM. errno = %d, %s\n",
		       errno, strerror (errno));
      return -1;
    }
  sem_itimer.it_interval.tv_sec = 0;
  sem_itimer.it_interval.tv_usec = 0;
  sem_itimer.it_value.tv_sec = 0;
  sem_itimer.it_value.tv_usec = 0;
  getitimer (ITIMER_REAL, &sem_itimer_backup);
  sem_itimer_backup.it_value.tv_sec = 0;
  sem_itimer_backup.it_value.tv_usec = 0;

#endif
  do
    {
      if(restart_interrupt_ptr && !*restart_interrupt_ptr)
	{
	  rcs_print_error("restart_interrupt_ptr=%p, (*restart_interrupt_ptr)=%d\n",
			  restart_interrupt_ptr,
			  (*restart_interrupt_ptr));
	  return -1;
	  semwait_restart_ptr=0;
	}
	
#ifndef HAVE_SEMTIMEDOP
 
      sem_itimer.it_interval.tv_sec = time_left;
      sem_itimer.it_interval.tv_usec = (fmod (time_left + 1.0, 1.0) * 1E6);
      sem_itimer.it_value.tv_sec = sem_itimer.it_interval.tv_sec;
      sem_itimer.it_value.tv_usec = sem_itimer.it_interval.tv_usec;
      setitimer (ITIMER_REAL, &sem_itimer, &sem_itimer_backup);
      
#endif

#if 0
      rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		       "Semaphore itimer setup: \n\tit_interval {%d secs and %d usecs}\n\tit_value {%d secs and %d  usecs}\n",
		       sem_itimer.it_interval.tv_sec,
		       sem_itimer.it_interval.tv_usec,
		       sem_itimer.it_value.tv_sec,
		       sem_itimer.it_value.tv_usec);
#endif
      timeout_set=1;
      semwait_restart_ptr = restart_interrupt_ptr;
      
#ifndef HAVE_SEMTIMEDOP
      retval = rcs_sem_wait_internal (sem,restart_interrupt_ptr,NULL);
      
#else 
      struct timespec semtimedop_timespec;
      semtimedop_timespec.tv_sec = (time_t) timeout;
      semtimedop_timespec.tv_nsec = (long) (1e9* (timeout -semtimedop_timespec.tv_sec ));
      
      retval = rcs_sem_wait_internal (sem,restart_interrupt_ptr,&semtimedop_timespec);
#endif

      timeout_set=0;
      semwait_restart_ptr=0;
      
#ifndef HAVE_SEMTIMEDOP
      setitimer (ITIMER_REAL, &sem_itimer_backup, &sem_itimer);
#endif
      
      
#if 0
      rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY,
		       "Semaphore itimer removed.\n");
#endif
      current_time = etime ();
      elapsed_time = current_time - start_time;
      time_left = timeout - elapsed_time;
      if (retval == -1)
	{

#ifdef   HAVE_SEMTIMEDOP
	if(EAGAIN == errno) 
	{
	      retval = -2;
	      last_semwait_alarm_count = semwait_alarm_count;
	      continue;
	}
#endif 


	  if (EINTR == errno
	      && last_semwait_alarm_count < semwait_alarm_count)
	    {
	      retval = -2;
	      last_semwait_alarm_count = semwait_alarm_count;
	      continue;
	    }
	  rcs_print_error ("sem_wait: ERROR: %s %d\n", strerror (errno),errno);
	  semwait_restart_ptr=0;
	  return -1;
	}
    }
  while (time_left > 5e-3 && retval < 0);
  semwait_restart_ptr=0;
  
#ifndef HAVE_SEMTIMEDOP

  setitimer (ITIMER_REAL, &sem_itimer_backup, NULL);
  if (old_sigalarm_handler == SIG_ERR || old_sigalarm_handler == SIG_DFL)
    {
      old_sigalarm_handler = SIG_IGN;
    }
  signal (SIGALRM, old_sigalarm_handler);
  
#endif

#ifdef POSIX_SEMAPHORES
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait() : sem=%p, sem->sem=%p sem->key=%d(0x%X) returning.\n",
		  (void*)sem,sem->sem,sem->key,sem->key);
#else
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait() : sem=%p, sem->id%d(0x%X) sem->key=%d(0x%X) returning.\n",
		  (void*)sem,sem->semid,sem->semid,sem->key,sem->key);
#endif
  return (retval);

  /* USE_ITIMER_SIGNAL */
#else

#ifdef POSIX_SEMAPHORES
  return sem_wait (sem->sem);
#else
  struct sembuf sops;
  double elapsed_time, current_time, start_time;

  semval = semctl (sem->semid, 0, GETVAL, sem_arg);
  ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);  
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait() : called sem=%p,sem->sem_id=%d(0x%X),sem->key=%d(0x%X),semval=%d,ncount=%d  . . . called\n",
		  (void*)sem,sem->semid,sem->semid,
		  sem->key,sem->key,semval,ncount,retval);


  start_time = current_time = 0.0;
  if (timeout >= 0.0)
    {
      start_time = etime ();
    }

  sops.sem_num = 0;		/* only one semaphore in set */
  sops.sem_op = SEM_TAKE;
  if (timeout < 0.0)
    {
      sops.sem_flg = 0;		/* wait indefinitely */
    }
  else
    {
      sops.sem_flg = IPC_NOWAIT;	/* Do not wait, I'll pole the semaphore. */
    }
  if (timeout >= 0.0)
    {
      current_time = etime ();
    }
  elapsed_time = current_time - start_time;
  while (elapsed_time < timeout || timeout < 0.0)
    {
      if (timeout >= 0.0)
	{
	  current_time = etime ();
	  elapsed_time = current_time - start_time;
	  if (elapsed_time > timeout)
	    {
	      return -2;
	    }
	}
      if (semop (sem->semid, &sops, 1) == -1)
	{
	  if (errno == EINTR)
	    {
	      /* interrupted system call-- restart it */
	      if (timeout != 0.0)
		{
		  if(restart_interrupt_ptr && !*restart_interrupt_ptr)
		    {
		      return -1;
		    }
		  continue;
		}
	      else
		{
		  return -1;
		}
	    }
	  else if (errno == EAGAIN)	/* Not waiting. */
	    {
	      if(restart_interrupt_ptr && !*restart_interrupt_ptr)
		{
		  return -1;
		}
	      continue;
	    }
	  else
	    {
	      rcs_print_error
		("semop(semid=%d, {sem_num=%d,sem_op=%d,sem_flg=%d},nsops=1) returned -1 with errno=%d : %s\n",
		 sem->semid, sops.sem_num, sops.sem_op, sops.sem_flg,errno, strerror (errno));
	      return -1;
	    }
	}
      else
	{
	  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
			  "rcs_sem_wait() : sem->sem=%p returning.\n",
			  (void*)sem->sem);
	  return 0;
	}
    }
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_wait() : sem->sem=%p returning.\n",
		  (void*)sem->sem);
  return (0);
#endif /* POSIX_SEMAPHORES */
#endif /* USE_ITIMER_SIGNALS  */

}

int
rcs_sem_post (rcs_sem_t * sem)
{
#if defined(POSIX_SEMAPHORES)
  int retval;
  retval = sem_post(sem->sem);
  if (retval == -1)
    {
      rcs_print_error("sem_post(%ld): ERROR -- %s %d\n",
		      (long)sem->sem,strerror (errno), errno);
      while(retval == -1 && errno == EINTR)
	{
	  retval = sem_post(sem->sem);
	}	  
    }
  return retval;
#else
  struct sembuf sops;
  int lerrno;
  int restarted;
  int semval;

#ifndef sunos5
  union semun sem_arg;
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 0;
#else
  int sem_arg = 0;
#endif

  if(!sem)
    {
      return -1;
    }

  rcs_print_debug (PRINT_SEMAPHORE_ACTIVITY, 
		   "rcs_sem_post(%p) {semid=%d(0x%X),key=%d(0x%X),sem->removed=%d} called.\n",
		   (void*)sem,sem->semid,sem->semid,sem->key,sem->key,
		   sem->removed);

  if(sem->removed)
    {
      return -1;
    }
  sops.sem_num = 0;		/* only one semaphore in set */
  sops.sem_flg = 0;		/* wait indefinitely */
  sops.sem_op = SEM_GIVE;

#if 0
  if (!sem->blocking_type)
    {
      if(semctl (sem->semid, 0, GETVAL, sem_arg) == 1)
	{
	  /* it's given-- leave it alone */
	  return 0;
	}
    }
#endif

  if(sem->blocking_type)
    {
      if(semctl (sem->semid, 0, GETVAL, sem_arg) >= MAX_SEM_BLOCKING_COUNT &&
	 semctl (sem->semid, 0, GETNCNT, sem_arg) < 1)
	{
	  return 0;
	}
    }
  lerrno=0;
  restarted=0;

  /* it's taken-- suppose now others take it again before
     we give it? they block, and this semgive will release
     one of them */
  while (1)
    {
      if (semop (sem->semid, &sops, 1) == -1)
	{
	  lerrno = errno;
	  if (lerrno == EINTR)
	    {
	      /* interrupted system call-- restart it */
	      if(!restarted)
		{
		  rcs_print_error ("semop(%d,&sops=%p{sops.sops.sem_num=%d,sops.sem_flg=%d,sops.sem_op=%d},1) :errno=%d : %s (rcs_sem_post restarting)\n",
				   sem->semid,
				   (void*)&sops,
				   sops.sem_num,
				   sops.sem_flg, 
				   sops.sem_op,
				   lerrno, 
				   strerror (lerrno));
		  restarted=1;
		}
	      continue;
	    }
	  else
	    {
	      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
	      rcs_print_error ("semop(%d,&sops=%p{sops.sops.sem_num=%d,sops.sem_flg=%d,sops.sem_op=%d},1) {semval=%d,sem->blocking_type=%d,sem->key=%d}:errno=%d : %s (rcs_sem_post returning -1)\n",
			       sem->semid,
			       (void*)&sops,
			       sops.sem_num,
			       sops.sem_flg, 
			       sops.sem_op,
			       semval,sem->blocking_type,
			       sem->key,
			       lerrno, 
			       strerror (lerrno));
	      return -1;
	    }
	}
      else
	{
	  return 0;
	}
    }
  return(0);
#endif /* POSIX_SEMAPHORES */
}

#if POSIX_SEMAPHORES
static int internal_sem_flush(rcs_sem_t *sem)
{
  int semval;
#ifdef  POSIX_SEMAPHORES
  int sem_getvalue_ret;
  int retval;
  int max_waiting_count = 1;
  int waiting_count=1;
#else
  int ncount;
  struct sembuf sops;
  union semun sem_arg;
  int sems_to_give;
  int tries=0;
  int sleeps=0;
#endif
 
#ifdef POSIX_SEMAPHORES
    if(!sem || !sem->sem)
    {
      return -1;
    }
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_flush() : sem->sem=%p\n",
		  (void*)sem->sem);
#ifndef darwin
  sem_getvalue_ret = sem_getvalue(sem->sem,&semval);
  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_wait(): sem_getvalue_ret=%d, sval=%d\n",sem_getvalue_ret,semval);
#endif
  retval = sem_post (sem->sem);
  if (retval == -1)
    {
      rcs_print_error ("sem_post: ERROR -- %s %d\n", strerror (errno), errno);
    }
#ifndef darwin
  retval = sem_getvalue (sem->sem, &semval);
  if (retval == -1)
    {
      rcs_print_error ("sem_getvalue: ERROR -- %s %d\n", strerror (errno),
		       errno);
    }
  if(sem->waiting_count_ptr)
    {
      waiting_count = *((int *)sem->waiting_count_ptr);
      if(waiting_count+1 > max_waiting_count)
	{
	  max_waiting_count=waiting_count+1;
	}
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_flush(%d) : max_waiting_count=%d\n",
		      sem->key,max_waiting_count);
    }
  while (semval < max_waiting_count)
    {
      if(!sem)
	{
	  return(-1);
	}
      retval = sem_post (sem->sem);
      if (retval == -1)
	{
	  rcs_print_error ("sem_post: ERROR -- %s %d\n", strerror (errno),
			   errno);
	  break;
	}
      if(!sem)
	{
	  return(-1);
	}
      retval = sem_getvalue (sem->sem, &semval);
      if (retval == -1)
	{
	  rcs_print_error ("sem_getvalue: ERROR -- %s %d\n", strerror (errno),
			   errno);
	  break;
	}
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_flush(): sem_getvalue_ret=%d, sval=%d\n",sem_getvalue_ret,semval);
      if(sem->waiting_count_ptr)
	{
	  waiting_count = *((int *)sem->waiting_count_ptr);
	  if(waiting_count+1 > max_waiting_count)
	    {
	      max_waiting_count=waiting_count+1;
	    }
	  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_flush(%d) : max_waiting_count=%d\n",
			  sem->key,max_waiting_count);
	}
    }
#endif
#else /* POSIX_SEMAPHORES */
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 0;

  sops.sem_num = 0;		/* only one semaphore in set */
  sops.sem_flg = IPC_NOWAIT;	/* wait indefinitely */
  sops.sem_op = SEM_GIVE;
  semval = semctl (sem->semid, 0, GETVAL, sem_arg);
  ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);

  rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		  "rcs_sem_flush() : sem=%p,sem->sem_id=%d(0x%X),sem->key=%d(0x%X),semval=%d,ncount=%d\n",
		  (void*)sem,sem->semid,sem->semid,
		  sem->key,sem->key,semval,ncount);

  if (ncount < 0)
    {
      ncount = 0;
    }
  if (semval > ncount)
    {
      return 0;
    }

  sems_to_give = ncount - semval + 1;

  /* it's taken-- suppose now others take it again before
     we give it? they block, and this semgive will release
     one of them until semval = 1;  */
  sops.sem_op = sems_to_give;
  while ((ncount > 0 || semval < 0))
    {
      if(sems_to_give > 0)
	{
	  if (semop (sem->semid, &sops, 1) == -1)
	    {
	      rcs_print_error ("semop(%d(0x%X),{sops.sem_op=%d},1) errno=%d : %s\n", 
			       sem->semid, sem->semid,sops.sem_op,errno, strerror (errno));
	      return(-1);
	    }
	}
      else
	{
	  if(sleeps < 2)
	    {
	      sleeps++;
	      esleep(0.01);
	    }
	  else
	    {
	      break;
	    }
	}
      semval = semctl (sem->semid, 0, GETVAL, sem_arg);
      ncount = semctl (sem->semid, 0, GETNCNT, sem_arg);
      tries++;
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,
		      "rcs_sem_flush() : tries=%d, sem=%p,sem->sem_id=%d(0x%X),sem->key=%d(0x%X),semval=%d,ncount=%d\n",
		      tries, (void*)sem,sem->semid,sem->semid,
		      sem->key,sem->key,semval,ncount);
      sems_to_give = ncount - semval;
      sops.sem_op = sems_to_give;
    }
/* !  POSIX_SEMAPHORES */
#endif
  return(0);
}
#endif

int
rcs_sem_flush(rcs_sem_t * sem)
{
#if POSIX_SEMAPHORES
  return internal_sem_flush(sem);
#else 
  return rcs_sem_post(sem);
#endif
}


int
rcs_sem_getvalue (rcs_sem_t * sem, unsigned int *sval)
{
#ifdef POSIX_SEMAPHORES
  int svali_temp = (int) *sval;
  int sem_getvalue_ret = sem_getvalue (sem->sem, &svali_temp);
  *sval = (unsigned int) svali_temp;
  return sem_getvalue_ret;
#else

#ifndef sunos5
  union semun sem_arg;
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = 0;
#else
  int sem_arg = 0;
#endif

  return (*sval = (unsigned int) semctl (sem->semid, 0, GETVAL, sem_arg));
#endif
}

rcs_sem_t *
rcs_sem_create_w_ulong_key (unsigned long int _key, int mode, int state, int blocking_type)
{
#ifndef POSIX_SEMAPHORES
  union semun sem_arg;
#endif
  rcs_sem_t *sem;

  if (_key < 1)
    {
      rcs_print_error ("rcs_sem_create: invalid key %ld\n", _key);
      return NULL;
    }

  rcs_sem_open_val = state;

#ifdef POSIX_SEMAPHORES
  sem = rcs_sem_open_w_ulong_key ( _key , O_CREAT, mode,blocking_type);
#else
  sem = rcs_sem_open_w_ulong_key ( _key, IPC_CREAT, mode,blocking_type);

  if (NULL == sem)
    {
      rcs_print_error ("sem_init: Pointer to semaphore object is NULL.\n");
      return NULL;
    }
  memset(&sem_arg,0,sizeof(sem_arg));
  sem_arg.val = state;
  semctl (sem->semid, 0, SETVAL, sem_arg);

#endif

  return sem;
}

int rcs_sem_inc_waiting(
#ifndef POSIX_SEMAPHORES
			__unused_parameter__ 
#endif
			volatile rcs_sem_t * sem)
{
#if POSIX_SEMAPHORES
  if(sem->waiting_count_ptr)
    {
      (*((int *)sem->waiting_count_ptr))++;
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_inc_waiting(%d) *sem->waiting_count_ptr =%d\n",sem->key,*sem->waiting_count_ptr);
    }
#endif
  return 0;
}

int rcs_sem_dec_waiting(
#ifndef POSIX_SEMAPHORES
			__unused_parameter__ 
#endif
			volatile rcs_sem_t * sem)
{
#if POSIX_SEMAPHORES
  if(sem && sem->waiting_count_ptr)
    {
      (*((int *)sem->waiting_count_ptr))--;
      rcs_print_debug(PRINT_SEMAPHORE_ACTIVITY,"rcs_sem_dec_waiting(%d) *sem->waiting_count_ptr =%d\n",sem->key,*sem->waiting_count_ptr);
    }
#endif
  return 0;
}

double rcs_sem_get_default_bsem_wait(void)
{
#ifdef POSIX_SEMAPHORES
  return (0.01);
#else
  return (-1.0);
#endif
}


