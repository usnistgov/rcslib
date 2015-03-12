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


#ifndef SEMAPHORE_HH
#define SEMAPHORE_HH


#include "_sem.h"		/* rcs_sem_t */
#if defined(USE_SIRUSR) && !defined(HAVE_CONFIG_H)
#ifdef EXTERN_C_STD_HEADERS
extern "C" {
#endif

#include <sys/stat.h>		/* S_IRUSR, etc. */
#ifdef EXTERN_C_STD_HEADERS
}
#endif
#endif

/* rw-rw-rw- permissions */
#ifndef MS_WINDOWS_API
#ifdef USE_SIRUSR
#define DEFAULT_SEM_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#else
#define DEFAULT_SEM_MODE (0777)
#endif
#else
#define DEFAULT_SEM_MODE (0)
#endif

#define RCS_SEMAPHORE_NOCREATE 0x00	/* just attach to existing semaphore */
#define RCS_SEMAPHORE_CREATE 0x01	/* create semaphore */

class RCS_SEMAPHORE
{
public:
  RCS_SEMAPHORE (unsigned long int id, int oflag, double _timeout,
		 int mode = DEFAULT_SEM_MODE, int state = 0, bool blocking_type=false);
   ~RCS_SEMAPHORE ();
  int wait ();
  int trywait ();
  int post ();
  int flush ();
  int getvalue ();
  /* additional non-POSIX functions */
  int setflag (int oflag);	/* change oflag-- one can toggle the
				   state of this being the master, so
				   that flexible destruction of OS
				   semaphore can be done */
  int valid ();
  int clear ();			// Make sure this semaphore will not
  // immediately be available.

  void interrupt_operation(void);
  void clear_interrupt_operation(void);
  void inc_waiting(void);
  void dec_waiting(void);

  unsigned long int id;
  double timeout;
  int oflag;
  int mode;
  int state;
  rcs_sem_t *sem;
  unsigned int sval;
  bool leave_resource;
  bool blocking_type;

private:
  bool interrupting_operation;
  int restart_after_interrupt;

private:
  //  Don't copy me.
  RCS_SEMAPHORE (const RCS_SEMAPHORE & sem);	
  RCS_SEMAPHORE &operator=(const RCS_SEMAPHORE & sem);

};

#endif
