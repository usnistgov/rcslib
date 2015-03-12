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


#ifndef SHM_HH
#define SHM_HH

#include <stddef.h>		// size_t

struct shm_t_struct;

#define RCS_SHAREDMEM_NOCREATE 0x00	/* just attach to existing sharedmem */
#define RCS_SHAREDMEM_CREATE 0x01	/* create sharedmem */

class RCS_SHAREDMEM
{
public:
  RCS_SHAREDMEM (long key, size_t size, int oflag, 
		 int mode = 0,
		 int wait_for_master=0, 
		 double wfm_delay=1.0, 
		 double wfm_period=-1.0);
  ~RCS_SHAREDMEM ();

  int nattch ();		/* how many processes are attached */
  int create_errno;		/* 0 or stored errno after shmget failed */
  void *addr;			/* pointer to shared memory */
  int delete_totally;		/* Flag to clean the sharedmem completely */
  bool leave_resource;

private:
    struct shm_t_struct * shm;
public:
  int created;

private:
  // Don't copy me.
  RCS_SHAREDMEM (const RCS_SHAREDMEM & shm);
  RCS_SHAREDMEM &operator=(const RCS_SHAREDMEM &shm);

};

// SHM_HH
#endif
