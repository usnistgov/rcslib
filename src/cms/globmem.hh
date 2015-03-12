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
* File: globmem.hh                                                       *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the Communication Management System(CMS). *
* Includes:                                                              *
*          1. class GLOBMEM.                                             *
* Note: GLOBMEM is used for communication between multiple VxWorks       *
* boards in a single backplane.                                          *
*************************************************************************/

#ifndef GLOBMEM_HH
#define GLOBMEM_HH

/* Include Files */

#ifdef VXWORKS
extern "C"
{
#include <vxWorks.h>		/* NULL */
#include <stddef.h>		/* size_t */
}

#else /* !VXWORKS */
extern "C"
{
#include <stdio.h>		/* NULL */
#include <stddef.h>		/* size_t */
}

#endif				/* !VXWORKS */

#include "cms.hh"

#if defined(WIN32) && defined(USE_BIT3)
#include "btapi.h"
#endif



/* default base address of most of our VME memory boards, which run from
   0x04E00000 to 0x04FFFFFF */
#define GLOBMEM_DEFAULT_ADDRESS ((void *) 0x04E00000)
#define GLOBMEM_DEFAULT_LOCK    ((void *) 0x04FFFF00)
#define GLOBMEM_MASTER 1	/* exactly one process, the first to connect */
#define GLOBMEM_NOMASTER 0	/* all other processes */


enum GLOBMEM_ADDRESS_TYPE
{
  INVALID_ADDRESS_TYPE,
  PROCESS_SPECIFIC_ADDRESS,
  HOST_SPECIFIC_ADDRESS,
  VME_ADDRESS,
  EISA_ADDRESS,
  GENERIC_ADDRESS,
  ISA_ADDRESS,
  PCI_ADDRESS
};

struct BL_ADDR_INFO;
struct DMA_ADDR_INFO;
struct GMEM_PRIVATE_INFO;

class GLOBMEM:public CMS
{
public:
  GLOBMEM (const char *bufline, const char *procline, int set_to_server = 0,
	   int set_to_master = 0);
    virtual ~ GLOBMEM ();

  CMS_STATUS main_access (void *user_data);
  GLOBMEM_ADDRESS_TYPE address_type;
  char address_string[CMS_CONFIG_LINELEN];
  unsigned long physical_address;
  virtual unsigned long get_physical_address (char *);
  PHYSMEM_HANDLE *physmem_handle;
  int get_access ();
  int release_access ();
  CMS_STATUS blocking_read (double);
  int check_if_transfers_complete ();	/* Has DMA completed */
  int I_locked_the_task;	/* BOOLEAN: Was this globmem object the one */
  /* that locked the task if it is locked? */

  /* data buffer stuff */
  char address_type_name[CMS_CONFIG_LINELEN];	/* name of address space */
  /* or ascii code */
  char *local_locks;		/* address of short int for TAS mutex */
  double sem_delay;		/* Time to wait between polling the semaphore. */
  int read_only;
  BL_ADDR_INFO *bl_info;
  DMA_ADDR_INFO *dma_info;
  char *my_lock;
  char *toggle_bit_address;
  char *was_read_address;
  int board_type;
  int lock_bus;
  int use_dma;

  int lock_task;
  int use_test_and_set;
  double sem_spin_delay;
  bool use_sem_spin_delay;
  struct GMEM_PRIVATE_INFO *gmem_private_info;
  bool no_mutex;
  bool toggle_split_buffer_toggle_bit_needed;
  //  bool use_physmem_handle;
private:
  bool check_locks_clear();
  void toggle_split_buffer_toggle_bit();

#ifdef VXWORKS
  SEM_ID bsem;
#endif

};

extern unsigned long (*get_physical_address_func) (char *);

#endif /* !defined(GLOBMEM_HH) */


