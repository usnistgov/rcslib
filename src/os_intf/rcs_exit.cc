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


/**********************************************************************
* File: rcs_exit.cc
* This module provides a portable way to make sure multiple
* functions are called before exiting.
* These functions should be written to take an int  and return void.
***********************************************************************/

#include "rcs_defs.hh"		// MS_WINDOWS_API,

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// exit()
#include <signal.h>		// signal() , SIGINT

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef MS_WINDOWS_API
#include <windows.h>		// GetCurrentProcessId()
#endif

#ifdef VXWORKS
#include <taskLib.h>		// taskIdSelf()
#endif

/* Forward Function Prototypes */
#include "rcs_exit.hh"
#include "linklist.hh"		//RCS_LINKED_LIST
#include "rcs_prnt.hh"		// rcs_print_error()
#include "timer.hh"		// esleep()

static RCS_LINKED_LIST *exit_list = (RCS_LINKED_LIST *) NULL;

struct RCS_EXIT_LIST_ENTRY
{
  long process_id;
  void (*fptr) (int);
};

// NOTE --
// the GNU VxWorks C++ cross-compiler (g++68k) has a bug, that
// prevents me from passing a pointer to a function as the first
// argument of a function.
#if defined(VXWORKS) && !defined(__CENTERLINE__)
int RCS_EXPORT
attach_rcs_exit_list (void *fptr)
#else
int RCS_EXPORT
attach_rcs_exit_list (void (*fptr) (int))
#endif
{
  RCS_EXIT_LIST_ENTRY entry;
  if (NULL == exit_list)
    {
      exit_list = new RCS_LINKED_LIST;
    }
  if (NULL == exit_list)
    {
      rcs_print_error ("attach_rcs_exit_list:: Out of Memory.\n");
      return -1;
    }
  entry.process_id = 0;
  entry.fptr = fptr;
#ifdef MS_WINDOWS_API
  entry.process_id = ((long) GetCurrentProcessId ());
#endif
#ifdef VXWORKS
  entry.process_id = (long) taskIdSelf ();
#endif
  return exit_list->store_at_tail (&entry, sizeof (entry), 1);
}

void RCS_EXPORT
rcs_cleanup (int code)
{
  RCS_EXIT_LIST_ENTRY *entry;
  long process_id = 0;
#ifdef MS_WINDOWS_API
  process_id = ((long) GetCurrentProcessId ());
#endif
#ifdef VXWORKS
  process_id = (long) taskIdSelf ();
#endif

  if (NULL == exit_list)
    {
      return;
    }
  entry = (RCS_EXIT_LIST_ENTRY *) exit_list->get_head ();
  while (NULL != entry)
    {
      if (entry->process_id == process_id && entry->fptr != NULL)
	{
	  entry->fptr (code);
	}
      entry = (RCS_EXIT_LIST_ENTRY *) exit_list->get_next ();
    }
  if (exit_list->list_size == 0)
    {
      delete exit_list;
      exit_list = (RCS_LINKED_LIST *) NULL;
    }
}

static int rcs_ready_for_exit = 0;
static int rcs_exit_sig = 0;
static void
rcs_exit_signal_handler (int sig)
{
  rcs_ready_for_exit = 1;
  rcs_exit_sig = sig;
}

void RCS_EXPORT
rcs_exit (int code)
{
  rcs_cleanup (code);
#ifndef VXWORKS
  if (code == -1)
    {
      rcs_print_error ("\n Errors Reported!!!\n Press ^C to exit.\n");
      signal (SIGINT, rcs_exit_signal_handler);
      int secs = 0;
      while (!rcs_ready_for_exit && secs < 600)
	{
	  esleep (1.0);
	  secs++;
	}
    }
#endif
  exit (code);
}
