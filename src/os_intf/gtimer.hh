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

/************************************************************************
* File: Global_timer.hh
*  This header file defines the interface used by the RCS_TIMER and etime utilities to access a Global Timer card, via some code created by Tom Wheatley in
Global_timer.cc and global_timer.s.
* It is used only by VxWorks versions of the library.
************************************************************************/

#ifndef GLOBAL_TIMER_HH
#define GLOBAL_TIMER_HH

/* Extern C the functions so they'll be easy to call from the VxWorks Shell */
#ifdef __cplusplus
extern "C"
{
#endif

/* global_timer_config should be called from the vxworks startup script
* after the RCS library is loaded and before any calls to etime().
* The argument is the address of the global timer board.
* (The address is probably 0x00C00010 .) */
  int global_timer_config (unsigned long);

/* get_Global_time is called by etime() */
  int get_Global_time ();

  int get_Global_elapsed_time (int);
#ifdef __cplusplus
}
#endif

/* This is the global variable that etime checks to see whether it should call
* get_Global_time(); */
extern int global_timer_available;


#endif
