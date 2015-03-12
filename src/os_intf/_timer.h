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


#ifndef _TIMER_H
#define _TIMER_H

/* Useful time routines */

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
extern "C"
{
#endif
/* clock tick resolution, in seconds */
  extern double clk_tck (void);

#ifndef TIMER_HH

/* number of seconds from standard epoch, to clock tick resolution */
  extern double  etime (void);

/* sleeps # of seconds, to clock tick resolution */
  extern void  esleep (double secs);

#endif

  void start_timer_server (int priority, int sem_id);
  void kill_timer_server (void);

#ifdef VXWORKS
  int covertSecondsToTicks (double secs);
#endif

  extern void print_etime (void);

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
}
#endif

extern int etime_disabled;
extern double etime_disable_time;
extern int esleep_use_yield;

#endif
