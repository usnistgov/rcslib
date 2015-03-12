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

/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#ifndef TIMER_HH
#define TIMER_HH

#ifndef _TIMER_H
#ifdef __cplusplus
extern "C"
{
#endif

  /* number of seconds from standard epoch, to clock tick resolution */
  extern double  etime (void);

  /* sleeps # of seconds, to clock tick resolution */
  extern void  esleep (double secs);

#ifdef __cplusplus
}
#endif
#endif

#ifdef __cplusplus

#ifdef MSDOS
#ifdef USE_SEMS_FOR_TIMER
#undef USE_SEMS_FOR_TIMER
#endif
#endif

#ifdef USE_SEMS_FOR_TIMER
class RCS_SEMAPHORE;
#endif



/* prototype for user-defined timing function */
typedef int (*RCS_TIMERFUNC) (void *_arg);

/* Getting rid of this stuff which noone uses and makes porting more
difficult */
#ifdef RCS_TIMER_USE_ITIMER

/* prototype for signal hander function */
typedef void (*RCS_SIGFUNC) (...);

#if (defined (SUN) || defined (LYNX)) && !defined(lynxosVME) && !defined(sunos4CC)

/* set up SIGALRM to go off after time, either once or periodically */
extern int itimer_set (double time, int periodic = 1);

/* attach a function to the SIGALRM handler */
extern void itimer_attach (RCS_SIGFUNC function);

/* block on arrival of SIGALRM */
extern void itimer_wait ();

#endif /* SUN or LYNX */

#endif /* RCS_TIMER_USE_ITIMER */



/*
  general-purpose timer, which can be used for waiting until a
  synchronous time tick, slept on for any period at all, or to
  obtain a time in system clock ticks from creation of the timer.
  */
class  RCS_TIMER
{
public:

  RCS_TIMER (double timeout, RCS_TIMERFUNC function =
	     (RCS_TIMERFUNC) 0, void *arg = 0);

#ifdef LINUXCNC_LIBNML_COMPAT
  RCS_TIMER(const char *process_name, const char *timer_config_file);
  RCS_TIMER(double _timeout, const char *process_name, const char *timer_config_file);
#endif   // LINUXCNC_LIBNML_COMPAT

  /* Timeout is wait interval, rounded up to clock tick resolution;
     function is external time base, if provided */
  ~RCS_TIMER ();
  int wait ();			/* wait on synch; returns # of cycles missed */
  double load ();		/* returns % loading on timer, 0.0 means
				   all waits, 1.0 means no time in wait */
  void sync ();			/* restart the wait interval. */
  double timeout;		/* copy of timeout */

private:
  RCS_TIMER(RCS_TIMER &);
  RCS_TIMER & operator=(const RCS_TIMER& _timer) { (*this)=_timer; return(*this);};

  void init (double _timeout, int _id);

  void zero_timer ();
  void set_timeout (double _timeout);
#if 0
  void read_config_file (char *process, char *config_file);
#endif
  RCS_TIMERFUNC function;	/* copy of function */
  void *arg;			/* arg for function */
  double last_time;		/* last wakeup, in ticks from epoch */
  double start_time;		/* epoch time at creation of timer */
  double idle;			/* accumulated idle time */
  int counts;			/* accumulated waits */
  int counts_since_real_sleep;
  int counts_per_real_sleep;
  double time_since_real_sleep;
#ifdef USE_SEMS_FOR_TIMER
  class RCS_SEMAPHORE **sems;
#endif
  int num_sems;
  int sem_key;
  int id;
  int create_sems;
  int poller_pid;
  double clk_tck_val;
};

// __cplusplus
#endif

#endif

/*
  Modification history:

  $Log$
  Revision 1.5  2005/07/01 14:16:57  proctor
  Put a dollar-log-dollar comment at the end

*/
