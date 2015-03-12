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
  timer.cc -- interval timer code.  A TIMER object lets you wait on
  the expiration of a cyclic period, to the resolution of the system
  clock.

  Ideally, we'd like to use the POSIX struct timespec, in timers.h,
  for second-nanosecond resolution in real time.  However, at the
  moment, most OSes do not have this interface since the POSIX realtime
  draft is still out.

  These functions use the BSD 'gettimeofday' interface for LynxOS and
  SunOS, and the tickLib and taskLib interface on VxWorks.
*/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "timer_no_config.h"
/* ! HAVE_CONFIG_H */
#endif 

#include "inetfile.hh"		// inet_file_open(), inet_file_gets()

#include "timer.hh"
#include "_timer.h"


#ifdef RCS_TIMER_USE_ITIMER

#if (defined (SUN) || defined (LYNX)) && !defined(lynxosVME)  && !defined(sunos4CC)

/* set up SIGALRM to go off after time, either once or periodically */
int
itimer_set (double time, int periodic)
{
  struct itimerval val;
  long secs, usecs;

  /* set up the interval timer */
  secs = (long) time;		/* lose the fraction */
  usecs = (long) ((time - (double) secs) * 1000000.0);
  val.it_value.tv_sec = secs;
  val.it_value.tv_usec = usecs;
  if (periodic == 0)
    {
      val.it_interval.tv_sec = val.it_interval.tv_usec = 0;
    }
  else
    {
      val.it_interval.tv_sec = val.it_value.tv_sec;
      val.it_interval.tv_usec = val.it_value.tv_usec;
    }
  return setitimer (ITIMER_REAL, &val, 0);
}

/* do-nothing default function for itimer_attach() */
static void
noaction (int i)
{
  return;
}

/* attach a function to the SIGALRM handler */
void
itimer_attach (RCS_SIGFUNC function)
{
  struct sigaction act;

  /* set up action to occur upon receipt of SIGALRM-- call function */
#if defined(sparcworks_sun5) || defined(sunos5CC)	/* CenterLine is picky, picky, picky */
  act.sa_handler = (void (*)(int)) ((function == 0) ? noaction : function);
#else
  act.sa_handler = ((function == 0) ? noaction : function);
#endif
  sigfillset (&act.sa_mask);	/* mask all signals while in SIGALRM */
  act.sa_flags = 0;		/* no special flags */
  sigaction (SIGALRM, &act, 0);
}

/* block on arrival of SIGALRM */
void
itimer_wait ()
{
  sigset_t mask;

  /* set up to block on SIGALRM */
  sigfillset (&mask);		/* block all signals */
  sigdelset (&mask, SIGALRM);	/* accept SIGALRM */
  sigsuspend (&mask);		/* block until SIGALRM is received */
}

#endif /* SUN or LYNX */

#endif /* RCS_TIMER_USE_ITIMER */


/* RCS_TIMER class */


void
RCS_TIMER::set_timeout (double _timeout)
{
  timeout = _timeout;
#if 0
  if (timeout < clk_tck ())
    {
      counts_per_real_sleep = (int) (clk_tck () / _timeout) + 1;
    }
  else
    {
      counts_per_real_sleep = 0;
    }
#endif
}

#if 0
void
RCS_TIMER::read_config_file (char *process_name, char *config_file)
{
  zero_timer ();
  if (0 == process_name || 0 == config_file)
    {
      return;
    }
  INET_FILE *ifp = inet_file_open (config_file, "r");
  if (0 == ifp)
    {
      return;
    }
  int process_name_length = strlen (process_name);
  char line[256];
  char *token;
  while (!inet_file_eof (ifp))
    {
      inet_file_gets (line, 256, ifp);
      token = strtok (line, " \r\n:\t\b,;");
      if (line[0] == '#')
	{
	  continue;
	}
      if (token == 0)
	{
	  continue;
	}
      if (strncmp (token, process_name, process_name_length))
	{
	  continue;
	}
      token = strtok (0, " \r\n:\t\b,;");
      if (0 == token)
	{
	  break;
	}
      timeout = atof (token);
      token = strtok (0, " \r\n:\t\b,;");
      if (0 == token)
	{
	  break;
	}
#if 0
      sem_key = strtol (token, 0, 0);
      token = strtok (0, " \r\n:\t\b,;");
      if (0 == token)
	{
	  break;
	}
      num_sems = strtol (token, 0, 0);
      token = strtok (0, " \r\n:\t\b,;");
      if (0 == token)
	{
	  break;
	}
#endif
      id = strtol (token, 0, 0);
      token = strtok (0, " \r\n:\t\b,;");
      if (0 == token)
	{
	  break;
	}
#if 0
      create_sems = strtol (token, 0, 0);
#endif
    }
  inet_file_close (ifp);
  init (timeout, id);
}
#endif



void
RCS_TIMER::zero_timer ()
{
  num_sems = 0;
#ifdef USE_SEMS_FOR_TIMER
  sems = 0;
#endif
  id = 0;
  function = 0;
  idle = 0.0;			/* set accumulated idle time to 0.0 */
  counts = 0;			/* set accumulated waits to 0 */
  start_time = etime ();	/* set creation time to now */
  time_since_real_sleep = start_time;
  counts_per_real_sleep = 0;
  counts_since_real_sleep = 0;
  clk_tck_val = clk_tck ();
  timeout = clk_tck_val;
}



void
RCS_TIMER::init (double _timeout, int _id)
{
  zero_timer ();
  id = _id;
#if 0
  num_sems = _num_sems;
  int sem_key = _sem_key;
  int create_sems = _create_sems;
  if (num_sems > 0 && id >= 0 && id < num_sems)
    {
      sems =
	(RCS_SEMAPHORE **) DEBUG_MALLOC (sizeof (RCS_SEMAPHORE *) * num_sems);
      for (int i = 0; i < num_sems; i++)
	{
	  sems[i] = new RCS_SEMAPHORE (sem_key + i, create_sems, -1);
	}
      sems[id]->post ();
      poller_pid = fork ();
      if (poller_pid == 0)
	{
	  timer_poll (_timeout, sems[id]);
	}
    }
  else
    {
      num_sems = 0;
    }
#endif
  set_timeout (_timeout);

}

#ifdef LINUXCNC_LIBNML_COMPAT
RCS_TIMER::RCS_TIMER(const char *process_name, const char *timer_config_file) {
  zero_timer ();
}

RCS_TIMER::RCS_TIMER(double _timeout, const char *process_name, const char *timer_config_file):
  timeout(0),function(0),arg(0),last_time(0),start_time(0),idle(0),
  counts(0),counts_since_real_sleep(0),counts_per_real_sleep(0),
  time_since_real_sleep(0),
#ifdef USE_SEMS_FOR_TIMER
  sems(0),
#endif
  num_sems(0),sem_key(0),id(0),create_sems(0),poller_pid(0),clk_tck_val(0)
{
  zero_timer ();
  counts_per_real_sleep = 0;
  counts_since_real_sleep = 0;

  if (_timeout < clk_tck_val)
    {
#if 0
      counts_per_real_sleep = (int) (clk_tck_val / _timeout);
#endif
      /* bump interval up to minimum system clock tick */
      timeout = clk_tck_val;
    }
  else
    {
      timeout = _timeout;
    }
  function = 0;
  arg = 0;
  last_time = etime ();		/* initialize start time and last time called
				   to current time since epoch */
  idle = 0.0;			/* set accumulated idle time to 0.0 */
  counts = 0;			/* set accumulated waits to 0 */
  start_time = etime ();	/* set creation time to now */
  time_since_real_sleep = start_time;
}

#endif   // LINUXCNC_LIBNML_COMPAT

RCS_TIMER::RCS_TIMER (double _timeout, RCS_TIMERFUNC _function, void *_arg):
  timeout(0),function(0),arg(0),last_time(0),start_time(0),idle(0),
  counts(0),counts_since_real_sleep(0),counts_per_real_sleep(0),
  time_since_real_sleep(0),
#ifdef USE_SEMS_FOR_TIMER
  sems(0),
#endif
  num_sems(0),sem_key(0),id(0),create_sems(0),poller_pid(0),clk_tck_val(0)
{
  zero_timer ();
  counts_per_real_sleep = 0;
  counts_since_real_sleep = 0;

  if (_timeout < clk_tck_val)
    {
#if 0
      counts_per_real_sleep = (int) (clk_tck_val / _timeout);
#endif
      /* bump interval up to minimum system clock tick */
      timeout = clk_tck_val;
    }
  else
    {
      timeout = _timeout;
    }
  function = _function;
  arg = _arg;
  last_time = etime ();		/* initialize start time and last time called
				   to current time since epoch */
  idle = 0.0;			/* set accumulated idle time to 0.0 */
  counts = 0;			/* set accumulated waits to 0 */
  start_time = etime ();	/* set creation time to now */
  time_since_real_sleep = start_time;
}

RCS_TIMER::RCS_TIMER(RCS_TIMER &):
  timeout(0),function(0),arg(0),last_time(0),start_time(0),idle(0),
  counts(0),counts_since_real_sleep(0),counts_per_real_sleep(0),
  time_since_real_sleep(0),
#ifdef USE_SEMS_FOR_TIMER
  sems(0),
#endif
  num_sems(0),sem_key(0),id(0),create_sems(0),poller_pid(0),clk_tck_val(0)
{
}

/* Restart the timing interval. */
void
RCS_TIMER::sync ()
{
  last_time = etime ();		/* initialize start time and last time called
				   to current time since epoch */
}

int
RCS_TIMER::wait ()
{
  double interval;		/* interval between this and last wakeup */
  double numcycles;		/* interval, in units of timeout */
  int missed = 0;		/* cycles missed */
  double remaining = 0.0;	/* time remaining until timeout */
  double time_in = 0.0;		/* time wait() was entered */
  double time_done = 0.0;	/* time user function finished */
  /* first call the user timing function, if any */
  if (function != 0)
    {
      /* set time in */
      time_in = etime ();

      if ((*function) (arg) == -1)
	{
	  return -1;		/* fatal error in timing function */
	}
      time_done = etime ();
    }
  else
    {
      /* set time in, time done not used */
      time_in = etime ();
    }

  /* calculate the interval-- for user timing functions, this is how
     long between this wakeup and the last wakeup.  For internal timers,
     this is how long we need to sleep to make it to the next interval
     on time. */
  interval = time_in - last_time;
  numcycles = interval / timeout;

  /* synchronize and set last_time correctly; update idle time */
  counts++;
  if (function != 0)
    {
      last_time = time_done;
    }
  idle += interval;
  missed=(int) numcycles;
  remaining = timeout -interval;
  esleep (remaining);
  last_time = etime ();
  return missed;
}

double
RCS_TIMER::load ()
{
  if (counts * timeout > 1e-9)
    return idle / (counts * timeout);
  return -1.0;
}

RCS_TIMER::~RCS_TIMER ()
{
#if 0
  if (poller_pid > 0)
    {
      kill (poller_pid, SIGINT);
#ifdef SUN
      waitpid (poller_pid, 0, 0);
#endif
      poller_pid = 0;
    }
  if (0 != sems)
    {
      for (int i = 0; i < num_sems; i++)
	{
	  if (sems[i] != 0)
	    {
	      delete sems[i];
	    }
	  sems[i] = 0;
	}
      DEBUG_FREE (sems);
    }
#endif
}
