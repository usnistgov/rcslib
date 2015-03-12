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
  _timer.c -- interval timer code.  A TIMER object lets you wait on
  the expiration of a cyclic period, to the resolution of the system
  clock.

  Ideally, we'd like to use the POSIX struct timespec, in timers.h,
  for second-nanosecond resolution in real time.  However, at the
  moment, most OSes do not have this interface since the POSIX realtime
  draft is still out.

  These functions use the BSD 'gettimeofday' interface for LynxOS and
  SunOS, and the tickLib and taskLib interface on VxWorks.

*/


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "_timer_no_config.h"
#endif /* ! HAVE_CONFIG_H */

#include "_timer.h"
#include "rcs_prnt.hh"		/* rcs_print_error */

#if HAVE_SCHED_H
#include <sched.h>
#endif

/* number of seconds in a system clock tick */
double
clk_tck ()
{
#if HAVE_CONFIG_H

#if HAVE_SYSCONF  && defined(_SC_CLK_TCK)
  return 1.0 / sysconf (_SC_CLK_TCK);
#define CLK_TCK_RETURNED
#else /* HAVE_SYSCONF */
#if defined(CLK_TCK)
  return 1.0 / CLK_TCK;
#define CLK_TCK_RETURNED
#endif /* defined(CLK_TCK) */
#endif /* HAVE_SYSCONF */

#if defined(MS_WINDOWS_API) && !defined(CLK_TCK_RETURNED)
  return(0.001);
#define CLK_TCK_RETURNED
#endif

#else /* HAVE_CONFIG_H */

#ifdef VXWORKS
#ifdef USE_GLOBAL_TIMER
  if (global_timer_available)
    {
      return (1E-6);
    }
#endif
  return 1.0 / (double) sysClkRateGet ();
#define CLK_TCK_RETURNED
#endif

#ifdef LYNX
  return 1.0 / (double) CLK_TCK;
#define CLK_TCK_RETURNED
#endif

#if defined(SUN) || defined(SGI) || defined(linux) || defined(darwin) || defined(qnx) || defined(IRIX)
  return 1.0 / (double) sysconf (_SC_CLK_TCK);
#define CLK_TCK_RETURNED
#endif

#if defined(MS_WINDOWS_API)
  return (0.001);
#define CLK_TCK_RETURNED
#endif

#ifdef __MSDOS__
  return (1.0 / (double) CLK_TCK);
#define CLK_TCK_RETURNED
#endif

#ifndef CLK_TCK_RETURNED
#ifdef CLK_TCK
  return (1.0 / CLK_TCK);
#define CLK_TCK_RETURNED
#endif
#endif

#endif /* HAVE_CONFIG_H */


#ifndef CLK_TCK_RETURNED
#error No definition for clk tck for this platform.
#endif


}


/*
 These values can be used with a debugger to prevent
 or diliberately cause timeouts.
 */

int etime_disabled = 0;
double etime_disable_time = 0.0;

#ifndef TWO_TO_THIRTYTWO
#define TWO_TO_THIRTYTWO  (4294967296.00)
#endif

/* number of seconds from some epoch, to clock tick resolution */
double
etime ()
{
#if HAVE_CONFIG_H

#if !defined(ETIME_RETURNED) && defined(HAVE_CLOCKGETTIME) && defined(CLOCK_REALTIME)
  double retval;
  struct timespec ts;
  if (clock_gettime (CLOCK_REALTIME, &ts))
    {
      rcs_print_error ("clock_gettime: errno = %d %s\n", errno,
		       strerror (errno));
    }
  retval = ((double) ts.tv_sec) + ((double) ts.tv_nsec) * 1e-9;
  return retval;
#define ETIME_RETURNED
#endif

#if !defined(ETIME_RETURNED) && HAVE_GETTIMEOFDAY
  double retval = 0.0;
  struct timeval tp;
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (gettimeofday (&tp, NULL) == -1)
    {
      rcs_print_error ("gettimeofday failed: (errno = %d) %s\n", errno,
		       strerror (errno));
      return 0.0;
    }
  retval = ((double) tp.tv_sec) + (((double) tp.tv_usec) / 1000000.0);
  return (retval);
#define ETIME_RETURNED
#endif


#if !defined(ETIME_RETURNED) && defined(HAVE_QUERYPERFORMANCECOUNTER) && defined(MS_WINDOWS_API)
  BOOL frequency_ok = 0;
  BOOL counter_ok = 0;
  LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  double dfreq;
  double dcount;
  frequency_ok = QueryPerformanceFrequency (&frequency);
  counter_ok = QueryPerformanceCounter (&counter);
  if (frequency_ok && counter_ok)
    {
      dfreq = frequency.HighPart * TWO_TO_THIRTYTWO + frequency.LowPart;
      dcount = counter.HighPart * TWO_TO_THIRTYTWO + counter.LowPart;
      if (dfreq > 0.0 && dcount > 0.0)
	{
	  return (dcount / dfreq);
	}
    }
  else
    {
      rcs_print_error("QueryPerformanceFrequency returned %d, QueryPerformanceCounter returned %d\n",(int)frequency_ok,(int)counter_ok);
    }
  return(0);
#define ETIME_RETURNED
#endif

#if !defined(ETIME_RETURNED) && defined(MS_WINDOWS_API)
  SYSTEMTIME st;
  FILETIME ft;
  SYSTEMTIME unixEpochSysTime;
  FILETIME unixEpochFileTime;
  ULARGE_INTEGER epoch_ui;
  ULARGE_INTEGER ui;
  memset(&unixEpochSysTime,0,sizeof(unixEpochSysTime));
  unixEpochSysTime.wYear = 1970;
  unixEpochSysTime.wMonth = 1;
  unixEpochSysTime.wDay = 1;
  SystemTimeToFileTime(&unixEpochSysTime,&unixEpochFileTime);
  GetSystemTime(&st);
  SystemTimeToFileTime(&st,&ft);
  ui.LowPart = ft.dwLowDateTime;
  ui.HighPart = ft.dwHighDateTime;
  epoch_ui.LowPart = unixEpochFileTime.dwLowDateTime;
  epoch_ui.HighPart = unixEpochFileTime.dwHighDateTime;
  return (1.0e-7 * (ui.QuadPart - epoch_ui.QuadPart));
#define ETIME_RETURNED
  /* MS_WINDOWS_API */
#endif

#else /* HAVE_CONFIG_H  */


#ifdef VXWORKS
#ifdef USE_CLOCK_GETTIME
  double retval;
  struct timespec ts;
#endif
#ifdef USE_GLOBAL_TIMER
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (global_timer_available)
    {
      return (((double) get_Global_time ()) * (1E-6));
    }
#endif

#ifdef USE_CLOCK_GETTIME
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (clock_gettime (CLOCK_REALTIME, &ts))
    {
      /* rcs_print_error("clock_gettime: errno = %d %s\n", errno, strerror(errno)); */
    }
  retval = ((double) ts.tv_sec) + ((double) ts.tv_nsec) * 1e-9;
  return retval;
#else
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  return ((double) tickGet ()) / ((double) sysClkRateGet ());
#endif
#define ETIME_RETURNED
#endif

#if defined (sunos4) || defined (LYNX) || defined(irix5)
  double retval = 0.0;
  struct timeval tp;

  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (gettimeofday (&tp, NULL) == -1)
    {
      rcs_print_error ("gettimeofday failed: (errno = %d) %s\n", errno,
		       strerror (errno));
      return 0.0;
    }
  retval = ((double) tp.tv_sec) + (((double) tp.tv_usec) / 1000000.0);
  return (retval);
#define ETIME_RETURNED
#endif

#if defined(sunos5) || (defined(os5) && defined(sparc))  || defined(irix6) || defined(irix64)
  double retval;
  struct timespec ts;

  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (clock_gettime (CLOCK_REALTIME, &ts))
    {
      rcs_print_error ("clock_gettime: errno = %d %s\n", errno,
		       strerror (errno));
    }
  retval = ((double) ts.tv_sec) + ((double) ts.tv_nsec) * 1e-9;
  return retval;
#define ETIME_RETURNED
#endif

#if defined(linux) || defined(darwin) || defined(qnx)  || defined(IRIX)

  struct timeval tp;
  double retval;

  if (0 != gettimeofday (&tp, NULL))
    {
      rcs_print_error ("etime: can't get time\n");
      return 0.0;
    }

  retval = ((double) tp.tv_sec) + ((double) tp.tv_usec) / 1000000.0;
  return retval;

#define ETIME_RETURNED
#endif /* linux */

#ifdef MS_WINDOWS_API
#ifdef USE_TOOL_HELP
/* The TimerCount function is more accurate than the GetTickCount() function
but you need toolhelp.dll which comes with Windows 3.1 but not Windows 3.0 or
Windows NT */
  TIMERINFO timer_info;
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  if (TimerCount (&timer_info))
    {
      return ((double) timer_info.dwmsSinceStart / 1000.0);
    }
  else
    {
      rcs_print_error ("etime: TimerCount returned false.\n");
    }
  return (0);
#define ETIME_RETURNED

  /* USE_TOOL_HELP */
#else

  BOOL frequency_ok = 0;
  BOOL counter_ok = 0;
  LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  double dfreq;
  double dcount;

  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  frequency_ok = QueryPerformanceFrequency (&frequency);
  counter_ok = QueryPerformanceCounter (&counter);
  if (frequency_ok && counter_ok)
    {
      dfreq = frequency.HighPart * TWO_TO_THIRTYTWO + frequency.LowPart;
      dcount = counter.HighPart * TWO_TO_THIRTYTWO + counter.LowPart;
      if (dfreq > 0.0 && dcount > 0.0)
	{
	  return (dcount / dfreq);
	}
    }
  return ((double) GetTickCount () / 1000.0);
#define ETIME_RETURNED

  /* USE_TOOL_HELP */
#endif 
  /* MS_WINDOWS_API */
#endif

#if  defined(__MSDOS__) && !defined(MS_WINDOWS_API)
#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND CLK_TCK
#endif
  if (etime_disabled)
    {
      return (etime_disable_time);
    }
  return clock () / CLOCKS_PER_SECOND;
#define ETIME_RETURNED

#endif /* __MSDOS__ */

#endif /* HAVE_CONFIG_H */

#ifndef ETIME_RETURNED
#error NO definition for etime for this platform
#endif
}

int esleep_use_yield = 0;

#if HAVE_CONFIG_H
#if ( defined(HAVE_SCHED_YIELD) || defined(HAVE_YIELD) )  && defined(HAVE_SELECT)
struct timeval last_esleep_tval;
#endif
#endif

double last_esleep_seconds_to_sleep=-199.99;

/* sleeps # of seconds */
void
esleep (double seconds_to_sleep)
{
#if HAVE_CONFIG_H

#if !defined(ESLEEP_RETURNED) && defined(MS_WINDOWS_API) && defined(HAVE_SLEEPEX)
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  SleepEx (((unsigned long) (seconds_to_sleep * 1000)), FALSE);
  return;
#define ESLEEP_RETURNED
#endif

#if ( ( defined(HAVE_SCHED_YIELD) || defined(HAVE_YIELD) )  && defined(HAVE_SELECT) ) && !defined(ESLEEP_RETURNED)

  struct timeval tval;
  static double clk_tck_val = 0;
  double total = seconds_to_sleep;	/* total sleep asked for */
  double started = etime ();	/* time when called */
  double left = total;

  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  if (clk_tck_val <= 0)
    {
      clk_tck_val = clk_tck ();
    }
  do
    {
      if (left < clk_tck_val && esleep_use_yield)
	{
#if HAVE_SCHED_YIELD
	  sched_yield ();
#else
	  yield ();
#endif
	}
      else
	{
	  tval.tv_sec = (long) left;	/* double->long truncates, ANSI */
	  tval.tv_usec = (long) ((left - (double) tval.tv_sec) * 1000000.0);
	  if (tval.tv_sec == 0 && tval.tv_usec == 0)
	    {
	      tval.tv_usec = 1;
	    }
	  last_esleep_tval = tval;
	  if (select (0, NULL, NULL, NULL, &tval) < 0)
	    {
	      if (errno != EINTR)
		{
		  break;
		}
	    }
	}
      left = total - etime () + started;
    }
  while (left > 0 && (left > clk_tck_val && esleep_use_yield));
  return;
#define ESLEEP_RETURNED
#endif

#if HAVE_NANOSLEEP && !defined(ESLEEP_RETURNED)
  struct timespec rqtp;
  rqtp.tv_sec = seconds_to_sleep;
  rqtp.tv_nsec = (seconds_to_sleep - rqtp.tv_sec) * 1E9;
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  if (nanosleep (&rqtp, NULL) < 0)
    {
      if (errno != EINTR)
	{
	  rcs_print_error
	    ("nanosleep({tv_sec=%d,tv_nsec=%ld}) error (errno=%d) %s\n",
	     (int)rqtp.tv_sec, rqtp.tv_nsec, errno, strerror (errno));
	}
    }
  return;
#define ESLEEP_RETURNED
#endif

#if HAVE_SLEEP && HAVE_USLEEP && !defined(ESLEEP_RETURNED)
  unsigned int whole_seconds = (int) (seconds_to_sleep);
  unsigned int useconds =
    (int) ((seconds_to_sleep - ((double) whole_seconds)) * 1e6);
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 1e-6)
    return;
  if (whole_seconds > 0)
    {
      sleep (whole_seconds);
    }
  if (useconds > 0)
    {
      usleep (useconds);
    }
  return;
#define ESLEEP_RETURNED
#endif

#else /* HAVE_CONFIG_H */

#ifdef VXWORKS
#if 1
  int t = (int) (seconds_to_sleep * (double) sysClkRateGet ());

  last_esleep_seconds_to_sleep = seconds_to_sleep;
  taskDelay (t <= 0 ? 1 : t);	/* at least sleep a tick */
#else
  struct timespec nantime;
  nantime.tv_sec = (time_t) seconds_to_sleep;
  nantime.tv_nsec = ((double) (seconds_to_sleep - nantime.tv_sec) * 1e9);
  return nanosleep (&nantime, NULL);
#endif
  return;
#define ESLEEP_RETURNED

#endif

#if defined (SUN) || defined (LYNX) || defined(irix5) || defined(irix6) || defined(irix64) || defined(linux) || defined(darwin) || defined(qnx) 
#if defined(sunos5) || defined(LINUX_KERNEL_2_2_OR_LATER)
  struct timeval tval;
  static double clk_tck_val = 0;
  double total = seconds_to_sleep;	/* total sleep asked for */
  double started = etime ();	/* time when called */
  double left = total;
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  if (clk_tck_val <= 0)
    {
      clk_tck_val = clk_tck ();
    }
  do
    {
      if (left < clk_tck_val && esleep_use_yield)
	{
#ifdef linux
	  sched_yield ();
#else
	  yield ();
#endif
	}
      else
	{
	  tval.tv_sec = (long) left;	/* double->long truncates, ANSI */
	  tval.tv_usec = (long) ((left - (double) tval.tv_sec) * 1000000.0);
	  if (tval.tv_sec == 0 && tval.tv_usec == 0)
	    {
	      tval.tv_usec = 1;
	    }
	  if (select (0, NULL, NULL, NULL, &tval) < 0)
	    {
	      if (errno != EINTR)
		{
		  break;
		}
	    }
	}
      left = total - etime () + started;
    }
  while (left > 0 && (left > clk_tck_val && esleep_use_yield));
  return;
#define ESLEEP_RETURNED


  /* defined(sunos5) || defined(LINUX_KERNEL_2_2_OR_LATER) */
#else
  unsigned int whole_seconds = (int) (seconds_to_sleep);
  unsigned int useconds =
    (int) ((seconds_to_sleep - ((double) whole_seconds)) * 1e6);
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 1e-6)
    return;
  if (whole_seconds > 0)
    {
      sleep (whole_seconds);
    }
  if (useconds > 0)
    {
      usleep (useconds);
    }
  return;
#define ESLEEP_RETURNED

  /* defined(sunos5) || defined(LINUX_KERNEL_2_2_OR_LATER) */
#endif

#if 0
  struct timespec rqtp;
  rqtp.tv_sec = seconds_to_sleep;
  rqtp.tv_nsec = seconds_to_sleep * 1E9;

  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  if (nanosleep (&rqtp, NULL) < 0)
    {
      rcs_print_error ("nanosleep error %d %s\n", errno, strerror (errno));
    }
  return;
#define ESLEEP_RETURNED

  /* 0 */
#endif

  /*  defined (SUN) || defined (LYNX) || defined(irix5) || defined(irix6) || defined(irix64) || defined(linux) || defined(darwin) || defined(qnx)  */
#endif

#ifdef MS_WINDOWS_API
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  SleepEx (((unsigned long) (seconds_to_sleep * 1000)), FALSE);
  return;
#define ESLEEP_RETURNED

#else
#if defined(__MSDOS__)
  last_esleep_seconds_to_sleep = seconds_to_sleep;
  if (seconds_to_sleep <= 0.0)
    return;
  delay ((unsigned int) (seconds_to_sleep * 1000.0));
  return;
#define ESLEEP_RETURNED

  /* __MSDOS__ */
#endif

  /* MS_WINDOWS_API */
#endif

#endif /* HAVE_CONFIG_H */

#ifndef ESLEEP_RETURNED
#error No definition for esleep for this platform.
#endif
}



#ifndef NO_STDIO
void
print_etime ()
{
  printf ("etime = %f\n", etime ());
}
#endif

#ifdef VXWORKS
/*
 * Floating point support seems to be flaking out, isolate
 * some of it here for testing.
 */
int
covertSecondsToTicks (double secs)
{
  int ticks = 0;

  /* If the timeout is less than zero, wait forever,
     if it equals zero don't wait otherwise convert the timeout,
     from seconds to ticks.
   */
  if (secs < 0)
    {
      ticks = WAIT_FOREVER;
    }
  else
    {
      ticks = (int) (secs * sysClkRateGet ());
    }

  return ticks;
}

#endif
