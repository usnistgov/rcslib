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


#include "timer.hh"


#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>


extern "C" void quittimetest ();

int quit_timetest_flag = 0;

static void
quittimetest (int sig)
{
  quit_timetest_flag = 1;
}

double period = 0.001;

#define MAX_HIST 10000
int hist[MAX_HIST];

#ifdef VXWORKS
extern "C" int timetest ();

int
timetest ()
#else
int
main (int argc, char **argv)
#endif
{

  int count = 0;
  int hist_index;
  int lo_hist_index = MAX_HIST;
  int hi_hist_index = 0;
  double start_time, end_time;
  double avg_time, total_time;
  double min_time = 1E99;
  double max_time = -1E99;
  double last_time;

  if (argc > 1)
    {
      period = strtod (argv[1], 0);
    }
  signal (SIGINT, quittimetest);
  start_time = etime ();
  last_time = start_time;

  for (int i = 0; i < MAX_HIST; i++)
    {
      hist[i] = 0;
    }

  last_time = etime ();
  esleep (period);
  while (!quit_timetest_flag)
    {
      count++;
      double cur_time = etime ();
      double diff = (cur_time - last_time);
      if (min_time > diff)
	{
	  min_time = diff;
	}
      if (max_time < diff)
	{
	  max_time = diff;
	}
      hist_index = ((int) (sqrt (MAX_HIST) * diff / period));
      if (hist_index < 0)
	{
	  hist_index = 0;
	}
      if (hist_index > MAX_HIST - 1)
	{
	  hist_index = MAX_HIST - 1;
	}
      if (hi_hist_index < hist_index)
	{
	  hi_hist_index = hist_index;
	}
      if (lo_hist_index > hist_index)
	{
	  lo_hist_index = hist_index;
	}
      hist[hist_index]++;
      if (count % ((int) (5.0 / period)) == 0 || period > 5.0)
	{
	  total_time = (cur_time - start_time);
	  avg_time = count > 0 ? total_time / count : -1;
	  printf
	    ("count: %d min_time:%f max_time:%f total_time:%f avg_time: %f\n",
	     count, min_time, max_time, total_time, avg_time);
	}
      last_time = cur_time;
      esleep (period);
    }

  end_time = etime ();
  total_time = end_time - start_time;
  avg_time = count > 0 ? total_time / count : -1;
  printf ("count: %d min_time:%f max_time:%f total_time:%f avg_time: %f\n",
	  count, min_time, max_time, total_time, avg_time);
  printf ("\n\n");
  printf ("time:%f\n", total_time);
  printf ("count:%d\n", count);
  if (count > 0)
    {
      printf ("period:%f\n", total_time / count);
    }
  if (total_time > 0.0)
    {
      printf ("frequency:%f\n", count / total_time);
    }

  printf ("\n\nHistogram:\n");
  printf ("Period    \tCount  \tPercent\n");
  for (int i = lo_hist_index; i <= hi_hist_index; i++)
    {
      if (i > lo_hist_index + 2 && i < hi_hist_index - 1 && hist[i] == 0
	  && hist[i - 1] == 0 && hist[i + 1] == 0)
	{
	  if (hist[i - 2] != 0)
	    {
	      printf (" . . . \n");
	    }
	  continue;
	}
      printf ("%6.6f \t%6.6d \t%6.6f%\n", period / sqrt (MAX_HIST) * i,
	      hist[i], 100 * ((double) hist[i] / count));;
    }
  printf ("\n");


#ifdef VXWORKS
  return (0);
#endif
}
