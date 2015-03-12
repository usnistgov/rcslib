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


#include "_timer.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

static want_to_quit_tesleep = 0;

static void
quit_tesleep (int sig)
{
  want_to_quit_tesleep = 1;
}
main (int argc, char **argv)
{
  double last_time;
  double cycle_time;
  double current_time;
  double max_time = 0.0;
  double min_time = 1e9;
  double start_time;
  double stop_time;
  int num_cycles;
  double total_time;
  int i;

  if (argc > 1)
    {
      cycle_time = atof (argv[1]);
    }
  else
    {
      cycle_time = clk_tck ();
    }

  if (argc > 2)
    {
      num_cycles = strtol (argv[2], NULL, 0);
    }
  else
    {
      num_cycles = 10000;
    }

  signal (SIGINT, quit_tesleep);
  printf ("cycle_time = %f\n", cycle_time);
  printf ("num_cycles = %d\n", num_cycles);
  fflush (stdout);
  esleep (cycle_time);

  start_time = last_time = etime ();
  for (i = 0; i < num_cycles && !want_to_quit_tesleep; i++)
    {
      esleep (cycle_time);
      current_time = etime ();
      if (current_time - last_time > max_time)
	{
	  max_time = (current_time - last_time);
	}
      if (current_time - last_time < min_time)
	{
	  min_time = (current_time - last_time);
	}
      last_time = current_time;
    }
  stop_time = etime ();
  total_time = stop_time - start_time;
  printf ("num_cycles = %d\n", i);
  printf ("total time = %f\n", total_time);
  printf ("avg cycle time = %f\n", (total_time) / i);
  printf ("min cycle time = %f\n", min_time);
  printf ("max cycle time = %f\n", max_time);

}
