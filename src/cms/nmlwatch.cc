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


#include "nmlwatch.h"
#include "rcs.hh"		/* rcs_print(), class NML, class RCS_TIMER */

extern "C"
{
#include <stdio.h>		/* printf() */
#include <string.h>		/* */
#include <stdlib.h>		// strtod()
#include <signal.h>		/* signal(), SIGINT */
}

static unsigned char stop = 1;

#if (!defined(VXWORKS) && !defined(_Windows)) || defined(__CONSOLE__) || defined(gnuwin32)
int
main (int argc, char **argv)
{
  static char config_file_buf[256];
  static char buffer_name_buf[256];
  static char sample_period_buf[40];
  static char *config_file = NULL;
  static char *buffer_name = NULL;
  double sample_period = -1;

  if (argc > 1)
    {
      config_file = argv[1];
    }
  if (argc > 2)
    {
      buffer_name = argv[2];
    }
  if (argc > 3)
    {
      sample_period = strtod (argv[3], NULL);
    }

  if (NULL == config_file)
    {
      config_file = config_file_buf;
      printf ("Configuration File?\n");
      fgets (config_file_buf, 256, stdin);
    }
  if (NULL == buffer_name)
    {
      buffer_name = buffer_name_buf;
      printf ("Buffer Name?\n");
      fgets (buffer_name_buf, 256, stdin);
    }
  if (sample_period < 0)
    {
      printf ("Sample Period?\n");
      fgets (sample_period_buf, 256, stdin);
      sample_period = strtod (sample_period_buf, NULL);
    }
  nml_watch (config_file, buffer_name, sample_period);
}
#endif

void
stop_watching_nml (int sig)
{
  stop = 1;
}

int
format_stub (NMLTYPE type, void *buffer, CMS * cms)
{
  return (0);
}

int
nml_watch (char *config_file, char *buffer, double sample_period)
{
  NML *nml;
  RCS_TIMER *timer;
  NMLmsg *nmlmsg;
  NMLTYPE peek_ret;
  double start_time;
  rcs_print ("NMLWATCH(%s,%s,%lf) called.\n",
	     config_file, buffer, sample_period);
  timer = new RCS_TIMER (sample_period);
  stop = 0;
  nml = new NML (format_stub, buffer, "nmlwatch", config_file);
  if (NULL == nml)
    {
      rcs_print_error ("NMLWATCH: Can't create nml channel.\n");
      delete timer;
      return -1;
    }
  if (!nml->valid ())
    {
      rcs_print_error ("NMLWATCH: Can't create nml channel.\n");
      delete nml;
      delete timer;
      return -1;
    }
  signal (SIGINT, stop_watching_nml);
  rcs_print ("Waiting for messages.\n");
  start_time = etime ();
  while (!stop)
    {
      peek_ret = nml->peek ();
      switch (peek_ret)
	{
	case 0:
	  break;

	case -1:
	  rcs_print_error ("NMLWATCH: Can`t read nml channel.\n");
	  delete nml;
	  delete timer;
	  return (-1);

	default:
	  nmlmsg = nml->get_address ();
	  rcs_print ("NML message %ld of size %ld  recieved at t=%lf.\n",
		     nmlmsg->type, nmlmsg->size, etime () - start_time);
	  break;
	}
      timer->wait ();
    }
  rcs_print ("NMLWATCH is quitting.\n");
  delete nml;
  delete timer;
  return (0);
}
