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

/* Program Name to print at startup. */
static char program_id[] = "perfsvr version 1.7 compiled on " __DATE__;

#include "rcs_defs.hh"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdio.h>		/* printf() */
#include <stdlib.h>		/* exit() */
#include <string.h>		/* strcmp() */

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#include "cms_cfg.hh"		/* CMS_CONFIG_LINELEN */
#include "nml.hh"		/* NML, NMLTYPE */
#include "nml_srv.hh"		/* run_nml_servers() */
#include "nmlmsg.hh"		/* NMLmsg */
#include "cms.hh"		/* CMS  */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "timer.hh"		/* etime() */
#include "perftype.hh"		/* perf_types_format() */

#ifdef VXWORKS
#include "vxWorks.h"
#include "taskLib.h"
#endif

#define MAX_NML_CHANNELS 100
char *default_perfsvr_config_file = "test.nml";
char *default_perfsvr_host_name = "localhost";



#ifdef VXWORKS
extern "C" int killperfsvr ();
int perfsvr_tid = 0;


int
killperfsvr ()
{
  if (perfsvr_tid > 0)
    {
      kill (perfsvr_tid, 2);
    }
}
#endif


extern "C" int run_perfsvr (char *config_file, char *host_name);

#ifdef VXWORKS
extern "C" void perfsvr (char *config_file, char *host_name,
			 int print_everything);

void
perfsvr (char *config_file, char *host_name, int print_everything)
#else
int
main (int argc, char **argv)
#endif
{


  //set_rcs_print_destination(RCS_PRINT_TO_STDOUT);
  //set_rcs_print_flag(PRINT_EVERYTHING);

  /* Print File name, version, and compile date. */
  rcs_puts (program_id);
  rcs_errors_printed = 0;
  if (max_rcs_errors_to_print < 40)
    {
      max_rcs_errors_to_print = 40;
    }


#ifndef VXWORKS
  char config_file[80];
  char host_name[80];
  printf ("PERFSVR:\n");
  if (argc > 2)
    {
      /* Get host_name and config_file from command line */
      strcpy (config_file, argv[1]);
      strcpy (host_name, argv[2]);
    }
  else
    {
      /* Prompt user for config_file and host name */
      printf ("Configuration File? [%s]", default_perfsvr_config_file);
      fgets (config_file, 80, stdin);
      if (config_file[0] == 0 || config_file[0] == '\r'
	  || config_file[0] == '\n')
	{
	  strncpy (config_file, default_perfsvr_config_file, 80);
	}

      printf ("Host Name? [%s]", default_perfsvr_host_name);
      fgets (host_name, 80, stdin);
      if (host_name[0] == 0 || host_name[0] == '\r' || host_name[0] == '\n')
	{
	  strncpy (host_name, default_perfsvr_host_name, 80);
	}
    }


  printf ("print everything ? (y/n) ");
  char print_everything[16];
  fgets (print_everything, 16, stdin);
  if (print_everything[0] == 'y' || print_everything[0] == 'Y')
    {
      set_rcs_print_flag (PRINT_EVERYTHING);
    }
  run_perfsvr (config_file, host_name);
#else

  if (config_file == NULL)
    {
      config_file = default_perfsvr_config_file;
    }
  if (host_name == NULL)
    {
      host_name = default_perfsvr_host_name;
    }

  if (print_everything == 1)
    {
      set_rcs_print_flag (PRINT_EVERYTHING);
    }
  perfsvr_tid =
    taskSpawn ("tperfsvr", 100, VX_FP_TASK, 0x4000, (FUNCPTR) run_perfsvr,
	       (int)config_file, (int)host_name, 0, 0, 0, 0, 0, 0, 0, 0);
#endif
}

int
run_perfsvr (char *config_file, char *host_name)
{
  FILE *fp;
  char line[CMS_CONFIG_LINELEN];
  int line_len, line_number = 0;
  char buffer_name[CMS_CONFIG_LINELEN];
  char buffer_host[CMS_CONFIG_LINELEN];
  NML *nml_channel[MAX_NML_CHANNELS];
  int num_nml_channels;

  /* Open the configuration file. */
  if ((fp = fopen (config_file, "r")) == NULL)
    {
      rcs_print_error ("nmlperf: Can't open '%s'\n", config_file);
      exit (-1);
    }

  /* Read the configuration file line by line to find buffers. */
  num_nml_channels = 0;
  while (!feof (fp) && num_nml_channels < MAX_NML_CHANNELS)
    {
      if ((fgets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	{
	  break;
	}

      line_number++;
      line_len = strlen (line);
      if (line_len > CMS_CONFIG_LINELEN)
	{
	  rcs_print_error
	    ("cms_cfg: Line length of line number %d in %s exceeds max length of %d",
	     line_number, config_file, CMS_CONFIG_LINELEN);
	}

      if (line_len > 1)
	{
	  if (line[line_len - 2] == '\\')
	    {
	      if ((fgets
		   (line + line_len - 2, CMS_CONFIG_LINELEN - line_len,
		    fp)) == NULL)
		{
		  break;
		}
	      line_number++;
	    }
	}

      if (line[0] != 'B')
	{
	  continue;
	}

      sscanf (line + 1, "%s %*s %s", buffer_name, buffer_host);

      /* Try to connect locally if the buffer host matches this one. */

      if (!strcmp (buffer_host, host_name))
	{
	  printf ("\nAttempting to connect to %s locally . . .\n",
		  buffer_name);
	  nml_channel[num_nml_channels] =
	    new NML (perf_types_format, buffer_name, "perfsvr", config_file);
	  num_nml_channels++;
	}
    }

  if (NULL != fp)
    {
      fclose (fp);
      fp = NULL;
    }

  printf ("Press Control-C to stop server(s).\n");
  run_nml_servers ();
  return 0;
}
