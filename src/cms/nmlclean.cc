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
/* ident tag */
#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(x)
#endif
#endif

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else

#include "rcs_defs.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#endif

static char __attribute__((unused)) program_id[] = __FILE__ "version 1.2 compiled on" __DATE__;

#include "nml.hh"
#include "cms.hh"
#include "cms_cfg.hh"
#include "rcs_prnt.hh"
#include "timer.hh"

#ifdef VXWORKS
extern "C" void nmlclean (char *config_file);

void
nmlclean (char *config_file)
#else
int
main (int argc, char **argv)
#endif
{

  FILE *fp;
  char proc_line[CMS_CONFIG_LINELEN];
  char line[CMS_CONFIG_LINELEN];
  int line_len, line_number = 0;
  char buffer_name[CMS_CONFIG_LINELEN];

#ifndef DISABLE_RCS_PRINT
  set_rcs_print_destination (RCS_PRINT_TO_FILE);
  set_rcs_print_file("nmlclean.log");
  set_abort_on_rcs_error(false);
  set_pause_on_rcs_error(false);
  //  set_rcs_print_flag(PRINT_EVERYTHING);
  //  max_rcs_errors_to_print=-1;
#endif

#ifdef DEBUG_MEMORY
  enable_debug_memory();
#endif

#ifndef VXWORKS
  char config_file[80];
  if (argc > 1)
    {
      /* Get host_name and config_file from command line */
      strcpy (config_file, argv[1]);
    }
  else
    {
      /* Prompt user for config_file and host name */
      printf ("Configuration File? ");
      if(!fgets (config_file, 80, stdin))
	{
	  exit(1);
	}
    }
#endif

  if(getenv("NMLCLEAN_LOCAL_ONLY") == 0)
    {
      /* Open the configuration file. */
      if ((fp = fopen (config_file, "r")) == NULL)
	{
	  rcs_print_error ("nmlclean: can't open '%s'\n", config_file);
	  exit (-1);
	}


      int rbuf_count=0;

      /* Read the configuration file line by line to find buffers. */
      while (!feof (fp))
	{
	  NML *rc_nml = (NML *) NULL;

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
	  //      printf("%s\n",line);

	  if (line[0] != 'B')
	    {
	      continue;
	    }

	  rbuf_count++;
	  sscanf (line + 1, "%s ", buffer_name);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(proc_line,sizeof(proc_line)),
			  "P nmlclean %s REMOTE anyhost  R 0 5 0 0 STOPONCONNREF CLEANING_FLAG\n",
			  buffer_name);
	  rc_nml = new NML (line, proc_line);
	  rcs_print("nmlclean(REMOTE):  %s\n", buffer_name);
	  rc_nml->print_info();
	  if (NULL != rc_nml)
	    {
	      rc_nml->ignore_format_chain = 1;
	      if (NULL != rc_nml->cms)
		{
		  rc_nml->cms->delete_totally = true;
		}
	      delete rc_nml;
	      rc_nml = (NML *) NULL;
	    }
	  esleep(0.02);
	}
      fclose(fp);
      esleep(0.5);
    }


  /* Open the configuration file. */
  if ((fp = fopen (config_file, "r")) == NULL)
    {
      rcs_print_error ("nmlclean: can't open '%s'\n", config_file);
      exit (-1);
    }

  int lbuf_count=0;

  while (!feof (fp))
    {
      NML *lc_nml = (NML *) NULL;

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
      lbuf_count++;
      sscanf (line + 1, "%s ", buffer_name);
      SNPRINTF_FUNC (SNPRINTF_ARGS(proc_line,sizeof(proc_line)),
		     "P nmlclean %s LOCAL anyhost R 0 5 0 0 CLEANING_FLAG\n",
		     buffer_name);
      lc_nml = new NML (line, proc_line);
      rcs_print("nmlclean(LOCAL):  %s\n", buffer_name);
      lc_nml->print_info();
      if (NULL != lc_nml)
	{
	  lc_nml->ignore_format_chain = 1;
	  if (NULL != lc_nml->cms)
	    {
	      lc_nml->cms->delete_totally = true;
	    }
	  delete lc_nml;
	  lc_nml = (NML *) NULL;
	}
      esleep(0.02);
    }
  fclose(fp);
  exit (0);
}
