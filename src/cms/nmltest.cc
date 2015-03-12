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
static char program_id[] = "nmltest version 3.0 compiled on " __DATE__;

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else

#include "rcs_defs.hh"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "inetfile.hh"
#include "sokintrf.h"
#include "nml.hh"
#include "cms.hh"
#include "cms_cfg.hh"
#include "rcs_prnt.hh"
#include "rcsvers.hh"
#include "linklist.hh"
#include "cmd_msg.hh"
#include "stat_msg.hh"
#include "_timer.h"

#ifdef ENABLE_RCS_DIAG
#include "nmldiag.hh"
#endif


extern "C" {
extern void adainit(void);
}

enum LOCAL_CONNECT
{
  LC_NOT_ATTEMPTED = 0,
  LC_FAILED,
  LC_SUCCEEDED
};

char LC_ARRAY[3][3] = { "NA", "F", "S" };

enum REMOTE_CONNECT
{
  RC_NOT_ATTEMPTED = 0,
  RC_FAILED,
  RC_SUCCEEDED
};

char RC_ARRAY[3][3] = { "NA", "F", "S" };

static bool force_local=false;
static bool force_remote=false;
static bool local_only=false;
static bool remote_only=false;

int
nmltest_format (NMLTYPE, void *,CMS *)
{
  rcs_print ("nmltest_format called.\n");
  return (0);
}

void
clean_string (char *str, int len)
{
  if (NULL == str)
    {
      return;
    }
  if (len > 1024)
    {
      return;
    }
  str[len - 1] = 0;
  char *temp = (char *) malloc (len);
  memset (temp, 0, len);
  char *t = temp;
  char *s = str;
  char c = *s;
  while (c != 0)
    {
      if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
	{
	  *t = c;
	  t++;
	}
      s++;
      c = *s;
    }
  strcpy (str, temp);
  free (temp);
}



#ifdef VXWORKS
int test_all = 0;
extern "C" void 
nmltest (char *config_file = NULL,
	 char *host_name = NULL,
	 int master = 0,
	 int wait_for_read = 0,
	 int test_all = 0,
	 int use_passwd = 0, 
	 int show_diag_info = 0);

void
nmltest (char *config_file,
	 char *host_name,
	 int master,
	 int wait_for_read, 
	 int test_all, 
	 int use_passwd, 
	 int show_diag_info)
#else
int
main (int argc, char **argv)
#endif
{
  int i=0;
  INET_FILE *fp=0;
  LOCAL_CONNECT local_connect_status = LC_NOT_ATTEMPTED;
  REMOTE_CONNECT remote_connect_status = RC_NOT_ATTEMPTED;
  char proc_line[CMS_CONFIG_LINELEN];
  char line[CMS_CONFIG_LINELEN];
  char output_line[80];
  char buffer_name[CMS_CONFIG_LINELEN];
  char buffer_host[CMS_CONFIG_LINELEN];
  NMLTYPE lc_msg_type = 0;
  NMLTYPE rc_msg_type = 0;
  int msg_size = 0;
  int line_len, line_number = 0;
  RCS_LINKED_LIST *output_list=0;

  memset(proc_line,0,sizeof(proc_line));
  memset(line,0,sizeof(line));
  memset(output_line,0,sizeof(output_line));
  memset(buffer_name,0,sizeof(buffer_name));
  memset(buffer_host,0,sizeof(buffer_host));

  force_local = (getenv("FORCE_LOCAL") != 0);
  force_remote = (getenv("FORCE_REMOTE") != 0);
  local_only = (getenv("LOCAL_ONLY") != 0);
  remote_only = (getenv("REMOTE_ONLY") != 0);


#if HAVE_ADAINIT
  adainit();
#endif

  /* Print the program name and version info. */
  rcs_print ("\n***** NML Test Program **************************\n");
  rcs_print (" This program can be used to test whether all buffers\n");
  rcs_print (" in a configuration file can be connected to or it can test\n");
  rcs_print (" only one buffer/process connection.\n");
  rcs_print
    (" It is best to use a version of nmltest using the same version\n");
  rcs_print (" of the RCS library your application(s) use.\n");
  rcs_print ("Just press enter when prompted to accept default.\n");
  print_rcs_version ();
  rcs_print ("%s\n",program_id);
  rcs_print ("\n\n");

  rcs_print ("%s usage:\n",argv[0]);
  rcs_print ("\t\t <nml_config_file> --buf <buffer_name> --proc <proc_name>\n");
  rcs_print ("\t\t OR\n");
  rcs_print ("\t\t <nml_config_file> --test_all --host <host_name>\n");
  rcs_print ("\n");
  rcs_print ("Environment variables that can be set:\n");
  rcs_print ("\texport NMLTEST_USE_PASSWD=1; # Query for passwd\n");
  rcs_print ("\texport NMLTEST_DUMP_HEX_DATA=1; # Dump buffer contents in hex\n");
  rcs_print ("\n");


  output_list = new RCS_LINKED_LIST;
  if (NULL == output_list)
    {
      printf ("Couldn`t malloc output list.\n");
      exit (-1);
    }

  set_rcs_print_destination (RCS_PRINT_TO_STDOUT);
#ifndef VXWORKS
  char config_file[80];
  memset(config_file,0,sizeof(config_file));
  strcpy (config_file, "UNDEFINED");
  char host_name[80];
  memset(host_name,0,sizeof(host_name));
  strcpy (host_name, "UNDEFINED");
  char proc_name[80];
  memset(proc_name,0,sizeof(proc_name));
  strcpy (proc_name, "nmltest");
  int master = -1;
  int wait_for_read = -1;
  int test_all = -1;
#ifdef ENABLE_RCS_DIAG
  int show_diag_info = -1;
#endif
  int use_passwd = 0;
  i=0;

  strncpy(proc_name,"nmltest",sizeof(proc_name));

  if(argc == 2 && strstr(argv[1],".nml") && argv[1][0] != '-')
    {
      strcpy (config_file, argv[1]);
      test_all =1;
      master = 0;
      wait_for_read=0;
      memset(buffer_name,0,sizeof(buffer_name));
      memset(proc_name,0,sizeof(proc_name));
      strcpy(host_name,"localhost");
    }
  else if(argc < 2  && getenv("CONFIG_NML"))
    {
      strcpy (config_file, getenv("CONFIG_NML"));
      test_all =1;
      master = 0;
      wait_for_read=0;
      memset(buffer_name,0,sizeof(buffer_name));
      memset(proc_name,0,sizeof(proc_name));
      strcpy(host_name,"localhost");
    }
  else
    {
      if(argc > 1)
	{
	  strcpy (config_file, argv[1]);
	}
      if(argc > 3 && !strcmp(argv[2],"--buf"))
	{
	  test_all = 0;
	  strcpy(buffer_name,argv[3]);
	  if(argc > 5 && !strcmp(argv[4],"--proc"))
	    {
	      strcpy(proc_name,argv[5]);
	    }
	}
      else if(argc > 2 && !strcmp(argv[2],"--test_all"))
	{
	  test_all = 1;
	  if(argc > 4 && !strcmp(argv[3],"--host"))
	    {
	      strcpy(host_name,argv[4]);
	    }
	}
      else
	{
	  memset(buffer_name,0,sizeof(buffer_name));
	  memset(proc_name,0,sizeof(proc_name));
	  if(argc > 2)
	    {
	      strcpy (host_name, argv[2]);
	    }
      
	  if (argc > 3)
	    {
	      /* Get host_name and config_file from command line */
	      master = (argv[3][0] == 'y' || argv[3][0] == 'Y');
	    }
	  if (argc > 4)
	    {
	      wait_for_read = (argv[4][0] == 'y' || argv[4][0] == 'Y');
	    }
	}
    }
#endif

  /* Prompt user for config_file and host name */
#ifdef VXWORKS
  if (NULL == config_file)
    {
      config_file = (char *) malloc (80);
      printf ("Configuration File? ");
      if(!fgets (config_file, 80, stdin))
	{
	  return;
	}
#else
  if (!strcmp (config_file, "UNDEFINED"))
    {
      printf ("Configuration File? ");
      if(!fgets (config_file, 80, stdin))
	{
	  exit(1);
	}
    }
#endif
  clean_string (config_file, 80);


#ifndef VXWORKS
  if (-1 == test_all)
    {
      char test_all_str[80];
      memset (test_all_str, 0, 80);
      printf ("Test all buffers in the file? [yn] (default = No) ");
      if(!fgets (test_all_str, 80, stdin))
	{
	  exit(1);
	}
      test_all = (test_all_str[0] == 'y' || test_all_str[0] == 'Y');
    }
#endif

#ifdef ENABLE_RCS_NMLMOD
  int cmd_type = 0;
  int stat_type = 0;
#endif
  int dump_hex_data = (getenv("NMLTEST_DUMP_HEX_DATA") != 0);
  if (test_all)
    {
#ifdef VXWORKS
      if (NULL == host_name)
	{
	  host_name = (char *) malloc (80);
	  printf ("Host Name? ");
	  if(!fgets (host_name, 80, stdin)) 
	    {
	      return;
	    }
	  clean_string (host_name, 80);
	}
#else
      if (!strcmp (host_name, "UNDEFINED"))
	{
	  printf ("Host Name? ");
	  if(!fgets (host_name, 80, stdin))
	    {
	      exit(1);
	    }
	  clean_string (host_name, 80);
	}
#endif

#ifdef ENABLE_RCS_DIAG
      if (-1 == show_diag_info)
	{
	  char show_diag_info_str[80];
	  memset (show_diag_info_str, 0, 80);
	  printf ("Show Diagnostics Info? [yn] (default = No)  ");
	  if(!fgets (show_diag_info_str, 80, stdin))
	    {
	      exit(1);
	    }
	  show_diag_info = (show_diag_info_str[0] == 'y'
			    || show_diag_info_str[0] == 'Y');
	}
#endif

      if (-1 == master)
	{
	  char master_str[80];
	  memset (master_str, 0, 80);
	  printf ("Master? [yn] (default = No)  ");
	  if(!fgets (master_str, 80, stdin))
	    {
	      exit(1);
	    }
	  master = (master_str[0] == 'y' || master_str[0] == 'Y');
	}

      if (-1 == wait_for_read)
	{
	  char wait_for_read_str[80];
	  memset (wait_for_read_str, 0, 80);
	  printf ("Wait for read? [yn] (default = No) ");
	  if(!fgets (wait_for_read_str, 80, stdin))
	    {
	      exit(1);
	    }
	  wait_for_read = (wait_for_read_str[0] == 'y'
			   || wait_for_read_str[0] == 'Y');
	}

    }
  else
    {
      if(!buffer_name[0])
	{
	  printf ("Buffer Name? ");
	  if(!fgets (buffer_name, 80, stdin))
	    {
	      exit(1);
	    }
	  clean_string (buffer_name, 80);

	  char dump_hex_data_str[80];
	  memset (dump_hex_data_str, 0, 80);
	  printf ("Dump Hex Data ? [yn] (default = No)  ");
	  if(!fgets (dump_hex_data_str, 80, stdin))
	    {
	      exit(1);
	    }
	  dump_hex_data = (dump_hex_data_str[0] == 'y'
			   || dump_hex_data_str[0] == 'Y');

	}
      while(!proc_name[0] || proc_name[0] == '\r' || proc_name[0] == '\n')
	{
	  printf ("Process Name? ");
	  if(!fgets (proc_name, 80, stdin))
	    {
	      exit(1);
	    }
	  clean_string (proc_name, 80);
	}
      char cmd_type_str[80];
      memset (cmd_type_str, 0, 80);
#ifdef ENABLE_RCS_NMLMOD
      printf ("RCS_CMD_CHANNEL ? [yn] (default = No)  ");
      if(!fgets (cmd_type_str, 80, stdin)) 
	{
	  exit(1);
	}
      cmd_type = (cmd_type_str[0] == 'y' || cmd_type_str[0] == 'Y');
      char stat_type_str[80];
      memset (stat_type_str, 0, 80);
      printf ("RCS_STAT_CHANNEL ? [yn] (default = No)  ");
      if(!fgets (stat_type_str, 80, stdin))
	{
	  exit(1);
	}
      stat_type = (stat_type_str[0] == 'y' || stat_type_str[0] == 'Y');
#endif

#ifdef ENABLE_RCS_DIAG
      if (-1 == show_diag_info)
	{
	  char show_diag_info_str[80];
	  memset (show_diag_info_str, 0, 80);
	  printf ("Show Diagnostics Info? [yn] (default = No)  ");
	  if(!fgets (show_diag_info_str, 80, stdin))
	    {
	      exit(1);
	    }
	  show_diag_info = (show_diag_info_str[0] == 'y'
			    || show_diag_info_str[0] == 'Y');
	}
#endif

    }


#ifndef VXWORKS
  use_passwd = (getenv("NMLTEST_USE_PASSWD") != 0);
#endif

  char login[16];
  char passwd[16];

  memset (login, 0, 16);
  memset (passwd, 0, 16);

  if (use_passwd)
    {
      printf ("login:");
      if(!fgets (login, 16, stdin)) 
	{
	  exit(1);
	}
      clean_string (login, 16);
      printf ("passwd:");
      if(!fgets (passwd, 16, stdin))
	{
	  exit(1);
	}
      clean_string (passwd, 16);
    }

  if (!test_all)
    {
      NML *nml = NULL;
      rcs_print ("Calling constructor . . .\n");
#ifdef ENABLE_RCS_NMLMOD
      if (cmd_type)
	{
	  nml =
	    new RCS_CMD_CHANNEL (nmltest_format, buffer_name, proc_name,
				 config_file, -1);
	}
      else if (stat_type)
	{
	  nml =
	    new RCS_STAT_CHANNEL (nmltest_format, buffer_name, proc_name,
				  config_file, -1);
	}
      else
	{
	  nml =
	    new NML (nmltest_format, buffer_name, proc_name, config_file, -1,
		     0);
	}
#else
      nml =
	new NML (nmltest_format, buffer_name, proc_name, config_file, -1, 0);
#endif
      rcs_print ("Constructor returned %p.\n", (void*)nml);
      if(nml && nml->cms)
	{
	  rcs_print ("nml->cms->BufferLine=%s.\n", nml->cms->BufferLine);
	  rcs_print ("nml->cms->ProcessLine=%s.\n", nml->cms->ProcessLine);
	}
      if (nml == NULL)
	{
	  exit (-1);
	}
      int valid_ret = nml->valid();
      rcs_print ("NML::valid() returned %d\n",valid_ret);
      if (!valid_ret)
	{
	  rcs_print ("NML object is NOT valid.\n");
	  rcs_print ("nml->error_type = %d\n", nml->error_type);
	  exit (-1);
	}
      int type = nml->peek ();
      rcs_print ("NML::peek() returned %d\n", type);
      rcs_print ("\n");
      if(type == -1)
	{
	  rcs_print("nml->error_type =%d\n", nml->error_type);
	}
      while(type == -1 && nml->error_type == NML_TIMED_OUT)
	{
	  type = nml->peek ();
	  rcs_print ("NML::peek() returned %d\n", type);
	  rcs_print ("\n");
	  if(type == -1)
	    {
	      rcs_print("nml->error_type =%d\n", nml->error_type);
	    }
	}
      rcs_print ("nml->cms->header.write_id = %ld\n",
		 nml->cms->header.write_id);
      rcs_print ("nml->cms->header.was_read = %ld\n",
		 nml->cms->header.write_id);
      NMLmsg *msg = nml->get_address ();
      if (type > 0 && msg != NULL)
	{
#ifdef ENABLE_RCS_NMLMOD
	  if (cmd_type)
	    {
	      RCS_CMD_MSG *cmd_msg = (RCS_CMD_MSG *) nml->get_address ();
	      rcs_print ("RCS_CMD_MSG *cmd_msg = nml->get_address() = %p\n",
			 (void*)cmd_msg);
	      rcs_print ("cmd_msg->type = %ld\n", cmd_msg->type);
	      rcs_print ("cmd_msg->size = %ld\n", cmd_msg->size);
	      rcs_print ("cmd_msg->serial_number = %d\n",
			 cmd_msg->serial_number);
	    }
	  else if (stat_type)
	    {
	      RCS_STAT_MSG *stat_msg = (RCS_STAT_MSG *) nml->get_address ();
	      rcs_print ("RCS_STAT_MSG *stat_msg = nml->get_address() = %p\n",
			 (void*)stat_msg);
	      rcs_print ("stat_msg->type = %ld\n", stat_msg->type);
	      rcs_print ("stat_msg->size = %ld\n", stat_msg->size);
	      rcs_print ("stat_msg->command_type = %ld\n",
			 stat_msg->command_type);
	      rcs_print ("stat_msg->echo_serial_number = %d\n",
			 stat_msg->echo_serial_number);
	      rcs_print ("stat_msg->status = %d\n", stat_msg->status);
	      rcs_print ("stat_msg->state = %d\n", stat_msg->state);
	      rcs_print ("stat_msg->line = %d\n", stat_msg->line);
	      rcs_print ("stat_msg->line = %d\n", stat_msg->line);
	      rcs_print ("stat_msg->source_line = %d\n",
			 stat_msg->source_line);
	      rcs_print ("stat_msg->source_file = %s\n",
			 stat_msg->source_file);
	    }
	  else
	    {
	      rcs_print ("NMLmsg *msg = nml->get_address() = %p\n", msg);
	      rcs_print ("msg->type = %ld\n", msg->type);
	      rcs_print ("msg->size = %ld\n", msg->size);
	    }
#else
	  rcs_print ("NMLmsg *msg = nml->get_address() = %p\n", (void*)msg);
	  rcs_print ("msg->type = %ld\n", (long) msg->type);
	  rcs_print ("msg->size = %ld\n", msg->size);
#endif
	  if (dump_hex_data)
	    {
	      rcs_print ("\n Raw Data:\n");
	      char *cdata = (char *) msg;
	      for (i = 0; i < msg->size; i++)
		{
		  unsigned int x = (0xFF & ((unsigned int) cdata[i]));
		  rcs_print ("%2.2X", x);
		  if (i % 4 == 3)
		    {
		      rcs_print (" ");
		    }
		  if (i % 16 == 15)
		    {
		      rcs_print ("\n");
		    }
		  if (i % 64 == 63)
		    {
		      rcs_print ("\n");
		    }
		}
	      if (NULL != nml->cms->encoded_data)
		{
		  rcs_print ("\n Encoded Data:\n");
		  char *ecdata = (char *) nml->cms->encoded_data;
		  for (i = 0; i < msg->size; i++)
		    {
		      unsigned int x = (0xFF & ((unsigned int) ecdata[i]));
		      rcs_print ("%2.2X", x);
		      if (i % 4 == 3)
			{
			  rcs_print (" ");
			}
		      if (i % 16 == 15)
			{
			  rcs_print ("\n");
			}
		      if (i % 64 == 63)
			{
			  rcs_print ("\n");
			}
		    }
		}
	    }
	}

#ifdef ENABLE_RCS_DIAG
      if (show_diag_info)
	{
	  NML_DIAGNOSTICS_INFO *ndi = nml->get_diagnostics_info ();
	  if (NULL != ndi)
	    {
	      rcs_print ("********************************************\n");
	      rcs_print ("* Diagnostics for %s\n", nml->cms->BufferName);
	      ndi->print ();
	      rcs_print ("********************************************\n");

	    }
	  else
	    {
	      rcs_print ("No diagnostics available.\n");
	    }
	}
#endif

      rcs_print ("get_msg_count returned %d\n.", nml->get_msg_count ());
      if (nml->cms->queuing_enabled)
	{
	  rcs_print ("get_queue_length returned %d\n.",
		     nml->get_queue_length ());
	  rcs_print ("get_space_available returned %d\n",
		     nml->get_space_available ());
	}
      if (NULL != nml)
	{
	  delete nml;
	  nml = NULL;
	}
      rcs_print ("\n");
      exit (0);
    }

  /* Open the configuration file. */
  if ((fp = inet_file_open (config_file, "r")) == NULL)
    {
      rcs_print_error ("nmltest: can't open '%s'\n", config_file);
      exit (-1);
    }


  /* Read the configuration file line by line to find buffers. */
  while (!inet_file_eof (fp))
    {
      NML *lc_nml = (NML *) NULL;
      NML *rc_nml = (NML *) NULL;
      local_connect_status = LC_NOT_ATTEMPTED;
      remote_connect_status = RC_NOT_ATTEMPTED;
      if ((inet_file_gets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	{
	  break;
	}

      line_number++;
      line_len = (int) strlen (line);
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
	      if ((inet_file_gets
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
      rcs_print ("Comparing host_name=%s with buffer_host %s = %d\n", 
		 host_name,buffer_host,!strcmp (buffer_host, host_name));
      if (!remote_only &&
	  (force_local || 
	   !strcmp (buffer_host, host_name) ||
	   dl_address_is_local(buffer_host,1)))
	{
	  printf ("\nAttempting to connect to %s locally . . .\n",
		  buffer_name);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(proc_line,sizeof(proc_line)),
			  "P nmltest %s LOCAL %s     R 0 5 %d 0\n",
			  buffer_name, buffer_host, master);
	  lc_nml = new NML (line, proc_line);
	  if (NULL != lc_nml)
	    {
	      lc_nml->ignore_format_chain = 1;
	      lc_nml->login (login, passwd);
	      if (lc_nml->valid ())
		{
		  if (wait_for_read)
		    {
		      printf ("Waiting for the buffer to be read.\n");
		      while (!lc_nml->check_if_read ())
			esleep(0.01);
		      printf ("Buffer was read.\n");
		    }
		  lc_msg_type = lc_nml->get_msg_type ();
		  if (lc_msg_type != -1)
		    {
		      msg_size = (lc_nml->get_address ())->size;
		      local_connect_status = LC_SUCCEEDED;
		    }
		  else
		    {
		      printf ("NML::get_msg_type() returned -1\n");
		      local_connect_status = LC_FAILED;
		    }
		  if (lc_msg_type == 0)
		    {
		      lc_msg_type = (lc_nml->get_address ())->type;
		    }
		  printf ("get_msg_count returned %d\n",
			  lc_nml->get_msg_count ());
		  if (lc_nml->cms->queuing_enabled)
		    {
		      printf ("get_queue_length returned %d\n",
			      lc_nml->get_queue_length ());
		      printf ("get_space_available returned %d\n",
			      lc_nml->get_space_available ());
		    }

		}
	      else
		{
		  printf ("NML::valid returned 0.\n");
		  printf ("NML::error_type = %d\n", lc_nml->error_type);
		  local_connect_status = LC_FAILED;
		}
#ifdef ENABLE_RCS_DIAG
	      if (show_diag_info)
		{
		  NML_DIAGNOSTICS_INFO *ndi = lc_nml->get_diagnostics_info ();
		  if (NULL != ndi)
		    {
		      ndi->print ();
		    }
		}
#endif

	    }
	  else
	    {
	      local_connect_status = LC_FAILED;
	    }
	}
      if (!local_only  &&  (!master || force_remote))
	{
	  printf ("\nAttempting to connect to %s remotely . . .\n",
		  buffer_name);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(proc_line,sizeof(proc_line)),
			  "P nmltest %s REMOTE %s    R 0 5 0 0\n",
			  buffer_name, (host_name[0]?host_name:"remotehost"));
	  rc_nml = new NML (line, proc_line);
	  if (NULL != rc_nml)
	    {
	      if (login[0] != 0 && login[0] != '\n' && login[0] != '\r')
		{
		  rc_nml->login (login, passwd);
		}
	      rc_nml->ignore_format_chain = 1;
	      if (rc_nml->valid ())
		{
		  if (wait_for_read)
		    {
		      printf ("Waiting for the buffer to be read.\n");
		      while (!rc_nml->check_if_read ())
			esleep(0.01);
		      printf ("Buffer was read.\n");
		    }
		  rc_msg_type = rc_nml->get_msg_type ();
		  if (rc_msg_type != -1)
		    {
		      msg_size = (rc_nml->get_address ())->size;
		      remote_connect_status = RC_SUCCEEDED;
		    }
		  else
		    {
		      printf ("NML::get_msg_type returned -1\n");
		      remote_connect_status = RC_FAILED;
		    }
		  if (rc_msg_type == 0)
		    {
		      rc_msg_type = (rc_nml->get_address ())->type;
		    }
		  printf ("get_msg_count returned %d\n",
			  rc_nml->get_msg_count ());
		  if (rc_nml->cms->queuing_enabled)
		    {
		      printf ("get_queue_length returned %d\n",
			      rc_nml->get_queue_length ());
		      printf ("get_space_available returned %d\n",
			      rc_nml->get_space_available ());
		    }

		}
	      else
		{
		  printf ("NML::valid returned 0.\n");
		  printf ("NML::error_type = %d\n", rc_nml->error_type);
		  remote_connect_status = RC_FAILED;
		}
#ifdef ENABLE_RCS_DIAG
	      if (show_diag_info)
		{
		  NML_DIAGNOSTICS_INFO *ndi = rc_nml->get_diagnostics_info ();
		  if (NULL != ndi)
		    {
		      ndi->print ();
		    }
		}
#endif

	    }
	  else
	    {
	      remote_connect_status = RC_FAILED;
	    }
	}

      if (local_connect_status == LC_SUCCEEDED &&
	  lc_nml)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(output_line,sizeof(output_line)),
			  "%20.20s\t%s\t%s\t%ld\t%d\t%d\t%d\t%d",
			  buffer_name,
			  LC_ARRAY[local_connect_status],
			  RC_ARRAY[remote_connect_status],
			  lc_msg_type,
			  msg_size,
			  lc_nml->get_msg_count (), 
			  lc_nml->get_queue_length(), 
			  lc_nml->check_if_read ());
	}
      else if (remote_connect_status == RC_SUCCEEDED &&
	       rc_nml)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(output_line,sizeof(output_line)),
			  "%20.20s\t%s\t%s\t%ld\t%d\t%d\t%d\t%d",
			  buffer_name,
			  LC_ARRAY[local_connect_status],
			  RC_ARRAY[remote_connect_status],
			  rc_msg_type,
			  msg_size,
			  rc_nml->get_msg_count (), 
			  rc_nml->get_queue_length(), 
			  rc_nml->check_if_read ());
	}
      else
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(output_line,sizeof(output_line)),
			  "%20.20s\t%s\t%s\t--\t--\t--\t--\t--",
			  buffer_name,
			  LC_ARRAY[local_connect_status],
			  RC_ARRAY[remote_connect_status]);
	}
      output_list->store_at_tail (output_line, strlen (output_line) + 1, 1);

      if (NULL != lc_nml)
	{
	  delete lc_nml;
	  lc_nml=0;
	}
      if (NULL != rc_nml)
	{
	  delete rc_nml;
	  rc_nml=0;
	}
    }


  /* Print a nice header. */
  printf ("NMLTEST: terminology\n");
  printf
    ("LCS - Local Connect Status S(Success)|F(Fail)|NA(Not Attempted)\n");
  printf
    ("RCS - Remote Connect Status (What did you think RCS stood for?)\n");
  printf ("msg - NML message type\n");
  printf ("size - sizeof(NML message)\n");
  printf ("cnt - Number of times this buffer has been written to.\n");
  printf ("ql - queue length = how many messages written but not read from queue\n");
  printf
    ("read? - Has this message been read by someone other than NMLTEST?\n");
  printf ("\n");



  printf ("NMLTEST: Summary\n");
  printf ("%20.20s\tLCS\tRCS\tmsg\tsize\tcnt\tql\tread?\n", "BufferName");



  if (NULL != output_list)
    {
      char *next_line;
      next_line = (char *) output_list->get_head ();
      while (NULL != next_line)
	{
	  puts (next_line);
	  next_line = (char *) output_list->get_next ();
	}
      delete output_list;
    }
#ifndef VXWORKS
  return 0;
#endif
}
