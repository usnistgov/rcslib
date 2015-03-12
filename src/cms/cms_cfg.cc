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

/*************************************************************************
* File: cms_cfg.cc                                                       *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for the  Communication Management System (CMS).      *
*          Includes:                                                     *
*                    1. cms_config -- CMS function which reads
*                         configuration file.                            *
*************************************************************************/


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "cms_cfg_no_config.h"
#endif
// HAVE_CONFIG_H

extern bool verbose_nml_error_messages;

#include "cms.hh"		// class CMS
#include "cms_cfg.hh"


#ifdef ENABLE_RCS_SOKINTRF
#include "sokintrf.h" 		// dl_inet_ntoa(),dl_gethostbyname(),dl_gethostname()
#endif

#ifdef ENABLE_RCS_TCP
#include "tcpmem.hh"		/* class TCPMEM  */
#endif

#ifdef ENABLE_RCS_STCP
#include "stcpmem.hh"		/* class STCPMEM  */
#endif

#ifdef ENABLE_RCS_UDP
#include "udpmem.hh"		/* class UDPMEM  */
#endif

#ifdef ENABLE_RCS_GDRS_IM
#include "gdrs_im_clnt.hh"	/* class GDRS_IM_CLNT  */
#endif

#ifdef ENABLE_RCS_PHANTOM
#include "phantom.hh"		/* class PHANTOMMEM */
#endif

#ifdef ENABLE_RCS_FILEMEM
#include "filemem.hh"		// class FILEMEM
#endif

#ifdef ENABLE_RCS_LOCMEM
#include "locmem.hh"		// class LOCMEM
#endif

#ifdef ENABLE_RCS_GLOBMEM
#include "globmem.hh"		/* class GLOBMEM  */
#endif

#if defined(ENABLE_RCS_BBDMEM) && !defined(VXWORKS)
#error BBDMEM can only be used with vxworks.
#endif

#ifdef VXWORKS
#ifdef ENABLE_RCS_BBDMEM
#include "bbdmem.hh"		/* class BBDMEM  */
#endif
#endif

#if defined(ENABLE_RCS_SHMEM)
#include "shmem.hh"		/* class SHMEM  */
#endif

#if defined(ENABLE_RCS_TTY)
#include "ttymem.hh"
#endif

#if defined(ENABLE_RCS_RTLMEM)
#include "rtlmem.hh"		// class RTLMEM
#endif

#if defined(ENABLE_RCS_OE_INTRF) 
#include "oemem.hh"
#include "oemque.hh"
#include "oedsto.hh"
#endif

#if defined(ENABLE_RCS_NMLCFGSVR)
#include "nmlcfgsvr_clntcalls.hh"
#endif

#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		// RCS_LINKED_LIST


// The following came from rpatel@gdrs.com -- Rcslib_Tweaks_for_GDRS.doc 
// Monday Nov. 27, 2006
 // relates to above DEPRECATE - only needed for MSVC 2005 (8.0)
#if defined(WIN32) && defined(USE__STRDUP)
#define strdup _strdup
#endif


/* Class CMS friend functions. */

class CONFIG_FILE_INFO
{
public:
  CONFIG_FILE_INFO (): lines_list(0)
  {
    lines_list = NULL;
    memset(file_name,0,sizeof(file_name));
  };

  ~CONFIG_FILE_INFO ()
  {
    if (NULL != lines_list)
      {
	delete lines_list;
	lines_list = NULL;
      }
  };

  class RCS_LINKED_LIST *lines_list;
  char file_name[80];

private:
  // Prevent copying
  CONFIG_FILE_INFO(const CONFIG_FILE_INFO &_cfi);
  CONFIG_FILE_INFO &operator=(const CONFIG_FILE_INFO &_cfi);
};

static RCS_LINKED_LIST *config_file_list = NULL;
static int loading_config_file = 0;

static bool cmscfg_ignore_remote=false;
static bool cmscfg_ignore_no_bufline=false;
static bool cmscfg_last_buffer_was_ignored_remote=false;
static bool cmscfg_last_buffer_was_ignored_no_bufline=false;

void 
set_cmscfg_ignore_remote(int b)
{
  cmscfg_ignore_remote =(bool) (b != 0);
}

int
get_cmscfg_ignore_remote(void)
{
  return (int) cmscfg_ignore_remote;
}

int 
get_cmscfg_last_buffer_was_ignored_remote(void)
{
  return (int) cmscfg_last_buffer_was_ignored_remote;
}

void
clear_cmscfg_last_buffer_was_ignored_remote(void)
{
  cmscfg_last_buffer_was_ignored_remote=false;
}


void 
set_cmscfg_ignore_no_bufline(int b)
{
  cmscfg_ignore_no_bufline =(bool) (b != 0);
}

int
get_cmscfg_ignore_no_bufline(void)
{
  return (int) cmscfg_ignore_no_bufline;
}

int 
get_cmscfg_last_buffer_was_ignored_no_bufline(void)
{
  return (int) cmscfg_last_buffer_was_ignored_no_bufline;
}

void
clear_cmscfg_last_buffer_was_ignored_no_bufline(void)
{
  cmscfg_last_buffer_was_ignored_no_bufline=false;
}

int
load_nml_config_file (const char *file)
{
    if(!strncmp(file,"nmlcfgsvr:",10))
    {
        rcs_print_warning("call to load_nml_config_file(%s) with nmlcfgsvr: prototocol has no effect.\n",
                    file);
	return 0;
    }
  unload_nml_config_file (file);
  if (loading_config_file)
    {
      return -1;
    }
  loading_config_file = 1;
  if (NULL == file)
    {
      loading_config_file = 0;
      return -1;
    }
  if (NULL == config_file_list)
    {
      config_file_list = new RCS_LINKED_LIST ();
    }
  if (NULL == config_file_list)
    {
      loading_config_file = 0;
      return -1;
    }
  char line[CMS_CONFIG_LINELEN];	/* Temporary buffer for line from file. */

  CONFIG_FILE_INFO *info = new CONFIG_FILE_INFO ();
  info->lines_list = new RCS_LINKED_LIST ();
  strncpy (info->file_name, file, 80);
#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *fp;
  fp = inet_file_open ((char *) file, "r");
#else
  FILE *fp;
  fp = fopen (file, "r");
#endif
  if (fp == NULL)
    {
      rcs_print_error ("cms_config: can't open '%s'. Error = %d -- %s\n",
		       file, errno, strerror (errno));
      if (NULL != info)
	{
	  delete info;
	}
      loading_config_file = 0;
      return -1;
    }

#ifdef ENABLE_RCS_INET_FILES
  while (!inet_file_eof (fp))
#else
  while (!feof (fp))
#endif
    {
      memset(line,0,CMS_CONFIG_LINELEN);
#ifdef ENABLE_RCS_INET_FILES
      if ((inet_file_gets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	{
	  break;
	}
#else
      if ((fgets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	{
	  break;
	}
#endif
      int line_len = (int) strlen (line);
      while (line_len > 0 && 
	     (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
	     )
	{
	  line_len--;
	  line[line_len]=0;
	}
      if (line_len < 3) 
	{
	  if(line_len < 1 || line[line_len-1] != '\\') 
	    {
	      continue;
	    }
	}
      while (line[line_len - 1] == '\\')
	{
	  line_len--;
	  line[line_len]=0;
#ifdef ENABLE_RCS_INET_FILES
	  if ((inet_file_gets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) ==
	      NULL)
	    {
	      break;
	    }
#else
	  if ((fgets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) == NULL)
	    {
	      break;
	    }
#endif
	  line_len = (int) strlen (line);
	  if (line_len > CMS_CONFIG_LINELEN - 2)
	    {
	      break;
	    }
	  while (line_len > 0 && 
		 (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
		 )
	    {
	      line_len--;
	      line[line_len]=0;
	    }
	}

      if (line[0] == '#')
	{
	  continue;
	}
      info->lines_list->store_at_tail (line, line_len + 1, 1);
    }

  if (NULL != fp)
    {
#ifdef ENABLE_RCS_INET_FILES
      inet_file_close (fp);
#else
      fclose (fp);
#endif
      fp = NULL;
    }
  config_file_list->store_at_tail (info, sizeof (info), 0);
  loading_config_file = 0;
  return 0;
}

int
unload_nml_config_file (const char *file)
{
  if (loading_config_file)
    {
      return -1;
    }
  if (NULL == file)
    {
      return -1;
    }
  if (NULL == config_file_list)
    {
      return -1;
    }
  CONFIG_FILE_INFO *info = (CONFIG_FILE_INFO *) config_file_list->get_head ();
  while (NULL != info)
    {
      if (!strncmp (info->file_name, file, 80))
	{
	  config_file_list->delete_current_node ();
	  delete info;
	  return 0;
	}
      info = (CONFIG_FILE_INFO *) config_file_list->get_next ();
    }
  return -1;
}



class CONFIG_FILE_INFO *
get_loaded_nml_config_file (const char *file)
{
  if (NULL == file)
    {
      return NULL;
    }
  if (NULL == config_file_list)
    {
      return NULL;
    }
  CONFIG_FILE_INFO *info = (CONFIG_FILE_INFO *) config_file_list->get_head ();
  while (NULL != info)
    {
      if (!strncmp (info->file_name, file, 80))
	{
	  return info;
	}
      info = (CONFIG_FILE_INFO *) config_file_list->get_next ();
    }
  return NULL;
}

int
print_loaded_nml_config_file (const char *file)
{
  CONFIG_FILE_INFO *info = get_loaded_nml_config_file (file);
  if (NULL == info)
    {
      rcs_print ("Config file %s not loaded.\n",file);
      return -1;
    }
  if (NULL == info->lines_list)
    {
      return -1;
    }
  char *line = (char *) info->lines_list->get_head ();
  while (NULL != line)
    {
      rcs_print ("%s", line);
      int line_len = (int) strlen (line);
      if (line_len > 1)
	{
	  char last_char = line[line_len - 1];
	  if (last_char != '\n' && last_char != '\r')
	    {
	      rcs_print ("\n");
	    }
	}
      line = (char *) info->lines_list->get_next ();
    }
  rcs_print ("\n");
  return 0;
}

int
print_loaded_nml_config_file_list (void)
{
  if (loading_config_file)
    {
      rcs_print
	("In the process of loading a config file, please try again later.\n");
      return -1;
    }
  if (NULL == config_file_list)
    {
      rcs_print ("No Configuration files loaded.\n");
      return 0;
    }
  CONFIG_FILE_INFO *info = (CONFIG_FILE_INFO *) config_file_list->get_head ();
  while (NULL != info)
    {
      if (NULL != info->lines_list)
	{
	  rcs_print ("%s \t- - \t%d lines\n", info->file_name,
		     info->lines_list->list_size);
	}
      else
	{
	  rcs_print ("%s \t-1 lines", info->file_name);
	}
      info = (CONFIG_FILE_INFO *) config_file_list->get_next ();
    }
  return 0;
}

extern "C" int unload_all_nml_config_file (void);

int
unload_all_nml_config_file ()
{
  if (loading_config_file)
    {
      return -1;
    }
  if (NULL == config_file_list)
    {
      return -1;
    }
  CONFIG_FILE_INFO *info = (CONFIG_FILE_INFO *) config_file_list->get_head ();
  while (NULL != info)
    {
      config_file_list->delete_current_node ();
      delete info;
      info = (CONFIG_FILE_INFO *) config_file_list->get_next ();
    }
  if (config_file_list->list_size <= 0)
    {
      delete config_file_list;
      config_file_list = NULL;
    }
  return 0;
}

#if 0
static int
convert2lower (char *dest, char *src, int len)
{
  int i;
  if (src == NULL || dest == NULL)
    {
      rcs_print_error ("convert2lower passed NULL argument.\n");
      return -1;
    }

  for (i = 0; i < len; i++)
    {
      if (src[i] == 0)
	{
	  dest[i] = 0;
	  return i;
	}
      dest[i] = tolower (src[i]);
    }
  return i;
}
#endif

static int
convert2upper (char *dest, char *src, int len)
{
  int i;
  if (src == NULL || dest == NULL)
    {
      rcs_print_error ("convert2upper passed NULL argument.\n");
      return -1;
    }

  for (i = 0; i < len; i++)
    {
      if (src[i] == 0)
	{
	  dest[i] = 0;
	  return i;
	}
      dest[i] = toupper (src[i]);
    }
  return i;
}


int
cms_copy_with_buffers (CMS ** dest, CMS * src,
		       char *wordbuf,char *proc_type,char *buffer_type,
		       int set_to_server, int set_to_master,
		       enum CMS_CONNECTION_MODE connection_mode)
{
  if (NULL == dest || NULL == src)
    {
      return -1;
    }
  return cms_create_from_lines_with_buffers(dest, 
					    src->BufferLine, src->ProcessLine,
					    wordbuf,
					    proc_type,
					    buffer_type,
					    set_to_server, 
					    set_to_master,
					    connection_mode);
}


extern char *
get_buffer_line_with_buffers (const char *bufname, const char *filename,
			      char *linebuf, char *wordbuf)
{
  int line_len, line_number;
  if(!linebuf || !wordbuf || !bufname)
    {
      rcs_print_error("get_buffer_line_with_buffers bad argument.\n");
      return 0;
    }
  char *line = linebuf;
#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *fp = NULL;
#else
  FILE *fp = NULL;		/* FILE ptr to config file.  */
#endif
  char *word[4];		/* array of pointers to words from line */


  /* Open the configuration file. */
  RCS_LINKED_LIST *lines_list = NULL;
  CONFIG_FILE_INFO *info = get_loaded_nml_config_file (filename);
  if (NULL != info)
    {
      lines_list = info->lines_list;
      line = (char *) lines_list->get_head ();
    }

  if (NULL == lines_list)
    {
#ifdef ENABLE_RCS_INET_FILES
      fp = inet_file_open (filename, "r");
#else
      fp = fopen (filename, "r");
#endif
      if (fp == NULL)
	{
	  rcs_print_error ("cms_config: can't open '%s'. Error = %d -- %s\n",
			   filename, errno, strerror (errno));
	  loading_config_file = 0;
	  return NULL;
	}
    }


  /* Read the configuration file line by line until the lines matching */
  /*     bufname and procname are found.  */
  line_number = 0;
  int first_line = 1;


  while (1)
    {
      if (NULL != lines_list)
	{
	  if (!first_line)
	    {
	      line = (char *) lines_list->get_next ();
	    }
	  first_line = 0;
	  if (NULL == line)
	    {
	      break;
	    }
	}
      else
	{
	  memset(line,0,CMS_CONFIG_LINELEN);
#ifdef ENABLE_RCS_INET_FILES
	  if (inet_file_eof (fp))
	    {
	      break;
	    }
	  if ((inet_file_gets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	    {
	      break;
	    }
#else
	  if (feof (fp))
	    {
	      break;
	    }
	  if ((fgets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	    {
	      break;
	    }
#endif
	}


      line_number++;
      line_len = (int) strlen (line);
      while (line_len > 0 && 
	     (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
	     )
	{
	  line_len--;
	  line[line_len]=0;
	}
      while (line[line_len - 1] == '\\')
	{
	  line_len--;
	  line[line_len]=0;
#ifdef ENABLE_RCS_INET_FILES
	  if ((inet_file_gets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) ==
	      NULL)
	    {
	      break;
	    }
#else
	  if ((fgets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) == NULL)
	    {
	      break;
	    }
#endif
	  line_len = (int) strlen (line);
	  if (line_len > CMS_CONFIG_LINELEN - 2)
	    {
	      break;
	    }
	  line_number++;
	  while (line_len > 0 && 
		 (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
		 )
	    {
	      line_len--;
	      line[line_len]=0;
	    }
	}
      if (line_len > CMS_CONFIG_LINELEN)
	{
	  rcs_print_error
	    ("cms_cfg: Line length of line number %d in %s exceeds max length of %d",
	     line_number, filename, CMS_CONFIG_LINELEN);
	}


      /* Skip comment lines and lines starting with white space. */
      if (line[0] == CMS_CONFIG_COMMENTCHAR ||
	  strchr (" \t\n\r\0", line[0]) != NULL)
	{
	  continue;
	}


      /* Separate out the first four strings in the line. */
      if (separate_words_with_buffer (word, 4, line,wordbuf,CMS_CONFIG_LINELEN) != 4)
	{
	  continue;
	}

      if (!strcmp (word[1], bufname) && line[0] == 'B')
	{
	  /* Buffer line found, store the line and type. */
	  return line;
	}
    }
  return NULL;
}

enum CONFIG_SEARCH_ERROR_TYPE
{
  CONFIG_SEARCH_ERROR_NOT_SET,
  CONFIG_SEARCH_OK,
  BAD_CONFIG_FILE,
  NO_PROCESS_LINE,
  NO_BUFFER_LINE,
  MISC_CONFIG_SEARCH_ERROR
};

struct CONFIG_SEARCH_STRUCT
{
  enum CONFIG_SEARCH_ERROR_TYPE error_type;
  int buffer_line_number;
  int proc_line_number;
  const char *bufname;
  const char *bufname_for_proc_line;
  const char *procname;
  const char *filename;
  bool buffer_line_found;
  bool proc_line_found;
  int set_to_server;
  int set_to_master;
  size_t max_size_from_format;
  char buffer_line[CMS_CONFIG_LINELEN];	/* Line matching bufname. */
  char proc_line[CMS_CONFIG_LINELEN];	/* Line matching procname & bufname. */
  char buffer_type[CMS_CONFIG_LINELEN];	/* "SHMEM" or "GLOBMEM" */
  char proc_type[CMS_CONFIG_LINELEN];	/* "REMOTE" or "LOCAL" */
  char recvstring[CMS_CONFIG_LINELEN*2];
  char sendstring[CMS_CONFIG_LINELEN*2];
};

void find_proc_and_buffer_lines_with_buffers (CONFIG_SEARCH_STRUCT * s,
					      char *wordbuf,char *linebuf, enum CMS_CONNECTION_MODE connection_mode);


/* Function for initializing a CMS from a configuration file. */
 /* Returns 0 for success or -1 for error. */
int
cms_config_with_buffers (CMS ** cms, const char *bufname, const char *procname, const char *filename,
			 char *linebuf, char *wordbuf,char *buf, char *buf2,
			 int set_to_server, int set_to_master,size_t max_size_from_format,
			 enum CMS_CONNECTION_MODE connection_mode)
{
  CONFIG_SEARCH_STRUCT search;
  char *default_ptr = 0;

  if (0 == bufname || 0 == procname || 0 == filename)
    {
      return -1;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "cms_config arguments:\n");
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "bufname = %s\n", bufname);
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "procname = %s\n", procname);
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "filename = %s\n", filename);

  search.error_type = CONFIG_SEARCH_ERROR_NOT_SET;
  search.buffer_line_found = false;
  search.buffer_line_number = -1;
  search.proc_line_found = false;
  search.proc_line_number = -1;
  search.bufname = bufname;
  search.bufname_for_proc_line = bufname;
  search.procname = procname;
  search.filename = filename;
  search.set_to_master=set_to_master;
  search.set_to_server=set_to_server;
  search.max_size_from_format = max_size_from_format;
  find_proc_and_buffer_lines_with_buffers (&search,linebuf,wordbuf,connection_mode);
  if (NO_PROCESS_LINE == search.error_type)
    {
      search.bufname_for_proc_line = "default";
      find_proc_and_buffer_lines_with_buffers (&search,linebuf,wordbuf,connection_mode);
      if (search.error_type == CONFIG_SEARCH_OK)
	{
	  default_ptr = 0;
	  strncpy (buf, search.proc_line, CMS_CONFIG_LINELEN);
	  default_ptr = strstr (buf, "default");
	  if (default_ptr)
	    {
	      strcpy (buf2, default_ptr + 7);
	      strcpy (default_ptr, bufname);
	      default_ptr += strlen (bufname);
	      strcpy (default_ptr, buf2);
	      strncpy (search.proc_line, buf, CMS_CONFIG_LINELEN);
	    }
	  // strcat (search.proc_line, " defaultbuf");
	}
    }
  if (NO_PROCESS_LINE == search.error_type)
    {
      search.bufname_for_proc_line = bufname;
      search.procname = "default";
      find_proc_and_buffer_lines_with_buffers (&search,linebuf,wordbuf,connection_mode);
      if (search.error_type == CONFIG_SEARCH_OK)
	{
	  strncpy (buf, search.proc_line, CMS_CONFIG_LINELEN);
	  default_ptr = strstr (buf, "default");
	  if (default_ptr)
	    {
	      strcpy (buf2, default_ptr + 7);
	      strcpy (default_ptr, procname);
	      default_ptr += strlen (procname);
	      strcpy (default_ptr, buf2);
	      default_ptr = strstr (buf, "default");
	    }
	  if (default_ptr)
	    {
	      strcpy (buf2, default_ptr + 7);
	      strcpy (default_ptr, bufname);
	      default_ptr += strlen (bufname);
	      strcpy (default_ptr, buf2);
	      strncpy (search.proc_line, buf, CMS_CONFIG_LINELEN);
	    }
	  //strcat (search.proc_line, " defaultproc defaultbuf");
	}
    }
  if (NO_PROCESS_LINE == search.error_type)
    {
      search.bufname_for_proc_line = "default";
      search.procname = "default";
      find_proc_and_buffer_lines_with_buffers (&search,linebuf,wordbuf,connection_mode);
      if (search.error_type == CONFIG_SEARCH_OK)
	{
	  strncpy (buf, search.proc_line, CMS_CONFIG_LINELEN);
	  default_ptr = strstr (buf, "default");
	  if (default_ptr)
	    {
	      strcpy (buf2, default_ptr + 7);
	      strcpy (default_ptr, procname);
	      default_ptr += strlen (procname);
	      strcpy (default_ptr, buf2);
	      default_ptr = strstr (buf, "default");
	    }
	  if (default_ptr)
	    {
	      strcpy (buf2, default_ptr + 7);
	      strcpy (default_ptr, bufname);
	      default_ptr += strlen (bufname);
	      strcpy (default_ptr, buf2);
	      strncpy (search.proc_line, buf, CMS_CONFIG_LINELEN);
	    }
	  //strcat (search.proc_line, " defaultproc defaultbuf");
	}
    }
  if (CONFIG_SEARCH_OK == search.error_type)
    {
      return (cms_create_with_buffers (cms, search.buffer_line, search.proc_line,
				       search.buffer_type, search.proc_type,
				       wordbuf,
				       set_to_server, set_to_master,connection_mode));
    }
  switch (search.error_type)
    {
    case NO_BUFFER_LINE:
      if(cmscfg_ignore_no_bufline)
	{
	  cmscfg_last_buffer_was_ignored_no_bufline=true;
	  return -3;
	}
      else
	{
	  rcs_print_error
	    ("No buffer-line entry found for buffer %s in config file %s.\n",
	     bufname, filename);
	}
      break;

    case NO_PROCESS_LINE:
      rcs_print_error
	("No process-line entry found for process %s connecting to buffer %s in config file %s and no applicable defaults were found.\n",
	 procname, bufname, filename);
      break;

    default:
      break;
    }
  return (-1);
}

#if !defined(VXWORKS) && !defined(ENABLE_RCS_SOKINTRF)
static int no_sokintrf_warning_given=0;
#endif

int
hostname_matches_bufferline_with_buffer (const char *buffer_line,char *wordbuf)
{
  char *buffer_host = 0;
  char *word[4];		/* array of pointers to words from line */

  if (0 == buffer_line)
    {
      return 0;
    }

  /* Separate out the first four strings in the line. */
  if (separate_words_with_buffer (word, 4, buffer_line,wordbuf,CMS_CONFIG_LINELEN) != 4)
    {
      return 0;
    }
  char *realname = cms_check_for_host_alias(word[3]);
  if (realname == NULL) { 
    buffer_host = word[3];
  } else {
    buffer_host = realname;
  }

  if (buffer_host == 0)
    {
      return 0;
    }
#ifdef ENABLE_RCS_SOKINTRF
  return(dl_address_is_local(buffer_host,0));
#else
  return(1);
#endif
}

void
find_proc_and_buffer_lines_with_buffers (CONFIG_SEARCH_STRUCT * s,
					 char *linebuf, char *wordbuf,
					 enum CMS_CONNECTION_MODE connection_mode)
{
#if defined(ENABLE_RCS_NMLCFGSVR)
  enum NMLCFGSVR_STATUS ncs_status = NMLCFGSVR_STATUS_NOT_SET;
  char *nmlcfgsvr_options = 0;
  char *nmlcfgsvr_options_copy=0;
#endif
  if (s == 0)
    {
      return;
    }

  loading_config_file = 1;
#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *fp = NULL;
#else
  FILE *fp = NULL;		/* FILE ptr to config file.  */
#endif
  char *line = linebuf;
  int line_len, line_number;
  char *word[4];		/* array of pointers to words from line */

  int first_line = 1;

  /* Open the configuration file. */
  RCS_LINKED_LIST *lines_list = NULL;

#if defined(ENABLE_RCS_NMLCFGSVR)
  if(!strncmp(s->filename,"nmlcfgsvr:",10) || !strncmp(s->filename,"nmlcfgsvr/",10))
    {
      nmlcfgsvr_options=0;
      if(s->max_size_from_format > 0 && !strstr(s->filename+10,"size="))
	{
	  if(!nmlcfgsvr_options)
	    {
	      nmlcfgsvr_options= (char *) malloc(256);
	      memset(nmlcfgsvr_options,0,256);
	    }
	  nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			  "max_size_from_format=%lu %s",
			  (unsigned long) s->max_size_from_format, 
			  nmlcfgsvr_options_copy);
	  free(nmlcfgsvr_options_copy);
	  nmlcfgsvr_options_copy=0;
	}
      if(s->set_to_server != 0)
	{
	  if(!nmlcfgsvr_options)
	    {
	      nmlcfgsvr_options= (char *) malloc(256);
	      memset(nmlcfgsvr_options,0,256);
	    }
	  if(nmlcfgsvr_options[0])
	    {
	      nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	    }
	  else
	    {
	      nmlcfgsvr_options_copy = 0;
	    }
	  if(last_port_number_set > 0 && 
	     last_port_type_set == CMS_TCP_REMOTE_PORT_TYPE)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			      "set_to_server=%d port_to_use=%d %s",
			      s->set_to_server,last_port_number_set,
			      (nmlcfgsvr_options_copy?nmlcfgsvr_options_copy:""));
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			      "set_to_server=%d %s",
			      s->set_to_server,
			      (nmlcfgsvr_options_copy?nmlcfgsvr_options_copy:""));
	    }
	  if(nmlcfgsvr_options_copy)
	    {
	      free(nmlcfgsvr_options_copy);
	      nmlcfgsvr_options_copy=0;
	    }
	}
      ncs_status =
	nmlcfgsvr_create(s->filename+10,
			 s->bufname,
			 s->procname,
			 s->buffer_line,
			 s->proc_line,
			 s->recvstring,sizeof(s->recvstring),
			 s->sendstring,sizeof(s->sendstring),
			 nmlcfgsvr_options);
      if(ncs_status == NMLCFGSVR_CREATE_OK)
	{
	  s->buffer_line_found=true;
	  s->proc_line_found=true;
	  s->proc_line_number=1;
	  s->buffer_line_number=1;
	  sscanf(s->buffer_line,"%*s %*s %s",s->buffer_type);
	  sscanf(s->proc_line,"%*s %*s %*s %s",s->proc_type);
	  s->error_type = CONFIG_SEARCH_OK;
	}
      else
	{
	  s->error_type = MISC_CONFIG_SEARCH_ERROR;
	}
      return;
    }
#endif

  CONFIG_FILE_INFO *info = get_loaded_nml_config_file (s->filename);
  if (NULL != info)
    {
      lines_list = info->lines_list;
      line = (char *) lines_list->get_head ();
    }

  
  if (NULL == lines_list)
    {
#ifdef ENABLE_RCS_INET_FILES
      fp = inet_file_open (s->filename, "r");
#else
      fp = fopen (s->filename, "r");
#endif
      if (fp == NULL)
	{
	  rcs_print_error ("cms_config: can't open '%s'. Error = %d -- %s\n",
			   s->filename, errno, strerror (errno));
	  loading_config_file = 0;
	  s->error_type = BAD_CONFIG_FILE;
	  return;
	}
    }


  /* Read the configuration file line by line until the lines matching */
  /*     bufname and procname are found.  */
  line_number = 0;

  while (1)
    {
      if (NULL != lines_list)
	{
	  if (!first_line)
	    {
	      line = (char *) lines_list->get_next ();
	    }
	  first_line = 0;
	  if (NULL == line)
	    {
	      break;
	    }
	}
      else
	{
	  memset(line,0,CMS_CONFIG_LINELEN);
#ifdef ENABLE_RCS_INET_FILES
	  if (inet_file_eof (fp))
	    {
	      break;
	    }
	  if ((inet_file_gets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	    {
	      break;
	    }
#else
	  if (feof (fp))
	    {
	      break;
	    }
	  if ((fgets (line, CMS_CONFIG_LINELEN, fp)) == NULL)
	    {
	      break;
	    }
#endif
	}


      line_number++;
      line_len = (int) strlen (line);
      if (line_len < 3)
	{
	  continue;
	}
      while (line_len > 0 && 
	     (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
	     )
	{
	  line_len--;
	  line[line_len]=0;
	}
      while (line[line_len - 1] == '\\')
	{
	  line_len--;
	  line[line_len]=0;
#ifdef ENABLE_RCS_INET_FILES
	  if ((inet_file_gets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) ==
	      NULL)
	    {
	      break;
	    }
#else
	  if ((fgets (line + line_len, CMS_CONFIG_LINELEN - line_len, fp)) == NULL)
	    {
	      break;
	    }
#endif
	  line_len = (int) strlen (line);
	  if (line_len > CMS_CONFIG_LINELEN)
	    {
	      break;
	    }
	  line_number++;
	  while (line_len > 0 && 
		 (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'  ) 
		 )
	    {
	      line_len--;
	      line[line_len]=0;
	    }
	}
      if (line_len > CMS_CONFIG_LINELEN)
	{
	  rcs_print_error
	    ("cms_cfg: Line length of line number %d in %s exceeds max length of %d",
	     line_number, s->filename, CMS_CONFIG_LINELEN);
	}

      /* Skip comment lines and lines starting with white space. */
      if (line[0] == CMS_CONFIG_COMMENTCHAR ||
	  strchr (" \t\n\r\0", line[0]) != NULL)
	{
	  continue;
	}

#if defined(ENABLE_RCS_NMLCFGSVR)
      if(!strncmp(line,"nmlcfgsvr:",10) || !strncmp(line,"nmlcfgsvr/",10))
	{
	  /* Close the configuration file. */
	  if (NULL != fp)
	    {
#ifdef ENABLE_RCS_INET_FILES
	      inet_file_close (fp);
#else
	      fclose (fp);
#endif
	      fp = NULL;
	    }
	  nmlcfgsvr_options=0;
	  if(s->proc_line && s->proc_line_found)
	    {
	      nmlcfgsvr_options=strstr(s->proc_line,"nmlcfgsvr_options=");
	      if(nmlcfgsvr_options)
		{
		  nmlcfgsvr_options_copy= (char *) malloc(256);
		  strcpy(nmlcfgsvr_options_copy,nmlcfgsvr_options+18);
		  nmlcfgsvr_options = nmlcfgsvr_options_copy;
		  nmlcfgsvr_options_copy=0;
		}
	    }
	  if(s->max_size_from_format > 0 && !strstr(line+10,"size="))
	    {
	      if(!nmlcfgsvr_options)
		{
		  nmlcfgsvr_options= (char *) malloc(256);
		  memset(nmlcfgsvr_options,0,256);
		}
	      nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			      "max_size_from_format=%lu %s",
			      (unsigned long) s->max_size_from_format, 
			      nmlcfgsvr_options_copy);
	      free(nmlcfgsvr_options_copy);
	      nmlcfgsvr_options_copy=0;
	    }
	  if(s->set_to_server != 0)
	    {
	      if(!nmlcfgsvr_options)
		{
		  nmlcfgsvr_options= (char *) malloc(256);
		  memset(nmlcfgsvr_options,0,256);
		}
	      nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			      "set_to_server=%d %s",
			      s->set_to_server,nmlcfgsvr_options_copy);
	      free(nmlcfgsvr_options_copy);
	      nmlcfgsvr_options_copy=0;
	    }
	  ncs_status =
	    nmlcfgsvr_create(line+10,
			     s->bufname,
			     s->procname,
			     s->buffer_line,
			     s->proc_line,
			     s->recvstring,sizeof(s->recvstring),
			     s->sendstring,sizeof(s->sendstring),
			     nmlcfgsvr_options);
	  if(ncs_status == NMLCFGSVR_CREATE_OK)
	    {
	      s->buffer_line_found=true;
	      s->proc_line_found=true;
	      s->proc_line_number=1;
	      s->buffer_line_number=1;
	      sscanf(s->buffer_line,"%*s %*s %s",s->buffer_type);
	      sscanf(s->proc_line,"%*s %*s %*s %s",s->proc_type);
	      s->error_type = CONFIG_SEARCH_OK;
	    }
	  else
	    {
	      s->error_type = MISC_CONFIG_SEARCH_ERROR;
	    }
	  if(nmlcfgsvr_options && 
	     (nmlcfgsvr_options > (s->proc_line + sizeof(s->proc_line)) ||
	      nmlcfgsvr_options < s->proc_line))
	    {
	      free(nmlcfgsvr_options);
	      nmlcfgsvr_options=0;
	    }
	  return;
	}

      /* end of #if defined(ENABLE_RCS_NMLCFGSVR) */
#endif

      /* Separate out the first four strings in the line. */
      int sep_words_rep = separate_words_with_buffer (word, 4, line,wordbuf,CMS_CONFIG_LINELEN);
      if (sep_words_rep < 3)
	{
	  continue;
	}
      rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		       "s->proc_line_found=%d,line=%s,word[2]=%s,s->bufname_for_proc_line=%s,s->procname=%s,strcmp_bufname_ret=%d, strcmp_procname_ret=%d,strcmp_bufname_default_ret=%d,strcmp_procname_default_ret=%d\n",
		       s->proc_line_found,line,
		       word[2],
		       s->bufname_for_proc_line,
		       s->procname,
		       strcmp(word[2],s->bufname_for_proc_line),
		       strcmp (word[1],s->procname),
		       strcmp(word[2], "default"),
		       strcmp (word[1], "default"));
      if (!s->buffer_line_found && !strcmp (word[1], s->bufname) &&
	  line[0] == 'B')
	{
	  /* Buffer line found, store the line and type. */
	  strncpy (s->buffer_line, line, CMS_CONFIG_LINELEN);
	  if(!strncmp(word[2],"nmlcfgsvr:",10) || !strncmp(word[2],"nmlcfgsvr/",10))
	    {
	      strcpy(s->buffer_type,word[2]);
	    }
	  else
	    {
	      convert2upper (s->buffer_type, word[2], CMS_CONFIG_LINELEN);
	    }
	  s->buffer_line_found = true;
	  s->buffer_line_number = line_number;
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "cms_config found buffer line on line %d\n",
			   line_number);
	}
      else if (!s->proc_line_found && sep_words_rep == 4 &&
	       !strcmp (word[1], s->procname) &&
	       line[0] == 'P' && !strcmp (word[2], s->bufname_for_proc_line))
	{
	  /* Procedure line found, store the line and type. */
	  strncpy (s->proc_line, line, CMS_CONFIG_LINELEN);
	  enum CMS_CONNECTION_MODE cm=CMS_NORMAL_CONNECTION_MODE;
	  if(connection_mode == CMS_NORMAL_CONNECTION_MODE)
	    {
	      cm = cms_connection_mode;
	    }
	  else
	    {
	      cm = connection_mode;
	    }
	  switch (cm)
	    {
	    case CMS_NORMAL_CONNECTION_MODE:
	      convert2upper (s->proc_type, word[3], CMS_CONFIG_LINELEN);
	      if (!strncmp (s->proc_type, "AUTO", 4))
		{
		  if (!s->buffer_line_found)
		    {
		      rcs_print_error
			("Can't use process type AUTO unless the buffer line for %s is found earlier in the config file.\n",
			 s->bufname);
		      rcs_print_error ("Bad line:\n%s:%d %s\n", s->filename,
				       line_number, line);
		      s->error_type = MISC_CONFIG_SEARCH_ERROR;
		      return;
		    }
		  if (hostname_matches_bufferline_with_buffer (s->buffer_line,wordbuf))
		    {
		      strcpy (s->proc_type, "LOCAL");
		    }
		  else
		    {
		      strcpy (s->proc_type, "REMOTE");
		    }
		}
	      break;

	    case CMS_FORCE_LOCAL_CONNECTION_MODE:
	      if(strcmp(s->proc_type,"LOCAL"))
		{
		  char *stemp=strstr(s->proc_line,"AUTO");
		  if(stemp)
		    {
		      char *s2 = strdup(stemp);
		      strcpy(stemp+1,s2);
		      memcpy(stemp,"LOCAL",5);
		      free(s2);
		    }
		}
	      if(!strcmp(s->proc_type,"REMOTE"))
		{
		  char *stemp=strstr(s->proc_line,"REMOTE");
		  if(stemp) 
		    {
		      memcpy(stemp,"LOCAL ",6);
		    }
		  else
		    {
		      stemp=strstr(s->proc_line,"AUTO");
		      if(stemp)
			{
			  char *s2 = strdup(stemp);
			  strcpy(stemp+1,s2);
			  memcpy(stemp,"LOCAL",5);
			  free(s2);
			}
		    }
		}
	      strcpy (s->proc_type, "LOCAL");
	      break;

	    case CMS_FORCE_REMOTE_CONNECTION_MODE:
	      if(strcmp(s->proc_type,"REMOTE"))
		{
		  char *stemp=strstr(s->proc_line,"AUTO");
		  if(stemp)
		    {
		      char *s2 = strdup(stemp);
		      strcpy(stemp+2,s2);
		      memcpy(stemp,"REMOTE",6);
		      free(s2);
		    }
		}
	      if(!strcmp(s->proc_type,"LOCAL"))
		{
		  char *stemp=strstr(s->proc_line,"LOCAL");
		  if(stemp)
		    {
		      char *s2 = strdup(stemp);
		      strcpy(stemp+1,s2);
		      memcpy(stemp,"REMOTE",6);
		      free(s2);
		    }
		  else
		    {
		      stemp=strstr(s->proc_line,"AUTO");
		      if(stemp)
			{
			  char *s2 = strdup(stemp);
			  strcpy(stemp+2,s2);
			  memcpy(stemp,"REMOTE",6);
			  free(s2);
			}
		    }

		}
	      strcpy (s->proc_type, "REMOTE");
	      break;
	    }
	  s->proc_line_found = true;
	  s->proc_line_number = line_number;
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "cms_config found process line on line %d\n",
			   line_number);
	}
#if defined(ENABLE_RCS_NMLCFGSVR)
      else if(s->buffer_line_found && !s->proc_line_found 
	      && (!strncmp(s->buffer_type,"nmlcfgsvr:",10) || 
		  !strncmp(s->buffer_type,"nmlcfgsvr/",10))
	      && sep_words_rep == 4 && line[0] == 'P' &&
	       (!strcmp (word[1], s->procname) || !strcmp (word[1],"default"))
	      && !strcmp (word[2], s->bufname_for_proc_line))
	{
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "cms_config using default process line on line %d\n",
			   line_number);
	  strncpy (s->proc_line, line, CMS_CONFIG_LINELEN);	
	  s->proc_line_found = true;
	  s->proc_line_number = line_number;
	}
#endif

      if (s->proc_line_found && s->buffer_line_found)
	{
	  /* Close the configuration file. */
	  if (NULL != fp)
	    {
#ifdef ENABLE_RCS_INET_FILES
	      inet_file_close (fp);
#else
	      fclose (fp);
#endif
	      fp = NULL;
	    }
	  loading_config_file = 0;
#if defined(ENABLE_RCS_NMLCFGSVR)
	  if(!strncmp(s->buffer_type,"nmlcfgsvr:",10) || 
	     !strncmp(s->buffer_type,"nmlcfgsvr/",10))
	    {
	      break;
	    }
	  else
	    {
	      s->error_type = CONFIG_SEARCH_OK;
	      return;
	    }
#else
	  s->error_type = CONFIG_SEARCH_OK;
	  return;
#endif
	}
    }

  /* Close the configuration file. */
  if (NULL != fp)
    {
#ifdef ENABLE_RCS_INET_FILES
      inet_file_close (fp);
#else
      fclose (fp);
#endif
      fp = NULL;
    }
  loading_config_file = 0;


#if defined(ENABLE_RCS_NMLCFGSVR)
  if (!s->buffer_line_found)
    {
      s->error_type = NO_BUFFER_LINE;
      return;
    }

  if(!strncmp(s->buffer_type,"nmlcfgsvr:",10) || 
     !strncmp(s->buffer_type,"nmlcfgsvr/",10))
    {
      nmlcfgsvr_options=0;
      if(s->proc_line && s->proc_line_found)
	{
	  nmlcfgsvr_options=strstr(s->proc_line,"nmlcfgsvr_options=");
	  if(nmlcfgsvr_options)
	    {
	      nmlcfgsvr_options_copy= (char *) malloc(256);
	      strcpy(nmlcfgsvr_options_copy,nmlcfgsvr_options+18);
	      nmlcfgsvr_options = nmlcfgsvr_options_copy;
	      nmlcfgsvr_options_copy=0;
	    }
	}
      if(s->max_size_from_format > 0 && !strstr(s->buffer_type+10,"size="))
	{
	  if(!nmlcfgsvr_options)
	    {
	      nmlcfgsvr_options= (char *) malloc(256);
	      memset(nmlcfgsvr_options,0,256);
	    }
	  nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			  "max_size_from_format=%lu %s",
			  (unsigned long) s->max_size_from_format, 
			  nmlcfgsvr_options_copy);
	  free(nmlcfgsvr_options_copy);
	  nmlcfgsvr_options_copy=0;
	}
      if(s->set_to_server != 0)
	{
	  if(!nmlcfgsvr_options)
	    {
	      nmlcfgsvr_options= (char *) malloc(256);
	      memset(nmlcfgsvr_options,0,256);
	    }
	  nmlcfgsvr_options_copy = strdup(nmlcfgsvr_options);
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(nmlcfgsvr_options,256),
			  "set_to_server=%d %s",
			  s->set_to_server,nmlcfgsvr_options_copy);
	  free(nmlcfgsvr_options_copy);
	  nmlcfgsvr_options_copy=0;
	}
      ncs_status =
	nmlcfgsvr_create(s->buffer_type+10,
			 s->bufname,
			 s->procname,
			 s->buffer_line,
			 s->proc_line,
			 s->recvstring,sizeof(s->recvstring),
			 s->sendstring,sizeof(s->sendstring),
			 nmlcfgsvr_options);
      if(ncs_status == NMLCFGSVR_CREATE_OK)
	{
	  s->buffer_line_found=true;
	  s->proc_line_found=true;
	  s->proc_line_number=1;
	  s->buffer_line_number=1;
	  sscanf(s->buffer_line,"%*s %*s %s",s->buffer_type);
	  sscanf(s->proc_line,"%*s %*s %*s %s",s->proc_type);
	  s->error_type = CONFIG_SEARCH_OK;
	}
      else
	{
	  s->error_type = MISC_CONFIG_SEARCH_ERROR;
	}
      if(nmlcfgsvr_options && 
	 (nmlcfgsvr_options > (s->proc_line + sizeof(s->proc_line)) ||
	  nmlcfgsvr_options < s->proc_line))
	{
	  free(nmlcfgsvr_options);
	  nmlcfgsvr_options=0;
	}
      return;
    }
      /* end of #if defined(ENABLE_RCS_NMLCFGSVR) */
#endif

  /* Missing either procname or bufname or both. */
  if (!s->buffer_line_found)
    {
      s->error_type = NO_BUFFER_LINE;
      return;
    }

  if (!s->proc_line_found)
    {
      s->error_type = NO_PROCESS_LINE;
      return;
    }
  return;
}


int
cms_create_from_lines_with_buffers(CMS ** cms, 
				   const char *buffer_line, 
				   const char *proc_line,
				   char *wordbuf, 
				   char *proc_type, char *buffer_type,
				   int set_to_server, int set_to_master,
				   enum CMS_CONNECTION_MODE connection_mode)
{
  char *word[4];		/* array of pointers to words from line */

  if (4 != separate_words_with_buffer (word, 4, proc_line,wordbuf,CMS_CONFIG_LINELEN))
    {
      rcs_print_error ("cms_config: invalid proc_line=(%s)\n", proc_line);
      return -1;
    }

  convert2upper (proc_type, word[3], CMS_CONFIG_LINELEN);

  if (4 != separate_words_with_buffer (word, 4, buffer_line,wordbuf,CMS_CONFIG_LINELEN))
    {
      rcs_print_error ("cms_config: invalid buffer_line=(%s)\n", buffer_line);
      return -1;
    }

  convert2upper (buffer_type, word[2], CMS_CONFIG_LINELEN);

  return (cms_create_with_buffers (cms, buffer_line, proc_line,
				   buffer_type, proc_type, 
				   wordbuf, 
				   set_to_server, set_to_master,connection_mode));
}

int
cms_create_with_buffers (CMS ** cms, 
			 const char *buffer_line, 
			 const char *proc_line,
			 char *buffer_type, char *proc_type,
			 char *wordbuf,
			 int set_to_server, int set_to_master,
			 enum CMS_CONNECTION_MODE)
{
  cmscfg_last_buffer_was_ignored_remote=false;
  const char *effective_proc_type;
  if (NULL == cms || NULL == buffer_line || NULL == proc_line ||
      NULL == buffer_type || NULL == proc_type)
    {
      rcs_print_error ("cms_create passed NULL argument.\n");
      return -1;
    }

  /* Both lines have been found, select the appropriate class from */
  /* CMS's derived classes and call its constructor. */
  if (!strcmp (buffer_type, "PHANTOM") || !strcmp (proc_type, "PHANTOM"))
    {
#ifdef ENABLE_RCS_PHANTOM
      *cms = new PHANTOMMEM (buffer_line, proc_line);
      rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%X = new PHANTOMEM(%s,%s)\n",
		       *cms, buffer_line, proc_line);
      if (NULL == *cms)
	{
	  if (verbose_nml_error_messages)
	    {
	      rcs_print_error
		("cms_config: Can't create PHANTOMMEM object.\n");
	    }
	  return (-1);
	}
      else
	{
	  return (0);
	}
#else
      rcs_print_error("The RCS library was compiled without support for PHANTOMMEM.\n");
      return(-1);
#endif      
    }
  effective_proc_type = proc_type;
  if (!strcmp (proc_type, "AUTO"))
    {
      if (hostname_matches_bufferline_with_buffer (buffer_line,wordbuf))
	{
	  effective_proc_type="LOCAL";
	}
      else
	{
	  effective_proc_type="REMOTE";
	}
    }
  
      
  if (!strcmp (effective_proc_type, "REMOTE"))
    {
      if(cmscfg_ignore_remote)
	{
	  bool cloned=false;
	  char *clone_buf_eq=  (char *) strstr(buffer_line,"CLONED_ON=");
	  while(clone_buf_eq)
	    {
	      char *cloned_on_host=clone_buf_eq+10;
	      size_t hostlen=strcspn(cloned_on_host,"\t \r\n:=,");
	      char orig_end_host = *(cloned_on_host+hostlen);
	      *(cloned_on_host+hostlen) = 0;
	      if(dl_address_is_local(cloned_on_host,0))
		{
		  cloned=true;
		  *(cloned_on_host+hostlen) = orig_end_host;
		  clone_buf_eq=0;
		  break;
		}
	      *(cloned_on_host+hostlen) = orig_end_host;
	      clone_buf_eq=strstr(cloned_on_host+hostlen,"CLONED_ON=");
	    }
	  if(!cloned)
	    {
	      cmscfg_last_buffer_was_ignored_remote=true;
	      return -2;
	    }
	}
      if (!strcmp (buffer_type, "OEDSTO") ||
	  ( !strcmp (buffer_type, "OEMEM")
	    && !strstr(buffer_line,"queue") 
	    && !strstr(buffer_line,"QUEUE"))
	  )
	{
#ifdef ENABLE_RCS_OE_INTRF
	  *cms = new OEDSTO (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new OEDSTO(%s,%s,%d,%d)\n", *cms, buffer_line,
			   proc_line, set_to_server, set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new SHMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during SHMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("RCS library was compiled without OE interface support.\n");
	  return(-1);
#endif 
	  // #ifdef ENABLE_RCS_SHMEM

	}
      else if (!strcmp (buffer_type, "OEMQUE") ||
	       ( !strcmp (buffer_type, "OEMEM")
		 && (strstr(buffer_line,"queue") 
		 || strstr(buffer_line,"QUEUE")))
	       )
	{
#ifdef ENABLE_RCS_OE_INTRF
	  *cms = new OEMQUE (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new OEMQUE(%s,%s,%d,%d)\n", *cms, buffer_line,
			   proc_line, set_to_server, set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new SHMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during SHMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("RCS library was compiled without OE interface support.\n");
	  return(-1);
#endif 
	  // #ifdef ENABLE_RCS_SHMEM

	}
      else if (NULL != strstr (proc_line, "serialPortDevName="))
	{
#ifdef ENABLE_RCS_TTY
	  *cms = new TTYMEM (buffer_line, proc_line);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%p = new TTYMEM(%s,%s)\n",
			   (void*) (*cms), buffer_line, proc_line);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new TTYMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Error  %d(%s) occured during TTYMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
#else
	  rcs_print_error ("The RCS libary was compiled without support for TTYMEM  on this platform.\n");
	  return (-1);
#endif
	}
      else if (NULL != strstr (buffer_line, "STCP="))
	{
#ifdef ENABLE_RCS_STCP
	  *cms = new STCPMEM (buffer_line, proc_line);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%p = new STCPMEM(%s,%s)\n",
			   (void *) (*cms), buffer_line, proc_line);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new STPCMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Error  %d(%s) occured during STPCMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
#else
	  rcs_print_error("The RCS library was compiled without support for STCP.\n");
	  return(-1);
#endif
	}
      else if (NULL != strstr (buffer_line, "TCP="))
	{
#ifdef ENABLE_RCS_TCP
	  *cms = new TCPMEM (buffer_line, proc_line);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%p = new TCPMEM(%s,%s)\n",
			   (void *) (*cms), buffer_line, proc_line);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new TPCMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Error  %d(%s) occured during TPCMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
#else
	  rcs_print_error("The RCS library was compiled without support for TCP.\n");
	  return(-1);
#endif
	}
      else if (NULL != strstr (buffer_line, "UDP="))
	{
#ifdef ENABLE_RCS_UDP
	  *cms = new UDPMEM (buffer_line, proc_line);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%p = new UDPMEM(%s,%s)\n",
			   (void *) (*cms), buffer_line, proc_line);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new UDPMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Error %d(%s) occured during UDPMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  return (0);
#else
	  rcs_print_error("The RCS library was compiled without support for UDP.\n");
	  return(-1);
#endif
	}
      else if (NULL != strstr (buffer_line, "GDRS_IM="))
	{
#ifdef ENABLE_RCS_GDRS_IM
	  *cms = new GDRS_IM_CLNT(buffer_line, proc_line);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "%p = new GDRS_IM_CLNT(%s,%s)\n",
			   (void *) (*cms), buffer_line, proc_line);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new GDRS_IMMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Error %d(%s) occured during GDRS_IMMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  return (0);
#else
	  rcs_print_error("The RCS library was compiled without support for GDRS_IM.\n");
	  return(-1);
#endif
	}
      else
	{
	  rcs_print_error ("No remote connection configured that could be supportd.\n");
	  return (-1);
	}
    }
  else if (!strcmp (effective_proc_type, "LOCAL"))
    {
#if defined(VXWORKS) || defined(USE_BIT3) || defined(LINUX_VME) || defined(ENABLE_RCS_GLOBMEM)
      if (!strcmp (buffer_type, "GLOBMEM"))
	{
	  *cms = new GLOBMEM (buffer_line, proc_line, set_to_server,
			      set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new GLOBMEM(%s,%s,%d,%d)\n", *cms,
			   buffer_line, proc_line, set_to_server,
			   set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new GLOBMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during GLOBMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
	}
#endif
#ifdef VXWORKS
#ifdef ENABLE_RCS_BBDMEM

      if (!strcmp (buffer_type, "BBDMEM"))
	{
	  *cms = new BBDMEM (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new BBDMEM(%s,%s,%d,%d)\n", *cms,
			   buffer_line, proc_line, set_to_server,
			   set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new BBDMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during BBDMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
	}
      // end  #ifdef ENABLE_RCS_BBDMEM
#endif
      // end  #ifdef VXWORKS
#endif
      if (!strcmp (buffer_type, "SHMEM"))
	{
#ifdef ENABLE_RCS_SHMEM
	  *cms = new SHMEM (buffer_line, proc_line, set_to_server,
			    set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new SHMEM(%s,%s,%d,%d)\n", 
			   (void *) (*cms), buffer_line,
			   proc_line, set_to_server, set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new SHMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during SHMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("RCS library was compiled without shared memory support.\n");
	  return(-1);
#endif 
	  // #ifdef ENABLE_RCS_SHMEM

	}
      if (!strcmp (buffer_type, "OEDSTO") ||
	  ( !strcmp (buffer_type, "OEMEM")
	    && !strstr(buffer_line,"queue") 
	    && !strstr(buffer_line,"QUEUE"))
	  )
	{
#ifdef ENABLE_RCS_OE_INTRF
	  *cms = new OEDSTO (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new OEDSTO(%s,%s,%d,%d)\n", *cms, buffer_line,
			   proc_line, set_to_server, set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new SHMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during SHMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("RCS library was compiled without OE interface support.\n");
	  return(-1);
#endif 
	  // #ifdef ENABLE_RCS_SHMEM

	}
      else if (!strcmp (buffer_type, "OEMQUE") ||
	       ( !strcmp (buffer_type, "OEMEM")
		 && (strstr(buffer_line,"queue") 
		 || strstr(buffer_line,"QUEUE")))
	       )
	{
#ifdef ENABLE_RCS_OE_INTRF
	  *cms = new OEMQUE (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new OEMQUE(%s,%s,%d,%d)\n", *cms, buffer_line,
			   proc_line, set_to_server, set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new SHMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during SHMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("RCS library was compiled without OE interface support.\n");
	  return(-1);
#endif 
	  // #ifdef ENABLE_RCS_SHMEM

	}
      if (!strcmp (buffer_type, "RTLMEM"))
	{
#ifdef ENABLE_RCS_RTLMEM
	  *cms = new RTLMEM (buffer_line, proc_line, set_to_server,
			     set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new RTLMEM(%s,%s,%d,%d)\n", *cms,
			   buffer_line, proc_line, set_to_server,
			   set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new RTLMEM object.\n");
		}
	      return (-1);
	    }
	  else if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during RTLMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  else
	    {
	      return (0);
	    }
#else
	  rcs_print_error("The RCS library was compiled without support for RTLMEM.\n");
	  return(-1);
#endif
	  // # ifdef ENABLE_RCS_RTLMEM
	}

      if (!strcmp (buffer_type, "LOCMEM"))
	{
#ifdef ENABLE_RCS_LOCMEM
	  *cms =
	    new LOCMEM (buffer_line, proc_line, set_to_server, set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new LOCMEM(%s,%s,%d,%d)\n", 
			   (void *) (*cms),
			   buffer_line, proc_line, set_to_server,
			   set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new LOCMEM object.\n");
		}
	      return (-1);
	    }
	  if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during LOCMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  return (0);
#else
	  rcs_print_error("The RCS library was compiled without support for LOCMEM.\n");
	  return(-1);
#endif

	}

      if (!strcmp (buffer_type, "FILEMEM"))
	{
#ifdef ENABLE_RCS_FILEMEM
	  *cms =
	    new FILEMEM (buffer_line, proc_line, set_to_server,
			 set_to_master);
	  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
			   "%p = new FILEMEM(%s,%s,%d,%d)\n", 
			   (void *) (*cms),
			   buffer_line, proc_line, set_to_server,
			   set_to_master);
	  if (NULL == *cms)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: Can't create new FILEMEM object.\n");
		}
	      return (-1);
	    }
	  if ((*cms)->status < 0)
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("cms_config: %d(%s) Error occured during FILEMEM create.\n",
		     (*cms)->status, (*cms)->status_string ((*cms)->status));
		}
	      return (-1);
	    }
	  return (0);
#else
	  rcs_print_error("RCS library was built without FILEMEM support.\n");
#endif
	}
      rcs_print_error ("cms_config: invalid buffer_type (%s)\n", buffer_type);
      rcs_print_error ("cms_config: buffer_line = (%s)\n", buffer_line);
      return (-1);
    }
  else
    {
      rcs_print_error ("cms_config: invalid proc_type (%s)\n", proc_type);
      rcs_print_error ("cms_config: proc_line = (%s)\n", proc_line);
      return (-1);
    }
  return (0);
}
