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
* File: nml.cc                                                           *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for the  Neutral Messaging Language (NML).           *
*          Includes:                                                     *
*                    1. Member functions for class NML.                  *
*************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "nml_no_config.h"
#endif
// HAVE_CONFIG_H

#include "rcsvers.hh"		// rcs_version_printed, print_rcs_version()
#include "nml.hh"		/* class NML */
#include "nmlmsg.hh"		/* class NMLmsg */
#include "cms.hh"		/* class CMS */
#include "timer.hh"		// esleep()
#include "cms_cfg.hh"		/* cms_config(), cms_copy() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */
#include "physmem.hh"		// class PHYSMEM_HANDLE
#include "nml_c_data.hh"	// class NML_C_DATA

#ifdef ENABLE_RCS_SERVER
#include "nml_srv.hh"		/* NML_Default_Super_Server */
#endif

#ifdef ENABLE_RCS_DIAG
#include "nmldiag.hh"		// NML_DIAGNOSTICS_INFO
#endif

#include "cms_up.hh"

#ifdef ENABLE_RCS_XML
#include "cms_xml_up.hh"
#endif

#if defined(ENABLE_RCS_NMLCFGSVR)
#include "nmlcfgsvr_clntcalls.hh"
#endif

#ifdef ENABLE_RCS_SOKINTRF
#include "sokintrf.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#endif

#ifdef ENABLE_RCS_TCP
#include "rem_msg.hh"
#include "ntohhton.hh"
#include "recvn.h"
#include "sendn.h"
#endif

#include <string>
#include <iostream>
#include <sstream>

class NML_EXTRA_THREAD_INFO
{
public:
#ifdef POSIX_THREADS
  pthread_t pthread_threadId;
#endif
};

class NML_EXTRA_STRING_BUFFERS
{
public:
  char linebuf[CMS_CONFIG_LINELEN];
  char wordbuf[CMS_CONFIG_LINELEN];
  char proc_type[CMS_CONFIG_LINELEN];
  char buffer_type[CMS_CONFIG_LINELEN];
};

#if defined(ENABLE_RCS_NMLCFGSVR)
class NMLCFG_SERVER_DATA
{
public:
  char buffer_line[CMS_CONFIG_LINELEN];
  char options_line[CMS_CONFIG_LINELEN];
  char process_line[CMS_CONFIG_LINELEN];
  enum NMLCFGSVR_STATUS ncs_status;
};
#endif

class NML_INTERNALS
{
public:
  NML_INTERNALS();
  ~NML_INTERNALS();
  int channel_list_id;		/* List id of this channel. */
  int pid;
  int remote_to_local_reader_pid;
  NMLmsg * diffMsg;
  NMLmsg *lastMessageWritten;
  char bufname[40];
  char procname[40];
  char cfgfilename[160];
  double blocking_read_poll_interval;
  CMS *cms_for_msg_string_conversions;
  int registered_with_server;
  SchemaGenMsg *msgForSchemaGen;
  char xmllogfilename[256];
  int xmllogcount;
  RCS_LINKED_LIST *xml_style_properties;
  class NML_EXTRA_STRING_BUFFERS *extra_bufs;
  int global_xml_style_properties_count;
  bool max_size_from_format_set;
  unsigned long max_size_from_format;
  enum NML_CONNECTION_MODE connect_mode;
#if defined(ENABLE_RCS_NMLCFGSVR)
  class NMLCFG_SERVER_DATA *cfgsvr_data;
#endif
#ifdef _Windows
  // HANDLE replaced with void *
  void *task_handle;
#endif
  size_t min_message_size;
  size_t message_size_add;
  size_t message_size_roundup;
  bool use_cms_for_read_for_format_output;
  bool use_cms_for_read_for_run_format_chain;
  bool get_msg_type_only;
  char recvstring[CMS_CONFIG_LINELEN*2];
  char sendstring[CMS_CONFIG_LINELEN*2];
  char buffer_line[CMS_CONFIG_LINELEN*2];
  char proc_line[CMS_CONFIG_LINELEN*2];
  void *file_vBufP;
  unsigned long file_vBufP_size;
  bool name_from_format_set;
  const char *name_from_format;
  NMLTYPE required_type;

  class NML_INTERNALS *loopback;
  
private:
  // Prevent copying.
  NML_INTERNALS(const NML_INTERNALS &_ni);
  NML_INTERNALS &operator=(const NML_INTERNALS &_ni);
};


NML_INTERNALS::NML_INTERNALS():
  channel_list_id(0),
  pid(0),
  remote_to_local_reader_pid(0),
  diffMsg(0),
  lastMessageWritten(0),
  blocking_read_poll_interval(0),
  cms_for_msg_string_conversions(0),
  registered_with_server(0),
  msgForSchemaGen(0),
  xmllogcount(0),
  xml_style_properties(0),
  extra_bufs(0),
  global_xml_style_properties_count(0),
  max_size_from_format_set(false),
  max_size_from_format(0),
  connect_mode(NML_NORMAL_CONNECTION_MODE),
#if defined(ENABLE_RCS_NMLCFGSVR)
  cfgsvr_data(0),
#endif
#ifdef _Windows
  task_handle(0),
#endif
  min_message_size(sizeof(NMLmsg)),message_size_add(0),message_size_roundup(0),
  use_cms_for_read_for_format_output(false),
  use_cms_for_read_for_run_format_chain(false),
  get_msg_type_only(false),
  file_vBufP(0),
  file_vBufP_size(0),
  name_from_format_set(false),
  name_from_format(0),
  loopback(0)
{
  memset(this,0,sizeof(NML_INTERNALS));
  blocking_read_poll_interval = -1.0;
  loopback = this;

#ifdef ENABLE_RCS_SOKINTRF
  load_socket_interface();
#endif

}

NML_INTERNALS::~NML_INTERNALS()
{
  if(cms_for_msg_string_conversions)
    {
      delete cms_for_msg_string_conversions;
      cms_for_msg_string_conversions=0;
    }
  if(extra_bufs)
    {
      delete extra_bufs;
      extra_bufs=0;
    }
  if(xml_style_properties)
    {
      delete xml_style_properties;
      xml_style_properties=0;
    }
#if defined(ENABLE_RCS_NMLCFGSVR)
  if(cfgsvr_data)
    {
      delete cfgsvr_data;
      cfgsvr_data=0;
    }
#endif

  if(file_vBufP && file_vBufP_size > 0)
    {
      free(file_vBufP);
      file_vBufP=0;
    }

  memset(this,0,sizeof(NML_INTERNALS));
  blocking_read_poll_interval = -1.0;
  loopback = 0;

#ifdef ENABLE_RCS_SOKINTRF
  unload_socket_interface();
#endif
}  


bool nml_allow_null_f_ptr=false;
bool nml_cleanup_started=false;

/* Pointer to a global list of NML channels. */
RCS_LINKED_LIST *NML_Main_Channel_List =
  (RCS_LINKED_LIST *) NULL;

#ifdef POSIX_THREADS
static pthread_mutex_t list_mutex=PTHREAD_MUTEX_INITIALIZER;
#endif


bool nml_print_hostname_on_error = false;
bool verbose_nml_error_messages = true;

static char NML_ERROR_TYPE_STRINGS[17][80] = {
  "NML_NO_ERROR",
  "NML_BUFFER_NOT_READ",
  "NML_TIMED_OUT",
  "NML_INVALID_CONFIGURATION",
  "NML_FORMAT_ERROR",
  "NML_INTERNAL_CMS_ERROR",
  "NML_NO_MASTER_ERROR",
  "NML_INVALID_MESSAGE_ERROR",
  "NML_QUEUE_FULL_ERROR",
  "NML_INVALID_CONSTRUCTOR_ARG",
  "NML_INTERRUPTED_OPERATION",
  "NML_NO_FORMAT_FUNCTION",
  "NML_OUT_OF_MEMORY_ERROR",
  "NML_IGNORED_REMOTE",
  "NML_IGNORED_NO_BUFLINE",
  "NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR",
  "NML_REQUIRED_MSG_TYPE_ERROR"
};

static inline size_t round_up_size(size_t val, size_t round_number)
{
  size_t r1;
  size_t mod1;
  size_t lfinal;
  size_t retval;
  r1 = val + round_number;
  mod1 = r1%round_number;
  lfinal = r1 - mod1;
  retval = (size_t) lfinal;
  return retval;
}

static char *default_nml_config_file = NULL;
int nml_reset_errors_printed = 1;

void
set_default_nml_config_file (const char * cfg_file)
{
  if (cfg_file == NULL)
    {
      default_nml_config_file = NULL;
    }
  default_nml_config_file = (char *) DEBUG_MALLOC (strlen (cfg_file) + 1);
  strcpy (default_nml_config_file, cfg_file);
}

const char *
get_default_nml_config_file ()
{
  return default_nml_config_file;
}

class SchemaGenMsg: public NMLmsg
{
public:
  SchemaGenMsg ():NMLmsg (999, sizeof (SchemaGenMsg))
  {
    type = 0;
  };
  void update (CMS *)
  {
  };

};

/*
* NML Member Functions
*/

#if NML_NEW_DELETE
/* Special new operator to allow the nml_cleanup function to
distinguish between dynamically and statically allocated NML
objects. */
void *
  NML::operator
new (size_t size)
{
  set_rcs_print_tag(0); 
  if (size < sizeof (NML))
    {
      rcs_print_error
	("void *NML::operator new() called with size (%ld) < sizeof(NML) (%ld) the code calling NML was probably not compiled with the correct header file version.\n",
	 size, sizeof (NML));
      size = sizeof (NML);
    }
  void *nml_space = NULL;
  nml_space = DEBUG_MALLOC (size + sizeof (int) * 2);
  if (NULL != nml_space)
    {
      memset (nml_space, 0, size);
    }
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "%p = NML::operater new(%ld)\n",
		   nml_space, size);
  return nml_space;
}

void
  NML::operator
delete (void *nml_space)
{
  set_rcs_print_tag(0); 
  rcs_print_debug (PRINT_NML_DESTRUCTORS, "NML::operater delete(%p)\n",
		   nml_space);

  if (NULL == nml_space)
    {
      return;
    }
  DEBUG_FREE (nml_space);
}
#endif

static const char *FORCE_NML_CONFIG=0;
static bool force_nml_config_checked = false;

#ifdef LINUXCNC_LIBNML_COMPAT
NML::NML (NML_FORMAT_PTR f_ptr,
       const char *buf, const char *proc, const char *file,
	  int set_to_server, int set_to_master) :
 ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  set_rcs_print_tag(buf); 
  ni = new NML_INTERNALS();
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      set_rcs_print_tag(0); 
      return;
    }
  ni->connect_mode = NML_NORMAL_CONNECTION_MODE;

  init_rcs_print_mode_flags();

  if(!force_nml_config_checked) {
    FORCE_NML_CONFIG=getenv("FORCE_NML_CONFIG");
    if(FORCE_NML_CONFIG) {
      rcs_print("NML::NML(...) Config file of \"%s\" overridden by FORCE_NML_CONFIG environment variable to \"%s\"\n",file,FORCE_NML_CONFIG);
    }
    force_nml_config_checked=true;
  }
  if(FORCE_NML_CONFIG) {
    file = FORCE_NML_CONFIG;
  }

  rcs_print_debug (PRINT_NML_CONSTRUCTORS,
		   "NML::NML(%p,%s,%s,%s,%d,%d)\n",
		   (void *) f_ptr, buf, proc, file, set_to_server, set_to_master);
  

  loopback=this;
  extra_thread_info =0;
  info_printed = 0;
  forced_type = 0;
  interrupting_operation=false;
  leave_resource=false;
  immediate_spawned_server = 0;

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
    }
#endif

  if (0 != buf)
    {
      strncpy (ni->bufname, buf, sizeof(ni->bufname));
    }
  if (0 != proc)
    {
      strncpy (ni->procname, proc, sizeof(ni->procname));
    }
  if (NULL == file)
    {
      file = default_nml_config_file;
    }
  if (0 != file)
    {
      strncpy (ni->cfgfilename, file, sizeof(ni->cfgfilename));
    }


#ifndef DISABLE_RCS_PRINT
  if (rcs_errors_printed >= max_rcs_errors_to_print
      && max_rcs_errors_to_print > 0 && nml_reset_errors_printed)
    {
      rcs_errors_printed = 0;
      rcs_print
	("\nResetting rcs_errors_printed because a new NML channel is being created.\n");
    }
#endif


  already_deleted = 0;
  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);

  reconstruct (f_ptr, buf, proc, file, set_to_server, set_to_master);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
      check_format_name();
    }
  setup_cc_bufs();
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "finished NML constructor.\n");
  set_rcs_print_tag(0); 
}
 
#endif  // LINUXCNC_LIBNML_COMPAT

/******************************************************************
* Constructor for NML:
* Parameters:
* NML_FORMAT_PTR f_ptr - address of the function to be used to format messages.
* char *buf - Name of the buffer to connect to as written in the config file.
* char *proc - Name of the calling process as expected in the config file.
* char *file - Name of the configuration file.
* int set_to_server - If 1 this NML will consider its calling process to
*   be an NML_SERVER, which effects how and when messages are encoded and
*   decoded.
* int set_to_master - Passed to the CMS constructor - how this is used
*   depends on the type of CMS buffer. In general the master is responsible
*   for creating/initializing the buffer. If set_to_master == 1 then this
*   process will be considered the master, if set_to_master == 0 then
*   the the configuration file determines if this is the master, and it
*   set_to_master == -1 then this will not be the master.
* NOTES:
*  1. Borland C++(for DOS and Windows) does not allow default
*   parameters to be specified both here and in the header file.
*  2. All pointers are first initialized to NULL so that it can be determined
*  later if the constructor returned before creating the objects
*  the pointers are intended to point at.
******************************************************************/
NML::NML (NML_FORMAT_PTR f_ptr,
	  const char *buf, const char *proc,
	  const char *file, int set_to_server, int set_to_master, 
	  enum NML_CONNECTION_MODE _connect_mode):
  ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  set_rcs_print_tag(buf); 
  ni = new NML_INTERNALS();
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      set_rcs_print_tag(0); 
      return;
    }
  ni->connect_mode = _connect_mode;

  init_rcs_print_mode_flags();

  if(!force_nml_config_checked) {
    FORCE_NML_CONFIG=getenv("FORCE_NML_CONFIG");
    if(FORCE_NML_CONFIG) {
      rcs_print("NML::NML(...) Config file of \"%s\" overridden by FORCE_NML_CONFIG environment variable to \"%s\"\n",file,FORCE_NML_CONFIG);
    }
    force_nml_config_checked=true;
  }
  if(FORCE_NML_CONFIG) {
    file = FORCE_NML_CONFIG;
  }

  rcs_print_debug (PRINT_NML_CONSTRUCTORS,
		   "NML::NML(%p,%s,%s,%s,%d,%d)\n",
		   (void *) f_ptr, buf, proc, file, set_to_server, set_to_master);
  

  loopback=this;
  extra_thread_info =0;
  info_printed = 0;
  forced_type = 0;
  interrupting_operation=false;
  leave_resource=false;
  immediate_spawned_server = 0;

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
    }
#endif

  if (0 != buf)
    {
      strncpy (ni->bufname, buf, sizeof(ni->bufname));
    }
  if (0 != proc)
    {
      strncpy (ni->procname, proc, sizeof(ni->procname));
    }
  if (NULL == file)
    {
      file = default_nml_config_file;
    }
  if (0 != file)
    {
      strncpy (ni->cfgfilename, file, sizeof(ni->cfgfilename));
    }


#ifndef DISABLE_RCS_PRINT
  if (rcs_errors_printed >= max_rcs_errors_to_print
      && max_rcs_errors_to_print > 0 && nml_reset_errors_printed)
    {
      rcs_errors_printed = 0;
      rcs_print
	("\nResetting rcs_errors_printed because a new NML channel is being created.\n");
    }
#endif


  already_deleted = 0;
  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);

  reconstruct (f_ptr, buf, proc, file, set_to_server, set_to_master);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
      check_format_name();
    }
  setup_cc_bufs();
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "finished NML constructor.\n");
  set_rcs_print_tag(0); 
}

static bool ignore_format_name=false;

void
nmlSetIgnoreFormatName(void)
{
  ignore_format_name=true;
}

void
NML::check_format_name()
{
  ni->required_type =-1;
  set_rcs_print_tag(0); 
  if (NULL == cms || ignore_format_name)
    {
      return;
    }
  set_rcs_print_tag(cms->BufferName); 
  const char *fn = get_name_from_format();
  if(!fn)
    {
      return;
    }
  set_rcs_print_tag(cms->BufferName); 
  static char temp_fname[CMS_CONFIG_LINELEN];
  if(strstr(cms->proclineupper,"IGNORE_FORMAT_NAME"))
    {
      return;
    }
  if(getenv("IGNORE_FORMAT_NAME")) {
    return;
  }

  char *format_name_eq = strstr (cms->BufferLine, "format_name=");
  if (format_name_eq != NULL)
    {
      strncpy(temp_fname,format_name_eq+12,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(strcmp(temp_fname,fn))
	{
	  rcs_print_error("Format name from config does not match name passed to CMS::check_type_info() %s != %s\n",
			  temp_fname,fn);
	  rcs_print_error("format_name_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  format_name_eq,
			  cms->BufferLine);
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  format_name_eq = strstr (cms->BufferLine, "FORMAT_NAME=");
  if (format_name_eq != NULL)
    {
      strncpy(temp_fname,format_name_eq+12,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(strcmp(temp_fname,fn))
	{
	  rcs_print_error("Format name from config does not match name passed to CMS::check_type_info() %s != %s\n",
			  temp_fname,fn);
	  rcs_print_error("format_name_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  format_name_eq,
			  cms->BufferLine);
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  char *format_source_eq = strstr (cms->BufferLine, "format_source=");
  if (format_source_eq != NULL)
    {
      strncpy(temp_fname,format_source_eq+14,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(!strstr(temp_fname,fn))
	{
	  rcs_print_error("Format_Source name from config does not contain name passed to CMS::check_type_info() %s does not contain %s\n",
			  temp_fname,fn);
	  rcs_print_error("format_source_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  format_source_eq,
			  cms->BufferLine);
	  rcs_print_error("Use format_name= setting or check the code that calls the constructor/sets the format function.\n");
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  format_source_eq = strstr (cms->BufferLine, "FORMAT_SOURCE=");
  if (format_source_eq != NULL)
    {
      strncpy(temp_fname,format_source_eq+14,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(!strstr(temp_fname,fn))
	{
	  rcs_print_error("Format_Source name from config does not contain name passed to CMS::check_type_info() %s does not contain %s\n",
			  temp_fname,fn);
	  rcs_print_error("format_source_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  format_source_eq,
			  cms->BufferLine);
	  rcs_print_error("Use format_name= setting or check the code that calls the constructor/sets the format function.\n");
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  char *header_eq = strstr (cms->BufferLine, "header=");
  if (header_eq != NULL)
    {
      strncpy(temp_fname,header_eq+7,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(!strstr(temp_fname,fn))
	{
	  rcs_print_error("Header name from config does not contain name passed to CMS::check_type_info() %s does not contain %s\n",
			  temp_fname,fn);
	  rcs_print_error("header_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  header_eq,
			  cms->BufferLine);
	  rcs_print_error("Use format_name= setting or check the code that calls the constructor/sets the format function.\n");
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  header_eq = strstr (cms->BufferLine, "HEADER=");
  if (header_eq != NULL)
    {
      strncpy(temp_fname,header_eq+7,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(!strstr(temp_fname,fn))
	{
	  rcs_print_error("Header name from config does not contain name passed to CMS::check_type_info() %s does not contain %s\n",
			  temp_fname,fn);
	  rcs_print_error("header_eq=\"%s\", cms->BufferLine=\"%s\"\n",
			  header_eq,
			  cms->BufferLine);
	  rcs_print_error("Use format_name= setting or check the code that calls the constructor/sets the format function.\n");
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR;
	}
      return;
    }
  char *msg_type_eq = strstr (cms->BufferLine, "msg_type=");
  if (msg_type_eq != NULL)
    {
      strncpy(temp_fname,msg_type_eq+9,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(temp_fname[0] >= '0' && temp_fname[0] <= '9')
	{
	  ni->required_type = (NMLTYPE) atoi(temp_fname);
	}
      else
	{
	  ni->required_type = get_type_id_for_name(temp_fname);
	}
      if(ni->required_type < 0)
	{
	  rcs_print_error("%s in the config sets a message type that is not included when the format function calls CMS::check_type_info().\n",
			  msg_type_eq);
	  rcs_print_error("cms->BufferLine=\"%s\"\n",
			  cms->BufferLine);
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_REQUIRED_MSG_TYPE_ERROR;
	}
      return;
    }
  msg_type_eq = strstr (cms->BufferLine, "MSG_TYPE=");
  if (msg_type_eq != NULL)
    {
      strncpy(temp_fname,msg_type_eq+9,sizeof(temp_fname));
      size_t s = strcspn(temp_fname,"\t \r\n,;");
      if(s > 0 && s < sizeof(temp_fname))
	{
	  *(temp_fname+s)=0;
	}
      if(temp_fname[0] >= '0' && temp_fname[0] <= '9')
	{
	  ni->required_type = (NMLTYPE) atoi(temp_fname);
	}
      else
	{
	  ni->required_type = get_type_id_for_name(temp_fname);
	}
      if(ni->required_type < 0)
	{
	  rcs_print_error("%s in the config sets a message type that is not included when the format function calls CMS::check_type_info().\n",
			  msg_type_eq);
	  rcs_print_error("cms->BufferLine=\"%s\"\n",
			  cms->BufferLine);
	  cms->status = CMS_MISC_ERROR;
	  error_type=NML_REQUIRED_MSG_TYPE_ERROR;
	}
      return;
    }
}


int
create_NML (NML ** nml, NML_FORMAT_PTR f_ptr,
	    char *buf, char *proc, char *file)
{
  set_rcs_print_tag(buf); 
  *nml = new NML (f_ptr, buf, proc, file);
  if (NULL == nml)
    {
      set_rcs_print_tag(0); 
      return -1;
    }
  if (!(*nml)->valid ())
    {
      set_rcs_print_tag(0); 
      return -1;
    }
  if (NULL != (*nml)->cms)
    {
      (*nml)->cms->sizeof_message_header = (*nml)->sizeof_message_header;
      char *forced_type_eq =
	strstr ((*nml)->cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {

	      (*nml)->forced_type = temp;
	    }
	}
    }
  set_rcs_print_tag(0); 
  return 0;
}

void
free_NML (NML * nml)
{
  set_rcs_print_tag(0); 
  if (NULL != nml)
    {
      delete nml;
    }
}

int
NML::login (const char *name, const char *passwd)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  set_rcs_print_tag(cms->BufferName); 
  int rval = cms->login (name, passwd);
  set_rcs_print_tag(0); 
  return rval;
}

int
NML::wait_for_anything(double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  set_rcs_print_tag(cms->BufferName); 
  int rval = cms->wait_for_anything(timeout);
  set_rcs_print_tag(0); 
  return(rval);
}


int
NML::wait_for_read(double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  set_rcs_print_tag(cms->BufferName); 
  int rval = cms->wait_for_read(timeout);
  set_rcs_print_tag(0); 
  return(rval);
}

int
NML::wait_for_clear(double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  set_rcs_print_tag(cms->BufferName); 
  int rval = cms->wait_for_clear(timeout);
  set_rcs_print_tag(0); 
  return(rval);
}

int
NML::wait_for_write(double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  set_rcs_print_tag(cms->BufferName); 
  int rval = cms->wait_for_write(timeout);
  set_rcs_print_tag(0); 
  return(rval);
}

int
NML::wait_for_queue_length_over(int _ql, double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  return cms->wait_for_queue_length_over(_ql,timeout);
}

int
NML::wait_for_queue_length_under(int _ql, double timeout)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return 1;
    }
  return cms->wait_for_queue_length_under(_ql,timeout);
}

static bool set_to_server_env_checked=false;
static bool use_set_to_server_override=false;
static int set_to_server_override=0;
const char *set_to_server_env_ptr=0;

void
nmlSetToServer(void)
{
  set_rcs_print_tag(0); 
  set_to_server_env_checked=true;
  use_set_to_server_override=true;
  set_to_server_override=1;
}

void nmlSetIgnoreRemote(void)
{
  set_cmscfg_ignore_remote(1);
}

void nmlSetIgnoreNoBufline(void)
{
  set_cmscfg_ignore_no_bufline(1);
}

size_t NML::forced_min_size = 0;
bool NML::forced_min_size_env_checked = false;

unsigned long
NML::get_max_size_from_format(void)
{
  set_rcs_print_tag(0); 
  if(!forced_min_size_env_checked) {
    char *env = getenv("NML_FORCED_MIN_SIZE");
    if(env) {
      forced_min_size = atoi(env);
    }
    forced_min_size_env_checked=true;
  }

  if(!ni)
    {
      return 0;
    }
  if(!format_chain)
    {
      ni->max_size_from_format_set=false;
      ni->max_size_from_format=0;
      return 0;
    }
  if(ni->max_size_from_format_set)
    {
      return ni->max_size_from_format;
    }
  CMS * orig_cms = cms;
  CMS *cms_to_get_size=0;
  cms_to_get_size=new CMS(0);
  if(!cms_to_get_size)
    {
      ni->max_size_from_format_set=false;
      ni->max_size_from_format=0;
      return 0;
    }
  cms_to_get_size->searching_for_max_size_from_size_list=true;
  cms = cms_to_get_size;
  run_format_chain(0,0);
  ni->max_size_from_format = (unsigned long) cms_to_get_size->max_size_from_size_list;
  ni->max_size_from_format_set = cms_to_get_size->max_size_from_size_list_set;
  cms_to_get_size->searching_for_max_size_from_size_list=false;
  cms=0;
  delete cms_to_get_size;
  cms_to_get_size=0;
  cms=orig_cms;
  if(ni->max_size_from_format < (unsigned int) forced_min_size ) {
    ni->max_size_from_format = (unsigned int) forced_min_size;
  }
  return ni->max_size_from_format;
}

NMLTYPE
NML::get_type_id_for_name(const char *name)
{
  set_rcs_print_tag(0); 
  if(!ni)
    {
      return 0;
    }
  if(!format_chain)
    {
      return 0;
    }
  CMS * orig_cms = cms;
  CMS *cms_to_get_type_id=0;
  cms_to_get_type_id=new CMS(0);
  if(!cms_to_get_type_id)
    {
      ni->name_from_format_set=false;
      ni->name_from_format=0;
      return 0;
    }
  cms_to_get_type_id->name_to_lookup_type_id=name;
  cms_to_get_type_id->type_id_looked_up=-1;

  cms = cms_to_get_type_id;
  run_format_chain(0,0);
  cms_to_get_type_id->name_to_lookup_type_id=0;
  NMLTYPE t = cms_to_get_type_id->type_id_looked_up;
  cms_to_get_type_id->type_id_looked_up=-1;
  cms=0;
  delete cms_to_get_type_id;
  cms_to_get_type_id=0;
  cms=orig_cms;
  return t;
}


const char *
NML::get_name_from_format(void)
{
  set_rcs_print_tag(0); 
  header_file_name=0;
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName); 
    }
  if(!ni)
    {
      return 0;
    }
  if(!format_chain)
    {
      ni->name_from_format_set=false;
      ni->name_from_format=0;
      return 0;
    }
  if(ni->name_from_format_set)
    {
      return ni->name_from_format;
    }
  CMS * orig_cms = cms;
  CMS *cms_to_get_name=0;
  cms_to_get_name=new CMS(0);
  if(!cms_to_get_name)
    {
      ni->name_from_format_set=false;
      ni->name_from_format=0;
      return 0;
    }
  cms_to_get_name->checking_for_nsname_from_format_check_type_info=true;
  cms = cms_to_get_name;
  run_format_chain(0,0);
  ni->name_from_format = cms_to_get_name->nsname_from_format_check_type_info;
  ni->name_from_format_set = (ni->name_from_format != 0);
  header_file_name = cms->get_header_file();
  uses_unbounded = cms->get_uses_unbounded();
  cms_to_get_name->checking_for_nsname_from_format_check_type_info=false;
  cms=0;
  delete cms_to_get_name;
  cms_to_get_name=0;
  cms=orig_cms;
  set_rcs_print_tag(0); 
  return ni->name_from_format;
}

static const char *default_nmlcfgsvr_options = 0;

void
set_default_nmlcfgsvr_options(const char *_default_nmlcfgsvr_options) {
  if(!default_nmlcfgsvr_options) {
    free((void *)default_nmlcfgsvr_options);
    default_nmlcfgsvr_options = 0;
  }
  if(_default_nmlcfgsvr_options == 0) {
    return;
  }
  default_nmlcfgsvr_options = 
    strdup(_default_nmlcfgsvr_options);
}

int
NML::cfgsvr_query(
#if defined(ENABLE_RCS_NMLCFGSVR)
		  const char *buf,
		  const char *proc, 
		  const char *cfgsvr,
		  int set_to_server
#else
		  __unused_parameter__ const char *,
		  __unused_parameter__ const char *, 
		  __unused_parameter__ const char *,
		  __unused_parameter__ int
#endif
		  )
{
#if defined(ENABLE_RCS_NMLCFGSVR)
  int cms_config_ret;
  get_max_size_from_format();

  set_rcs_print_tag(buf); 

  cms_config_ret=-99;
  if(0 == ni->cfgsvr_data)
    {
      ni->cfgsvr_data = new NMLCFG_SERVER_DATA();
    }
  
  std::ostringstream options;
  memset(ni->cfgsvr_data->options_line,0,sizeof(ni->cfgsvr_data->options_line));
  if(ni->max_size_from_format > 8 && ni->max_size_from_format_set) {
    options << " max_size_from_format="
	    << ni->max_size_from_format;
  }

  const char *fn = get_name_from_format();
  if(fn) {							
    options << " format_name=" << fn;
  }

  fn = header_file_name;
  if(fn) {	
    options << " header=" << fn;
  }

  if(uses_unbounded) {
    options << " neutral=1";
  }
  
  if(set_to_server != 0) {
    options << " set_to_server=" << set_to_server;
  }
  
  if(default_nmlcfgsvr_options != 0) {
    options << " " << default_nmlcfgsvr_options;
  }

  const char *NMLCFGSVR_OPTIONS_ENV = getenv("NMLCFGSVR_OPTIONS");
  if(NMLCFGSVR_OPTIONS_ENV) {
    options << " " << NMLCFGSVR_OPTIONS_ENV;
  }


  ni->cfgsvr_data->ncs_status = NMLCFGSVR_STATUS_NOT_SET;
  std::string options_string = options.str();
  std::cout << "options_string = " << options_string << std::endl;
  const char *options_cP = options_string.c_str();
  size_t options_cP_len = strlen(options_cP);
  if(options_cP_len > sizeof(ni->cfgsvr_data->options_line)) {
    rcs_print_error("nmlcfgsvr options length exceeded (%d > %d in %s).\n",
		    (int) (options_cP_len), 
		    (int) (sizeof(ni->cfgsvr_data->options_line)),
		    options_cP);
  }
  strncpy(ni->cfgsvr_data->options_line,
	  options_cP,
	  sizeof(ni->cfgsvr_data->options_line));
  printf("ni->cfgsvr_data->options_line=%s\n",
	 ni->cfgsvr_data->options_line);
  ni->cfgsvr_data->ncs_status =
    nmlcfgsvr_create(cfgsvr,
		     buf,proc,
		     ni->cfgsvr_data->buffer_line,
		     ni->cfgsvr_data->process_line,
		     ni->recvstring,sizeof(ni->recvstring),
		     ni->sendstring,sizeof(ni->sendstring),
		     ni->cfgsvr_data->options_line,
		     -1.0, /* double timeout */
		     0, /* const char *domainset */
		     true /* bool print_errors */ );
  if(ni->cfgsvr_data->ncs_status == NMLCFGSVR_CREATE_OK)
    {
      set_rcs_print_tag(ni->cfgsvr_data->buffer_line); 
      cms_config_ret=
	cms_create_from_lines_with_buffers (&cms, 
					    ni->cfgsvr_data->buffer_line, 
					    ni->cfgsvr_data->process_line,
					    ni->extra_bufs->wordbuf,
					    ni->extra_bufs->proc_type,
					    ni->extra_bufs->buffer_type,
					    set_to_server);
      if(cms_config_ret == -1)
	{
	  if (verbose_nml_error_messages)
	    {
	      rcs_print_error ("NML: cms_create_from_lines returned -1.\n");
	    }
	  if (!info_printed && 
	      !get_cmscfg_last_buffer_was_ignored_remote() &&
	      !get_cmscfg_last_buffer_was_ignored_no_bufline())
	    {
	      print_info();
	    }
	  if (NULL != cms)
	    {
	      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
			       (void *)cms);
	      delete cms;
	      cms = (CMS *) NULL;
	    }
	  error_type = NML_INVALID_CONFIGURATION;
	  set_rcs_print_tag(0); 
	  return -1;
	}
      else if(cms_config_ret == -2)
	{
	  error_type = NML_IGNORED_REMOTE;
	  set_rcs_print_tag(0); 
	  return -1;
	}
      else if(cms && cms->cloned_buffer && (cms->ProcessType == CMS_REMOTE_TYPE || cms->ProcessType == CMS_AUTO_TYPE))
	{
	  if(!cms->isserver && !set_to_server)
	    {
	      if(!cms->ignore_cloned_buff)
		{
		  if(cms_create_from_lines_with_buffers (&cms_for_read, 
							 ni->cfgsvr_data->buffer_line, 
							 ni->cfgsvr_data->process_line,
							 ni->extra_bufs->wordbuf,
							 ni->extra_bufs->proc_type,
							 ni->extra_bufs->buffer_type,
							 0,0,CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		    {
		      if(cms_for_read)
			{
			  delete cms_for_read;
			  cms_for_read=0;
			}
		    }
		  if(cms_for_read->status == CMS_NO_MASTER_ERROR)
		    {
		      delete cms_for_read;
		      cms_for_read=0;
		    }
		}
	    }
	  else
	    {
	      delete cms;
	      cms=0;
	      if(cms_create_from_lines_with_buffers (&cms, 
						     ni->cfgsvr_data->buffer_line, 
						     ni->cfgsvr_data->process_line,
						     ni->extra_bufs->wordbuf,
						     ni->extra_bufs->proc_type,
						     ni->extra_bufs->buffer_type,
						     1,0,CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		{
		  if (NULL != cms)
		    {
		      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
				       (void *)cms);
		      delete cms;
		      cms = (CMS *) NULL;
		    }
		  error_type = NML_INVALID_CONFIGURATION;
		  set_rcs_print_tag(0); 
		  return -1;
		}
	      
	    }
	}
    }
  else
    {
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return -1;
    }
  set_rcs_print_tag(0); 
  return 0;
#else
  rcs_print_error("NML::cfgsvr_query called but RCS Library was not compiled with support for nmlcfgsvr.\n");
  set_rcs_print_tag(0); 
  return -1;
#endif
}

void
NML::format_chain_setup(void)
{
  if(format_chain == NULL)
    {
      format_chain = new RCS_LINKED_LIST();
    }
}


#if !defined(WIN32) && defined(HAVE_FORK)
static bool quit_nml_cloning=false;
static NML *remoteCloneNml=0;

static void
nml_spawn_sigint_handler(int)
{
  quit_nml_cloning=true;
  if(remoteCloneNml)
    {
      remoteCloneNml->interrupt_operation();
    }
}
#endif
// end of if !defined(WIN32) && defined(HAVE_FORK) 



int
NML::spawn_cloning_function(enum NML_CHANNEL_TYPE ct,
			   RCS_LINKED_LIST *_format_chain, char *buf, char *proc, char *file)
{
#if !defined(WIN32) && defined(HAVE_FORK)
  int fork_ret = fork();
  if(fork_ret != 0)
    {
      return fork_ret;
    }
  rcs_print("spawn_cloning_function for %s\n",buf);
  NML *localNML = 0;
  NML *remoteNML = 0;
  RCS_LINKED_LIST *format_chain_local = new RCS_LINKED_LIST();
  if(_format_chain)
    {
      NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) _format_chain->get_head ();
      while(f_ptr)
	{
	  format_chain_local->store_at_tail ((void *) f_ptr, 0, 0);
	  f_ptr = (NML_FORMAT_PTR) _format_chain->get_next ();
	}
    }
  RCS_LINKED_LIST *format_chain_remote = new RCS_LINKED_LIST();
  if(_format_chain)
    {
      NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) _format_chain->get_head ();
      while(f_ptr)
	{
	  format_chain_remote->store_at_tail ((void *) f_ptr, 0, 0);
	  f_ptr = (NML_FORMAT_PTR) _format_chain->get_next ();
	}
    }
  signal(SIGINT,nml_spawn_sigint_handler);

  localNML = (NML *) new NML(format_chain_local,buf,proc,file ,ct,-1,-1,NML_FORCE_LOCAL_CONNECTION_MODE);
  while(!localNML->valid())
    {
      delete localNML;
      if(quit_nml_cloning)
	{
	  exit(-1);
	}
      esleep(0.5);
      format_chain_local = new RCS_LINKED_LIST();
      if(_format_chain)
	{
	  NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) _format_chain->get_head ();
	  while(f_ptr)
	    {
	      format_chain_local->store_at_tail ((void *) f_ptr, 0, 0);
	      f_ptr = (NML_FORMAT_PTR) _format_chain->get_next ();
	    }
	}
      localNML = (NML *) new NML(format_chain_local,buf,proc,file ,ct,-1,-1,NML_FORCE_LOCAL_CONNECTION_MODE);
    }
  remoteNML = (NML *) new NML(format_chain_remote,buf,proc,file ,ct,-1,-1,NML_FORCE_REMOTE_CONNECTION_MODE);
  while(!remoteNML->valid())
    {
      delete remoteNML;
      if(quit_nml_cloning)
	{
	  delete localNML;
	  exit(-1);
	}
      esleep(0.5);
      format_chain_remote = new RCS_LINKED_LIST();
      if(_format_chain)
	{
	  NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) _format_chain->get_head ();
	  while(f_ptr)
	    {
	      format_chain_remote->store_at_tail ((void *) f_ptr, 0, 0);
	      f_ptr = (NML_FORMAT_PTR) _format_chain->get_next ();
	    }
	}
      remoteNML = (NML *) new NML(format_chain_remote,buf,proc,file ,ct,-1,-1,NML_FORCE_REMOTE_CONNECTION_MODE);
    }
  int count=0;

  while(!quit_nml_cloning)
    {
      double t1=etime();
      remoteCloneNml = remoteNML;
      int rtype=remoteNML->blocking_read(-1.0);
      if(rtype > 0)
	{
	  remoteCloneNml=0;
	  localNML->write(remoteNML->get_address());
	}
      remoteCloneNml=0;
      if(etime() - t1 < 0.005)
	{
	  esleep(0.01);
	}
      count++;
    }
  remoteCloneNml=0;
  if(remoteNML)
    {
      delete remoteNML;
      remoteNML=0;
    }
  if(localNML)
    {
      delete localNML;
      localNML=0;
    }
  nml_set_kill_servers_on_cleanup(false);
  nml_cleanup();
  exit(0);
#else
  return(0);
#endif
}


void
NML::reconstruct (NML_FORMAT_PTR f_ptr, const char *buf, const char *proc,
		  const char *file, int set_to_server, int set_to_master)
{
  set_rcs_print_tag(buf); 
  int cms_config_ret;
  cms_config_ret=-99;
  interrupting_operation=false;
  leave_resource=false;
  
  if(!set_to_server_env_checked)
    {
      set_to_server_env_checked =true;
#if HAVE_GETENV
      set_to_server_env_ptr=getenv("NML_SET_TO_SERVER");
      if(set_to_server_env_ptr && *set_to_server_env_ptr)
	{
	  set_to_server_override=strtol(set_to_server_env_ptr,0,0);
	  if(set_to_server_override != 0)
	    {
	      use_set_to_server_override=true;
	    }
	}
#endif
    }
  if(use_set_to_server_override && set_to_server==0 )
    {
      set_to_server = set_to_server_override;
    }

  cms = (CMS *) NULL;
  format_chain = (RCS_LINKED_LIST *) NULL;
  phantom_read = (NMLTYPE (*)())NULL;
  phantom_peek = (NMLTYPE (*)())NULL;
  phantom_write = (int (*)(NMLmsg *)) NULL;
  phantom_write_if_read = (int (*)(NMLmsg *)) NULL;
  phantom_check_if_read = (int (*)()) NULL;
  phantom_clear = (int (*)()) NULL;
  ni->channel_list_id = 0;
  error_type = NML_NO_ERROR;
  fast_mode = false;
  ignore_format_chain = 0;
  info_printed = 0;
  format_chain=0;

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
    }
#endif

  if (NULL == f_ptr && !nml_allow_null_f_ptr)
    {
      rcs_print_error ("NML:(Format Function Pointer) f_ptr == NULL.\n");
    }
  else
    {
      format_chain_setup();
      prefix_format_chain (f_ptr);
    }

  if(0 == ni->extra_bufs)
    {
      ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
    }

  if (0 == buf || 0 == proc || 0 == file)
    {
      error_type = NML_INVALID_CONSTRUCTOR_ARG;
      set_rcs_print_tag(0); 
      return;
    }


  cms=0;

#if defined(ENABLE_RCS_NMLCFGSVR)
  if(!strncmp(file,"nmlcfgsvr:",10) || !strncmp(file,"nmlcfgsvr/",10))
    {
      if(-1 == cfgsvr_query(buf,proc,file+10,set_to_server))
	{
	  set_rcs_print_tag(0); 
	  return;
	}
    }
#endif
      
  if(0 == cms)
    {
      set_rcs_print_tag(buf); 
      enum CMS_CONNECTION_MODE con_mode=CMS_NORMAL_CONNECTION_MODE;
      switch(ni->connect_mode)
	{
	case NML_FORCE_LOCAL_CONNECTION_MODE:
	  con_mode=CMS_FORCE_LOCAL_CONNECTION_MODE;
	  break;

	case NML_FORCE_REMOTE_CONNECTION_MODE:
	  con_mode=CMS_FORCE_REMOTE_CONNECTION_MODE;
	  break;

	case NML_NORMAL_CONNECTION_MODE:
	default:
	  con_mode=CMS_NORMAL_CONNECTION_MODE;
	  break;
	}
      cms_config_ret = cms_config_with_buffers(&cms, buf, proc, file, 
					       ni->extra_bufs->linebuf,
					       ni->extra_bufs->wordbuf,
					       ni->extra_bufs->proc_type,
					       ni->extra_bufs->buffer_type,
					       set_to_server, set_to_master,
					       ni->max_size_from_format,con_mode);
      if (-1 == cms_config_ret)
	{
	  set_error ();
	  if (!info_printed && 
	      !get_cmscfg_last_buffer_was_ignored_remote() &&
	      !get_cmscfg_last_buffer_was_ignored_no_bufline())
	    {
	      print_info (buf, proc, file);
	    }
	  if (NULL != cms)
	    {
	      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
			       (void *)cms);
	      delete cms;
	      cms = (CMS *) NULL;
	    }
	  set_rcs_print_tag(0); 
	  return;
	}
      else if(-2 == cms_config_ret)
	{
	  error_type = NML_IGNORED_REMOTE;
	  set_rcs_print_tag(0); 
	  return;
	} 
      else if(-3 == cms_config_ret)
	{
	  error_type = NML_IGNORED_NO_BUFLINE;
	  set_rcs_print_tag(0); 
	  return;
	} 
      else if(cms && cms->cloned_buffer && (cms->ProcessType == CMS_REMOTE_TYPE || cms->ProcessType == CMS_AUTO_TYPE) && ni->connect_mode == NML_NORMAL_CONNECTION_MODE)
	{
	  if(!cms->isserver && set_to_server <= 0)
	    {
	      if(!cms->ignore_cloned_buff)
		{
		  if(cms_config_with_buffers(&cms_for_read, buf, proc, file, 
					     ni->extra_bufs->linebuf,
					     ni->extra_bufs->wordbuf,
					     ni->extra_bufs->proc_type,
					     ni->extra_bufs->buffer_type,
					     0,0,
					     ni->max_size_from_format,
					     CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		    {
		      if(cms_for_read)
			{
			  delete cms_for_read;
			  cms_for_read=0;
			}
		    }
		}
	      if(cms_for_read->status == CMS_NO_MASTER_ERROR)
		{
		  delete cms_for_read;
		  cms_for_read=0;
		}
	    }
	  else
	    {
	      if(cms)
		{
		  delete cms;
		  cms=0;
		}
	      if(cms_config_with_buffers(&cms, buf, proc, file, 
					 ni->extra_bufs->linebuf,
					 ni->extra_bufs->wordbuf,
					 ni->extra_bufs->proc_type,
					 ni->extra_bufs->buffer_type,
					 set_to_server, set_to_master,
					 ni->max_size_from_format,
					 CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		{
		  if (NULL != cms)
		    {
		      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
				       (void *)cms);
		      delete cms;
		      cms = (CMS *) NULL;
		    }
		  error_type = NML_INVALID_CONFIGURATION;
		  set_rcs_print_tag(0); 
		  return;
		}
	      
	    }
	}
    }

  if (NULL == cms)
    {
      if (!info_printed &&
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info (buf, proc, file);
	}
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return;
    }

  if(ni->cms_for_msg_string_conversions)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
    }

  if (cms->status < 0)
    {
      error_type = NML_INVALID_CONFIGURATION;
      if (!info_printed &&
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info (buf, proc, file);
	}
      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n", 
		       (void *)cms);
      delete cms;
      cms = (CMS *) NULL;
      set_rcs_print_tag(0); 
      return;
    }

  // FAST MODE is a combination of options which allow certian checks during
  // a read or write operation to be avoided and therefore reduce the NML/CMS
  // overhead.
  if (!cms->is_phantom &&
      cms->ProcessType == CMS_LOCAL_TYPE &&
      !cms->neutral && !cms->isserver && !cms->enable_diagnostics &&
      !cms->enable_xml_logging)
    {
      fast_mode = true;
    }

  cms_inbuffer_header_size = &(cms->header.in_buffer_size);
  char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
  if (forced_type_eq != NULL)
    {
      long temp = strtol (forced_type_eq + 11, NULL, 0);
      if (temp > 0)
	{
	  forced_type = temp;
	  fast_mode = false;
	}
    }
  char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
  if (brpi_eq != NULL)
    {
      ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
    }
  if (cms->enable_xml_logging)
    {
      cms->fast_mode = false;
      fast_mode = false;
    }

  spawn_and_register_server_if_needed(set_to_server);
  add_to_channel_list ();
  if(ni->remote_to_local_reader_pid == 0 &&
     cms->cloned_buffer && cms->do_cloning && ni->connect_mode == NML_NORMAL_CONNECTION_MODE)
    {
      ni->remote_to_local_reader_pid = spawn_cloning_function(channel_type,format_chain,strdup(buf),strdup(proc),strdup(file));
    }
  set_rcs_print_tag(0); 
}

/******************************************************************
* Constructor for NML:
* Parameters:
* char *buf - Name of the buffer to connect to as written in the config file.
* char *proc - Name of the calling process as expected in the config file.
* char *file - Name of the configuration file.
* int set_to_server - If 1 this NML will consider its calling process to
*   be an NML_SERVER, which effects how and when messages are encoded and
*   decoded.
* int set_to_master - Passed to the CMS constructor - how this is used
*   depends on the type of CMS buffer. In general the master is responsible
*   for creating/initializing the buffer. If set_to_master == 1 then this
*   process will be considered the master, if set_to_master == 0 then
*   the the configuration file determines if this is the master, and it
*   set_to_master == -1 then this will not be the master.
* NOTES:
*  1. Borland C++(for DOS and Windows) does not allow default
*   parameters to be specified both here and in the header file.
*  2. All pointers are first initialized to NULL so that it can be determined
*  later if the constructor returned before creating the objects
*  the pointers are intended to point at.
*  3. This constructor does not register itself with the default server.
*  4. The NML object created by this constructor can not be used until
* the format_chain is constructed. (This may be done by
* derived classes. )
******************************************************************/
NML::NML(char *buf, char *proc, char *file,
	 enum NML_CHANNEL_TYPE ct,
	 int set_to_server, int set_to_master,
	 enum NML_CONNECTION_MODE connect_mode):
  ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0), 
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  memset(this,0,sizeof(NML));
  channel_type=ct;
  base_setup(buf,proc,file,ct,set_to_server,set_to_master,connect_mode);
}

NML::NML(RCS_LINKED_LIST *_format_chain,
	 const char *buf, const char *proc, const char *file,
	 enum NML_CHANNEL_TYPE ct,
	 int set_to_server, int set_to_master, 
	 enum NML_CONNECTION_MODE connect_mode):
  ni(0),cms(0),cms_for_read(0),format_chain(_format_chain),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  channel_type=ct;
  base_setup(buf,proc,file,ct,set_to_server,set_to_master,connect_mode);
}

void NML::base_setup(const char *buf, const char *proc, const char *file,
		     enum NML_CHANNEL_TYPE ct,
		     int set_to_server, int set_to_master,
		     enum NML_CONNECTION_MODE _connect_mode)
{
  set_rcs_print_tag(buf); 

  if(!force_nml_config_checked) {
    FORCE_NML_CONFIG=getenv("FORCE_NML_CONFIG");
    if(FORCE_NML_CONFIG) {
      rcs_print("NML::NML(...) Config file of \"%s\" overridden by FORCE_NML_CONFIG environment variable to \"%s\"\n",file,FORCE_NML_CONFIG);
    }
    force_nml_config_checked=true;
  }
  if(FORCE_NML_CONFIG) {
    file = FORCE_NML_CONFIG;
  }

  ni = new NML_INTERNALS();
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      return;
    }
  ni->connect_mode = _connect_mode;
  rcs_print_debug (PRINT_NML_CONSTRUCTORS,
		   "(no format function) NML::NML(%s,%s,%s,%d,%d)\n",
		   buf, proc, file, set_to_server, set_to_master);

  loopback=this;
  channel_type=ct;
  extra_thread_info =0;
#ifdef POSIX_THREADS
  extra_thread_info = new NML_EXTRA_THREAD_INFO();
  extra_thread_info->pthread_threadId = pthread_self();
#endif
  ni->msgForSchemaGen = 0;
  ni->lastMessageWritten = 0;
  ni->diffMsg = 0;
  ni->xmllogcount = 0;
  if (NULL == file)
    {
      file = default_nml_config_file;
    }
  ni->extra_bufs=0;
  ni->registered_with_server = 0;
  ni->cms_for_msg_string_conversions = 0;
  if(buf)
    {
      strncpy (ni->bufname, buf, 40);
    }
  if(proc)
    {
      strncpy (ni->procname, proc, 40);
    }
  if(file)
    {
      strncpy (ni->cfgfilename, file, 160);
    }

  ni->blocking_read_poll_interval = -1.0;
  info_printed = 0;
  forced_type = 0;
  already_deleted = 0;
  interrupting_operation=false;
  leave_resource=false;
  ni->max_size_from_format=0;
  ni->max_size_from_format_set=false;

  immediate_spawned_server = 0;

  cms = (CMS *) NULL;
  phantom_read = (NMLTYPE (*)())NULL;
  phantom_peek = (NMLTYPE (*)())NULL;
  phantom_write = (int (*)(NMLmsg *)) NULL;
  phantom_write_if_read = (int (*)(NMLmsg *)) NULL;
  phantom_check_if_read = (int (*)()) NULL;
  phantom_clear = (int (*)()) NULL;
  ni->channel_list_id = 0;
  error_type = NML_NO_ERROR;
  ignore_format_chain = 0;
  fast_mode = false;
  ni->xml_style_properties=0;
  ni->global_xml_style_properties_count=0;

  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);

  if(!set_to_server_env_checked)
    {
      set_to_server_env_checked =true;
#if HAVE_GETENV
      set_to_server_env_ptr=getenv("NML_SET_TO_SERVER");
      if(set_to_server_env_ptr && *set_to_server_env_ptr)
	{
	  set_to_server_override=strtol(set_to_server_env_ptr,0,0);
	  if(set_to_server_override != 0)
	    {
	      use_set_to_server_override=true;
	    }
	}
#endif
    }
  if(use_set_to_server_override && set_to_server==0 )
    {
      set_to_server = set_to_server_override;
    }

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      error_type = NML_INVALID_CONFIGURATION;
      return;
    }
#endif

  if(0 == ni->extra_bufs)
    {
      ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
    }


  set_rcs_print_tag(buf); 
  enum CMS_CONNECTION_MODE con_mode=CMS_NORMAL_CONNECTION_MODE;
  switch(ni->connect_mode)
    {
    case NML_FORCE_LOCAL_CONNECTION_MODE:
      con_mode=CMS_FORCE_LOCAL_CONNECTION_MODE;
      break;
      
    case NML_FORCE_REMOTE_CONNECTION_MODE:
      con_mode=CMS_FORCE_REMOTE_CONNECTION_MODE;
      break;
      
    case NML_NORMAL_CONNECTION_MODE:
    default:
      con_mode=CMS_NORMAL_CONNECTION_MODE;
      break;
    }


#if defined(ENABLE_RCS_NMLCFGSVR)
  if(file && (!strncmp(file,"nmlcfgsvr:",10) || !strncmp(file,"nmlcfgsvr/",10)))
    {
      if(-1 == cfgsvr_query(buf,proc,file+10,set_to_server))
	{
	  set_rcs_print_tag(0); 
	  return;
	}
    }
#endif

  if (0 == buf || 0 == proc || 0 == file)
    {
      error_type = NML_INVALID_CONSTRUCTOR_ARG;
      set_rcs_print_tag(0); 
      return;
    }

  if(cms == 0)
    {
      int cms_config_ret =
	cms_config_with_buffers (&cms, buf, proc, file, 
				 ni->extra_bufs->linebuf,
				 ni->extra_bufs->wordbuf,
				 ni->extra_bufs->proc_type,
				 ni->extra_bufs->buffer_type,
				 set_to_server, set_to_master,
				 ni->max_size_from_format,
				 con_mode);
      if (-1 == cms_config_ret)
	{
	  if(get_cmscfg_last_buffer_was_ignored_remote())
	    {
	      error_type = NML_IGNORED_REMOTE;
	      set_rcs_print_tag(0); 
	      return;
	    }
	  else if(get_cmscfg_last_buffer_was_ignored_no_bufline())
	    {
	      error_type = NML_IGNORED_NO_BUFLINE;
	      set_rcs_print_tag(0); 
	      return;
	    }
	  else
	    {
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error ("NML: cms_config returned -1.\n");
		}
	      if (!info_printed)
		{
		  print_info (buf, proc, file);
		}
	      if (NULL != cms)
		{
		  rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
				   (void *) cms);
		  delete cms;
		  cms = (CMS *) NULL;
		}
	      error_type = NML_INVALID_CONFIGURATION;
	    }
	  set_rcs_print_tag(0); 
	  return;
	}
      else if(-2 == cms_config_ret)
	{
	  if(get_cmscfg_last_buffer_was_ignored_no_bufline())
	    {
	      error_type = NML_IGNORED_NO_BUFLINE;
	      set_rcs_print_tag(0); 
	      return;
	    }
	  else
	    {
	      error_type = NML_IGNORED_REMOTE;
	      set_rcs_print_tag(0); 
	    }
	  return;
	}
      else if(-3 == cms_config_ret)
	{
	  if(get_cmscfg_last_buffer_was_ignored_no_bufline())
	    {
	      error_type = NML_IGNORED_NO_BUFLINE;
	      set_rcs_print_tag(0); 
	      return;
	    }
	  else
	    {
	      error_type = NML_IGNORED_REMOTE;
	      set_rcs_print_tag(0); 
	    }
	  return;
	}
      else if(cms && cms->cloned_buffer && 
	      (cms->ProcessType == CMS_REMOTE_TYPE || cms->ProcessType == CMS_AUTO_TYPE) 
	      && ni->connect_mode == NML_NORMAL_CONNECTION_MODE)
	{
	  if(!cms->isserver && set_to_server <= 0)
	    {
	      if(!cms->ignore_cloned_buff)
		{
		  if(cms_config_with_buffers (&cms_for_read, buf, proc, file, 
					      ni->extra_bufs->linebuf,
					      ni->extra_bufs->wordbuf,
					      ni->extra_bufs->proc_type,
					      ni->extra_bufs->buffer_type,
					      0,0,
					      ni->max_size_from_format,
					      CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		    {
		      if(cms_for_read)
			{
			  delete cms_for_read;
			  cms_for_read=0;
			}
		    }
		  if(cms_for_read->status == CMS_NO_MASTER_ERROR)
		    {
		      delete cms_for_read;
		      cms_for_read=0;
		    }
		}
	    }
	  else
	    {
	      delete cms;
	      cms=0;
	      if(cms_config_with_buffers (&cms, buf, proc, file, 
					  ni->extra_bufs->linebuf,
					  ni->extra_bufs->wordbuf,
					  ni->extra_bufs->proc_type,
					  ni->extra_bufs->buffer_type,
					  1, set_to_master,
					  ni->max_size_from_format,
					  CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
		{
		  if (NULL != cms)
		    {
		      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
				       (void *)cms);
		      delete cms;
		      cms = (CMS *) NULL;
		    }
		  error_type = NML_INVALID_CONFIGURATION;
		  set_rcs_print_tag(0); 
		  return;
		}
	    }
	}
    }
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      if (!info_printed && 	      
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info (buf, proc, file);
	}
      set_rcs_print_tag(0); 
      return;
    }

  if (cms->status < 0)
    {
      error_type = NML_INVALID_CONFIGURATION;
      if (!info_printed &&
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info (buf, proc, file);
	}
      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n", 
		       (void *)cms);
      delete cms;
      cms = (CMS *) NULL;
      set_rcs_print_tag(0); 
      return;
    }

#ifndef ENABLE_RCS_SERVER
  if(cms->isserver || cms->spawn_server)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      delete cms;
      cms=0;
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return;
    }
#endif

  add_to_channel_list ();
  if (!cms->is_phantom
      && cms->ProcessType == CMS_LOCAL_TYPE
      && !cms->neutral && !cms->enable_xml_logging && !cms->isserver)
    {
      fast_mode = true;
    }
  cms_inbuffer_header_size = &(cms->header.in_buffer_size);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
      char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
      if (brpi_eq != NULL)
	{
	  ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
	}
      setup_cc_bufs();
    }
  if(ni->remote_to_local_reader_pid == 0 && 
     cms->cloned_buffer && cms->do_cloning && ni->connect_mode == NML_NORMAL_CONNECTION_MODE)
    {
      ni->remote_to_local_reader_pid = spawn_cloning_function(channel_type,format_chain,strdup(buf),strdup(proc),strdup(file));
    }
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "finished NML constructor\n");
  set_rcs_print_tag(0); 
}

/******************************************************************
* Constructor for NML:
* Parameters:
* char *buffer_line - Buffer line  as written in the config file.
* char *proc_line - Process Line as expected in the config file.
* char *file - Name of the configuration file.
* int set_to_server - If 1 this NML will consider its calling process to
*   be an NML_SERVER, which effects how and when messages are encoded and
*   decoded.
* int set_to_master - Passed to the CMS constructor - how this is used
*   depends on the type of CMS buffer. In general the master is responsible
*   for creating/initializing the buffer. If set_to_master == 1 then this
*   process will be considered the master, if set_to_master == 0 then
*   the the configuration file determines if this is the master, and it
*   set_to_master == -1 then this will not be the master.
* NOTES:
*  1. Borland C++(for DOS and Windows) does not allow default
*   parameters to be specified both here and in the header file.
*  2. All pointers are first initialized to NULL so that it can be determined
*  later if the constructor returned before creating the objects
*  the pointers are intended to point at.
*  3. This constructor does not register itself with the default server.
*  4. The NML object created by this constructor can not be used until
* the format_chain is constructed. (This may be done by
* derived classes. )
******************************************************************/
NML::NML (const char *buffer_line, const char *proc_line,int set_to_server):
  ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  set_rcs_print_tag(buffer_line); 

  ni = new NML_INTERNALS();
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      set_rcs_print_tag(0); 
      return;
    }

  rcs_print_debug (PRINT_NML_CONSTRUCTORS,
		   "NML::NML(buffer_line=%s,proc_line=%s)\n", buffer_line,
		   proc_line);

  loopback=this;
  extra_thread_info =0;
#ifdef POSIX_THREADS
  extra_thread_info = new NML_EXTRA_THREAD_INFO();
  extra_thread_info->pthread_threadId = pthread_self();
#endif
  ni->extra_bufs=0;
  immediate_spawned_server = 0;
  interrupting_operation=false;
  leave_resource=false;

  ni->msgForSchemaGen = 0;
  ni->lastMessageWritten = 0;
  ni->diffMsg = 0;
  ni->xmllogcount = 0;
  ni->registered_with_server = 0;
  ni->cms_for_msg_string_conversions = 0;
  cms = (CMS *) NULL;
  ni->blocking_read_poll_interval = -1.0;
  forced_type = 0;
  info_printed = 0;
  already_deleted = 0;
  format_chain = (RCS_LINKED_LIST *) NULL;
  phantom_read = (NMLTYPE (*)())NULL;
  phantom_peek = (NMLTYPE (*)())NULL;
  phantom_write = (int (*)(NMLmsg *)) NULL;
  phantom_write_if_read = (int (*)(NMLmsg *)) NULL;
  phantom_check_if_read = (int (*)()) NULL;
  phantom_clear = (int (*)()) NULL;
  ni->channel_list_id = 0;
  error_type = NML_NO_ERROR;
  ignore_format_chain = 0;
  fast_mode = false;
  ni->xml_style_properties=0;
  ni->global_xml_style_properties_count=0;
  ni->max_size_from_format=0;
  ni->max_size_from_format_set=false;

  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);
  
  if(0 == ni->extra_bufs)
    {
      ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
    }
  int cms_create_ret = cms_create_from_lines_with_buffers (&cms, buffer_line, proc_line,
							   ni->extra_bufs->wordbuf,
							   ni->extra_bufs->proc_type,
							   ni->extra_bufs->buffer_type,
							   set_to_server);
  if (-1 == cms_create_ret)
    {
      if (verbose_nml_error_messages)
	{
	  rcs_print_error ("NML: cms_create_from_lines returned -1.\n");
	}
      if (!info_printed &&
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info ();
	}
      if (NULL != cms)
	{
	  rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
			   (void *)cms);
	  delete cms;
	  cms = (CMS *) NULL;
	}
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return;
    }
  else if(cms_create_ret == -2)
    {
      error_type = NML_IGNORED_REMOTE;
      set_rcs_print_tag(0); 
      return;
    }
  else if(cms_create_ret == -3)
    {
      error_type = NML_IGNORED_NO_BUFLINE;
      set_rcs_print_tag(0); 
      return;
    }
  else if(cms && cms->cloned_buffer && (cms->ProcessType == CMS_REMOTE_TYPE || cms->ProcessType == CMS_AUTO_TYPE))
    {
      if(!cms->isserver && !set_to_server)
	{
	  if(cms_create_from_lines_with_buffers (&cms_for_read, 
						 ni->buffer_line, 
						 ni->proc_line,
						 ni->extra_bufs->wordbuf,
						 ni->extra_bufs->proc_type,
						 ni->extra_bufs->buffer_type,
						 0,0,CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
	    {
	      if(cms_for_read)
		{
		  delete cms_for_read;
		  cms_for_read=0;
		}
	    }
	  if(cms_for_read &&
	     cms_for_read->status == CMS_NO_MASTER_ERROR)
	    {
	      delete cms_for_read;
	      cms_for_read=0;
	    }
	}
      else
	{
	  delete cms;
	  cms=0;
	  if(cms_create_from_lines_with_buffers (&cms, 
						 ni->buffer_line, 
						 ni->proc_line,
						 ni->extra_bufs->wordbuf,
						 ni->extra_bufs->proc_type,
						 ni->extra_bufs->buffer_type,
						 1,0,CMS_FORCE_LOCAL_CONNECTION_MODE) < 0)
	    {
	      if (NULL != cms)
		{
		  rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
				   (void *)cms);
		  delete cms;
		  cms = (CMS *) NULL;
		}
	      error_type = NML_INVALID_CONFIGURATION;
	      set_rcs_print_tag(0); 
	      return;
	    }
	      
	}
    }

	      
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return;
    }
  if(set_to_server < 0)
    {
      cms->isserver = 0;
      cms->spawn_server = 0;
    }
#ifndef ENABLE_RCS_SERVER
  if(cms->isserver || cms->spawn_server)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      error_type = NML_INVALID_CONFIGURATION;
      delete cms;
      cms=0;
      set_rcs_print_tag(0); 
      return;
    }
#endif

  if (cms->status < 0)
    {
      error_type = NML_INVALID_CONFIGURATION;
      if (verbose_nml_error_messages)
	{
	  rcs_print_error ("NML: cms->status = %d.\n", cms->status);
	}
      if (!info_printed &&
	  !get_cmscfg_last_buffer_was_ignored_remote() &&
	  !get_cmscfg_last_buffer_was_ignored_no_bufline())
	{
	  print_info ();
	}
      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n", (void *) cms);
      delete cms;
      cms = (CMS *) NULL;
      set_rcs_print_tag(0); 
      return;
    }
  add_to_channel_list ();
  if (!cms->is_phantom &&
      cms->ProcessType == CMS_LOCAL_TYPE && !cms->neutral && !cms->isserver)
    {
      fast_mode = true;
    }
  cms_inbuffer_header_size = &(cms->header.in_buffer_size);
  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
      char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
      if (brpi_eq != NULL)
	{
	  ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
	}
      spawn_and_register_server_if_needed(set_to_server);
    }
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "finished NML constructor\n");
  set_rcs_print_tag(0); 
}

/***************************************************************
* NML Member Function: add_to_channel_list()
* Purpose:
*  Adds a pointer to this NML object to the main NML list.
* The list is used by nml_cleanup to delete all NML objects owned by
* a process.
* Under VxWorks the list must be shared by all processes so we must
* remember which process (also known as a task) added it to the list.
* (This is a great reason for tossing VxWorks in the circular file!)
*****************************************************************/
void
NML::add_to_channel_list ()
{
  set_rcs_print_tag(0); 
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  ni->pid = GetCurrentProcessId ();
#else
#ifdef VXWORKS
  ni->pid = taskIdSelf ();
#else
  ni->pid = getpid ();
#endif
#endif

  if(!nml_cleanup_started)
    {
#ifdef POSIX_THREADS
      pthread_mutex_lock(&list_mutex);
#endif
      if (NULL == NML_Main_Channel_List)
	{
	  NML_Main_Channel_List = new RCS_LINKED_LIST;
	}
      if (NULL != NML_Main_Channel_List)
	{
	  ni->channel_list_id =
	    NML_Main_Channel_List->store_at_tail (this, sizeof (NML), 0);
	}
#ifdef POSIX_THREADS
      pthread_mutex_unlock(&list_mutex);
#endif
    } 

}

static bool hostname_mismatch_warning_given=false;

void 
NML::unregister_with_server()
{
  if(NULL != NML_Default_Super_Server)
    {
      NML_Default_Super_Server->remove_from_list(this);
    }
}

/********************************************************************
* NML Member Function: register_with_server()
* Purpose:
*  The NML_Default_Super_Server keeps a list of all of the NML objects
* that were specified as servers in the config file.
* When nml_start is called servers will be spawned for these buffers.
* The NML_Default_Super_Server also tries to reduce the number of processes
* spawned by grouping buffers with the same buffer number.
********************************************************************/
void
NML::register_with_server ()
{
  set_rcs_print_tag(0); 
#ifdef ENABLE_RCS_SERVER
  if(!valid())
    {
      return;
    }
#if !defined(__MSDOS__) || defined(WIN32)
  if (NULL != cms && !ni->registered_with_server)
    {
      if(get_cmscfg_ignore_remote() &&
	 !hostname_matches_bufferline_with_buffer (cms->BufferLine,ni->extra_bufs->wordbuf))
	{
	  return;
	}
#ifndef VXWORKS
      if(!hostname_mismatch_warning_given && ni->extra_bufs)
	{
	  if(!cms->cloned_buffer &&
	     !hostname_matches_bufferline_with_buffer (cms->BufferLine,ni->extra_bufs->wordbuf))
	    {
	      rcs_print_warning("Hostname %s for %s does not seem to match this system but %s is configured to spawn a server for it.\n",
				cms->BufferHost,ni->bufname,ni->procname);
	      hostname_mismatch_warning_given=true;
	    }
	}
#endif
      if (cms->spawn_server)
	{
	  if (NULL == NML_Default_Super_Server)
	    {
	      NML_Default_Super_Server = new NML_SUPER_SERVER;
	    }
	  NML_Default_Super_Server->add_to_list (this);
	  ni->registered_with_server = 1;
	}
    }
#endif
#endif
}

/**************************************************************
* NML Constructor:
* Parameters:
* NML *nml; - pointer to NML object to duplicate.
* int set_to_server - If 1 this NML will consider its calling process to
*   be an NML_SERVER, which effects how and when messages are encoded and
*   decoded.
* int set_to_master - Passed to the CMS constructor - how this is used
*   depends on the type of CMS buffer. In general the master is responsible
*   for creating/initializing the buffer. If set_to_master == 1 then this
*   process will be considered the master, if set_to_master == 0 then
*   the the configuration file determines if this is the master, and it
*   set_to_master == -1 then this will not be the master.
* NOTES:
*   All pointers are first initialized to NULL so that it can be determined
*  later if the constructor returned before creating the objects
*  the pointers are intended to point at.
*************************************************************/
NML::NML (NML * nml_ptr, int set_to_server, int set_to_master,
	  enum NML_CONNECTION_MODE _connect_mode):
  ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),
  cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  set_rcs_print_tag(0); 
  ni = new NML_INTERNALS();
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      return;
    }
  ni->connect_mode = _connect_mode;

  loopback=this;
  extra_thread_info =0;
#ifdef POSIX_THREADS
  extra_thread_info = new NML_EXTRA_THREAD_INFO(); 
  extra_thread_info->pthread_threadId = pthread_self();
#endif
  interrupting_operation=false;
  leave_resource=false;

  ni->diffMsg = 0;
  ni->extra_bufs=0;
  ni->xmllogcount = 0;
  ni->lastMessageWritten = 0;
  ni->registered_with_server = 0;
  ni->cms_for_msg_string_conversions = 0;
  already_deleted = 0;
  forced_type = 0;
  cms = (CMS *) NULL;
  format_chain = (RCS_LINKED_LIST *) NULL;
  error_type = NML_NO_ERROR;
  ignore_format_chain = 0;
  ni->channel_list_id = 0;
  fast_mode = false;
  info_printed = 0;
  ni->blocking_read_poll_interval = -1.0;
  ni->xml_style_properties=0;
  ni->global_xml_style_properties_count=0;
  immediate_spawned_server = 0;
  ni->msgForSchemaGen=0;
  memset(ni->cfgfilename,0,sizeof(ni->cfgfilename));
  memset(ni->xmllogfilename,0,sizeof(ni->xmllogfilename));
  memset(ni->bufname,0,sizeof(ni->bufname));
  memset(ni->procname,0,sizeof(ni->procname));
  phantom_read=0;
  phantom_peek=0;
  phantom_write=0;
  phantom_write_if_read=0;
  phantom_check_if_read=0;
  phantom_clear=0;
  cms_inbuffer_header_size=0;
  extra_data=0;
  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      error_type = NML_INVALID_CONFIGURATION;
      return;
    }
#endif


  if (NULL == nml_ptr)
    {
      rcs_print_error("Null pointer to NML object passed to NML constructor\n");
      error_type = NML_INVALID_CONSTRUCTOR_ARG;
      return;
    }
  strncpy (ni->bufname, nml_ptr->ni->bufname, sizeof(ni->bufname));
  set_rcs_print_tag(ni->bufname); 
  strncpy (ni->procname, nml_ptr->ni->procname, sizeof(ni->procname));
  strncpy (ni->cfgfilename, nml_ptr->ni->cfgfilename, sizeof(ni->cfgfilename));
  bool orig_nml_was_force_raw = false;
  if (NULL != nml_ptr->cms)
    {
      orig_nml_was_force_raw= nml_ptr->cms->force_raw;
      // Create a CMS channel identitical to the one from the argument
      // NML channel accept that the channel may be set_to_server or
      // set_to_master differently.
      if(0 == ni->extra_bufs)
	{
	  ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
	}
      
      cms_copy_with_buffers (&cms, nml_ptr->cms,
			     ni->extra_bufs->wordbuf,
			     ni->extra_bufs->proc_type,
			     ni->extra_bufs->buffer_type,
				 set_to_server, set_to_master);
      if (NULL != cms)
	{
	      cms->current_subdivision = nml_ptr->cms->current_subdivision;
	}
    }
  if (!ignore_format_chain && !orig_nml_was_force_raw)
    {
      format_chain_setup();
      if (NULL != nml_ptr && NULL != nml_ptr->ni &&
	  (NULL != nml_ptr->format_chain) && (NULL != format_chain))
	{
	  RCS_LINKED_LIST *from, *to;
	  NML_FORMAT_PTR format_func_ptr;
	  from = nml_ptr->format_chain;
	  to = format_chain;
	  format_func_ptr = (NML_FORMAT_PTR) from->get_head ();
	  while (NULL != format_func_ptr)
	    {
	      to->store_at_tail ((void *) format_func_ptr, 0, 0);
	      format_func_ptr = (NML_FORMAT_PTR) from->get_next ();
	    }
	}
    }
  if (NULL == cms)
    {
      set_rcs_print_tag(0); 
      return;
    }
  if(cms->spawn_server == 3)
    {
      cms->spawn_server = 0;
    }
#ifndef ENABLE_RCS_SERVER
  if(cms->isserver || cms->spawn_server)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      error_type = NML_INVALID_CONFIGURATION;
      set_rcs_print_tag(0); 
      return;
    }
#endif

  add_to_channel_list ();
  if (!cms->is_phantom &&
      cms->ProcessType == CMS_LOCAL_TYPE && !cms->neutral && !cms->isserver)
    {
      fast_mode = true;
    }
  cms_inbuffer_header_size = &(cms->header.in_buffer_size);
  char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
  if (forced_type_eq != NULL)
    {
      long temp = strtol (forced_type_eq + 11, NULL, 0);
      if (temp > 0)
	{
	  forced_type = temp;
	  fast_mode = true;
	}
    }
  char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
  if (brpi_eq != NULL)
    {
      ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
    }
#ifdef ENABLE_RCS_DIAG
  if (NULL != nml_ptr->cms->dpi)
    {
      CMS_DIAG_PROC_INFO *dpi = cms->get_diag_proc_info ();
      *dpi = *(nml_ptr->cms->get_diag_proc_info ());
      cms->set_diag_proc_info (dpi);
    }
  cms->first_diag_store = nml_ptr->cms->first_diag_store;
#endif
  if (NULL != cms->handle_to_global_data &&
      NULL != nml_ptr->cms->handle_to_global_data)
    {
      cms->handle_to_global_data->set_total_bytes_moved(
	nml_ptr->cms->handle_to_global_data->get_total_bytes_moved());
    }
  set_rcs_print_tag(0); 
}

/**************************************************************
* NML Reset Function
* Can be used instead of deleting and recreating an NML channel.
*************************************************************/
int
NML::reset ()
{
  set_rcs_print_tag(0); 
#ifdef ENABLE_RCS_SERVER
  if(immediate_spawned_server)
    {
      immediate_spawned_server->kill_server();
      esleep(0.01);
      delete immediate_spawned_server;
      immediate_spawned_server=0;
    }
#endif

  immediate_spawned_server = 0;
  interrupting_operation=false;
  leave_resource=false;

  int cms_copy_ret = 0;
  if (valid ())
    {
      return 1;
    }
  if (NULL != cms)
    {
      // Recreate a CMS channel identitical to the old one, do not
      // re-read the config file.
      CMS *oldcms = cms;
      cms = NULL;
      if(0 == ni->extra_bufs)
	{
	  ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
	}
      cms_copy_ret = cms_copy_with_buffers (&cms,oldcms,
			       ni->extra_bufs->wordbuf,
			       ni->extra_bufs->proc_type,
			       ni->extra_bufs->buffer_type,
			       0, 0);
      if (cms_copy_ret < 0)
	{
	  if (cms != NULL && cms != oldcms)
	    {
	      delete oldcms;
	    }
	  return 0;
	}

      spawn_and_register_server_if_needed(0);
      add_to_channel_list ();
      // FAST MODE is a combination of options which allow certian checks during
      // a read or write operation to be avoided and therefore reduce the NML/CMS
      // overhead.
      if (!cms->is_phantom &&
	  cms->ProcessType == CMS_LOCAL_TYPE &&
	  !cms->neutral && !cms->isserver && !cms->enable_diagnostics)
	{
	  fast_mode = true;
	}
      cms_inbuffer_header_size = &(cms->header.in_buffer_size);
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
      char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
      if (brpi_eq != NULL)
	{
	  ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
	}

      delete oldcms;
    }
  else
    {
      if(0 == ni->extra_bufs)
	{
	  ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
	}
      // Re-read the config file before creating a new CMS object.
      set_rcs_print_tag(ni->bufname); 
      if (cms_config_with_buffers (&cms, ni->bufname, ni->procname, ni->cfgfilename,
				   ni->extra_bufs->linebuf,
				   ni->extra_bufs->wordbuf,
				   ni->extra_bufs->proc_type,
				   ni->extra_bufs->buffer_type,
				   0, 0,ni->max_size_from_format) < 0)
	{
	  return 0;
	}
    }
  int valid_ret = valid();
#ifdef ENABLE_RCS_SERVER
  if(valid_ret && cms->spawn_server == 3 && !immediate_spawned_server )
    {
      if(!hostname_mismatch_warning_given && ni->extra_bufs)
	{
	  if(!hostname_matches_bufferline_with_buffer (cms->BufferLine,ni->extra_bufs->wordbuf))
	    {
	      rcs_print_warning("Hostname %s for %s does not seem to match this system but %s is configured to spawn a server for it.\n",
				cms->BufferHost,ni->bufname,ni->procname);
	      hostname_mismatch_warning_given=true;
	    }
	}
      immediate_spawned_server = new NML_SERVER(this);
      immediate_spawned_server->spawn();
    }
#endif
  return(valid_ret);
}


/*********************************************************
* NML Destructor:
* Notes:
*  1. Use if(NULL != ???) to avoid deleting objects that were
* never constructed.
*  2. The delete channel function was added because an error occured in
* running a server unded WIN32. An exception would occur as
* the last NML channel was being deleted from within nml_cleanup.
* After two days of being unable to debug this problem I
* replaced the delete nml with nml->delete_channel() as a workaround.
* The cause of this exception is still not understood.
*********************************************************/
NML::~NML ()
{
  set_rcs_print_tag(0); 
  rcs_print_debug (PRINT_NML_DESTRUCTORS, 
		   "NML::~NML() called  (this=%p,ni->channel_list_id=%d,already_deleted=%d)\n",
		   (void *)this,ni->channel_list_id,
		   already_deleted);
  if (already_deleted || !ni)
    {
      if (verbose_nml_error_messages)
	{
	  rcs_print_error ("NML channel being deleted more than once, or deleted without first properly initialized.(this=%p)\n",
			   (void *) this);
	}
      return;
    }
  delete_channel ();
  if(ni)
    {
      NML_INTERNALS *ni_to_delete=ni;
      ni=0;
      delete ni_to_delete;
    }
  already_deleted=true;
  set_rcs_print_tag(0); 
}

void
NML::delete_channel ()
{
  set_rcs_print_tag(0); 
  rcs_print_debug (PRINT_NML_DESTRUCTORS, "NML::delete_channel()  (this=%p,ni->channel_list_id=%d,immediate_spawned_server=%p)\n",
		   (void *)this,ni->channel_list_id,
		   (void *)immediate_spawned_server);
  //interrupt_operation();
  if(already_deleted)
    {
      return;
    }
  if(loopback != this)
    {
      if (verbose_nml_error_messages)
	{
	  rcs_print_error("Attempt to delete a channel that was not properly created. loopback=%p, this=%p \n",(void *)loopback,(void *) this);
	}
      return;
    }
#if !defined(WIN32) && defined(HAVE_KILL)
  if(ni && ni->remote_to_local_reader_pid > 0)
    {
      kill(ni->remote_to_local_reader_pid,SIGINT);
      ni->remote_to_local_reader_pid=0;
    }
#endif

  already_deleted=true;
  if(cms_for_read == cms || (ni && cms_for_read == ni->cms_for_msg_string_conversions))
    {
      cms_for_read = 0;
    }
  CMS *cms1;
  cms1 = ni->cms_for_msg_string_conversions;
  CMS *cms2;
  cms2 = cms;
  ni->cms_for_msg_string_conversions = (CMS *) NULL;
  cms = (CMS *) NULL;
  CMS *cms_for_read_local = cms_for_read;
  cms_for_read=0;
  
#if defined(ENABLE_RCS_NMLCFGSVR) && defined(ENABLE_RCS_SERVER)
  rcs_print_debug(PRINT_MISC,"NML::delete_channel: cms2=%p\n",(void *) cms2);
  char *nmlcfgsvr_to_contact =0;
  char *nmlcfgsvr_bufname =0;
  char *nmlcfgsvr_procname =0;
  double nmlcfgsvr_timeout=5.0;
  if(cms2 && cms2->nmlcfgsvr)
    {
      rcs_print_debug(PRINT_MISC,"NML::delete_channel: cms2->isserver=%d,cms2->delete_totally=%d,cms2->leave_resource=%d,cms2->nmlcfgsvr=%s,cms2->BufferName=%s,cms2->ProcessName=%s\n",
		      cms2->isserver,
		      cms2->delete_totally,
		      cms2->leave_resource,
		      cms2->nmlcfgsvr,
		      cms2->BufferName,
		      cms2->ProcessName);
      if((cms2->isserver || immediate_spawned_server || cms2->delete_totally) && !cms2->leave_resource
	 && cms2->nmlcfgsvr)
	{
	  nmlcfgsvr_to_contact = strdup(cms2->nmlcfgsvr);
	  nmlcfgsvr_bufname = strdup(cms2->BufferName);
	  nmlcfgsvr_procname = strdup(cms2->ProcessName);
	  nmlcfgsvr_timeout = cms2->timeout;
	  rcs_print_debug(PRINT_MISC,"NML::delete_channel: nmlcfgsvr_to_contact=%s,nmlcfgsvr_bufname=%s,nmlcfgsvr_procname=%s,nmlcfgsvr_timeout=%f\n",
		  nmlcfgsvr_to_contact,nmlcfgsvr_bufname,nmlcfgsvr_procname,nmlcfgsvr_timeout );
	}
    }
#endif

#ifdef ENABLE_RCS_SERVER
  if(immediate_spawned_server)
    {
      NML_SERVER *server_to_kill_and_delete;
      server_to_kill_and_delete = immediate_spawned_server;
      immediate_spawned_server=0;
      rcs_print_debug(PRINT_MISC,"calling server_to_kill_and_delete->kill_server() . . .\n");

      server_to_kill_and_delete->kill_server();
      rcs_print_debug(PRINT_MISC,"called server_to_kill_and_delete->kill_server().\n");
      esleep(0.01);
      rcs_print_debug(PRINT_MISC,"calling delete server_to_kill_and_delete; . . .\n");
      delete server_to_kill_and_delete;
      rcs_print_debug(PRINT_MISC,"called delete server_to_kill_and_delete;\n");
    }
#endif
  if(cms_for_read_local)
    {
      delete cms_for_read_local;
      cms_for_read_local=0;
    }
  if (NULL != cms1
      && cms2 != cms1)
    {
      if(ni->xml_style_properties == cms1->xml_style_properties)
	{
	  cms1->xml_style_properties =0;
	}
      delete cms1;
    }
  if (NULL != cms2)
    {
      rcs_print_debug (PRINT_NML_DESTRUCTORS, " delete (CMS *) %p;\n",
		       (void*) cms2);
      if(ni->xml_style_properties == cms2->xml_style_properties)
	{
	  cms2->xml_style_properties =0;
	}
      delete cms2;
    }
  if(NULL != ni->xml_style_properties)
    {
      delete ni->xml_style_properties;
      ni->xml_style_properties=NULL;
    }
  if (NULL != format_chain)
    {
      RCS_LINKED_LIST *l1;
      l1 = format_chain;
      format_chain = (RCS_LINKED_LIST *) NULL;
      delete l1;
    }

  if(!nml_cleanup_started)
    {
#ifdef POSIX_THREADS
      pthread_mutex_lock(&list_mutex);
#endif
      if (NULL != NML_Main_Channel_List && (0 != ni->channel_list_id))
	{
	  NML_Main_Channel_List->delete_node (ni->channel_list_id);
#if !defined(WINDOWS) && !defined(VXWORKS)
	  if (NML_Main_Channel_List->list_size == 0)
	    {
	      RCS_LINKED_LIST *l2;
	      l2 = NML_Main_Channel_List;
	      NML_Main_Channel_List = 0;	  
	      delete l2;
	    }
#endif
	}
#ifdef POSIX_THREADS
      pthread_mutex_unlock(&list_mutex);
#endif
    }

  if (0 != ni->msgForSchemaGen)
    {
      SchemaGenMsg *msg_to_delete;
      msg_to_delete = ni->msgForSchemaGen;
      ni->msgForSchemaGen = 0;      
      DEBUG_FREE (msg_to_delete);
    }
  if (0 != ni->lastMessageWritten)
    {
      NMLmsg *msg2_to_delete;
      msg2_to_delete = ni->lastMessageWritten;
      ni->lastMessageWritten = 0;      
      delete msg2_to_delete;
    }
  ni->diffMsg = 0;

  if(0 != ni->extra_bufs)
    {
      NML_EXTRA_STRING_BUFFERS *bufs_to_delete;
      bufs_to_delete = ni->extra_bufs;
      ni->extra_bufs=0;      
      delete bufs_to_delete;
    }
  if(extra_thread_info)
    {
      NML_EXTRA_THREAD_INFO *extra_thread_info_to_delete;
      extra_thread_info_to_delete = extra_thread_info;
      extra_thread_info=0;
      delete extra_thread_info_to_delete;
    }
#if defined(ENABLE_RCS_NMLCFGSVR) && defined(ENABLE_RCS_SERVER)
  rcs_print_debug(PRINT_MISC,"NML::delete_channel: nmlcfgsvr_to_contact=%p,nmlcfgsvr_bufname=%p,nmlcfgsvr_procname=%p,nmlcfgsvr_timeout=%f\n",
		  (void *) nmlcfgsvr_to_contact,
		  (void *) nmlcfgsvr_bufname,
		  (void *) nmlcfgsvr_procname,
		  nmlcfgsvr_timeout );
  if(nmlcfgsvr_to_contact && nmlcfgsvr_bufname && nmlcfgsvr_procname)
    {
      rcs_print_debug(PRINT_MISC,"NML::delete_channel: nmlcfgsvr_to_contact=%s,nmlcfgsvr_bufname=%s,nmlcfgsvr_procname=%s,nmlcfgsvr_timeout=%f\n",
		  nmlcfgsvr_to_contact,
		  nmlcfgsvr_bufname,
		  nmlcfgsvr_procname,
		  nmlcfgsvr_timeout );
      if(nmlcfgsvr_timeout < 0 || nmlcfgsvr_timeout > 5.0)
	{
	  nmlcfgsvr_timeout=5.0;
	}
      nmlcfgsvr_delete(nmlcfgsvr_to_contact,nmlcfgsvr_bufname,
		       ni->recvstring,sizeof(ni->recvstring),
		       ni->sendstring,sizeof(ni->sendstring), 
		       nmlcfgsvr_procname, 0,nmlcfgsvr_timeout);
    }
#endif

  if(nmlcfgsvr_to_contact) 
    {
      free(nmlcfgsvr_to_contact);
      nmlcfgsvr_to_contact =0;
    }
  if(nmlcfgsvr_bufname) 
    {
      free(nmlcfgsvr_bufname);
      nmlcfgsvr_bufname =0;
    }
  if(nmlcfgsvr_procname)
    {
      free(nmlcfgsvr_procname);
      nmlcfgsvr_procname =0;
    }

  rcs_print_debug (PRINT_NML_DESTRUCTORS, "Leaving ~NML()\n");
  set_rcs_print_tag(0); 
}

/*************************************************************
* NML Member Function: get_address()
* Purpose:
*  Returns the address of the local copy of the buffer.
*  Use this function instead of nml->cms->subdiv_data directly to prevent
*   users from pointing nml->cms->subdiv_data at something else. Or getting
*   a segmentation fault if cms was not properly constructed.
***************************************************************/
NMLmsg *
NML::get_address ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  return ((NMLmsg *) ni->cms_for_msg_string_conversions->subdiv_data);
	}
      error_type = NML_INVALID_CONFIGURATION;
      return ((NMLmsg *) NULL);
    }
  else
    {
      if(cms_for_read)
	{
	  return ((NMLmsg *) cms_for_read->subdiv_data);
	}
      return ((NMLmsg *) cms->subdiv_data);
    }
}

/*************************************************************
* NML Member Function: get_address_subdivision(int subdiv)
* Purpose:
*  Returns the address of the local copy of the buffer.
*  Use this function instead of nml->cms->subdiv_data directly to prevent
*   users from pointing nml->cms->subdiv_data at something else. Or getting
*   a segmentation fault if cms was not properly constructed.
***************************************************************/
NMLmsg *
NML::get_address_subdivision (int subdiv)
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return ((NMLmsg *) NULL);
    }
  else
    {
      cms->set_subdivision (subdiv);
      return ((NMLmsg *) cms->subdiv_data);
    }
}


/*************************************************************
* NML Member Function: get_address_subdivision(int subdiv)
* Purpose:
*  Returns the total number of subdivisions configured for this
* NML channel.
***************************************************************/
int
NML::get_total_subdivisions ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      return (1);
    }
  else
    {
      return (cms->total_subdivisions);
    }
}


/***********************************************************
* NML Member Function: clear()
* Purpose:
*  Clears the CMS buffer associated with this NML channel.
*
*  Returns:
* 0 if no error occured or -1 if error occured.
*
* Notes:
*  1. Some buffers can be identified as PHANTOM in the config file.
* this will cause cms->is_phantom to be set. This will cause this
* function to call the function pointed to by phantom_clear
* if it is not NULL rather than using CMS.
*  2. If an error occurs, users can check error_type before
* making any other NML calls
************************************************************/
int
NML::clear ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return (-1);
    }
  else
    {
      set_rcs_print_tag(cms->BufferName); 
      if (cms->is_phantom)
	{
	  if (NULL != phantom_clear)
	    {
	      return ((*phantom_clear) ());
	    }
	  else
	    {
	      set_rcs_print_tag(0); 
	      return (0);
	    }
	}
      CMS_STATUS return_value;
      error_type = NML_NO_ERROR;
      if (((int) (return_value = cms->clear ())) > 0)
	{
	  error_type = NML_INTERNAL_CMS_ERROR;
	}
      if (cms->status == CMS_TIMED_OUT)
	{
	  error_type = NML_TIMED_OUT;
	}
      if(cms->status == CMS_INTERRUPTED_OPERATION)
	{
	  error_type = NML_INTERRUPTED_OPERATION;
	}
      set_rcs_print_tag(0); 
      return (((int) return_value < 0) ? -1 : 0);
    }
    set_rcs_print_tag(0); 
    return(0);
}

/************************************************************
* NML Member Function: clean_buffers()
*  Tells cms to set buffers to zero so that we eliminate strange
* history effects. -- Should only be used by progams testing CMS/NML.
************************************************************/
void
NML::clean_buffers ()
{
  set_rcs_print_tag(0); 
  if (NULL != cms)
    {
      cms->clean_buffers ();
    }
}

/***********************************************************
* NML Member Function: check_if_read()
* Purpose:
*  Returns 1 if the buffer has been read since the last write,
* 0 if it hadn't, or -1 if some communications error prevented
* it from finding out.
* Notes:
*  1. Some buffers can be identified as PHANTOM in the config file.
* this will cause cms->is_phantom to be set. This will cause this
* function to call the function pointed to by phantom_check_if_read
* if it is not NULL rather than using CMS.
*  2. If an error occurs, users can check error_type before
* making any other NML calls
************************************************************/
int
NML::check_if_read ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return (-1);
    }
  else
    {
      set_rcs_print_tag(cms->BufferName); 
      if (cms->is_phantom)
	{
	  if (NULL != phantom_check_if_read)
	    {
	      set_rcs_print_tag(0); 
	      return ((*phantom_check_if_read) ());
	    }
	  else
	    { 
	      set_rcs_print_tag(0); 
	      return (0);
	    }
	}
      int return_value;
      error_type = NML_NO_ERROR;
      if ((return_value = cms->check_if_read ()) == -1)
	{
	  error_type = NML_INTERNAL_CMS_ERROR;
	}
      if (cms->status == CMS_TIMED_OUT)
	{
	  error_type = NML_TIMED_OUT;
	}
      if(cms->status == CMS_INTERRUPTED_OPERATION)
	{
	  error_type = NML_INTERRUPTED_OPERATION;
	}
      set_rcs_print_tag(0); 
      return (return_value);
    }
}


/***********************************************************
* NML Member Function: get_queue_length()
* Purpose:
*  Returns the number of messages queued in the buffer if queing
* was enabled for this buffer. 0 otherwise.
************************************************************/
int
NML::get_queue_length ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return (-1);
    }
  else
    {
      set_rcs_print_tag(cms->BufferName); 
      error_type = NML_NO_ERROR;
      int rval = cms->get_queue_length ();
      set_rcs_print_tag(0); 
      return(rval);
    }
}

/***********************************************************
* NML Member Function: get_space_available()
* Purpose:
*  Returns the approximate number of bytes that can be written
* to a queued buffer.
*
************************************************************/
int
NML::get_space_available ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return (-1);
    }
  else
    {
      error_type = NML_NO_ERROR;
      set_rcs_print_tag(cms->BufferName);
      int rval = cms->get_space_available ();
      set_rcs_print_tag(0); 
      return rval;
    }
}

/*************************************************************
* NML Member Function: valid()
* Purpose:
* Provides a check which can be used after an NML object is
* constructed to determine if everthing is in order.
* Returns: 1 if everything is O.K. 0 otherwise.
*************************************************************/
int
NML::valid ()
{
  set_rcs_print_tag(0); 
  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
		      (void *)this,error_type,
		      ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
      return (0);
    }
  set_rcs_print_tag(cms->BufferName); 

  if (cms->is_phantom)
    {
      error_type = NML_NO_ERROR;
      set_rcs_print_tag(0); 
      return (1);
    }

  if (CMS_NO_MASTER_ERROR == cms->status)
    {
      error_type = NML_NO_MASTER_ERROR;
      rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
		      (void *) this,error_type,
		      ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
      set_rcs_print_tag(0); 
      return (0);
    }

  if (NULL == cms->data)
    {
      error_type = NML_INVALID_CONFIGURATION;
      rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
		      (void *)this,error_type,
		      ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
      set_rcs_print_tag(0); 
      return (0);
    }

  if (cms->neutral && (NULL == cms->encoded_data) && !cms->isserver)
    {
      error_type = NML_INVALID_CONFIGURATION;
      rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
		      (void*) this,error_type,
		      ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
      set_rcs_print_tag(0); 
      return (0);
    }

  if (((int)cms->status) < 0)
    {
      if(error_type == NML_NO_ERROR)
	{
	  error_type = NML_INTERNAL_CMS_ERROR;
	}
      rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
		      (void*)this,error_type,
		      ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
      set_rcs_print_tag(0); 
      return (0);
    }

  if (!ignore_format_chain && (!cms || !cms->force_raw))
    {
      if (NULL == format_chain)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_debug(PRINT_MISC,"NML::valid() returning 0. this=%p,error_type=%d:%s\n",
			  (void *)this,error_type,
			  ((error_type>=0 &&error_type<12)?NML_ERROR_TYPE_STRINGS[error_type]:"Bad-Error-Type"));
	  set_rcs_print_tag(0); 
	  return (0);
	}
    }

  error_type = NML_NO_ERROR;
  set_rcs_print_tag(0); 
  return (1);
}

/***********************************************************
* NML Member Function: read()
* Purpose: Reads an NMLmsg from a CMS buffer.
* Returns:
*  0 The read was successful but the data was not updated since the last read.
*  -1 The buffer could not be read.
*  o.w. The type of the new NMLmsg is returned.
* Notes:
*   1. Users need to call NML::get_address in order to access the
* messages that were read.
*  2. Some buffers can be identified as PHANTOM in the config file.
* this will cause cms->is_phantom to be set. This will cause this
* function to call the function pointed to by phantom_read
* if it is not NULL rather than using CMS.
*  3. If an error occurs, users can check error_type before
* making any other NML calls
***********************************************************/
NMLTYPE NML::read ()
{
  set_rcs_print_tag(0); 
  error_type = NML_NO_ERROR;
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (fast_mode)
    {
      set_rcs_print_tag(lcms->BufferName); 	    
      lcms->read ();
      set_rcs_print_tag(0); 
      switch (lcms->status)
	{
	case CMS_READ_OLD:
	  return (0);
	case CMS_READ_OK:
	  if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	    {
	      rcs_print_error
		("NML: New data recieved but type of %ld is invalid.\n",
		 (long) ((NMLmsg *) lcms->subdiv_data)->type);
	      return -1;
	    }
	  return (((NMLmsg *) lcms->subdiv_data)->type);

	default:
	  set_error ();
	  return -1;
	}
    }

  /* Check pointers. */
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::read: CMS not configured.\n");
	}
      return (-1);
    }
  set_rcs_print_tag(lcms->BufferName); 	    

  /* Handle PHANTOMs */
  if (lcms->is_phantom)
    {
      if (NULL != phantom_read)
	{
	  set_rcs_print_tag(0); 
	  return ((*phantom_read) ());
	}
      else
	{
	  set_rcs_print_tag(0); 
	  return (0);
	}
    }

  /* Read using CMS */
  if (!lcms->force_raw)
    {
      lcms->set_mode (CMS_READ);
    }
  lcms->read ();

  if (!lcms->force_raw)
    {
      if (lcms->status == CMS_READ_OK)
	{
	    if(cms_for_read && lcms==cms_for_read)
	      {
		ni->use_cms_for_read_for_format_output=true;
	      }
	    if (-1 == format_output ())
	      {
		ni->use_cms_for_read_for_format_output=false;
		error_type = NML_FORMAT_ERROR;
		set_rcs_print_tag(0); 
		return (-1);
	      }
	    ni->use_cms_for_read_for_format_output=false;
	}
    }

  set_rcs_print_tag(0); 
  /* Choose the return value. */
  switch (lcms->status)
    {
    case CMS_READ_OLD:
      error_type = NML_NO_ERROR;
      return (0);
    case CMS_READ_OK:
      error_type = NML_NO_ERROR;
      if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	{
	  rcs_print_error
	    ("NML: New data recieved but type of %ld is invalid.\n",
	     (long) ((NMLmsg *) lcms->subdiv_data)->type);
	  return -1;
	}
      if (lcms->enable_xml_logging)
	{
	  internal_xml_log("nml_read",get_address());
	}
      return (((NMLmsg *) lcms->subdiv_data)->type);

    default:
      set_error ();
      return -1;
    }
}



/***********************************************************
* NML Member Function: blocking_read(double timeout)
* Purpose: Reads an NMLmsg from a CMS buffer after waiting up to timeout seconds for new data.
* Returns:
*  0 The read was successful but the data was not updated since the last read.
*  -1 The buffer could not be read.
*  o.w. The type of the new NMLmsg is returned.
* Notes:
*   1. Users need to call NML::get_address in order to access the
* messages that were read.
*  2. Some buffers can be identified as PHANTOM in the config file.
* this will cause cms->is_phantom to be set. This will cause this
* function to call the function pointed to by phantom_read
* if it is not NULL rather than using CMS.
*  3. If an error occurs, users can check error_type before
* making any other NML calls
***********************************************************/
NMLTYPE NML::blocking_read (double blocking_timeout)
{
  set_rcs_print_tag(0);
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    } 
  error_type = NML_NO_ERROR;
  if (fast_mode)
    {
      set_rcs_print_tag(lcms->BufferName); 
      lcms->blocking_read (blocking_timeout);
      set_rcs_print_tag(0); 
      switch (lcms->status)
	{
	case CMS_READ_OLD:
	  return (0);
	case CMS_READ_OK:
	  if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	    {
	      rcs_print_error
		("NML: New data recieved but type of %ld is invalid.\n",
		 (long) ((NMLmsg *) lcms->subdiv_data)->type);
	      return -1;
	    }
	  return (((NMLmsg *) lcms->subdiv_data)->type);
	case CMS_TIMED_OUT:
	  error_type = NML_NO_ERROR;
	  return 0;

	default:
	  set_error ();
	  return (-1);
	}
    }
  /* Check pointers. */
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::blocking_read: CMS not configured.\n");
	}
      return (-1);
    }

  set_rcs_print_tag(lcms->BufferName); 

  /* Handle PHANTOMs */
  if (lcms->is_phantom)
    {
      set_rcs_print_tag(0); 
      if (NULL != phantom_read)
	{
	  return ((*phantom_read) ());
	}
      else
	{
	  return (0);
	}
    }

  /* Read using CMS */
  if (!lcms->force_raw)
    {
      lcms->set_mode (CMS_READ);
    }
  if (lcms->BufferType == CMS_SHMEM_TYPE)
    {
      lcms->blocking_read (blocking_timeout);
    }
  else
    {
      double
	time_elapsed = 0.0;
      double
	start_time = 0.0;
      if (blocking_timeout > 0.0)
	{
	  start_time = etime ();
	}
      double
	current_brpi =
	ni->blocking_read_poll_interval;
      lcms->status = CMS_READ_OLD;
      if (current_brpi < 2e-2)
	{
	  current_brpi = 2e-2;
	}
      if (current_brpi > blocking_timeout / 2.0 && blocking_timeout > 1e-6)
	{
	  current_brpi = blocking_timeout / 2.0;
	}
      while (lcms->status == CMS_READ_OLD &&
	     (time_elapsed < blocking_timeout || blocking_timeout < 0.0))
	{

	  esleep (current_brpi);
	  lcms->read ();
	  if (blocking_timeout > 0.0 && lcms->status == CMS_READ_OLD)
	    {
	      time_elapsed = etime () - start_time;
	    }
	  if (time_elapsed < 0.0)
	    {
	      break;
	    }
	}
    }


  if (!lcms->force_raw)
    {
      if (lcms->status == CMS_READ_OK)
	{
	  if(cms_for_read && lcms==cms_for_read)
	    {
	      ni->use_cms_for_read_for_format_output=true;
	    }
	  if (-1 == format_output ())
	    {
	      ni->use_cms_for_read_for_format_output=false;
	      error_type = NML_FORMAT_ERROR;
	      set_rcs_print_tag(0); 
	      return (-1);
	    }
	  ni->use_cms_for_read_for_format_output=false;
	}
    }

  set_rcs_print_tag(0); 

  /* Choose the return value. */
  switch (lcms->status)
    {
    case CMS_READ_OLD:
      return (0);
    case CMS_READ_OK:
      if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	{
	  rcs_print_error
	    ("NML: New data recieved but type of %ld is invalid.\n",
	     (long) ((NMLmsg *) lcms->subdiv_data)->type);
	  return -1;
	}
      if (lcms->enable_xml_logging)
	{
	  internal_xml_log("nml_blocking_read",get_address());
	}
      return (((NMLmsg *) lcms->subdiv_data)->type);
    case CMS_TIMED_OUT:
      error_type = NML_NO_ERROR;
      return 0;

    default:
      set_error ();
      return (-1);
    }
}


void
NML::reconnect ()
{
  set_rcs_print_tag(0); 
  interrupting_operation=false;
  if (NULL != cms)
    {
      set_rcs_print_tag(cms->BufferName); 
      cms->reconnect ();
      set_rcs_print_tag(0); 
    }
}

void
NML::disconnect ()
{
  set_rcs_print_tag(0); 
  if (NULL != cms)
    {
      cms->disconnect ();
    }
}

/* Same as the read with no arguments except that the data is
 stored in a user supplied location . */
NMLTYPE NML::read (void *temp_data, long temp_size)
{
  set_rcs_print_tag(0); 
  NMLTYPE return_value =0;
  void * original_data = cms->data;
  long original_size = cms->size;
  long original_max_message_size = cms->max_message_size;
  CMS *lcms=cms;

  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  void * orig_subdiv_data = lcms->subdiv_data;
  original_data = lcms->data;
  lcms->subdiv_data = temp_data;
  lcms->data = temp_data;
  lcms->temp_data_size = temp_size;
  if (lcms->max_message_size > ((long) temp_size))
    {
      lcms->max_message_size = temp_size;
    }
  set_rcs_print_tag(lcms->BufferName); 
  return_value = read();
  set_rcs_print_tag(0); 
  lcms->data = original_data;
  lcms->subdiv_data = orig_subdiv_data;
  lcms->size = original_size;
  lcms->temp_data_size = 0;
  lcms->max_message_size = original_max_message_size;
  return return_value;
}

/* Same as the peek with no arguments except that the data is
 stored in a user supplied location . */
NMLTYPE NML::peek (void *temp_data, long temp_size)
{
  set_rcs_print_tag(0); 
  NMLTYPE return_value =0;
  void * original_data = cms->data;
  long original_size = cms->size;
  long original_max_message_size = cms->max_message_size;
  CMS *lcms=cms;

  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  void * orig_subdiv_data = lcms->subdiv_data;
  original_data = lcms->data;
  lcms->subdiv_data = temp_data;
  lcms->data = temp_data;
  lcms->temp_data_size = temp_size;
  if (lcms->max_message_size > ((long) temp_size))
    {
      lcms->max_message_size = temp_size;
    }
  set_rcs_print_tag(lcms->BufferName); 
  return_value = peek ();
  set_rcs_print_tag(0); 
  lcms->data = original_data;
  lcms->subdiv_data = orig_subdiv_data;
  lcms->size = original_size;
  lcms->temp_data_size = 0;
  lcms->max_message_size = original_max_message_size;
  return return_value;
}


/***********************************************************
* NML Member Function: peek()
* Purpose: Reads an NMLmsg from a CMS buffer without setting the
* was_read flag. The was_read flag is used by check_if_read and
* write_if_read.
* Returns:
*  0 The read was successful but the data was not updated since the last read.
*  -1 The buffer could not be read.
*  o.w. The type of the new NMLmsg is returned.
* Notes:
*   1. Users need to call NML::get_address in order to access the
* messages that were read.
*  2. Some buffers can be identified as PHANTOM in the config file.
* this will cause cms->is_phantom to be set. This will cause this
* function to call the function pointed to by phantom_read
* if it is not NULL rather than using CMS.
*  3. If an error occurs, users can check error_type before
* making any other NML calls
***********************************************************/
NMLTYPE NML::peek ()
{
  set_rcs_print_tag(0); 
  error_type = NML_NO_ERROR;
  if(!cms || already_deleted || loopback != this)
    {
      return -1;
    }
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (fast_mode)
    {
      set_rcs_print_tag(lcms->BufferName); 
      lcms->peek ();
      set_rcs_print_tag(0); 
      switch (lcms->status)
	{
	case CMS_READ_OLD:
	  return (0);
	case CMS_READ_OK:
	  if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	    {
	      rcs_print_error
		("NML: New data recieved but type of %ld is invalid.\n",
		 (long) ((NMLmsg *) lcms->subdiv_data)->type);
	      return -1;
	    }
	  return (((NMLmsg *) lcms->subdiv_data)->type);

	default:
	  set_error ();
	  return -1;
	}
    }
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::peek: CMS not configured.\n");
	}
      return (-1);
    }

  if (lcms->is_phantom)
    {
      if (NULL != phantom_peek)
	{
	  return ((*phantom_peek) ());
	}
      else


	{
	  return (0);
	}
    }

  set_rcs_print_tag(lcms->BufferName); 

  if (!lcms->force_raw)
    {
      lcms->set_mode (CMS_READ);
    }

  lcms->peek ();
  set_rcs_print_tag(0); 
  if (!lcms->force_raw)
    {
      if (lcms->status == CMS_READ_OK)
	{
	  if(cms_for_read && lcms==cms_for_read)
	    {
	      ni->use_cms_for_read_for_format_output=true;
	    }
	  if (-1 == format_output ())
	    {
	      ni->use_cms_for_read_for_format_output=false;
	      error_type = NML_FORMAT_ERROR;
	      return (-1);
	    }
	  ni->use_cms_for_read_for_format_output=false;
	}
    }

  switch (lcms->status)
    {
    case CMS_READ_OLD:
      return (0);
    case CMS_READ_OK:
      if (((NMLmsg *) lcms->subdiv_data)->type <= 0 && !lcms->isserver)
	{
	  rcs_print_error
	    ("NML: New data recieved but type of %ld is invalid.\n",
	     (long) ((NMLmsg *) lcms->subdiv_data)->type);
	  return -1;
	}
      if (lcms->enable_xml_logging)
	{
	  internal_xml_log("nml_peek",get_address());
	}
      return (((NMLmsg *) lcms->subdiv_data)->type);

    default:
      set_error ();
      return -1;
    }

}


/***********************************************************
* NML Member Function: get_msg_type()
* Purpose: Reads only the message type a CMS buffer.
* Returns:
*  0 The read was successful but the data was not updated since the last read.
*  -1 The buffer could not be read.
*  o.w. The type of the new NMLmsg is returned.
***********************************************************/
NMLTYPE NML::get_msg_type ()
{
  NMLmsg msg(1,sizeof(NMLmsg));
  set_rcs_print_tag(0); 
  error_type = NML_NO_ERROR;
  if(!cms || already_deleted || loopback != this)
    {
      return -1;
    }
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if(lcms->ProcessType == CMS_REMOTE_TYPE)
    {
      NMLTYPE t = (NMLTYPE) lcms->get_msg_type();
      if(t == -1 && lcms->status == CMS_NO_IMPLEMENTATION_ERROR)
	{
	  peek();
	  return ((NMLmsg *) lcms->subdiv_data)->type;
	}
      return t;
    }
  if (fast_mode || (cms->ProcessType == CMS_LOCAL_TYPE && !cms->neutral))
    {
      set_rcs_print_tag(lcms->BufferName); 
      lcms->get_msg_start (&msg, sizeof(msg));
      set_rcs_print_tag(0); 
      return (msg.type);
    }
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::peek: CMS not configured.\n");
	}
      return (-1);
    }

  if (lcms->is_phantom)
    {
      return -1;
    }

  bool orig_isserver = lcms->isserver;
  CMSMODE orig_read_mode = lcms->read_mode;
  int orig_read_updater_mode = lcms->read_updater_mode;

  set_rcs_print_tag(lcms->BufferName); 
  if(lcms->read_mode == CMS_ENCODE)
    {
      lcms->read_mode = CMS_DECODE;
      lcms->read_updater_mode = CMS_DECODE_DATA;
    }
  else if((lcms->read_mode == CMS_RAW_OUT && lcms->isserver && lcms->neutral))
    {
      lcms->read_mode = CMS_DECODE;
      lcms->read_updater_mode = CMS_DECODE_DATA;
    }
  lcms->isserver=0;
  lcms->last_im = CMS_NOT_A_MODE;
  if (!lcms->force_raw)
    {
      lcms->set_mode (CMS_READ);
    }
  lcms->get_msg_start (&msg, sizeof(msg));
  set_rcs_print_tag(0); 
  if(lcms->header.write_id == 0 ||
     lcms->header.in_buffer_size < 4)
    {
      return 0;
    }
  if (!lcms->force_raw)
    {
      if (lcms->status == CMS_READ_OK || 
	    lcms->status == CMS_READ_OLD || 
	    lcms->status == CMS_STATUS_NOT_SET
	  )
	{
	  if(cms_for_read && lcms==cms_for_read)
	    {
	      ni->use_cms_for_read_for_format_output=true;
	    }
	  void * orig_subdiv_data = lcms->subdiv_data;
	  lcms->subdiv_data = &msg;
	  ni->get_msg_type_only=true;
	  if (-1 == format_output ())
	    {
	      lcms->subdiv_data = orig_subdiv_data;
	      ni->get_msg_type_only=false;
	      ni->use_cms_for_read_for_format_output=false;
	      error_type = NML_FORMAT_ERROR;
	      lcms->isserver=orig_isserver;
	      lcms->read_updater_mode = orig_read_updater_mode;
	      lcms->read_mode = orig_read_mode;
	      return (-1);
	    }
	  lcms->subdiv_data = orig_subdiv_data;
	  ni->get_msg_type_only=true;
	  ni->use_cms_for_read_for_format_output=false;
	}
    }
  lcms->read_mode = orig_read_mode;
  lcms->isserver=orig_isserver;
  lcms->read_updater_mode = orig_read_updater_mode;
  lcms->last_im = CMS_NOT_A_MODE;
  return (msg.type);
}

/***********************************************************
* NML Member Function: format_output()
* Purpose: Formats the data read from a CMS buffer as required
* by the process that created this NML. The formatting converts
* messages from some platform indepent format to a platform
* specific format or vice versa. (Performing byte-swapping etc.)
* Returns:
*  0  The format was successful.
*  -1 An error occured.
* Notes:
*  1. There are 3 conditions under which format_output may be
* called.
*     i. The data is being read out as is. (cms->mode == CMS_RAW_OUT).
*    ii. A user needs the data in the local platform-specific or raw format
*        but the buffer has been encoded in a platform-independant or
*         neutral format.   (cms->mode == CMS_DECODE).
*   iii. An NML_SERVER needs the data encoded in a platform-independant
*        or neutral format to send it out over the network but the buffer
*        is in a local platform-specific or raw format.
*         (cms->mode == CMS_ENCODE)
*  2. This function uses a list of format functions supplied
* by the user and stored int the format_chain.
* Returns:
* 0 = Success.
* -1 = Error.
***********************************************************/
int
NML::format_output ()
{
  NMLTYPE new_type = 0;
  long new_size = 0;
  set_rcs_print_tag(0); 

  /* Check pointers */
  if (NULL == cms)
    {
      rcs_print_error ("NML: cms is NULL.\n");
      ni->use_cms_for_read_for_format_output=false;
      ni->use_cms_for_read_for_run_format_chain=false;
      return (-1);
    }
  set_rcs_print_tag(cms->BufferName); 
  CMS *lcms = cms;
  if(cms_for_read && ni &&  ni->use_cms_for_read_for_format_output)
    {
      lcms = cms_for_read;
      ni->use_cms_for_read_for_format_output=false;
      ni->use_cms_for_read_for_run_format_chain=true;
    }


  switch(channel_type)
    {
    case RCS_STAT_CHANNEL_TYPE:
      {	
	lcms->update_stat_msg_base = 1;
	lcms->update_stat_msg_base_in_format = 0;
      }
      break;

    case RCS_CMD_CHANNEL_TYPE:
      {
	lcms->update_cmd_msg_base = 1;
	lcms->update_cmd_msg_base_in_format = 0;
      }
      break;
      
    default:
      {
	lcms->update_cmd_msg_base = 1;
	lcms->update_cmd_msg_base_in_format = 0;
	lcms->update_stat_msg_base = 1;
	lcms->update_stat_msg_base_in_format = 0;
      }
      break;
    }

  if (lcms->force_raw)
    {
      set_rcs_print_tag(0); 
      return 0;
    }

  if (forced_type > 0)
    {
      new_type = forced_type;
    }

  switch (lcms->mode)
    {
    case CMS_RAW_OUT:
      if (lcms->current_alias && !lcms->neutral && !lcms->isserver)
	{
	  if (!lcms->current_alias->data)
	    {
	      lcms->current_alias->data = malloc (lcms->size);
	    }
	  if (lcms->current_alias->tptr)
	    {
	      memcpy (lcms->current_alias->data, lcms->subdiv_data, lcms->size);
	      lcms->current_alias->tptr (cms, lcms->subdiv_data, lcms->size,
					lcms->current_alias->data, lcms->size);
	    }
	}
      break;
    case CMS_DECODE:
      /* Check the status of CMS. */
      if (lcms->status == CMS_READ_OK ||
	  (ni->get_msg_type_only && lcms->status == CMS_READ_OLD))
	{
	  NML_FORMAT_PTR orig_alias_format_func = 0;
	  if (lcms->current_alias)
	    {
	      orig_alias_format_func = (NML_FORMAT_PTR)
		lcms->current_alias->extra_info;
	      lcms->current_alias->extra_info = 0;
	    }
	  /* Handle the generic part of the message. */
	  lcms->format_low_ptr = lcms->format_high_ptr = (char *) NULL;
	  lcms->rewind ();	/* Move to the start of encoded buffer. */
	  //lcms->beginClass("NMLmsgHeader");
	  if (forced_type > 0)
	    {
	      new_type = forced_type;
	    }
	  if (lcms->neutral_encoding_method != CMS_XML_ENCODING)
	    {
	      lcms->update_with_name ("type", new_type);	/* Get the message type from encoded buffer. */
	      if(ni->get_msg_type_only)
		{
		  ((NMLmsg *) lcms->subdiv_data)->type = new_type;	/* Store type in message. */
		  ni->get_msg_type_only=false;
		  return 0;
		}
	      lcms->update_with_name ("size", new_size);	/* Get the message size from encoded buffer. */
	      //lcms->endClass("NMLmsgHeader");

	      ((NMLmsg *) lcms->subdiv_data)->type = new_type;	/* Store type in message. */
	      ((NMLmsg *) lcms->subdiv_data)->size = new_size;	/* Store size in message. */


	      if (new_size > lcms->max_message_size)
		{
		  rcs_print_error ("NML: Message %ld of size  %ld \n",
				   new_type, new_size);
		  rcs_print_error
		    ("     too large for local buffer of %s of size %ld.\n",
		     lcms->BufferName, lcms->max_message_size);
		  if (verbose_nml_error_messages)
		    {
		      rcs_print_error
			("Check that all processes agree on buffer size.\n");
		    }
		  lcms->status = CMS_INSUFFICIENT_SPACE_ERROR;
		  if (lcms->current_alias)
		    {
		      lcms->current_alias->extra_info = (void *)
			orig_alias_format_func;
		    }
		  set_rcs_print_tag(0); 
		  return (-1);
		}
	    }

	  /* Check the list of format functions. */
	  if (!ignore_format_chain)
	    {
	      lcms->format_low_ptr = (char *) lcms->subdiv_data;
	      lcms->format_high_ptr = lcms->format_low_ptr + lcms->size;
	      if (NULL == format_chain)
		{
		  rcs_print_error ("NML::read: Format chain is NULL.\n");
		  if (lcms->current_alias)
		    {
		      lcms->current_alias->extra_info = (void *)
			orig_alias_format_func;
		    }
		  set_rcs_print_tag(0); 
		  return (-1);
		}
	      /* Run through the list of format functions. */
	      if (-1 == run_format_chain (new_type, lcms->subdiv_data))
		{
		  ni->use_cms_for_read_for_run_format_chain=false;
		  rcs_print_error ("NMLread: NMLformat error\n");
		  if (verbose_nml_error_messages)
		    {
		      rcs_print_error ("   (Buffer = %s, Process = %s)\n",
				       lcms->BufferName, lcms->ProcessName);
		    }
		  if (lcms->current_alias)
		    {
		      lcms->current_alias->extra_info = (void *)
			orig_alias_format_func;
		    }
		  set_rcs_print_tag(0); 
		  return (-1);
		}
	      ni->use_cms_for_read_for_run_format_chain=false;
	    }
	  if (lcms->current_alias)
	    {
	      if (!lcms->current_alias->data)
		{
		  lcms->current_alias->data = malloc (lcms->size);
		}
	      if (lcms->current_alias->tptr)
		{
		  memcpy (lcms->current_alias->data, lcms->subdiv_data,
			  lcms->size);
		  lcms->current_alias->tptr (cms, lcms->subdiv_data, lcms->size,
					    lcms->current_alias->data,
					    lcms->size);
		}
	      if (lcms->current_alias)
		{
		  lcms->current_alias->extra_info = (void *)
		    orig_alias_format_func;
		}
	    }

	}
      break;
    case CMS_ENCODE:
      /* Check the status of CMS. */
      if (lcms->status != CMS_MISC_ERROR)
	{
	  lcms->format_low_ptr = lcms->format_high_ptr = (char *) NULL;
	  lcms->rewind ();	/* Move to the start of the encoded buffer. */

	  if (lcms->current_alias)
	    {
	      if (!lcms->current_alias->data)
		{
		  lcms->current_alias->data = malloc (lcms->size);
		}
	      if (lcms->current_alias->tptr)
		{
		  memcpy (lcms->current_alias->data, lcms->subdiv_data,
			  lcms->size);
		  lcms->current_alias->tptr (cms, lcms->subdiv_data, lcms->size,
					    lcms->current_alias->data,
					    lcms->size);
		}
	    }
	  /* Get the type and size from the message. */
	  new_type = ((NMLmsg *) lcms->subdiv_data)->type;
	  new_size = ((NMLmsg *) lcms->subdiv_data)->size;

	  if (forced_type > 0)
	    {
	      new_type = forced_type;
	      ((NMLmsg *) lcms->subdiv_data)->type = forced_type;
	    }


	  /* Store the type and size in the encoded buffer. */
	  //lcms->beginClass("NMLmsgHeader");
	  lcms->update_with_name ("type", new_type);	/* Get the message type from encoded buffer. */
	  if(ni->get_msg_type_only)
	    {
	      ni->get_msg_type_only=false;
	      return 0;
	    }
	  lcms->update_with_name ("size", new_size);	/* Get the message size from encoded buffer. */
	  //lcms->endClass("NMLmsgHeader");

	  if (new_size > lcms->max_message_size)
	    {
	      rcs_print_error ("NML: Message %ld of size  %ld\n", new_type,
			       new_size);
	      rcs_print_error
		("     too large for local buffer of %s of size %ld.\n",
		 lcms->BufferName, lcms->max_message_size);
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error
		    ("Check that all processes agree on buffer size.\n");
		}
	      lcms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	      set_rcs_print_tag(0); 
	      return (-1);
	    }


	  /* Check the list of format functions. */
	  if (!ignore_format_chain)
	    {
	      lcms->format_low_ptr = (char *) lcms->subdiv_data;
	      lcms->format_high_ptr = lcms->format_low_ptr + lcms->size;
	      if (NULL == format_chain)
		{
		  rcs_print_error ("NML::read: Format chain is NULL.\n");
		  set_rcs_print_tag(0); 
		  return (-1);
		}

	      /* Run through the list of format functions. */
	      if (-1 == run_format_chain (new_type, lcms->subdiv_data))
		{
		  ni->use_cms_for_read_for_run_format_chain=false;
		  rcs_print_error ("NMLread: NMLformat error\n");
		  if (verbose_nml_error_messages)
		    {
		      rcs_print_error ("   (Buffer = %s, Process = %s)\n",
				       lcms->BufferName, lcms->ProcessName);
		    }
		  set_rcs_print_tag(0); 
		  return (-1);
		}
	      ni->use_cms_for_read_for_run_format_chain=false;

	      /* Get the new size of the message now that it's been encoded. */
	      lcms->get_encoded_msg_size ();
	    }
	}
      break;
    default:
      rcs_print_error ("NML::format_output: invalid format mode. (%d)\n",
		       lcms->mode);
      set_rcs_print_tag(0); 
      return (-1);
    }
  if (forced_type > 0)
    {
      new_type = forced_type;
      ((NMLmsg *) lcms->subdiv_data)->type = forced_type;
    }
  set_rcs_print_tag(0); 
  ni->use_cms_for_read_for_run_format_chain=false;
		  
  return (((int) lcms->status < 0) ? -1 : 0);
}

/*************************************************************
* NML Member Function: write_with_priority()
* Purpose: This write_with_priority function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write_with_priority (NMLmsg & nml_msg, int _priority)
{
  int retval = 0;
  set_rcs_print_tag(0); 
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName); 
      cms->set_priority(_priority);
    }
  retval = write(&nml_msg);
  if(cms)
    {
      cms->reset_priority();
    }
  set_rcs_print_tag(0);
  return retval;
}

/*************************************************************
* NML Member Function: write_with_priority()
* Purpose: This write_with_priority function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write_with_priority (NMLmsg *nml_msg, int _priority)
{
  int retval = 0;
  set_rcs_print_tag(0);
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName); 
      cms->set_priority(_priority);
    }

  retval = write(nml_msg);
  if(cms)
    {
      cms->reset_priority();
    }

  write_cc_list(nml_msg);
  set_rcs_print_tag(0);
  return retval;
}


/*************************************************************
* NML Member Function: write_with_bitwise_op()
* Purpose: This write_with_bitwise_op function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write_with_bitwise_op (NMLmsg & nml_msg, enum NML_BITWISE_OP_TYPE t)
{
  int retval = 0;
  set_rcs_print_tag(0);
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName); 
      switch(t)
	{
	case NML_BITWISE_NOP:
	  cms->set_bitwise_op(CMS_BITWISE_NOP);
	  break;

	case NML_BITWISE_AND_OP:
	  cms->set_bitwise_op(CMS_BITWISE_AND_OP);
	  break;
	case NML_BITWISE_OR_OP:
	  cms->set_bitwise_op(CMS_BITWISE_OR_OP);
	  break;

	default:
	  rcs_print_error("Invalid bitwise op %d\n",t);
	  break;
	}
    }
  retval = write(&nml_msg);
  if(cms)
    {
      cms->reset_bitwise_op();
    }
  set_rcs_print_tag(0);
  return retval;
}

/*************************************************************
* NML Member Function: write_with_bitwise_op()
* Purpose: This write_with_bitwise_op function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write_with_bitwise_op (NMLmsg *nml_msg, enum NML_BITWISE_OP_TYPE t)
{
  int retval = 0;
  set_rcs_print_tag(0);

  if(cms)
    {
      set_rcs_print_tag(cms->BufferName); 
      switch(t)
	{
	case NML_BITWISE_NOP:
	  cms->set_bitwise_op(CMS_BITWISE_NOP);
	  break;

	case NML_BITWISE_AND_OP:
	  cms->set_bitwise_op(CMS_BITWISE_AND_OP);
	  break;
	case NML_BITWISE_OR_OP:
	  cms->set_bitwise_op(CMS_BITWISE_OR_OP);
	  break;

	default:
	  rcs_print_error("Invalid bitwise op %d\n",t);
	  break;
	}
    }
  retval = write(nml_msg);
  if(cms)
    {
      cms->reset_bitwise_op();
    }
  write_cc_list(nml_msg);
  set_rcs_print_tag(0);
  return retval;
}


/*************************************************************
* NML Member Function: write()
* Purpose: This write function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write (NMLmsg & nml_msg)
{
  return (write (&nml_msg));	/* Call the other NML::write() */
}

/*************************************************************
* NML Member Function: write()
* Purpose: Writes a message to the global buffer.
* Parameters:
* NMLmsg *nml_msg - Address of the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
*************************************************************/
int
NML::write (NMLmsg * nml_msg)
{
  error_type = NML_NO_ERROR;
  set_rcs_print_tag(0);
  if (NULL == nml_msg)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write: Message is NULL.\n");
      return (-1);
    }

  if ((nml_msg->size == 0 || nml_msg->type == 0) && !cms->isserver)
    {
      if(cms) set_rcs_print_tag(cms->BufferName);
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write: Message size or type is zero.\n");
      rcs_print_error
	("NML: Check that the message was properly constructed.\n");
    }

  if(ni->required_type > 0 && nml_msg->type != ni->required_type)
    {
      rcs_print_error
	("NML: Configuration set required_type to %ld but write() called with type %ld.\n",
	 ni->required_type,nml_msg->type);
      error_type = NML_REQUIRED_MSG_TYPE_ERROR;
      return(-1);
    }


  write_cc_list(nml_msg);

  if (fast_mode)
    {
      set_rcs_print_tag(cms->BufferName);
      if(nml_msg->size > cms->size)
	{
	  rcs_print_error("Message of size %ld too large to be written in buffer of size %ld\n",
			  nml_msg->size,cms->size);
	  cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	  error_type = NML_INVALID_MESSAGE_ERROR;
	  if (!info_printed)
	    {
	      print_info ();
	    }
	  set_rcs_print_tag(0);
	  return(-1);
	}

      if(nml_msg->size > cms->max_message_size)
	{
	  rcs_print_error("Message of size %ld too large to be written in buffer of size %ld\n",
			  nml_msg->size,
			  cms->max_message_size);
	  cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	  error_type = NML_INVALID_MESSAGE_ERROR;
	  if (!info_printed)
	    {
	      print_info ();
	    }
	  set_rcs_print_tag(0);
	  return(-1);
	}
      *(cms_inbuffer_header_size) = nml_msg->size;
      cms->write (nml_msg);
      if (cms->status == CMS_WRITE_OK)
	{
	  set_rcs_print_tag(0);
	  return (0);
	}
      set_error ();
      set_rcs_print_tag(0);
      return (-1);
    }
  /* Check pointers. */
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::write: CMS not configured.\n");
	}
      return (-1);
    }

  /* Handle Phantom Buffers. */
  if (cms->is_phantom)
    {
      if (NULL != phantom_write)
	{
	  return ((*phantom_write) (nml_msg));
	}
      else
	{
	  return (0);
	}
    }

  set_rcs_print_tag(cms->BufferName);


  /* Set CMS to a write mode. */
  cms->set_mode (CMS_WRITE);

  if (cms->neutral_encoding_method == CMS_XML_ENCODING &&
      cms->enable_xml_differencing &&
      0 == ni->diffMsg && 0 != ni->lastMessageWritten && !cms->isserver)
    {
      if (ni->lastMessageWritten->type != 0
	  && ni->lastMessageWritten->type == nml_msg->type)
	{
	  setMessageForDiff (ni->lastMessageWritten);
	}
      else
	{
	  ni->lastMessageWritten->type = 0;
	  setMessageForDiff (0);
	}
    }

  if (cms->current_alias && cms->mode == CMS_RAW_IN
      && !cms->neutral && !cms->isserver)
    {
      if (!cms->current_alias->data)
	{
	  cms->current_alias->data = malloc (cms->size);
	}
      if (cms->current_alias->fptr)
	{
	  cms->current_alias->fptr (cms, nml_msg, nml_msg->size,
				    cms->current_alias->data, cms->size);
	  nml_msg = (NMLmsg *) cms->current_alias->data;
	}
    }

  /* Format the message if neccessary. */
  if (-1 == format_input (nml_msg))
    {
      error_type = NML_FORMAT_ERROR;
      set_rcs_print_tag(0);
      return -1;
    }


  if (CMS_RAW_IN == cms->mode)
    {
      cms->write (nml_msg);	/* Write the unformatted message.  */
      if (cms->enable_xml_logging)
	{
	  internal_xml_log("nml_write",nml_msg);
	}
    }
  else
    {
      cms->write (cms->subdiv_data);	/* Write the formatted message.  */
      if (cms->enable_xml_logging)
	{
	  internal_xml_log("nml_write",(NMLmsg*) cms->subdiv_data);
	}
    }

  if (CMS_WRITE_OK == cms->status)
    {
      error_type = NML_NO_ERROR;
      if (cms->neutral_encoding_method == CMS_XML_ENCODING &&
	  cms->enable_xml_differencing)
	{
	  if (0 == ni->lastMessageWritten)
	    {
	      ni->lastMessageWritten = (NMLmsg *) DEBUG_MALLOC (cms->size);
	    }
	  memcpy (ni->lastMessageWritten, nml_msg, nml_msg->size);
	  setMessageForDiff (0);
	}
      set_rcs_print_tag(0);
      return (0);
    }
  else
    {
      if (ni->lastMessageWritten)
	{
	  ni->lastMessageWritten->type = 0;
	}
    }
  set_rcs_print_tag(0);

  return set_error ();
}

/*************************************************************
* NML Member Function: set_error
* Purpose: This write function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
* Check error_type for more info.
*************************************************************/
int
NML::set_error ()
{
  set_rcs_print_tag(0);
  if (error_type != NML_NO_ERROR)
    {
      return -1;
    }

  if(get_cmscfg_last_buffer_was_ignored_remote())
  {
    clear_cmscfg_last_buffer_was_ignored_remote();
    error_type =  NML_IGNORED_REMOTE;
    return -1;
  }

  if(get_cmscfg_last_buffer_was_ignored_no_bufline())
  {
    clear_cmscfg_last_buffer_was_ignored_no_bufline();
    error_type =  NML_IGNORED_NO_BUFLINE;
    return -1;
  }

  if (NULL == cms)
    {
      error_type = NML_INVALID_CONFIGURATION;
      return 0;
    }

  /* Choose return value. */
  switch (cms->status)
    {
    case CMS_TIMED_OUT:
      error_type = NML_TIMED_OUT;
      return (-1);

    case CMS_QUEUE_FULL:
      error_type = NML_QUEUE_FULL_ERROR;
      break;

    case CMS_NO_MASTER_ERROR:
      error_type = NML_NO_MASTER_ERROR;
      break;

    case CMS_WRITE_WAS_BLOCKED:
      error_type = NML_BUFFER_NOT_READ;
      break;

    case CMS_STATUS_NOT_SET:	/* The status variable has not been set yet. */
    case CMS_READ_OLD:		/* Read successful, but data is old. */
    case CMS_READ_OK:		/* Read successful so far. */
    case CMS_WRITE_OK:		/* Write successful so far. */
    case CMS_CLEAR_OK:		/* A clear operation was successful.  */
      error_type = NML_NO_ERROR;
      break;

    case CMS_RESOURCE_CONFLICT_ERROR:
    case CMS_CREATE_ERROR:
    case CMS_CONFIG_ERROR:
      error_type = NML_INVALID_CONFIGURATION;
      break;

    case CMS_INTERRUPTED_OPERATION:
      error_type = NML_INTERRUPTED_OPERATION;
      break;

    case CMS_MISC_ERROR:
    default:
      error_type = NML_INTERNAL_CMS_ERROR;
      break;

    }

  if (error_type == NML_NO_ERROR)
    {
      return 0;
    }
  if (!info_printed)
    {
      if(cms->do_not_print_timeout_errors && 
	 cms->status == CMS_TIMED_OUT )
	{
	  return -1;
	}
      print_info ();
    }

  return -1;
}

/*************************************************************
* NML Member Function: write_if_read()
* Purpose: This write function provides users with an alternative
* style of writing a message.
* Parameters:
* NMLmsg &nml_msg - Reference to the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured. (Timeouts are considered errors.)
* Check error_type for more info.
*************************************************************/
int
NML::write_if_read (NMLmsg & nml_msg)
{
  return (write_if_read (&nml_msg));
}

/***********************************************************
* NML Member Function: write_if_read()
* Purpose: Write a message to the global buffer, but do not
*  over-write another message if that message is unread.
* Parameters:
*  NMLmsg *nml_msg - Address of the message to be written.
* Returns:
*  0 - The message was successfully written.
*  -1 - An error occured.
* (Timeouts, and unread buffers  are considered errors.)
* Check error_type for more info.
************************************************************/
int
NML::write_if_read (NMLmsg * nml_msg)
{
  error_type = NML_NO_ERROR;
  set_rcs_print_tag(0);

  if (NULL == nml_msg)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write: Message is NULL.\n");
      return (-1);
    }

  if ((nml_msg->size == 0 || nml_msg->type == 0) && !cms->isserver)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write: Message size or type is zero.\n");
      rcs_print_error
	("NML: Check that the message was properly constructed.\n");
    }
  if(ni->required_type > 0 && nml_msg->type != ni->required_type)
    {
      rcs_print_error
	("NML: Configuration set required_type to %ld but write() called with type %ld.\n",
	 ni->required_type,nml_msg->type);
      error_type = NML_REQUIRED_MSG_TYPE_ERROR;
      return(-1);
    }

  write_cc_list(nml_msg);

  if (fast_mode)
    {
      set_rcs_print_tag(cms->BufferName);
      cms->header.in_buffer_size = nml_msg->size;
      cms->write_if_read (nml_msg);
      set_rcs_print_tag(0);
      if (cms->status == CMS_WRITE_OK)
	{
	  return (0);
	}
      set_error ();
      return (-1);
    }
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::write_if_read: CMS not configured.\n");
	}
      return (-1);
    }

  set_rcs_print_tag(cms->BufferName);

  if (NULL == nml_msg)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write_if_read: Message is NULL.\n");
      set_rcs_print_tag(0);
      return (-1);
    }

  if ((nml_msg->size == 0 || nml_msg->type == 0) && !cms->isserver)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::write_if_read: Message size or type is zero.\n");
      if (verbose_nml_error_messages)
	{
	  rcs_print_error
	    ("NML: Check that the message was properly constructed.\n");
	}
    }

  if (cms->is_phantom)
    {
      set_rcs_print_tag(0);
      if (NULL != phantom_write_if_read)
	{
	  return ((*phantom_write_if_read) (nml_msg));
	}
      else
	{
	  return (0);
	}
    }


  cms->set_mode (CMS_WRITE);


  if (cms->neutral_encoding_method == CMS_XML_ENCODING &&
      cms->enable_xml_differencing &&
      0 == ni->diffMsg && 0 != ni->lastMessageWritten && !cms->isserver)
    {
      if (ni->lastMessageWritten->type != 0
	  && ni->lastMessageWritten->type == nml_msg->type)
	{
	  setMessageForDiff (ni->lastMessageWritten);
	}
      else
	{
	  ni->lastMessageWritten->type = 0;
	  setMessageForDiff (0);
	}
    }

  if (cms->current_alias && cms->mode == CMS_RAW_IN
      && !cms->neutral && !cms->isserver)
    {
      if (!cms->current_alias->data)
	{
	  cms->current_alias->data = malloc (cms->size);
	}
      if (cms->current_alias->fptr)
	{
	  cms->current_alias->fptr (cms, nml_msg, nml_msg->size,
				    cms->current_alias->data, cms->size);
	  nml_msg = (NMLmsg *) cms->current_alias->data;
	}
    }

  if (-1 == format_input (nml_msg))
    {
      error_type = NML_FORMAT_ERROR;
      return -1;
    }

  if (CMS_RAW_IN == cms->mode)
    {
      cms->write_if_read (nml_msg);
      if (cms->enable_xml_logging)
	{
	  internal_xml_log("nml_write",nml_msg);
	}
    }
  else
    {
      cms->write_if_read (cms->subdiv_data);
      if (cms->enable_xml_logging)
	{
	  internal_xml_log("nml_write",(NMLmsg *) cms->subdiv_data);
	}
    }


  if (CMS_WRITE_OK == cms->status)
    {
      error_type = NML_NO_ERROR;
      if (cms->neutral_encoding_method == CMS_XML_ENCODING &&
	  cms->enable_xml_differencing)
	{
	  if (0 == ni->lastMessageWritten)
	    {
	      ni->lastMessageWritten = (NMLmsg *) DEBUG_MALLOC (cms->size);
	    }
	  memcpy (ni->lastMessageWritten, nml_msg, nml_msg->size);
	  setMessageForDiff (0);
	}
      set_rcs_print_tag(0);
      return (0);
    }
  else
    {
      if (ni->lastMessageWritten)
	{
	  ni->lastMessageWritten->type = 0;
	}
    }
  set_rcs_print_tag(0);
  return (set_error ());
}

/*******************************************************************
* NML Member Function: format_input()
* Purpose: Formats the in an NML message to be writen to a CMS buffer
*  as required by the configuration file. The formatting converts
*  messages from some platform indepent format to a platform
*  specific format or vice versa. (Performing byte-swapping etc.)
* Parameters:
* NMLmsg *nml_msg - The address of the NML message.
* Returns:
* 0 = Success.
* -1 = Error.
* Notes:
*  1. There are 3 conditions that format_input may be called under.
*    i. The message will be written to the buffer without any formatting.
*       (cms->mode == CMS_RAW_IN)
*    ii. The message is in a native or raw format and needs to be encoded
*  in a neutral format before being sent over the network or into a
*  neutral buffer.
*        (cms->mode == CMS_ENCODE)
*   iii. The process calling this is a server which recieved a neutrally
* encoded buffer over the network which must be converted to native
* format before being written into a raw buffer.
*        (cms->mode == CMS_DECODE)
*  2. This function is for internal NML use only.
******************************************************************/
int
NML::format_input (NMLmsg * nml_msg)
{
  NML_FORMAT_PTR orig_alias_format_func = 0;
  set_rcs_print_tag(0);

  if (NULL == cms)
    {
      return -1;
    }
  set_rcs_print_tag(cms->BufferName);

  switch(channel_type)
    {
    case RCS_STAT_CHANNEL_TYPE:
      {	
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;

    case RCS_CMD_CHANNEL_TYPE:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
      }
      break;
      
    default:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;
    }

  if (ni->diffMsg)
    {
      cms->setBufferForDiff (ni->diffMsg, ni->diffMsg->size);
    }
  else
    {
      cms->setBufferForDiff (0, 0);
    }

  if (cms->force_raw)
    {
      cms->mode = CMS_RAW_IN;
    }


  switch (cms->mode)
    {
    case CMS_RAW_IN:
      /* Make sure the message size is not larger than the buffer size. */
      if (nml_msg->size > cms->size)
	{
	  rcs_print_error ("NML: Message size(%ld) too large for"
			   " CMS buffer size of %ld.\n",
			   nml_msg->size, cms->size);
	  cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	  set_rcs_print_tag(0);
	  return (-1);
	}

      if (nml_msg->size > cms->max_message_size)
	{
	  rcs_print_error ("NML: Message size(%ld) too large for"
			   " CMS max message size of %ld.\n",
			   nml_msg->size, cms->max_message_size);
	  cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	  set_rcs_print_tag(0);
	  return (-1);
	}
      cms->header.in_buffer_size = nml_msg->size;
      break;
    case CMS_ENCODE:
      if (cms->current_alias)
	{
	  if (!cms->current_alias->data)
	    {
	      cms->current_alias->data = malloc (cms->size);
	    }
	  if (cms->current_alias->fptr)
	    {
	      cms->current_alias->fptr (cms, nml_msg, nml_msg->size,
					cms->current_alias->data, cms->size);
	      nml_msg = (NMLmsg *) cms->current_alias->data;
	    }
	  orig_alias_format_func = (NML_FORMAT_PTR)
	    cms->current_alias->extra_info;
	  cms->current_alias->extra_info = 0;
	}
      rcs_print_debug (PRINT_MISC, "NML::format_input() mode=CMS_ENCODE\n");
      /* Make sure the message size is not larger than the buffer size. */
      if (nml_msg->size > cms->max_message_size)
	{
	  rcs_print_error ("NML: Message size(%ld) too large for"
			   " CMS buffer size of %ld.\n",
			   nml_msg->size, cms->max_message_size);
	  cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
	  set_rcs_print_tag(0);
	  return (-1);
	}

      cms->format_low_ptr = (char *) nml_msg;
      cms->format_high_ptr = cms->format_low_ptr + nml_msg->size;
      /* Handle the generic part of the message. */
      cms->rewind ();		/* Move to the start of the encoded buffer. */

      //cms->beginClass("NMLmsgHeader");
      cms->update_with_name ("type", nml_msg->type);	/* Get the message type from encoded buffer. */
      cms->update_with_name ("size", nml_msg->size);	/* Get the message size from encoded buffer. */
      //cms->endClass("NMLmsgHeader");

      /* Check list of format functions. */
      if (!ignore_format_chain)
	{
	  if (NULL == format_chain)
	    {
	      rcs_print_error ("NML::read: Format chain is NULL.\n");
	      if (cms->current_alias)
		{
		  cms->current_alias->extra_info =
		    (void *) orig_alias_format_func;
		}
	      set_rcs_print_tag(0);
	      return (-1);
	    }

	  /* Run through list of format functions. */
	  if (-1 == run_format_chain (nml_msg->type, nml_msg))
	    {
	      rcs_print_error ("NMLwrite: format error\n");
	      if (verbose_nml_error_messages)
		{
		  rcs_print_error ("   (Buffer = %s, Process = %s)\n",
				   cms->BufferName, cms->ProcessName);
		}
	      if (cms->current_alias)
		{
		  cms->current_alias->extra_info =
		    (void *) orig_alias_format_func;
		}
	      set_rcs_print_tag(0);
	      return (-1);
	    }
	}
      /* Determine the new  size of the message now that its encoded. */
      cms->header.in_buffer_size = cms->get_encoded_msg_size ();
      if (cms->current_alias)
	{
	  cms->current_alias->extra_info = (void *) orig_alias_format_func;
	}
      break;
    case CMS_DECODE:
      {
	NMLTYPE new_type=-1;
	long new_size=0;
	rcs_print_debug (PRINT_MISC, "NML::format_input() mode=CMS_DECODE\n");
	cms->format_low_ptr = cms->format_high_ptr = (char *) NULL;
	cms->rewind ();		/* Move to the start of the encoded buffer. */

	//cms->beginClass("NMLmsg header");
	if (cms->neutral_encoding_method != CMS_XML_ENCODING)
	  {
	    cms->update_with_name ("type", new_type);	/* Get the message type from encoded buffer. */
	    cms->update_with_name ("size", new_size);	/* Get the message size from encoded buffer. */
	    //cms->endClass("NMLmsg header");


	    /* Make sure the message size is not larger than the buffer size. */
	    if (new_size > cms->size)
	      {
		rcs_print_error ("NMLwrite: Message size(%ld) too large for"
				 " CMS buffer size of %ld.\n",
				 new_size, cms->size);
		cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
		set_rcs_print_tag(0);
		return (-1);
	      }

	    if (new_size > cms->max_message_size)
	      {
		rcs_print_error ("NMLwrite: Message size(%ld) too large for"
				 " CMS max message size of %ld.\n",
				 new_size, cms->max_message_size);
		cms->status = CMS_INSUFFICIENT_SPACE_ERROR;
		set_rcs_print_tag(0);
		return (-1);
	      }
	  }
	cms->format_low_ptr = (char *) cms->subdiv_data;
	cms->format_high_ptr = cms->format_low_ptr + cms->size;

	/* Store the new type and size in the raw message. */
	((NMLmsg *) cms->subdiv_data)->type = new_type;
	((NMLmsg *) cms->subdiv_data)->size = new_size;

	/* Check the list of format functions. */
	if (!ignore_format_chain)
	  {
	    if (NULL == format_chain)
	      {
		rcs_print_error ("NML::read: Format chain is NULL.\n");
		set_rcs_print_tag(0);
		return (-1);
	      }

	    /* Run through the list of format functions. */
	    if (-1 == run_format_chain (new_type, cms->subdiv_data))
	      {
		rcs_print_error ("NMLwrite: format error\n");
		rcs_print_error ("   (Buffer = %s, Process = %s)\n",
				 cms->BufferName, cms->ProcessName);
		set_rcs_print_tag(0);
		return (-1);
	      }
	  }
	/* Choose a size that will ensure the entire message will be read out. */
	if (cms->format_size < ((long) sizeof (NMLmsg)))
	  {
	    cms->format_size = sizeof (NMLmsg);
	  }
	if (cms->format_size > new_size &&
	    new_size == ((NMLmsg *) cms->subdiv_data)->size &&
	    cms->neutral_encoding_method != CMS_XML_ENCODING)
	  {
	    ((NMLmsg *) cms->subdiv_data)->size = (long) cms->format_size;
	  }
	cms->header.in_buffer_size = ((NMLmsg *) cms->subdiv_data)->size;
	if (cms->current_alias)
	  {
	    if (!cms->current_alias->data)
	      {
		cms->current_alias->data = malloc (cms->size);
	      }
	    if (cms->current_alias->fptr)
	      {
		memcpy (cms->current_alias->data, cms->subdiv_data, cms->size);
		cms->current_alias->fptr (cms, cms->subdiv_data, cms->size,
					  cms->current_alias->data, cms->size);
		nml_msg = (NMLmsg *) cms->current_alias->data;
	      }
	  }
      }
      break;
    default:
      rcs_print_error ("NML::format_input: invalid mode (%d).\n", cms->mode);
      set_rcs_print_tag(0);
      return (-1);
    }
  ni->diffMsg = 0;
  set_rcs_print_tag(0);

  return (((int) cms->status < 0) ? -1 : 0);
}


int
NML::run_format_chain (NMLTYPE type, void *buf)
{
  int retval=0;
  set_rcs_print_tag(0);
  CMS *lcms = cms;
  if(cms_for_read && ni && ni->use_cms_for_read_for_run_format_chain)
    {
      lcms=cms_for_read;
      ni->use_cms_for_read_for_run_format_chain=false;
    }

  NML_FORMAT_PTR format_function;

  if(!cms)
    {
      rcs_print_error("NML::run_format_chain() called when cms is NULL\n");
      return -1;
    }
  set_rcs_print_tag(lcms->BufferName);

  if (lcms->current_alias != 0)
    {
      format_function = (NML_FORMAT_PTR) lcms->current_alias->extra_info;
      if (0 != format_function)
	{
	  lcms->extra_data=extra_data;
	  lcms->min_message_size = ni->min_message_size;
	  lcms->message_size_add = ni->message_size_add;
	  lcms->message_size_roundup = ni->message_size_roundup;
	  retval = ((*format_function) (type, buf, lcms));
	  if(lcms->updater != NULL 
	     && lcms->neutral_encoding_method == CMS_XML_ENCODING 
	     && !lcms->updater->check_type_info_called)
	    {
	      rcs_print_error("Format function never called lcms->check_type_info and we are using XML. Check that the function was generated/written using the newer \"update_with_name\" style.");
	      set_rcs_print_tag(0);
	      retval=-1;
	    }
	  if(lcms->status == CMS_UPDATE_ERROR) {
	    return(-1);
	  }	
	  return retval;
	}
    }
  if(!format_chain)
    {
      rcs_print_error("NML::run_format_chain() called with format_chain=NULL\n");
      set_rcs_print_tag(0);
      return -1;
    }

  rcs_print_debug (PRINT_MISC,
		   "NML::run_format_chain(type=%ld,buf=%p) mode=CMS_ENCODE\n",
		   (long) type, buf);
  format_function = (NML_FORMAT_PTR) format_chain->get_head ();
  lcms->extra_data=extra_data;
  lcms->min_message_size = ni->min_message_size;
  lcms->message_size_add = ni->message_size_add;
  lcms->message_size_roundup = ni->message_size_roundup;
  while (NULL != format_function)
    {
      switch ((*format_function) (type, buf, lcms))
	{
	case -1:
	  set_rcs_print_tag(0);
	  return (-1);
	case 0:
	  break;
	case 1:
	  if(lcms->updater != NULL 
	     && lcms->neutral_encoding_method == CMS_XML_ENCODING 
	     && !lcms->updater->check_type_info_called)
	    {
	      rcs_print_error("Format function never called lcms->check_type_info and we are using XML. Check that the function was generated/written using the newer \"update_with_name\" style.");
	      set_rcs_print_tag(0);
	      return(-1);
	    }
	  set_rcs_print_tag(0);
	  if(lcms->status == CMS_UPDATE_ERROR) {
	    return(-1);
	  }
	  return (0);
	}
      format_function = (NML_FORMAT_PTR) format_chain->get_next ();
    }
  if(lcms->updater != NULL 
     && lcms->neutral_encoding_method == CMS_XML_ENCODING 
     && !lcms->updater->check_type_info_called)
    {
      rcs_print_error("Format function never called lcms->check_type_info and we are using XML. Check that the function was generated/written using the newer \"update_with_name\" style.\n");
      set_rcs_print_tag(0);
      return(-1);
    }
  set_rcs_print_tag(0);
  if(lcms->status == CMS_UPDATE_ERROR) {
    return(-1);
  }
  return (0);
}

int
NML::prefix_format_chain (NML_FORMAT_PTR f_ptr)
{
  set_rcs_print_tag(0);
  if(!ni)
    {
      return -1;
    }
  ni->max_size_from_format=0;
  ni->max_size_from_format_set=false;

  if (NULL == format_chain)
    {
      format_chain = new RCS_LINKED_LIST;
    }
  if (NULL != format_chain && f_ptr)
    {
      bool already_added=false;
      NML_FORMAT_PTR fc_ptr = (NML_FORMAT_PTR) format_chain->get_head();
      while(fc_ptr)
	{
	  if(fc_ptr == f_ptr)
	    {
	      already_added=true;
	      break;
	    }
	  fc_ptr = (NML_FORMAT_PTR) format_chain->get_next();
	}
      if(!already_added)
	{
	  format_chain->store_at_head ((void *) f_ptr, 0, 0);
	}
    }
  check_format_name();

  setup_cc_list_format_chain();
  return (0);
}


int
NML::check_if_transfers_complete ()
{
  set_rcs_print_tag(0);
  if (NULL == cms)
    {
      return 1;
    }
  return cms->check_if_transfers_complete ();
}


/**************************************************************************
* NML member function: msg2str
* Parameter: NMLmsg &msg -- Reference to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::msg2str (NMLmsg & msg)
{
  return msg2str (&msg);
}


/**************************************************************************
* NML member function: msg2str
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::msg2str (NMLmsg * nml_msg)
{
  CMS *orig_cms = cms;
  char *str = NULL;
  set_rcs_print_tag(0);
  if (NULL == nml_msg)
    {
      return NULL;
    }
  if (NULL == cms)
    {
      int msg_length = nml_msg->size;
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ((ni->cms_for_msg_string_conversions->size > 16 * msg_length &&
	       ni->cms_for_msg_string_conversions->size > 2048) ||
	      ni->cms_for_msg_string_conversions->size < 4 * msg_length)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  ni->cms_for_msg_string_conversions =
	    new CMS (nml_msg->size * 4 + 16 + (16 - (nml_msg->size % 16)));
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_DISPLAY_ASCII_ENCODING);
  cms->set_mode (CMS_ENCODE);
  if (-1 == format_input (nml_msg))
    {
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms = orig_cms;
      return ((char *) NULL);
    }
  str = (char *) cms->encoded_data;
  cms->restore_normal_updater ();
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
  cms = orig_cms;
  return (const char *) str;
}


/**************************************************************************
* NML member function: msg2xml
* Parameter: NMLmsg &msg -- Reference to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::msg2xml (
#ifdef ENABLE_RCS_XML
	      NMLmsg & msg
#else
	      __unused_parameter__ NMLmsg &
#endif
	      )
{
#ifdef ENABLE_RCS_XML
  return msg2xml (&msg);
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}


/**************************************************************************
* NML member function: msg2xml
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::msg2xml (
#ifdef ENABLE_RCS_XML
	      NMLmsg  *nml_msg
#else
	      __unused_parameter__ NMLmsg *
#endif
	      )
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_XML
  CMS *orig_cms;
  char *str;
  size_t new_size;

  rcs_print_debug (PRINT_MISC, "NML::msg2xml()\n");

  orig_cms = cms;
  str = NULL;
  new_size=0;

  if (NULL == nml_msg)
    {
      return NULL;
    }
  get_max_size_from_format();

  if (NULL == cms)
    {
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ( ni->cms_for_msg_string_conversions->size < nml_msg->size)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = round_up_size(nml_msg->size,8192);
	  if(((size_t)NML::forced_min_size) > new_size) {
	    new_size = (long) round_up_size(NML::forced_min_size,8192);
	  }
	  ni->cms_for_msg_string_conversions = new CMS ( (unsigned long) new_size);
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_XML_ENCODING);
  cms->set_mode (CMS_ENCODE);
  ((CMS_XML_UPDATER *) cms->temp_updater)->make_xml_pretty = 1;
  if (-1 == format_input (nml_msg))
    {
      ((CMS_XML_UPDATER *) cms->temp_updater)->make_xml_pretty = 0;
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms = orig_cms;
      return ((char *) NULL);
    }
  ((CMS_XML_UPDATER *) cms->temp_updater)->make_xml_pretty = 0;
  str = (char *) cms->encoded_data;
  cms->restore_normal_updater ();
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
  cms = orig_cms;
  return (const char *) str;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}

void
NML::write_encoded_data_to_file(NMLmsg *nml_msg, 
            const int _encoding_method,
            const char *filename)
{
    void *encoded_data=0;
    long encoded_size = 0;
    if(!nml_msg)
      {
	rcs_print_error("NML::write_encoded_data_to_file nml_msg is NULL\n");
	return;
      }
    if(!nml_msg->type)
      {
	rcs_print_error("NML::write_encoded_data_to_file nml_msg->type is 0\n");
	return;
      }
    this->msg_to_encoded_data(nml_msg,encoded_data,encoded_size,_encoding_method);
    if(encoded_size > 0)
    {
        FILE *f = fopen(filename,"wb");
        if(!f)
        {
            rcs_print_error("Could not open %s for writing. -- %s\n", filename, strerror(errno));
            return;
        }
	if(_encoding_method == CMS_XML_ENCODING && 
	   encoded_size > 1 &&
	   ((char *)encoded_data)[encoded_size-1] == 0)
	  {
	    fwrite(encoded_data,encoded_size-1,1,f);
	  }
	else
	  {
	    fwrite(encoded_data,encoded_size,1,f);
	  }
        fclose(f);
    }   
}

void
NML::msg_to_encoded_data(NMLmsg *nml_msg, 
		    void *&encoded_data, long &encoded_size,
		    const int _encoding_method)
{
  set_rcs_print_tag(0);
  size_t new_size=0;

  if(!nml_msg)
    {
      rcs_print_error("NML::msg_to_encoded_data nml_msg is NULL\n");
      return;
    }
  if(!nml_msg->type)
    {
      rcs_print_error("NML::msg_to_encoded_data nml_msg->type is 0\n");
      return;
    }

  CMS *orig_cms = cms;
  const enum CMS_NEUTRAL_ENCODING_METHOD encoding_method = (enum CMS_NEUTRAL_ENCODING_METHOD) _encoding_method;	  

  if (NULL == cms)
    {
      long size_needed = nml_msg->size;
      if(((long)NML::forced_min_size) > size_needed) {
	size_needed = (long) round_up_size(NML::forced_min_size,8192);
      }
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ( ni->cms_for_msg_string_conversions->size < size_needed)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = (long) round_up_size(size_needed,8192);
	  ni->cms_for_msg_string_conversions = new CMS ( (unsigned long) new_size);
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;
    }
  
  cms->set_temp_updater(encoding_method);

  switch(channel_type)
    {
    case RCS_STAT_CHANNEL_TYPE:
      {	
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;

    case RCS_CMD_CHANNEL_TYPE:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
      }
      break;
      
    default:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;
    }

  cms->updater->set_mode(CMS_ENCODE_DATA);
  cms->format_low_ptr = (char *) nml_msg;
  cms->format_high_ptr = cms->format_low_ptr + nml_msg->size;
  cms->rewind();
  cms->update_with_name ("type", nml_msg->type);
  cms->update_with_name ("size", nml_msg->size);
  run_format_chain(nml_msg->type,nml_msg);
  encoded_data = cms->encoded_data;
  encoded_size = cms->get_encoded_msg_size();
  cms->restore_normal_updater();
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
   cms = orig_cms;
}

NMLmsg *
NML::encoded_data_to_msg(void *encoded_data, 
                        long encoded_size,
                        const int _encoding_method)
{
  set_rcs_print_tag(0);
  CMS *orig_cms = cms;
  size_t new_size=0;
  
  const enum CMS_NEUTRAL_ENCODING_METHOD encoding_method = (enum CMS_NEUTRAL_ENCODING_METHOD) _encoding_method;	  

  if (NULL == cms)
    {
      long max_size = this->get_max_size_from_format();
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ( ni->cms_for_msg_string_conversions->size < max_size ||
	       ni->cms_for_msg_string_conversions->encoded_data_size < 
	       encoded_size)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = round_up_size(max_size>encoded_size?max_size:encoded_size,8192);
	  ni->cms_for_msg_string_conversions = new CMS ( (unsigned long) new_size);
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (encoding_method);
  bool orig_zero_encoded_data_when_set = (bool) (cms->zero_encoded_data_when_set != 0);
  void *orig_encoded_data = cms->encoded_data;
  long orig_encoded_data_size= cms->encoded_data_size;
  cms->encoded_data=0;
  cms->encoded_data_size=0;
  cms->zero_encoded_data_when_set=false;
  cms->set_encoded_data(encoded_data,encoded_size);
  cms->zero_encoded_data_when_set=orig_zero_encoded_data_when_set;

  switch(channel_type)
    {
    case RCS_STAT_CHANNEL_TYPE:
      {	
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;

    case RCS_CMD_CHANNEL_TYPE:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
      }
      break;
      
    default:
      {
	cms->update_cmd_msg_base = 1;
	cms->update_cmd_msg_base_in_format = 0;
	cms->update_stat_msg_base = 1;
	cms->update_stat_msg_base_in_format = 0;
      }
      break;
    }

  cms->updater->set_mode(CMS_DECODE_DATA);
  cms->rewind();
  NMLmsg *nml_msg = (NMLmsg *) cms->subdiv_data;
  /* Handle the generic part of the message. */
  cms->format_low_ptr = (char *) cms->subdiv_data;
  cms->format_high_ptr = cms->format_low_ptr + cms->size;
  cms->update_with_name ("type",nml_msg->type );
  cms->update_with_name ("size", nml_msg->size);
  run_format_chain(nml_msg->type,nml_msg);
  cms->set_encoded_data(orig_encoded_data,orig_encoded_data_size);
  cms->restore_normal_updater();
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
  cms = orig_cms;
  return (NMLmsg *) nml_msg;
}

void
NML::writeEncodedMessageMemoryMapToFile(NMLmsg *nml_msgP,  const int encoding_method, const char *filename)
{
  set_rcs_print_tag(0);
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName);
    }
  const char *s = this->getEncodedMessageMemoryMap(nml_msgP,encoding_method);
  if(s && *s)
    {
      FILE *f = fopen(filename,"wb");
      if(!f)
        {
	  rcs_print_error("can't open %s for writing. -- %s", 
			  filename,strerror(errno));
	  return;
        }
      size_t slen=strlen(s);
      size_t fwrite_ret =fwrite(s,1,slen,f);
      if(fwrite_ret < slen)
        {
	  rcs_print_error("fwrite(%p,1,%d,%p) to %s returned only %d. -- %s", 
			  s,((int) slen),
			  f,filename,
			  ((int)fwrite_ret),
			  strerror(errno));
        }
      fclose(f);
    }
  set_rcs_print_tag(0);
}



/**************************************************************************
* NML member function: msg2xml
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::getEncodedMessageMemoryMap(
                    	 NMLmsg  *nml_msg,
                         const int _encoding_method
	      )
{
  const enum CMS_NEUTRAL_ENCODING_METHOD encoding_method = (enum CMS_NEUTRAL_ENCODING_METHOD) _encoding_method;	  
  set_rcs_print_tag(0);
  CMS *orig_cms=0;
  char *str=0;
  size_t new_size=0;

  rcs_print_debug (PRINT_MISC, "NML::getMessageMemoryMap()\n");

  orig_cms = cms;
  str = NULL;
  new_size=0;

  if (NULL == nml_msg)
    {
      return NULL;
    }
  if (NULL == cms)
    {
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ( ni->cms_for_msg_string_conversions->size < nml_msg->size)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = round_up_size(nml_msg->size,8192);
	  ni->cms_for_msg_string_conversions = new CMS ( (unsigned long) new_size);
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;

      switch(channel_type)
	{
	case RCS_STAT_CHANNEL_TYPE:
	  {	
	    cms->update_stat_msg_base = 1;
	    cms->update_stat_msg_base_in_format = 0;
	  }
	  break;

	case RCS_CMD_CHANNEL_TYPE:
	  {
	    cms->update_cmd_msg_base = 1;
	    cms->update_cmd_msg_base_in_format = 0;
	  }
	  break;
      
	default:
	  {
	    cms->update_cmd_msg_base = 1;
	    cms->update_cmd_msg_base_in_format = 0;
	    cms->update_stat_msg_base = 1;
	    cms->update_stat_msg_base_in_format = 0;
	  }
	  break;
	}
    }
  cms->set_temp_updater (encoding_method);
  cms->set_mode (CMS_ENCODE);
  bool orig_enable_message_memory_map= cms->enable_message_memory_map;
  cms->enable_message_memory_map=true;
  bool orig_memory_map_use_raw = cms->memory_map_use_raw;
  cms->memory_map_use_raw = false;
  bool orig_looking_for_dvar = cms->looking_for_dvar;
  //cms->looking_for_dvar=true;
  cms->set_message_memory_map_size(nml_msg->size);
  cms->memory_map_offset=0;
  if (-1 == format_input (nml_msg))
    {
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms->enable_message_memory_map=orig_enable_message_memory_map;
      cms->looking_for_dvar= orig_looking_for_dvar;
      cms->memory_map_use_raw = orig_memory_map_use_raw;
      cms = orig_cms;
      return ((char *) NULL);
    }
  cms->restore_normal_updater ();
  str = cms->message_memory_map_cP;
  cms->enable_message_memory_map=orig_enable_message_memory_map;
  cms->looking_for_dvar= orig_looking_for_dvar;
  cms->memory_map_use_raw = orig_memory_map_use_raw;
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
  cms = orig_cms;
  return (const char *) str;
}

void
NML::writeRawMessageMemoryMapToFile(NMLmsg *nml_msgP, const char *filename)
{
    const char *s = this->getRawMessageMemoryMap(nml_msgP);
    if(s)
    {
        FILE *f = fopen(filename,"wb");
        if(!f)
        {
            rcs_print_error("can't open %s for writing. -- %s", filename,strerror(errno));
            return;
        }
	size_t slen=strlen(s);
	size_t fwrite_ret = fwrite(s,1,slen,f);
        if(fwrite(s,1,slen,f) < slen)
        {
	  rcs_print_error("fwrite(%p,1,%d,%p) to %s returned only %d. -- %s", 
			  s,((int)slen),
			  f,filename,
			  ((int)fwrite_ret),
			  strerror(errno));
        }
        fclose(f);
    }
}

/**************************************************************************
* NML member function: msg2xml
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::getRawMessageMemoryMap(
			 NMLmsg  *nml_msg
	      )
{
  set_rcs_print_tag(0);
  CMS *orig_cms;
  char *str;
  size_t new_size;

  rcs_print_debug (PRINT_MISC, "NML::getMessageMemoryMap()\n");

  orig_cms = cms;
  str = NULL;
  new_size=0;

  if (NULL == nml_msg)
    {
      return NULL;
    }
  if (NULL == cms)
    {
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ( ni->cms_for_msg_string_conversions->size < nml_msg->size)
	    {
	      delete ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = round_up_size(nml_msg->size,8192);
	  ni->cms_for_msg_string_conversions = new CMS ( (unsigned long) new_size);
	}
      ni->cms_for_msg_string_conversions->xml_style_properties =
	ni->xml_style_properties;
      ni->cms_for_msg_string_conversions->global_xml_style_properties_count =
	ni->global_xml_style_properties_count;
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_PACKED_ENCODING);
  cms->set_mode (CMS_ENCODE);
  bool orig_enable_message_memory_map= cms->enable_message_memory_map;
  cms->enable_message_memory_map=true;
  bool orig_looking_for_dvar = cms->looking_for_dvar;
  //cms->looking_for_dvar=true;
  bool orig_memory_map_use_raw = cms->memory_map_use_raw;
  cms->memory_map_use_raw = true;
  cms->set_message_memory_map_size(nml_msg->size);
  if (-1 == format_input (nml_msg))
    {
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms->enable_message_memory_map=orig_enable_message_memory_map;
      cms->looking_for_dvar= orig_looking_for_dvar;
      cms->memory_map_use_raw = orig_memory_map_use_raw;
      cms = orig_cms;
      return ((char *) NULL);
    }
  cms->restore_normal_updater ();
  str = cms->message_memory_map_cP;
  cms->enable_message_memory_map=orig_enable_message_memory_map;
  cms->looking_for_dvar= orig_looking_for_dvar;
  cms->memory_map_use_raw = orig_memory_map_use_raw;
  if(0 != ni->cms_for_msg_string_conversions && orig_cms == 0)
    {
      ni->cms_for_msg_string_conversions->xml_style_properties=0;
    }
  cms = orig_cms;
  return (const char *) str;
}

/**************************************************************************
* NML member function: msg2xmlSchema
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a string.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
const char *
NML::xmlSchema (void)
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_XML
  SchemaGenMsg *tempschemaMsg = 0;
  CMS *orig_cms = cms;
  char *str = NULL;

  if (NULL == cms)
    {
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  ni->cms_for_msg_string_conversions = new CMS (65536);
	}
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_XML_ENCODING);
  cms->set_mode (CMS_ENCODE);
  if (0 == ni->msgForSchemaGen)
    {
      tempschemaMsg = new SchemaGenMsg ();
      ni->msgForSchemaGen =
	(SchemaGenMsg *) DEBUG_MALLOC (cms->size + sizeof (SchemaGenMsg));
      *ni->msgForSchemaGen = *tempschemaMsg;
      delete tempschemaMsg;
    }
  NMLmsg *nml_msg = (NMLmsg *) ni->msgForSchemaGen;
  ((CMS_XML_UPDATER *) cms->updater)->startSchemaGen ();
  while (!((CMS_XML_UPDATER *) cms->updater)->is_schema_ready ())
    {
      if (-1 == format_input (nml_msg))
	{
	  ((CMS_XML_UPDATER *) cms->updater)->startSchemaGen ();
	  cms->restore_normal_updater ();
	  error_type = NML_FORMAT_ERROR;
	  cms = orig_cms;
	  return ((char *) NULL);
	}
    }
  ((CMS_XML_UPDATER *) cms->updater)->cancelSchemaGen ();
  str = (char *) cms->encoded_data;
  cms->restore_normal_updater ();
  cms = orig_cms;
  return (const char *) str;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}

#ifdef ENABLE_RCS_XML
static int
writestr_with_repeated_tries (int fd, const char *cdata)
{
  long cdatasize = (long) strlen (cdata);
  long bytes_written = 0;
  while (bytes_written < cdatasize)
    {
#ifndef WIN32
      ssize_t write_ret;
#else
      unsigned long write_ret;
#endif
      const char *cptr = cdata + bytes_written;
      const void *vptr = (void *) cptr;
      size_t writesize = cdatasize - bytes_written;
#ifdef VXWORKS
#define LOCAL_CHAR_PTR_CAST (char *)
#else
#define LOCAL_CHAR_PTR_CAST
#endif
      if ((write_ret = write (fd, LOCAL_CHAR_PTR_CAST vptr, (unsigned int) writesize)) < 0)
	{
	  rcs_print_error ("write failed -- %d:%s\n",
			   errno, strerror (errno));
	  return -1;
	}
#ifdef LOCAL_CHAR_PTR_CAST
#undef LOCAL_CHAR_PTR_CAST
#endif
      bytes_written += write_ret;
    }
  return bytes_written;
}
#endif

int NML::internal_xml_log(
#ifdef ENABLE_RCS_XML
			  const char *log_type, 
			  NMLmsg *msg
#else
			  __unused_parameter__ const char *, 
			  __unused_parameter__ NMLmsg *
#endif
			  )
{
#ifdef ENABLE_RCS_XML
  const char *xml_data = 0;
  CMS_HEADER cms_header_copy = cms->header;
  if(0 == cms || 0 == cms->ProcessName || 0 == cms->BufferName)
    {
      cms->header = cms_header_copy;
      return -1;
    }
  if((cms->neutral ||  cms->isserver) 
     && cms->neutral_encoding_method == CMS_XML_ENCODING )
    {
      if(((char *)cms->encoded_data)[0] == '<')
	{
	  xml_data = (char *) cms->encoded_data;
	}
    }
  else if (msg != 0)
    {
      xml_data = msg2xml(msg);
    }
  if(0 == xml_data)
    {
      cms->header = cms_header_copy;
      return -1;
    }

  SNPRINTF_FUNC ( SNPRINTF_ARGS(ni->xmllogfilename,sizeof(ni->xmllogfilename)),
		  "%s_%s_%s_%s_%s_%d_%d.xml",
		  (cms->nmltypename?cms->nmltypename:""),log_type, 
		  cms->ProcessName, cms->BufferName,
		  cms->short_status_string (cms->status), 
		  ni->pid, ni->xmllogcount);

  ni->xmllogcount++;
  int fd = open (ni->xmllogfilename, O_CREAT | O_TRUNC | O_WRONLY, 0644);
  if (fd < 0)
    {
      cms->header = cms_header_copy;
      return -1;
    }
  
  if (writestr_with_repeated_tries (fd, xml_data) < 0)
    {
      close (fd);
      cms->header = cms_header_copy;
      return -1;
    }
  if (writestr_with_repeated_tries (fd, "\n") < 0)
  {
    close (fd);
    cms->header = cms_header_copy;
    return -1;
  }
  cms->header = cms_header_copy;
  return 0;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

int
NML::xmlSchemaSaveAs (
#ifdef ENABLE_RCS_XML
		      const char *filename
#else
		      __unused_parameter__ const char *
#endif
		      )
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_XML
  int fd = open (filename, O_CREAT | O_TRUNC | O_WRONLY, 0644);
  if (fd < 0)
    {
      rcs_print_error ("xmlSchemaSaveAs(%s) : open failed -- %d:%s\n",
		       filename, errno, strerror (errno));
      return -1;
    }
  const char *xmlschema = xmlSchema ();
  if (0 == xmlschema)
    {
      rcs_print_error ("xmlSchema() failed\n");
      close (fd);
      return -1;
    }
  if (writestr_with_repeated_tries (fd, xmlschema) < 0)
    {
      close (fd);
      return -1;
    }
#ifdef HAVE_FSYNC
  if (fsync (fd) < 0)
    {
      rcs_print_error ("xmlSchemaSaveAs: fsync failed --%d:%s\n",
		       errno, strerror (errno));
      close (fd);
      return -1;
    }
#endif
  close (fd);
  return 0;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

NMLmsg *
NML::readMsgFromXmlFile (
#ifdef ENABLE_RCS_XML
			 const char *filename
#else
			 __unused_parameter__ const char *
#endif
			 )
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_XML
  rcs_print_debug (PRINT_MISC, "NML::readMsgFromXmlFile(%s)\n", filename);

#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *ifp = 0;
#endif
  FILE *fp = 0;
  CMS *orig_cms = cms;

  size_t size_needed = 65536;

#if HAVE_STAT
  struct stat stat_struct;
  if(stat(filename,&stat_struct) < 0)
    {
      fprintf(stderr,"stat(%s) failed -- %s\n",
	      filename,strerror(errno));
      return 0;
    }
  if(stat_struct.st_size == 0)
    {
      return 0;
    }
  if(stat_struct.st_size > 0 &&
     size_needed < ((size_t) stat_struct.st_size))
    {
      size_needed = stat_struct.st_size;
    }
#endif
  size_t max_message_size = (size_t) get_max_size_from_format();
  if(size_needed < max_message_size)
    {
      size_needed = max_message_size;
    }
  size_needed += (4096 - size_needed%4096);

  if (NULL == cms)
    {
      if(ni->cms_for_msg_string_conversions &&
	 ((size_t) ni->cms_for_msg_string_conversions->size) < size_needed)
	{
	  delete ni->cms_for_msg_string_conversions;
	  ni->cms_for_msg_string_conversions=NULL;
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  ni->cms_for_msg_string_conversions = new CMS ((long) size_needed);
	}
      cms = ni->cms_for_msg_string_conversions;
    }

  cms->set_temp_updater (CMS_XML_ENCODING);
  cms->set_mode (CMS_ENCODE);
  size_t xml_size = cms->encoded_data_size;
  cms->restore_normal_updater ();

  if (filename)
    {
#ifdef ENABLE_RCS_INET_FILES
      ifp = inet_file_open ((char *) filename, "r");
      if (ifp == 0)
	{
	  rcs_print_error ("readMsgFromXmlFile(%s) : open failed %d:%s\n",
			   filename, errno, strerror (errno));
	  return NULL;
	}
#else
      fp = fopen (filename, "r");
      if (fp == 0)
	{
	  rcs_print_error ("readMsgFromXmlFile(%s) : open failed %d:%s\n",
			   filename, errno, strerror (errno));
	  return NULL;
	}
#endif
    }
  else
    {
      fp = stdin;
    }

  int bytes_read_this_time = 0;
  char *xmldata = (char *) DEBUG_MALLOC (xml_size);
  char *ptr = xmldata;
  int bytes_read = 0;


  while (1)
    {
#ifdef ENABLE_RCS_INET_FILES
      if (ifp)
	{
	  if (inet_file_feof (ifp))
	    {
	      break;
	    }
	}
      else
	{
	  if (feof (fp))
	    {
	      break;
	    }
	}
#else
      if (feof (fp))
	{
	  break;
	}
#endif

      if (bytes_read + 1 >= ((int)xml_size) )
	{
	  rcs_print_error ("xml message file is too large.\n");
	  DEBUG_FREE (xmldata);
#ifdef ENABLE_RCS_INET_FILES
	  if (ifp)
	    {
	      inet_file_close (ifp);
	    }
	  else
	    {
	      fclose (fp);
	    }
#else
	  fclose (fp);
#endif
	  return NULL;
	}
#ifdef ENABLE_RCS_INET_FILES
      if (ifp)
	{
	  if ((inet_file_gets (ptr, xml_size - bytes_read - 1, ifp)) == NULL)
	    {
	      break;
	    }
	}
      else
	{
	  if ((fgets (ptr, xml_size - bytes_read - 1, fp)) == NULL)
	    {
	      break;
	    }
	}
#else
      if ((fgets (ptr, (int) (xml_size - bytes_read - 1), fp)) == NULL)
	{
	  break;
	}
#endif
      rcs_print_debug (PRINT_MISC, "NML::readMsgFromXmlFile read %s\n", ptr);
      bytes_read_this_time = (int) strlen (ptr);
      if (bytes_read_this_time > 0)
	{
	  ptr += bytes_read_this_time;
	  bytes_read += bytes_read_this_time;
	}
    }
  *ptr = 0;
#ifdef ENABLE_RCS_INET_FILES
  if (ifp)
    {
      inet_file_close (ifp);
    }
  else
    {
      fclose (fp);
    }
#else
  fclose (fp);
#endif
  cms = orig_cms;
  NMLmsg *msgToReturn = 0;
  if (xml2msg (xmldata) > 0)
    {
      msgToReturn = get_address ();
    }
  DEBUG_FREE (xmldata);
  return msgToReturn;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}


NMLmsg *
NML::read_encoded_data_from_file (
#ifdef ENABLE_RCS_XML
				  const char *filename,
				  const int _encoding_method
#else
				  __unused_parameter__ const char *,
				  __unused_parameter__ const int
#endif
				  )
{
  set_rcs_print_tag(0);
#if HAVE_STAT
  struct stat stat_struct;
  if(stat(filename,&stat_struct) < 0)
    {
      rcs_print_error("stat(%s) failed -- %s\n",
		      filename,strerror(errno));
      return 0;
    }
  if(stat_struct.st_size <= 0)
    {
      rcs_print_error("stat(%s) returned with invalid st_size=%ld\n",
		      filename,
		      ((long) stat_struct.st_size));
      return 0;
    }

  if(!ni->file_vBufP || ni->file_vBufP_size < ((size_t) stat_struct.st_size))
    {
      ni->file_vBufP = realloc(ni->file_vBufP,stat_struct.st_size);
      ni->file_vBufP_size = stat_struct.st_size;
    }
	  
  FILE *f = fopen(filename,"r");
  if(!f)
    {
      fprintf(stderr,"Can't open %s for reading -- %s\n",
	      filename,
	      strerror(errno));
      return 0;
    }
  size_t fread_ret = fread(ni->file_vBufP,1,
			   stat_struct.st_size,f); 
  if(((long) fread_ret) != stat_struct.st_size) 
    {
      rcs_print_error("fread returned %u when %ld was expected while reading from %s\n",
		      ((unsigned int)fread_ret),
		      stat_struct.st_size,
		      filename);
      fclose(f);
      return 0;
    }
  fclose(f);

  return encoded_data_to_msg(ni->file_vBufP,
			     stat_struct.st_size,
			     _encoding_method);
			     
#else
  rcs_print_error("NML::read_encoded_data_from_file() not implemented since stat function not available.\n");
  return 0;
#endif
}


int
NML::xmlMsgSaveAs (
#ifdef ENABLE_RCS_XML
		   NMLmsg & nml_msg,
		   const char *filename
#else
		   __unused_parameter__ NMLmsg &,
		   __unused_parameter__ const char *
#endif
		   )
{
#ifdef ENABLE_RCS_XML
  return xmlMsgSaveAs (&nml_msg, filename);
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

int
NML::xmlMsgSaveAs (
#ifdef ENABLE_RCS_XML
		   NMLmsg * nml_msg,
		   const char *filename
#else
		   __unused_parameter__ NMLmsg *,
		   __unused_parameter__ const char *
#endif		   
		   )
{
#ifdef ENABLE_RCS_XML
  rcs_print_debug (PRINT_MISC, "NML::xmlMsgSaveAs(%p,%s)\n",
		   (void *)nml_msg, filename);
  int fd = open (filename, O_CREAT | O_TRUNC | O_WRONLY, 0644);
  if (fd < 0)
    {
      rcs_print_error ("xmlMsgSaveAs(...,%s) : open failed -- %d:%s\n",
		       filename, errno, strerror (errno));
      return -1;
    }
  const char *xmlmsgdata = msg2xml (nml_msg);
  if (0 == xmlmsgdata)
    {
      rcs_print_error ("msg2xml() failed\n");
      close (fd);
      return -1;
    }
  if (writestr_with_repeated_tries (fd, xmlmsgdata) < 0)
    {
      close (fd);
      return -1;
    }
  if (writestr_with_repeated_tries (fd, "\n") < 0)
    {
      close (fd);
      return -1;
    }

#ifdef HAVE_FSYNC
  if (fsync (fd) < 0)
    {
      rcs_print_error ("xmlMsgSaveAs: fsync failed --%d:%s\n",
		       errno, strerror (errno));
      close (fd);
      return -1;
    }
#endif
  close (fd);
  return 0;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

/**************************************************************************
* NML member function: str2msg
* Parameter: NMLmsg *msg -- Pointer to message to be converted into a NMLmsg.
* Returns: Returns a pointer to the cms->encoded_data buffer if successful
* since this should contain the string or NULL if there was an error.
***************************************************************************/
NMLTYPE NML::str2msg (const char *string)
{
  CMS *
    orig_cms =
    cms;
  set_rcs_print_tag(0);
  if (NULL == string)
    {
      return -1;
    }
  if (NULL == cms)
    {
      int
	string_length = (int) 
	strlen (string);
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ((ni->cms_for_msg_string_conversions->size > 16 * string_length &&
	       ni->cms_for_msg_string_conversions->size > 2048) ||
	      ni->cms_for_msg_string_conversions->size < 4 * string_length)
	    {
	      delete
		ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  ni->cms_for_msg_string_conversions =
	    new CMS (string_length * 4 + 16 + (16 - (string_length % 16)));
	}
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_DISPLAY_ASCII_ENCODING);
  cms->set_mode (CMS_DECODE);
  strcpy ((char *) cms->encoded_data, (const char *) string);
  cms->status = CMS_READ_OK;
  if (-1 == format_output ())
    {
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms = orig_cms;
      return -1;
    }
  cms->restore_normal_updater ();
  cms = orig_cms;

  switch (cms->status)
    {
    case CMS_READ_OLD:
      error_type = NML_NO_ERROR;
      return (0);
    case CMS_READ_OK:
      error_type = NML_NO_ERROR;
      return (((NMLmsg *) cms->subdiv_data)->type);
    case CMS_TIMED_OUT:
      error_type = NML_TIMED_OUT;
      return -1;
      
    case CMS_INTERRUPTED_OPERATION:
      error_type = NML_INTERRUPTED_OPERATION;
      return -1;

    case CMS_MISC_ERROR:
    case CMS_NO_MASTER_ERROR:
      error_type = NML_INTERNAL_CMS_ERROR;
    default:
      return -1;
    }

}


/**************************************************************************
* NML member function: xml2msg
* Parameter: string * pointer to a string in XML format to be converted to 
* an NML message strunture.
* Returns: The type of message created or -1 if unsuccessful.
* The message is stored in the locatation that can be obtained to get_address();
***************************************************************************/
NMLTYPE 
NML::xml2msg (
#ifdef ENABLE_RCS_XML
	      const char *string
#else
	      __unused_parameter__ const char *
#endif
	      )
{
#ifdef ENABLE_RCS_XML
  CMS *orig_cms;
  size_t new_size;
  CMS_STATUS retstatus;
  NMLTYPE rettype;
  int string_length;

  rcs_print_debug (PRINT_MISC, "NML::xml2msg(%s)\n", string);
  
  if (NULL == string)
    {
      return -1;
    }
  
  string_length = (int) strlen (string);
  orig_cms = cms;
  new_size = 0;
  retstatus = CMS_STATUS_NOT_SET;
  rettype = 0;

  if (NULL == cms)
    {
      if (NULL != ni->cms_for_msg_string_conversions)
	{
	  if ((ni->cms_for_msg_string_conversions->size > 512 * string_length &&
	       ni->cms_for_msg_string_conversions->size > 8192) ||
	      ni->cms_for_msg_string_conversions->size < 20 * string_length)
	    {
	      delete
		ni->cms_for_msg_string_conversions;
	      ni->cms_for_msg_string_conversions = 0;
	    }
	}
      if (NULL == ni->cms_for_msg_string_conversions)
	{
	  new_size = round_up_size(string_length*32,8192);
	  ni->cms_for_msg_string_conversions = new CMS ( (long) new_size);
	}
      cms = ni->cms_for_msg_string_conversions;
    }
  cms->set_temp_updater (CMS_XML_ENCODING);
  cms->set_mode (CMS_DECODE);
  if (string_length > cms->max_encoded_message_size)
    {
      rcs_print_error
	("NML::xml2msg -- xml string of length %d is too large (>%ld)\n",
	 string_length, cms->max_encoded_message_size);
      cms = orig_cms;
      return -1;
    }
  strcpy ((char *) cms->encoded_data, (const char *) string);
  cms->status = CMS_READ_OK;
  if (-1 == format_output ())
    {
      cms->restore_normal_updater ();
      error_type = NML_FORMAT_ERROR;
      cms = orig_cms;
      return -1;
    }
  cms->restore_normal_updater ();
  retstatus = cms->status;
  rettype=0;
  if (retstatus == CMS_READ_OK)
    {
      rettype = (((NMLmsg *) cms->subdiv_data)->type);
    }
  cms = orig_cms;

  switch (retstatus)
    {
    case CMS_READ_OLD:
      error_type = NML_NO_ERROR;
      return (0);
    case CMS_READ_OK:
      error_type = NML_NO_ERROR;
      return rettype;
    case CMS_TIMED_OUT:
      error_type = NML_TIMED_OUT;
      return -1;

    case CMS_INTERRUPTED_OPERATION:
      error_type = NML_INTERRUPTED_OPERATION;
      return -1;

    case CMS_MISC_ERROR:
    case CMS_NO_MASTER_ERROR:
      error_type = NML_INTERNAL_CMS_ERROR;
    default:
      return -1;
    }
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

#ifndef DISABLE_RCS_PRINT
static int
  info_message_printed =
  0;
#endif

char
  cwd_buf[256];

#if ENABLE_RCS_SOKINTRF
char
  host_name_buf[MAXHOSTNAMELEN];
#endif

char
  last_bufname[10];
char
  last_procname[10];
char
  last_cfg_file[40];

/**************************************************************************
* NML member function: print_info()
* Prints the buffer, process names and configuration file information.
***************************************************************************/
void
NML::print_info (const char *_bufname, const char *_procname, const char *_cfg_file)
{
#ifndef DISABLE_RCS_PRINT
  const char *bn = _bufname;
  const char *pn = _procname;
  const char *cf = _cfg_file;
  set_rcs_print_tag(0);
  if(!ni)
    {
      return;
    }
  if(!bn)
    {
      bn=ni->bufname;
    }
  if(!pn)
    {
      pn = ni->procname;
    }
  if(!cf)
    {
      cf = ni->cfgfilename;
    }
  info_printed = 1;
  if (!verbose_nml_error_messages)
    {
      return;
    }
  if (NULL == cms || error_type != NML_NO_ERROR)
    {
      if (max_rcs_errors_to_print <= rcs_errors_printed &&
	  max_rcs_errors_to_print >= 0)
	{
	  return;
	}
    }
  if (error_type == NML_QUEUE_FULL_ERROR && !cms_print_queue_full_messages)
    {
      return;
    }
  if(error_type == NML_INTERRUPTED_OPERATION)
    {
      return;
    }

  if (NULL != cms)
    {
      if (cms->status < 0)
	{
	  if (max_rcs_errors_to_print <= rcs_errors_printed &&
	      max_rcs_errors_to_print >= 0)
	    {
	      return;
	    }
	}
    }
  if (NULL != bn && NULL != pn && NULL != cf)
    {
      if (!strncmp (bn, last_bufname, 10)
	  && !strncmp (pn, last_procname, 10)
	  && !strncmp (cf, last_cfg_file, 40) &&
	  bn[0] != 0 && last_bufname[0] != 0 &&
	  pn[0] != 0 && last_procname[0] != 0 &&
	  cf[0] != 0 && last_cfg_file[0] != 0)
	{
	  return;
	}
      strncpy (last_bufname, bn, 10);
      strncpy (last_procname, pn, 10);
      strncpy (last_cfg_file, cf, 40);
    }
  if (!info_message_printed)
    {
      rcs_print
	("\n**********************************************************\n");
#if !defined(MS_WINDOWS_API) && (!defined(HAVE_CONFIG_H) || defined(HAVE_GETCWD))
      rcs_print ("* Current Directory = %s\n", getcwd (cwd_buf, 256));
#endif
#ifdef ENABLE_RCS_SOKINTRF
      if (nml_print_hostname_on_error)
	{
	  dl_gethostname (host_name_buf, MAXHOSTNAMELEN);
	  if (host_name_buf[0] != 0)
	    {
	      rcs_print ("* Host = %s\n", host_name_buf);
	    }
	}
#endif
      rcs_print ("* ");
      print_rcs_version ();
      info_message_printed = 1;
    }
  rcs_print
    ("\n**********************************************************\n");
  if (NULL != cms)
    {
      rcs_print ("* BufferName = %s\n", cms->BufferName);
      rcs_print ("* BufferType = %d\n", cms->BufferType);
      rcs_print ("* ProcessName = %s\n", cms->ProcessName);
      rcs_print ("* Configuration File = %s\n", ni->cfgfilename);
      rcs_print ("* CMS Status = %d (%s)\n", cms->status,
		 cms->status_string (cms->status));
      if(last_error_bufs[0][0] || last_error_bufs[1][0] || last_error_bufs[2][0] || last_error_bufs[3][0])
	{
	  rcs_print ("* Recent errors repeated:\n");
	  if(last_error_bufs[0][0]) rcs_print ("%s\n", last_error_bufs[0]);
	  if(last_error_bufs[1][0]) rcs_print ("%s\n", last_error_bufs[1]);
	  if(last_error_bufs[2][0]) rcs_print ("%s\n", last_error_bufs[2]);
	  if(last_error_bufs[3][0]) rcs_print ("%s\n", last_error_bufs[3]);
	}
      memset (last_error_bufs[0], 0, 100);
      memset (last_error_bufs[1], 0, 100);
      memset (last_error_bufs[2], 0, 100);
      memset (last_error_bufs[3], 0, 100);
      if (NULL == strstr (cms->BufferLine, "\n"))
	{
	  rcs_print ("* BufferLine: %s\n", cms->BufferLine);

	}
      else
	{
	  rcs_print ("* BufferLine: %s", cms->BufferLine);
	}
      if (NULL == strstr (cms->ProcessLine, "\n"))
	{
	  rcs_print ("* ProcessLine: %s\n", cms->ProcessLine);
	}
      else
	{
	  rcs_print ("* ProcessLine: %s", cms->ProcessLine);
	}
    }
  else
    {
      if (NULL != bn)
	{
	  rcs_print ("* BufferName = %s\n", bn);
	}
      if (NULL != pn)
	{
	  rcs_print ("* ProcessName = %s\n", pn);
	}
    }
  if (NULL != cf)
    {
      rcs_print ("* Config File = %s\n", cf);
    }
  rcs_print ("* error_type = %d (%s)\n", error_type,
	     NML_ERROR_TYPE_STRINGS[error_type]);
  rcs_print
    ("************************************************************\n\n");

#endif
  //# ifndef DISABLE_RCS_PRINT

}

void
nml_start ()
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_SERVER
#if !defined(__MSDOS__) || defined(WIN32)
  spawn_nml_servers ();
#endif /* __MSDOS__ */
#endif
}

static bool kill_servers_on_cleanup=true;

void 
nml_set_kill_servers_on_cleanup (bool _b)
{
  set_rcs_print_tag(0);
  kill_servers_on_cleanup=_b;
}

void 
nml_remove_all_not_on_list(RCS_LINKED_LIST *preserve_list)
{
  set_rcs_print_tag(0);
  if(!preserve_list)
    {
      return;
    }
  NML *nml=0;
  NML *preservenml=0;
  bool preserve_this_one=false;
  rcs_print_debug(PRINT_MISC,"nml_remove_all_not_on_list called()\n");
#ifdef POSIX_THREADS
  pthread_mutex_lock(&list_mutex);
#endif
  nml = (NML *) NML_Main_Channel_List->get_head ();
  while(nml)
    {
      preserve_this_one=false;
      if(nml->already_deleted)
	{
	  NML_Main_Channel_List->delete_current_node();
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	  continue;
	}
      preservenml = (NML *) preserve_list->get_head ();
      while(preservenml)
	{
	  if(preservenml == nml)
	    {
	      preserve_this_one = true;
	      break;
	    }
	  preservenml = (NML *) preserve_list->get_next ();
	}
      rcs_print_debug(PRINT_MISC,"nml_remove_all_not_on_list() preserving %p\n",
		      (void *)nml);

      if(!preserve_this_one)
	{
	  nml->immediate_spawned_server=0;
	  NML_Main_Channel_List->delete_current_node();
	  nml->set_leave_resource(true);
#ifdef POSIX_THREADS
	  pthread_mutex_unlock(&list_mutex);
#endif
	  delete nml;
#ifdef POSIX_THREADS
	  pthread_mutex_lock(&list_mutex);
#endif
	}
      nml = (NML *) NML_Main_Channel_List->get_next();
    }
#ifdef POSIX_THREADS
  pthread_mutex_unlock(&list_mutex);
#endif
  rcs_print_debug(PRINT_MISC,"nml_remove_all_not_on_list returning()\n");
}
       
int NML::get_ni_pid()
{
  set_rcs_print_tag(0);
  if(ni)
    {
      return ni->pid;
    }
  return(-1);
}

#ifdef POSIX_THREADS
static pthread_t cancelled_threads[512];
static int num_cancelled_threads=0;
#endif

void
mark_cancelled_thread_id(void *_tidP)
{
  if(!_tidP)
    {
      return;
    }
#ifdef POSIX_THREADS
  pthread_t ptid = *((pthread_t *) _tidP);
  int index = (num_cancelled_threads%ARRAY_LENI(cancelled_threads));
  cancelled_threads[index] = ptid;
  num_cancelled_threads++;
#endif
}

int
is_cancelled_thread_id(void * _tidP)
{
  if(!_tidP)
    {
      return -1;
    }
#ifdef POSIX_THREADS
  pthread_t ptid = *((pthread_t *) _tidP);
  for(int i = 0 ; i < (int) ARRAY_LEN(cancelled_threads) && 
	i < num_cancelled_threads; i++)
    {
      if(pthread_equal(cancelled_threads[i],
		       ptid))
	{
	  return (int) true;
	}
    }
#endif
  return (int) false;
}

void
nml_cleanup ()
{
  set_rcs_print_tag(0);
  rcs_print_debug(PRINT_MISC,"nml_cleanup() : kill_servers_on_cleanup=%d\n",
		  kill_servers_on_cleanup);
  nml_cleanup_started=true;
  NML *nml=0;
#ifdef POSIX_THREADS
  bool channels_might_need_interrupting=true;
  int tries=0;
#endif

#ifdef VXWORKS
  int current_pid=-1;
  current_pid = taskIdSelf ();
#endif

#ifdef POSIX_THREADS
  pthread_t myid = pthread_self();
#endif

#if MS_WINDOWS_API && HAVE_GET_CURRENT_THREAD
  // HANDLE replaced with void *
  void *current_task_handle=0;
  current_task_handle = GetCurrentThread ();
#endif

#ifdef POSIX_THREADS
  while(channels_might_need_interrupting && tries < 20)
    {
      pthread_mutex_lock(&list_mutex);
      channels_might_need_interrupting=false;
      if (NULL != NML_Main_Channel_List)
	{
	  nml = (NML *) NML_Main_Channel_List->get_head ();
	  while(nml)
	    {
	      if(nml->already_deleted)
		{
		  NML_Main_Channel_List->delete_current_node();
		  nml = (NML *) NML_Main_Channel_List->get_next ();
		  continue;
		}
	      if(nml->extra_thread_info && 
		 !pthread_equal(myid,nml->extra_thread_info->pthread_threadId) &&
		 nml->extra_thread_info->pthread_threadId >0)
		{
		  channels_might_need_interrupting=true;
		  break;
		}
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	    }
	}
      pthread_mutex_unlock(&list_mutex);
      esleep(0.25);
      tries++;
    }
  pthread_mutex_lock(&list_mutex);
  if (NULL != NML_Main_Channel_List)
    {
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while(nml)
	{
	  if(nml->already_deleted)
	    {
	      NML_Main_Channel_List->delete_current_node();
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
	  if(nml->extra_thread_info && !pthread_equal(myid,nml->extra_thread_info->pthread_threadId) &&
	     nml->extra_thread_info->pthread_threadId >0)
	    {
	      pthread_mutex_unlock(&list_mutex);
	      nml->interrupt_operation();
	      pthread_mutex_lock(&list_mutex);
	    }
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
    }
  pthread_mutex_unlock(&list_mutex);
  esleep(0.1);
  pthread_mutex_lock(&list_mutex);
  if(NML_Main_Channel_List)
    {
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while(nml)
	{
	  if(nml->already_deleted)
	    {
	      NML_Main_Channel_List->delete_current_node();
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
	  if(nml->extra_thread_info 
	     && !pthread_equal(myid,nml->extra_thread_info->pthread_threadId) &&
	     nml->extra_thread_info->pthread_threadId >0)
	    {
	      if(!kill_servers_on_cleanup)
		{
		  nml = (NML *) NML_Main_Channel_List->get_next ();
		  continue;
		}
	      pthread_t thread_to_cancel = 
		nml->extra_thread_info->pthread_threadId;
	      nml->extra_thread_info->pthread_threadId= 
		(pthread_t) 0;
	      if(thread_to_cancel == 0)
		{
		  continue;
		}
	      bool already_cancelled=
		is_cancelled_thread_id((void *) (&thread_to_cancel));
	      if(already_cancelled)
		{
		  continue;
		}
	      mark_cancelled_thread_id((void *) (&thread_to_cancel));
	      pthread_mutex_unlock(&list_mutex);
	      // printf("%s:%d pthread_cancel(%d (0x%X))\n",
	      // 		     __FILE__,__LINE__,
	      // 	     (int) thread_to_cancel, (unsigned) thread_to_cancel);
	      pthread_cancel(thread_to_cancel);
	      pthread_mutex_lock(&list_mutex);
	    }
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
    }
  pthread_mutex_unlock(&list_mutex);
  esleep(0.1);
  pthread_mutex_lock(&list_mutex);
  if (NULL != NML_Main_Channel_List)
    {
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while(nml)
	{
	  if(nml->already_deleted)
	    {
	      NML_Main_Channel_List->delete_current_node();
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
	  if(nml->extra_thread_info 
	     && !pthread_equal(myid,nml->extra_thread_info->pthread_threadId) &&
	     nml->extra_thread_info->pthread_threadId >0)
	    {
	      pthread_mutex_unlock(&list_mutex);
	      nml->interrupt_operation();
	      pthread_mutex_lock(&list_mutex);
	    }
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
    }
  pthread_mutex_unlock(&list_mutex);
  esleep(0.1);
  pthread_mutex_lock(&list_mutex);
  if (NULL != NML_Main_Channel_List)
    {
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while(nml)
	{
	  if(nml->already_deleted)
	    {
	      NML_Main_Channel_List->delete_current_node();
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
	  if(nml->extra_thread_info 
	     && !pthread_equal(myid,nml->extra_thread_info->pthread_threadId) &&
	     nml->extra_thread_info->pthread_threadId >0)
	    {
	      if(!kill_servers_on_cleanup)
		{
		  nml = (NML *) NML_Main_Channel_List->get_next ();
		  continue;
		}
	      pthread_mutex_unlock(&list_mutex);
	      pthread_join(nml->extra_thread_info->pthread_threadId,0);
	      pthread_mutex_lock(&list_mutex);
	    }
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
    }
  pthread_mutex_unlock(&list_mutex);
  esleep(0.1);
#endif

#ifdef POSIX_THREADS
  pthread_mutex_lock(&list_mutex);
#endif
  if (NULL != NML_Main_Channel_List)
    {
      rcs_print_debug (PRINT_NML_DESTRUCTORS,
		       "Deleting %d channels from the NML_Main_Channel_List.\n",
		       NML_Main_Channel_List->list_size);
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while (NULL != nml)
	{
	  if(nml->already_deleted)
	    {
	      NML_Main_Channel_List->delete_current_node();
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
#ifdef VXWORKS
	  if (current_pid != nml->get_ni_pid())
	    {
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
#endif
#if MS_WINDOWS_API && HAVE_GET_CURRENT_THREAD
	  if (current_task_handle != nml->task_handle)
	    {
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
#endif
	  if ((nml->cms != NULL && nml->loopback == nml) &&
	      (
	       (nml->cms->BufferName[0] >= 'a' && nml->cms->BufferName[0] <= 'z'  ) ||
	       (nml->cms->BufferName[0] >= 'A' && nml->cms->BufferName[0] <= 'Z'  ) ||
	       (nml->cms->BufferName[0] >= '0' && nml->cms->BufferName[0] <= '9'  ) ||
	       nml->cms->BufferName[0] == '_' || nml->cms->BufferName[0] == '.')
	      )
	    {
	      rcs_print_debug (PRINT_NML_DESTRUCTORS,
			       "Deleting %s NML channel from NML_Main_Channel_List.\n",
			       nml->cms->BufferName);
	    }
	  else
	    {
	      nml = (NML *) NML_Main_Channel_List->get_next ();
	      continue;
	    }
	  if(!kill_servers_on_cleanup)
	    {
	      nml->immediate_spawned_server=0;
	    }

#ifdef POSIX_THREADS
	  pthread_mutex_unlock(&list_mutex);
#endif
	  nml->delete_channel ();

#ifdef POSIX_THREADS
	  pthread_mutex_lock(&list_mutex);
#endif

	  rcs_print_debug (PRINT_NML_DESTRUCTORS,
			   "NML channel deleted from NML_Main_Channel_List\n");
	  if (NULL == NML_Main_Channel_List)
	    {
	      return;
	    }
	  NML_Main_Channel_List->delete_current_node ();
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
      if (NULL != NML_Main_Channel_List)
	{
#if defined(VXWORKS) || defined(MS_WINDOWS_API)
	  if (0 == NML_Main_Channel_List->list_size)
	    {
#endif
	      delete NML_Main_Channel_List;
	      NML_Main_Channel_List = (RCS_LINKED_LIST *) NULL;
#if defined(VXWORKS) || defined(MS_WINDOWS_API)
	    }
#endif
	}
    }
#ifdef POSIX_THREADS
  pthread_mutex_unlock(&list_mutex);
#endif


#ifdef ENABLE_RCS_SERVER
#if !defined(__MSDOS__) || defined(WIN32)
  // This code was added after Rashmi Patel of GDRS discovered segfaults and memory leaks on Windows/VxWorks.
  // kill_servers was moved here from the beginning of  nml_cleanup and cms_server_list_cleanup which calls 
  // delete cms_server_list was added. 
  // If a segfault occurs here consider testing version October.2006 for comparison.
  if(kill_servers_on_cleanup)
    {
      nml_server_cleanup ();
    }

  cms_server_list_cleanup();

#endif /* __MSDOS__ */
#endif

  nmlClearHostAliases ();
  set_default_nmlcfgsvr_options(0);
}

void
nml_wipeout_lists ()
{
  set_rcs_print_tag(0);
#ifdef POSIX_THREADS
  pthread_mutex_lock(&list_mutex);
#endif
  if (NULL != NML_Main_Channel_List)
    {
      delete NML_Main_Channel_List;
      NML_Main_Channel_List = (RCS_LINKED_LIST *) NULL;
    }
#ifdef POSIX_THREADS
  pthread_mutex_unlock(&list_mutex);
#endif

#ifdef ENABLE_RCS_SERVER
#ifndef __MSDOS__
  if (NULL != NML_Default_Super_Server)
    {
      delete NML_Default_Super_Server;
      NML_Default_Super_Server = (NML_SUPER_SERVER *) NULL;
    }
#endif
#endif

}


int
NML::print_queue_info ()
{
  set_rcs_print_tag(0);
  if (NULL == cms)
    {
      rcs_print_error ("NML::print_queue_info() - NULL == cms\n");
      return (-1);
    }
  set_rcs_print_tag(cms->BufferName);
  if (!cms->queuing_enabled)
    {
      rcs_print_error ("NML::print_queue_info() - Queing Not Enabled.\n");
      set_rcs_print_tag(0);
      return (-1);
    }
  if (cms->ProcessType != CMS_LOCAL_TYPE)
    {
      rcs_print_error
	("NML::print_queue_info() - REMOTE Connection: Queing Data Not Available.\n");
      set_rcs_print_tag(0);
      return (-1);
    }
  rcs_print
    ("head = %ld(0x%lX); tail=%ld(0x%lX); queue_length=%ld,end_queue_space=%ld(0x%lX); write_id=%ld\n",
     cms->queuing_header.head, (unsigned long)cms->queuing_header.head,
     cms->queuing_header.tail, (unsigned long)cms->queuing_header.tail,
     cms->queuing_header.queue_length,
     cms->queuing_header.end_queue_space,(unsigned long) cms->queuing_header.end_queue_space,
     cms->queuing_header.write_id);
  set_rcs_print_tag(0);
  return (0);
}

static bool nml_wait_open_called_once=false;
static bool debug_nml_wait_open=false;

// Function added at Steve Balakirsky's request. Polls a channel indefinitely
// waiting for it to open.
NML *
nmlWaitOpen (NML_FORMAT_PTR fPtr, char *buffer, char *name, char *file,
	     double sleepTime)
{
  set_rcs_print_tag(buffer);
  NML *nmlChannel = 0;

#ifndef DISABLE_RCS_PRINT
#if HAVE_GETENV
  if(!nml_wait_open_called_once)
    {
      if(0 != getenv("DEBUG_RCSLIB") || 
	 0 != getenv("RCS_DEBUG") ||
	 0 != getenv("RCS_PRINT_DEST") ||
	 0 != getenv("RCS_PRINT_FLAGS") ||
	 0 != getenv("PAUSE_ON_RCS_ERROR") ||
	 0 != getenv("NML_WAIT_DEBUG"))
	{
	  debug_nml_wait_open=true;
	}
      else
	{
	  if(0 != getenv("NO_NML_WAIT_WARNING"))
	    {
	      rcs_print_warning("nmlWaitOpen turns off error messages. : Set the environment variable NML_WAIT_DEBUG to 1 to leave them on. Set NO_NML_WAIT_WARNING to stop getting this warning.\n");
	    }
	}
    }
  RCS_PRINT_DESTINATION_TYPE olddest = get_rcs_print_destination ();
  if(!debug_nml_wait_open)
    {
	set_rcs_print_destination (RCS_PRINT_TO_NULL);
    }
  // end of if HAVE_GETENV
#endif

  // end of ifndef DISABLE_RCS_PRINT
#endif

  nmlChannel = new NML (fPtr, buffer, name, file);
  while (!nmlChannel->reset ())
    {
      esleep (sleepTime);
    }
#if !defined(DISABLE_RCS_PRINT) && defined(HAVE_GETENV)
  if(!debug_nml_wait_open)
    {
      set_rcs_print_destination (olddest);
    }
#endif

  set_rcs_print_tag(0);
  return (nmlChannel);
}

// Special functions for dealing with subdivisions

/* Write a message. (Use reference) */
int
NML::write_subdivision (int subdiv, NMLmsg & nml_msg)
{
  set_rcs_print_tag(0);
  if (NULL != cms)
    {
      set_rcs_print_tag(cms->BufferName);
      if (cms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  int retval = write (nml_msg);
  set_rcs_print_tag(0);
  return retval;
}


/* Write a message. (Use pointer) */
int
NML::write_subdivision (int subdiv, NMLmsg * nml_msg)
{
  set_rcs_print_tag(0);
  if (NULL != cms)
    {
      set_rcs_print_tag(cms->BufferName);
      if (cms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  int retval = write (nml_msg);
  set_rcs_print_tag(0);
  return retval;
}


/* Write to subdivision only if buffer was_read. (Use reference) */
int
NML::write_if_read_subdivision (int subdiv, NMLmsg & nml_msg)
{
  set_rcs_print_tag(0);
  if (NULL != cms)
    {
      set_rcs_print_tag(cms->BufferName);
      if (cms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  int retval = write_if_read (nml_msg);
  set_rcs_print_tag(0);
  return retval;
}

/* Write to subdivision only if buffer was_read. (Use pointer) */
int
NML::write_if_read_subdivision (int subdiv, NMLmsg * nml_msg)
{
  set_rcs_print_tag(0);
  if (NULL != cms)
    {
      set_rcs_print_tag(cms->BufferName);
      if (cms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  int retval = write_if_read (nml_msg);
  set_rcs_print_tag(0);
  return retval;
}


/* Read from a particular subdivision. */
NMLTYPE NML::read_subdivision (int subdiv)
{
  set_rcs_print_tag(0);
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (NULL != lcms)
    {
      set_rcs_print_tag(lcms->BufferName);
      if (lcms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  NMLTYPE retval = read ();
  set_rcs_print_tag(0);
  return retval;
}


/* Read from a particular subdivision. (Wait for new data). */
NMLTYPE NML::blocking_read_subdivision (int subdiv, double timeout)
{
  set_rcs_print_tag(0);
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (NULL != lcms)
    {
      set_rcs_print_tag(lcms->BufferName);
      if (lcms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  NMLTYPE retval = blocking_read (timeout);
  set_rcs_print_tag(0);
  return retval;
}

/* Read buffer without changing was_read */
NMLTYPE NML::peek_subdivision (int subdiv)
{
  set_rcs_print_tag(0);
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (NULL != lcms)
    {
      set_rcs_print_tag(lcms->BufferName);
      if (lcms->set_subdivision (subdiv) < 0)
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  NMLTYPE retval = peek ();
  set_rcs_print_tag(0);
  return(retval);
}


NMLTYPE NML::blocking_read_extended (double timeout, double poll_interval)
{
  set_rcs_print_tag(0);
  CMS *lcms=cms;
  if(cms_for_read)
    {
      lcms=cms_for_read;
    }
  if (lcms == NULL)
    {
      return -1;
    }
  set_rcs_print_tag(lcms->BufferName);
  if (lcms->BufferType == CMS_SHMEM_TYPE)
    {
      NMLTYPE rval = blocking_read (timeout);
      set_rcs_print_tag(0);
      return(rval);
    }
  else
    {
      NMLTYPE
	type =
	0;
      double
	time_elapsed =
	0.0;
      double
	start_time =
	etime ();
      while (!type && (time_elapsed < timeout || timeout < 0.0))
	{
	  esleep (poll_interval);
	  type = read ();
	  if (timeout > 0.0 && !type)
	    {
	      time_elapsed = etime () - start_time;
	    }
	  if (time_elapsed < 0.0)
	    {
	      break;
	    }
	}
      set_rcs_print_tag(0);
      return type;
    }
  set_rcs_print_tag(0);
  return -1;
}


/* Get the number of messages written to this buffer so far. */
int
NML::get_msg_count ()
{
  set_rcs_print_tag(0);
  if (NULL == cms)
    {
      return -1;
    }
  set_rcs_print_tag(cms->BufferName);
  int rval = cms->get_msg_count ();
  set_rcs_print_tag(0);
  return(rval);
}



/* Get Diagnostics Information. */
NML_DIAGNOSTICS_INFO *
NML::get_diagnostics_info ()
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_DIAG
  if (NULL == cms)
    {
      return NULL;
    }
  return (NML_DIAGNOSTICS_INFO *) cms->get_diagnostics_info ();
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}


void
nmlSetHostAlias (const char *hostName, const char *hostAlias)
{
  set_rcs_print_tag(0);
  if (NULL == cmsHostAliases)
    {
      cmsHostAliases = new RCS_LINKED_LIST;
    }
  CMS_HOST_ALIAS_ENTRY entry;
  strncpy (entry.host, hostName, 64);
  strncpy (entry.alias, hostAlias, 64);
  cmsHostAliases->store_at_tail (&entry, sizeof (entry), 1);
}

void
nmlClearHostAliases ()
{
  set_rcs_print_tag(0);
  if (NULL != cmsHostAliases)
    {
      delete cmsHostAliases;
      cmsHostAliases = NULL;
    }
}

void
nmlAllowNormalConnection ()
{
  set_rcs_print_tag(0);
  cms_connection_mode = CMS_NORMAL_CONNECTION_MODE;
}

void
nmlForceRemoteConnection ()
{
  set_rcs_print_tag(0);
  cms_connection_mode = CMS_FORCE_REMOTE_CONNECTION_MODE;
}

void
nmlForceLocalConnection ()
{
  set_rcs_print_tag(0);
  cms_connection_mode = CMS_FORCE_LOCAL_CONNECTION_MODE;
}


/* This function is only useable with encoding methods such as 
   xml that allow partial messages to be sent. */
int
NML::setMessageForDiff (NMLmsg * nml_msg)
{
  int retval = 0;
  set_rcs_print_tag(0);
  ni->diffMsg = nml_msg;
  if (0 != cms)
    {
      set_rcs_print_tag(cms->BufferName);
      if (0 == nml_msg)
	{
	  retval = (int) cms->setBufferForDiff (0, 0);
	}
      else
	{
	  retval =
	    (int) cms->setBufferForDiff ((void *) nml_msg, nml_msg->size);
	}
      if (retval >= 0)
	{
	  set_rcs_print_tag(0);
	  return 0;
	}
      else
	{
	  set_rcs_print_tag(0);
	  return -1;
	}
    }
  set_rcs_print_tag(0);
  return -1;
}

int
NML::xmlSetStyleProperty (
#ifdef ENABLE_RCS_XML
			  const char *propstr
#else
			  __unused_parameter__ const char *
#endif
			  )
{
  set_rcs_print_tag(0);
#ifdef ENABLE_RCS_XML
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "NML::xmlSetStyleProperty(%s)\n", propstr);
  ni->global_xml_style_properties_count++;
  if (0 == ni->xml_style_properties)
    {
      ni->xml_style_properties = new RCS_LINKED_LIST ();
    }
  ni->xml_style_properties->store_at_tail ((void *) propstr, strlen (propstr) + 1,
				       1);
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "NML::xmlSetStyleProperty xml_style_properties=%p\n",
		   (void *)ni->xml_style_properties);
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "NML::xmlSetStyleProperty xml_style_properties->list_size=%d\n",
		   ni->xml_style_properties->list_size);
  if (cms)
    {
      set_rcs_print_tag(cms->BufferName);
      cms->xml_style_properties = ni->xml_style_properties;
      cms->global_xml_style_properties_count = ni->global_xml_style_properties_count;
      cms->xmlSetStyleProperty (propstr);
      set_rcs_print_tag(0);
    }
  return 0;
#else
  rcs_print_error("XML support was not selected when the RCS library was compiled.\n");
  return -1;
#endif
}

int
NML::addTransferAlias (const char *name,
		       transfer_from_function_ptr fptr,
		       transfer_to_function_ptr tptr,
		       NML_FORMAT_PTR format_func)
{
  set_rcs_print_tag(0);
  if (cms)
    {
      return cms->addTransferAlias (name, fptr, tptr, (void *) format_func);
    }
  return -1;
}

int
NML::setTransferAlias (const char *name)
{
  set_rcs_print_tag(0);
  if (cms)
    {
      return cms->setTransferAlias (name);
    }
  return -1;
}

int
NML::copyMsg (NMLmsg * nml_msg, void *addr, unsigned long max_size)
{
  set_rcs_print_tag(0);
  if (!cms || !nml_msg || !addr)
    {
      return -1;
    }
  if (((long) max_size) < nml_msg->size)
    {
      return -1;
    }
  memcpy (addr, nml_msg, nml_msg->size);
  cms->setCopyBuff (addr, nml_msg->size, max_size);
  run_format_chain (nml_msg->type, (void *) nml_msg);
  cms->setCopyBuff (0, 0, 0);
  return 0;
}

void
NML::interrupt_operation(void)
{
  set_rcs_print_tag(0);
  interrupting_operation=true;
  if(cms)
    {
      cms->interrupt_operation();
    }
}

void
NML::clear_interrupt_operation(void)
{
  set_rcs_print_tag(0);
  if(cms)
    {
      cms->clear_interrupt_operation();
    }
  interrupting_operation=false;
}

void
NML::set_leave_resource(bool b)
{
  set_rcs_print_tag(0);
  if(cms)
    {
      cms->set_leave_resource(b);
    }
  leave_resource=b;
}

int
NML::get_read_count(void)
{
  set_rcs_print_tag(0);
  if(cms)
    {
      set_rcs_print_tag(cms->BufferName);
      int rval = cms->get_read_count();
      set_rcs_print_tag(0);
      return(rval);
    }
  return -1;
}

int
NML::get_is_clear(void)
{
  set_rcs_print_tag(0);
   if(cms)
    {
      set_rcs_print_tag(cms->BufferName);
      int rval = cms->get_is_clear();
      set_rcs_print_tag(0);
      return(rval);
    }
  return -1;
} 

NML &
NML::operator=(const NML &nml)
{
  set_rcs_print_tag(0);
  rcs_print_error("NML::operator= should never be called.\n");

  strncpy (ni->bufname, nml.ni->bufname, sizeof(ni->bufname));
  strncpy (ni->procname, nml.ni->procname, sizeof(ni->procname));
  strncpy (ni->cfgfilename, nml.ni->cfgfilename, sizeof(ni->cfgfilename));
  bool orig_nml_was_force_raw = false;
  if (NULL != nml.cms)
    {
      orig_nml_was_force_raw= nml.cms->force_raw;
      // Create a CMS channel identitical to the one from the argument
      // NML channel accept that the channel may be set_to_server or
      // set_to_master differently.
      if(0 == ni->extra_bufs)
	{
	  ni->extra_bufs = new NML_EXTRA_STRING_BUFFERS();
	}
      
      cms_copy_with_buffers (&cms, nml.cms,
			     ni->extra_bufs->wordbuf,
			     ni->extra_bufs->proc_type,
			     ni->extra_bufs->buffer_type,
			     0,0);
      if (NULL != cms)
	{
	      cms->current_subdivision = nml.cms->current_subdivision;
	}
    }
  if (!ignore_format_chain && !orig_nml_was_force_raw)
    {
      format_chain = new RCS_LINKED_LIST();
      if (NULL != nml.ni &&
	  (NULL != nml.format_chain) && (NULL != format_chain))
	{
	  RCS_LINKED_LIST *from, *to;
	  NML_FORMAT_PTR format_func_ptr;
	  from = nml.format_chain;
	  to = format_chain;
	  format_func_ptr = (NML_FORMAT_PTR) from->get_head ();
	  while (NULL != format_func_ptr)
	    {
	      to->store_at_tail ((void *) format_func_ptr, 0, 0);
	      format_func_ptr = (NML_FORMAT_PTR) from->get_next ();
	    }
	}
    }
  if (NULL == cms)
    {
      return (*this);
    }
  if(cms->spawn_server == 3)
    {
      cms->spawn_server = 0;
    }
#ifndef ENABLE_RCS_SERVER
  if(cms->isserver || cms->spawn_server)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
      error_type = NML_INVALID_CONFIGURATION;
      return(*this);
    }
#endif

  add_to_channel_list ();
  if (!cms->is_phantom &&
      cms->ProcessType == CMS_LOCAL_TYPE && !cms->neutral && !cms->isserver)
    {
      fast_mode = true;
    }
  cms_inbuffer_header_size = &(cms->header.in_buffer_size);
  char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
  if (forced_type_eq != NULL)
    {
      long temp = strtol (forced_type_eq + 11, NULL, 0);
      if (temp > 0)
	{
	  forced_type = temp;
	  fast_mode = true;
	}
    }
  char *brpi_eq = strstr (cms->buflineupper, "BRPI=");
  if (brpi_eq != NULL)
    {
      ni->blocking_read_poll_interval = strtod (brpi_eq + 5, NULL);
    }
#ifdef ENABLE_RCS_DIAG
  if (NULL != nml.cms->dpi)
    {
      CMS_DIAG_PROC_INFO *dpi = cms->get_diag_proc_info ();
      *dpi = *(nml.cms->get_diag_proc_info ());
      cms->set_diag_proc_info (dpi);
    }
  cms->first_diag_store = nml.cms->first_diag_store;
#endif
  if (NULL != cms->handle_to_global_data &&
      NULL != nml.cms->handle_to_global_data)
    {
      cms->handle_to_global_data->set_total_bytes_moved(
	nml.cms->handle_to_global_data->get_total_bytes_moved());
    }
  return(*this);
}

NML::NML(class NML_C_DATA *_ncd):
  ni(0),cms(0),cms_for_read(0),format_chain(0),cms_inbuffer_header_size(0),
  extra_data(0),
  error_type(NML_NO_ERROR),phantom_read(0),phantom_peek(0),
  phantom_write(0),phantom_write_if_read(0),phantom_check_if_read(0),
  phantom_clear(0),ignore_format_chain(0),queue_length(0),
  fast_mode(false),info_printed(0),
  channel_type(INVALID_NML_CHANNEL_TYPE),sizeof_message_header(0),
  forced_type(0),interrupting_operation(false),leave_resource(false),
  already_deleted(false),immediate_spawned_server(0),
  extra_thread_info(0),cc_list(0),
  header_file_name(0),
  uses_unbounded(false),
  loopback(0)
{
  set_rcs_print_tag(0);
  if(!_ncd)
    {
      rcs_print_error("_ncd is NULL\n");
      return;
    }

#ifdef ENABLE_RCS_SOKINTRF
  load_socket_interface();
#endif

  NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) _ncd->f_function;
  const char *buf = _ncd->bufname;
  const char *proc = _ncd->procname;
  const char *file = _ncd->cfgname;
  int set_to_server = _ncd->set_to_server;
  int set_to_master = _ncd->set_to_master;
  extra_data = _ncd->extra_data;

  ni = new NML_INTERNALS();
  ni->min_message_size = _ncd->min_message_size;
  ni->message_size_add = _ncd->message_size_add;  
  ni->message_size_roundup = _ncd->message_size_roundup;
  
  if(!ni)
    {
      error_type=NML_OUT_OF_MEMORY_ERROR;
      return;
    }

  init_rcs_print_mode_flags();
  set_rcs_print_tag(buf);

  rcs_print_debug (PRINT_NML_CONSTRUCTORS,
		   "NML::NML(%p,%s,%s,%s,%d,%d)\n",
		   (void *) f_ptr, buf, proc, file, set_to_server, set_to_master);

  loopback=this;
  extra_thread_info =0;
  info_printed = 0;
  forced_type = 0;
  interrupting_operation=false;
  leave_resource=false;
  immediate_spawned_server = 0;

#ifndef ENABLE_RCS_SERVER
  if(set_to_server > 0)
    {
      rcs_print_error("RCS library was compiled without server support.\n");
    }
#endif

  if (0 != buf)
    {
      strncpy (ni->bufname, buf, 40);
    }
  if (0 != proc)
    {
      strncpy (ni->procname, proc, 40);
    }
  if (NULL == file)
    {
      file = default_nml_config_file;
    }
  if (0 != file)
    {
      strncpy (ni->cfgfilename, file, 160);
    }

#ifndef DISABLE_RCS_PRINT
  if (rcs_errors_printed >= max_rcs_errors_to_print
      && max_rcs_errors_to_print > 0 && nml_reset_errors_printed)
    {
      rcs_errors_printed = 0;
      rcs_print
	("\nResetting rcs_errors_printed because a new NML channel is being created.\n");
    }
#endif


  already_deleted = 0;
  channel_type = NML_GENERIC_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NMLmsg);

  reconstruct (f_ptr, buf, proc, file, set_to_server, set_to_master);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      char *forced_type_eq = strstr (cms->buflineupper, "FORCE_TYPE=");
      if (forced_type_eq != NULL)
	{
	  long temp = strtol (forced_type_eq + 11, NULL, 0);
	  if (temp > 0)
	    {
	      forced_type = temp;
	      fast_mode = false;
	    }
	}
    }
  rcs_print_debug (PRINT_NML_CONSTRUCTORS, "finished NML constructor.\n");
  set_rcs_print_tag(0);
}
  

void
NML::setup_cc_bufs(NML_FORMAT_PTR _f_ptr)
{
  set_rcs_print_tag(0);
  if(!cms || !ni)
    {
      return;
    }
  char *file = ni->cfgfilename;
  char *proc = ni->procname;
  char *cc_file = file;
  char *cc_file_ptr = strstr(cms->buflineupper,"CC_FILE=");
  if(cc_file_ptr)
    {
      ptrdiff_t cc_file_name_offset= (ptrdiff_t ) (cc_file_ptr - cms->buflineupper);
      cc_file = strdup(cms->BufferLine + cc_file_name_offset);
      size_t spc_index = strcspn(cc_file," \t\r\n");
      if(spc_index > 1 && spc_index < strlen(cc_file))
	{
	  *(cc_file + spc_index) = 0;
	}
    }
  cc_file_ptr = strstr(cms->proclineupper,"CC_FILE=");
  if(cc_file_ptr)
    {
      if(cc_file && cc_file != file)
	{
	  free(cc_file);
	}
      ptrdiff_t cc_file_name_offset= (ptrdiff_t ) (cc_file_ptr - cms->proclineupper);
      cc_file = strdup(cms->ProcessLine + cc_file_name_offset);
      size_t spc_index = strcspn(cc_file," \t\r\n");
      if(spc_index > 1 && spc_index < strlen(cc_file))
	{
	  *(cc_file + spc_index) = 0;
	}
    }
  char *cc_buf_ptr = strstr(cms->buflineupper,"CC_BUF=");
  while(cc_buf_ptr)
    {
      ptrdiff_t cc_buf_name_offset= (ptrdiff_t ) (cc_buf_ptr - cms->buflineupper);
      if(cc_buf_name_offset < 1 || cc_buf_name_offset > (CMS_CONFIG_LINELEN-8))
	{
	  break;
	}
      char *cc_buf_name = strdup(cms->BufferLine + cc_buf_name_offset);
      size_t spc_index = strcspn(cc_buf_name," \t\r\n");
      if(spc_index > 1 && spc_index < strlen(cc_buf_name))
	{
	  *(cc_buf_name + spc_index) = 0;
	}
      NML *cc_nml = 0;
      if(_f_ptr)
	{
	  cc_nml = new NML(_f_ptr,cc_buf_name,proc,cc_file);
	}
      else
	{
	  cc_nml = new NML(cc_buf_name,proc,cc_file,channel_type);
	}
      if(!cc_nml)
	{
	  free(cc_buf_name);
	  break;
	}
      int orig_cc_nml_ignore_format_chain = cc_nml->ignore_format_chain;
      cc_nml->ignore_format_chain=true;
      if(!cc_nml->valid())
	{
	  free(cc_buf_name);
	  delete cc_nml;
	  cc_nml=0;
	  cc_buf_ptr = strstr(cc_buf_ptr+8,"CC_BUF=");
	  continue;
	}
      cc_nml->ignore_format_chain = orig_cc_nml_ignore_format_chain;
      if(0 == cc_list)
	{
	  cc_list = new RCS_LINKED_LIST();
	}
      cc_list->store_at_tail(cc_nml,sizeof(NML),0);	  
      free(cc_buf_name);
      cc_buf_ptr = strstr(cc_buf_ptr+8,"CC_BUF=");
    }
  cc_buf_ptr = strstr(cms->proclineupper,"CC_BUF=");
  while(cc_buf_ptr)
    {
      ptrdiff_t cc_buf_name_offset= (ptrdiff_t ) (cc_buf_ptr - cms->proclineupper);
      if(cc_buf_name_offset < 1 || cc_buf_name_offset > (CMS_CONFIG_LINELEN-8))
	{
	  break;
	}
      char *cc_buf_name = strdup(cms->ProcessLine + cc_buf_name_offset);
      size_t spc_index = strcspn(cc_buf_name," \t\r\n");
      if(spc_index > 1 && spc_index < strlen(cc_buf_name))
	{
	  *(cc_buf_name + spc_index) = 0;
	}

      NML *cc_nml = 0;
      if(_f_ptr)
	{
	  cc_nml = new NML(_f_ptr,cc_buf_name,proc,cc_file);
	}
      else
	{
	  cc_nml = new NML(cc_buf_name,proc,cc_file,channel_type);
	}
      if(!cc_nml)
	{
	  free(cc_buf_name);
	  break;
	}
      int orig_cc_nml_ignore_format_chain = cc_nml->ignore_format_chain;
      cc_nml->ignore_format_chain=true;
      if(!cc_nml->valid())
	{
	  free(cc_buf_name);
	  delete cc_nml;
	  cc_nml=0;
	  cc_buf_ptr = strstr(cc_buf_ptr+8,"CC_BUF=");
	  continue;
	}
      cc_nml->ignore_format_chain = orig_cc_nml_ignore_format_chain;
      if(0 == cc_list)
	{
	  cc_list = new RCS_LINKED_LIST();
	}
      cc_list->store_at_tail(cc_nml,sizeof(NML),0);	  
      free(cc_buf_name);
      cc_buf_ptr = strstr(cc_buf_ptr+8,"CC_BUF=");
    }
  if(cc_file && cc_file != file)
    {
      free(cc_file);
    }
  setup_cc_list_format_chain();
}


void
NML::setup_cc_list_format_chain()
{
  set_rcs_print_tag(0); 
  if(0 != format_chain && 0 != cc_list)
    {
      NML *cc_nml = (NML *) cc_list->get_head();
      while(cc_nml)
	{
	  if(cc_nml->format_chain)
	    {
	      delete cc_nml->format_chain;
	      cc_nml->format_chain=0;
	    }
	  NML_FORMAT_PTR f_ptr = (NML_FORMAT_PTR) format_chain->get_tail();
	  while(f_ptr)
	    {
	      cc_nml->prefix_format_chain(f_ptr);
	      f_ptr = (NML_FORMAT_PTR) format_chain->get_last();
	    }
	  cc_nml = (NML *) cc_list->get_next();
	}
    }
}

void
NML::write_cc_list(NMLmsg *msg)
{
  set_rcs_print_tag(0); 
  if(!cc_list || !msg)
    {
      return;
    }
    NML *cc_nml = (NML *) cc_list->get_head();
    while(cc_nml)
      {
	cc_nml->write(msg);
	cc_nml = (NML *) cc_list->get_next();
      }
}
    
double
NML::get_double_var(NMLmsg *nml_msg, const char *varname, bool &got_dvar)
{
  set_rcs_print_tag(varname);
  got_dvar=false;

  if(!varname)
    {
      rcs_print_error("NML::get_double_var varname == NULL\n");
      return -1.0;
    }

  if(!nml_msg)
    {
      rcs_print_error("NML::get_double_var msg == NULL\n");
      return -1.0;
    }

  if ((nml_msg->size == 0 || nml_msg->type == 0) && !cms->isserver)
    {
      error_type = NML_INVALID_MESSAGE_ERROR;
      rcs_print_error ("NML::get_double_var Message size or type is zero.\n");
      rcs_print_error
	("NML: Check that the message was properly constructed.\n");
    }

  /* Check pointers. */
  if (NULL == cms)
    {
      if (error_type != NML_INVALID_CONFIGURATION)
	{
	  error_type = NML_INVALID_CONFIGURATION;
	  rcs_print_error ("NML::write: CMS not configured.\n");
	}
      return (-1);
    }

  set_rcs_print_tag(cms->BufferName);


  cms->looking_for_dvar=true;
  cms->inside_wrong_struct=false;
  cms->var_to_look_for = varname;
  cms->var_struct_to_look_for_len = (int) strlen(varname)-1;
  while(cms->var_struct_to_look_for_len > 0 &&
	varname[cms->var_struct_to_look_for_len] != '.' &&
	varname[cms->var_struct_to_look_for_len] != 0)
    {
      cms->var_struct_to_look_for_len--;
    }
  cms->varname_only_to_look_for= varname + cms->var_struct_to_look_for_len;
  if(*cms->varname_only_to_look_for == '.')
    {
      cms->var_struct_to_look_for_len++;
      cms->varname_only_to_look_for++;
    }
  if(cms->var_struct_to_look_for_len > 0)
    {
      cms->inside_wrong_struct = true;
    }
  cms->dvar_found = false;
  cms->dvar= -99999.999;
  memset(cms->cur_var_struct,0,sizeof(cms->cur_var_struct));
  memset(cms->tbuf,0,sizeof(cms->cur_var_struct));

  run_format_chain (nml_msg->type, nml_msg);
  got_dvar = cms->dvar_found;
  cms->dvar_found = false;
  cms->looking_for_dvar=false;
  cms->varname_only_to_look_for = 0;
  cms->var_to_look_for = 0;
  set_rcs_print_tag(0);

  return cms->dvar;
}

int 
NML::setup_subscription(double _subscription_period) {
  if(cms)
    {
      return (int) cms->setup_subscription(_subscription_period);
    }
  return -1;
}

int
NML::cancel_subscription() {
    if(cms)
    {
      return (int) cms->cancel_subscription();
    }
  return -1;
}


void
NML::spawn_and_register_server_if_needed(int set_to_server)
{
  if(!cms)
    {
      return;
    }
  if(!valid())
    {
      return;
    }

#ifdef ENABLE_RCS_SERVER
    if(cms->spawn_server == 3 && ! immediate_spawned_server)
    {
      immediate_spawned_server = new NML_SERVER(this);
      immediate_spawned_server->spawn();
    }
#if  !defined(__MSDOS__) || defined(WIN32)
  if ((cms->spawn_server != 0 || cms->isserver != 0 || set_to_server > 0) &&  cms->spawn_server != 3 && !immediate_spawned_server)
    {
      register_with_server ();
    }
#endif
#endif
}

enum NMLCFGSVR_STATUS
NML::getCfgSvrStatus()
{
#if defined(ENABLE_RCS_NMLCFGSVR)
  if(ni && ni->cfgsvr_data)
    {
      return ni->cfgsvr_data->ncs_status;
    }
#endif
  return NMLCFGSVR_STATUS_NOT_SET;
}

void nmlGetBufferList(char *buf, const int maxlen, const char sepChar)
{
  if (!NML_Main_Channel_List || !buf || maxlen < 1)
    {
      rcs_print_error("nmlGetBufferList failed.NML_Main_Channel_List=%p, buf=%p,maxlen=%d\n",
		      NML_Main_Channel_List,buf,maxlen);
      return;
    }

  memset(buf,0,maxlen);
  char *p=buf;
  size_t bytes_left= (size_t) maxlen;
  NML *nmlP = (NML *) NML_Main_Channel_List->get_head();
  while(nmlP && bytes_left > 0)
    {
      size_t namelen = strlen(nmlP->cms->BufferName);
      if(bytes_left < namelen + 32)
	{
	  rcs_print_error("nmlGetBufferList buf overflowed: NML_Main_Channel_List->size=%d, buf=%p,maxlen=%d\n",
			  NML_Main_Channel_List->list_size,buf,maxlen);
	  strncat(p,"#OUTOFSPACE",bytes_left);
	  break;
	}
      strncat(p,nmlP->cms->BufferName,bytes_left);
      p += namelen;
      *p =sepChar;
      p++;
      bytes_left -= (size_t) (1+namelen);
      nmlP = (NML *) NML_Main_Channel_List->get_next();
    }
}

static char *static_nml_buffer_list=0;
static size_t static_nml_buffer_list_size=0;

const char * nmlGetStaticBufferList(const char sepChar)
{
  if(!NML_Main_Channel_List)
    {
      return "";
    }
  size_t max_namelen = 0;
  NML *nmlP = (NML *) NML_Main_Channel_List->get_head();
  while(nmlP)
    {
      size_t namelen = strlen(nmlP->cms->BufferName);
      if(max_namelen < namelen)
	{
	  max_namelen=namelen;
	}
      nmlP = (NML *) NML_Main_Channel_List->get_next();
    }
  size_t size_needed = 32+(max_namelen+3)*NML_Main_Channel_List->list_size;
  if(!static_nml_buffer_list || static_nml_buffer_list_size < size_needed)
    {
      size_t size_to_alloc = size_needed + 512 - (size_needed%256);
      char * orig_static_nml_buffer_list = static_nml_buffer_list;
      static_nml_buffer_list=0;
      static_nml_buffer_list_size = 0;
      static_nml_buffer_list = (char *) realloc(orig_static_nml_buffer_list,size_to_alloc);
      static_nml_buffer_list_size = size_to_alloc;
    }
  nmlGetBufferList(static_nml_buffer_list,(const int)static_nml_buffer_list_size,sepChar);
  return (const char *) static_nml_buffer_list;
}

const char *
nmlGetRemoteTcpServerBufferList(const char *hostName, 
				const short port,
				const int use_ipv6)
{
#ifdef ENABLE_RCS_TCP
  struct dl_sa *server_saP = dl_create_sa(hostName,port,use_ipv6);
  int socket_fd = (int) dl_tcp_socket(use_ipv6);
  char *bind_to_host = 
    getenv("NML_BINDTO_HOST");
  struct dl_sa *client_saP = dl_create_sa(bind_to_host,0,use_ipv6);
  if(dl_bind (socket_fd, dl_sa_addr(client_saP), dl_sa_len(client_saP)) < 0)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  if(dl_connect (socket_fd, 
		 dl_sa_addr(server_saP),
		 dl_sa_len(server_saP)) < 0)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  char temp_buffer[20];
  memset(temp_buffer,0,sizeof(temp_buffer));
  hton_uint32_array_set(temp_buffer,1,(unsigned long)REMOTE_CMS_GET_BUFFER_LIST_REQUEST_TYPE);
  if(sendn(socket_fd,temp_buffer,sizeof(temp_buffer),0,-1.0) != sizeof(temp_buffer))
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }  
  memset(temp_buffer,0,sizeof(temp_buffer));
  if(recvn(socket_fd,temp_buffer,8,0,-1.0,0,1) != 8)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  int list_size = ntoh_uint32_array_get(temp_buffer,1);
  if(list_size < 1)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  char *buffer = (char *) malloc(list_size);
  recvn(socket_fd,buffer,list_size,0,-1.0,0,1);
  dl_closesocket(socket_fd);
  dl_free_sa(server_saP);
  dl_free_sa(client_saP);
  return (const char *) buffer;
#else
  return "";
#endif
}



const char *
nmlGetRemoteTcpServerBufferInfo(const char *bufferName,
				const char *hostName, 
				const short port,
				const int use_ipv6)
{
#ifdef ENABLE_RCS_TCP
  struct dl_sa *server_saP = dl_create_sa(hostName,port,use_ipv6);
  int socket_fd = (int) dl_tcp_socket(use_ipv6);
  struct dl_sa *client_saP = dl_create_sa(0,0,use_ipv6);
  if(dl_bind (socket_fd, dl_sa_addr(client_saP), dl_sa_len(client_saP)) < 0)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  if(dl_connect (socket_fd, 
		 dl_sa_addr(server_saP),
		 dl_sa_len(server_saP)) < 0)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  char temp_buffer[84];
  memset(temp_buffer,0,sizeof(temp_buffer));
  hton_uint32_array_set(temp_buffer,1,(unsigned long)REMOTE_CMS_GET_BUFFER_INFO_REQUEST_TYPE);
  strncpy(temp_buffer+20,bufferName,63);
  if(sendn(socket_fd,temp_buffer,sizeof(temp_buffer),0,-1.0) != sizeof(temp_buffer))
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }  
  memset(temp_buffer,0,sizeof(temp_buffer));
  if(recvn(socket_fd,temp_buffer,8,0,-1.0,0,1) != 8)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  int list_size = ntoh_uint32_array_get(temp_buffer,1);
  if(list_size < 1)
    {
      dl_closesocket(socket_fd);
      dl_free_sa(server_saP);
      dl_free_sa(client_saP);
      return "";
    }
  char *buffer = (char *) malloc(list_size);
  recvn(socket_fd,buffer,list_size,0,-1.0,0,1);
  dl_closesocket(socket_fd);
  dl_free_sa(server_saP);
  dl_free_sa(client_saP);
  return (const char *) buffer;
#else
  return "";
#endif
}


NML *nmlGetBufferFromListByName(const char *bufferName)
{
  if(!NML_Main_Channel_List || !bufferName)
    {
      return 0;
    }
  NML *nmlP = (NML *) NML_Main_Channel_List->get_head();
  while(nmlP)
    {
      if(nmlP->cms && !strcmp(nmlP->cms->BufferName,bufferName))
	{
	  return nmlP;
	}
      nmlP = (NML *) NML_Main_Channel_List->get_next();
    }
  return 0;
}


void nmlGetBufferInfo(const char *bufferName,
		      char *buffer, 
		      const int maxlen)
{
  NML *nmlP = nmlGetBufferFromListByName(bufferName);
  if(!nmlP || !nmlP->cms || !nmlP->ni)
    {
      return;
    }
  memset(buffer,0,maxlen);
  snprintf(buffer,
	   (maxlen-1),
	   "<%s\n\tbuffer_number=\"%ld\"\n\tBufferLine=\"%s\"\n\tProcessLine=\"%s\"\n\tCONFIG_NML=\"%s\"\n\tStatus=\"%d (%s)\"\n\tname_from_format=\"%s\"\n/>\n",
	   bufferName,
	   nmlP->cms->buffer_number,
	   nmlP->cms->BufferLine,
	   nmlP->cms->ProcessLine,
	   nmlP->ni->cfgfilename,
	   nmlP->cms->status,
	   nmlP->cms->status_string (nmlP->cms->status),
	   nmlP->get_name_from_format());
}

const char * nmlGetStaticBufferInfo(const char *bufferName)
{
  static char buf[1024];
  nmlGetBufferInfo(bufferName,buf,((int)sizeof(buf)));
  return (const char *) buf;
}


      



  
