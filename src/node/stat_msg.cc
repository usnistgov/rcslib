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


#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#endif

#include "nml.hh"
#include "nmlmsg.hh"
#include "cms.hh"
#include "cms_enum_info.h"

#include "linklist.hh"		// class RCS_LINKED_LIST

#include "stat_msg.hh"

#include <string.h>		// memset()

// RCS_STATUS
#ifndef MAX_ENUM_RCS_STATUS_STRING_LENGTH
#define MAX_ENUM_RCS_STATUS_STRING_LENGTH 21
#endif
#ifndef ENUM_RCS_STATUS_LENGTH
#define ENUM_RCS_STATUS_LENGTH 5
#endif

static const char
  enum_RCS_STATUS_string_list[ENUM_RCS_STATUS_LENGTH]
  [MAX_ENUM_RCS_STATUS_STRING_LENGTH] = {
  "RCS_DONE",			/* 0,1 */
  "RCS_ERROR",			/* 1,3 */
  "RCS_EXEC",			/* 2,2 */
  "UNINITIALIZED_STATUS",	/* 3,-1 */
  ""
};

static const int enum_RCS_STATUS_int_list[ENUM_RCS_STATUS_LENGTH] = {
  RCS_DONE,			/* 0,1 */
  RCS_ERROR,			/* 1,3 */
  RCS_EXEC,			/* 2,2 */
  UNINITIALIZED_STATUS,		/* 3,-1 */
};

const char *
enum_RCS_STATUS_symbol_lookup (long v)
{
  switch (v)
    {
    case RCS_DONE:
      return ("RCS_DONE");	/* 1 */
    case RCS_ERROR:
      return ("RCS_ERROR");	/* 3 */
    case RCS_EXEC:
      return ("RCS_EXEC");	/* 2 */
    case UNINITIALIZED_STATUS:
      return ("UNINITIALIZED_STATUS");	/* -1 */
    default:
      break;
    }
  return("!!UNDEFINED_SYMBOL!!");
}

static const struct cms_enum_info enum_RCS_STATUS_info_struct = {
  "RCS_STATUS",
  (const char **) enum_RCS_STATUS_string_list,
  enum_RCS_STATUS_int_list,
  MAX_ENUM_RCS_STATUS_STRING_LENGTH,
  ENUM_RCS_STATUS_LENGTH,
  (cms_symbol_lookup_function_t) enum_RCS_STATUS_symbol_lookup
};


RCS_STAT_MSG::RCS_STAT_MSG (NMLTYPE t, size_t sz):
  NMLmsg (t, (long) sz),
  command_type(-1),echo_serial_number(-1),status(UNINITIALIZED_STATUS),
  state(-1),line(-1),source_line(-1)
{
// just avoiding an inline function.
  command_type = -1;
  echo_serial_number = -1;
  status = UNINITIALIZED_STATUS;
  state = -1;
  line = -1;
  source_line = -1;
  memset (source_file, 0, 64);

}


void
RCS_STAT_MSG::update_stat_msg_base (CMS * cms)
{
  if (!cms)
    return;

  if (cms->update_stat_msg_base)
    {
      cms->beginClass ("RCS_STAT_MSG", "NMLmsg");
      cms->update_with_name ("command_type", command_type);
      cms->update_with_name ("echo_serial_number", echo_serial_number);
      status =
	(enum RCS_STATUS) cms->update_enumeration_with_name ("status",
							     (int) status,
							     (void *) &status,
							     &enum_RCS_STATUS_info_struct);
      cms->update_with_name ("state", state);
      cms->update_with_name ("line", line);
      cms->update_with_name ("source_line", source_line);
      cms->update_with_name ("source_file", source_file, 64);
      cms->endClass ("RCS_STAT_MSG", "NMLmsg");
    }
}

int
RCS_STAT_MSG_format (NMLTYPE t, void * buf, CMS * cms)
{
  if(!cms)
    {
      return -1;
    }

  if(cms->searching_for_max_size_from_size_list)
    {
      return 0;
    }

#ifdef LINUXCNC_LIBNML_COMPAT
  cms->update_stat_msg_base_in_format = 1;
  cms->update_stat_msg_base = 0;
#endif

  if (cms->update_stat_msg_base_in_format && buf)
    {
      cms->beginClass ("RCS_STAT_MSG", "NMLmsg");
      cms->update_with_name ("command_type",
			     ((RCS_STAT_MSG *) buf)->command_type);
      cms->update_with_name ("echo_serial_number",
			     ((RCS_STAT_MSG *) buf)->echo_serial_number);
      ((RCS_STAT_MSG *) buf)->status =
	(enum RCS_STATUS) cms->update_enumeration_with_name ("status",
							     (int) ((RCS_STAT_MSG *) buf)->status, (void *) &(((RCS_STAT_MSG *) buf)->status), &enum_RCS_STATUS_info_struct);
      cms->update_with_name ("state", ((RCS_STAT_MSG *) buf)->state);
      cms->update_with_name ("line", ((RCS_STAT_MSG *) buf)->line);
      cms->update_with_name ("source_line",
			     ((RCS_STAT_MSG *) buf)->source_line);
      cms->update_with_name ("source_file",
			     ((RCS_STAT_MSG *) buf)->source_file, 64);
      cms->endClass ("RCS_STAT_MSG", "NMLmsg");
    }

  switch (t)
    {
    case RCS_GENERIC_STATUS_TYPE:
      ((RCS_GENERIC_STATUS *) buf)->update (cms);
      return (1);

    default:
      return (0);
    }
  return (0);
}

RCS_GENERIC_STATUS::RCS_GENERIC_STATUS ():
RCS_STAT_MSG (RCS_GENERIC_STATUS_TYPE, sizeof (RCS_GENERIC_STATUS))
{
// Just avoiding an inline function.
}

void
RCS_GENERIC_STATUS::update (CMS *)
{
// Just avoiding an inline function.
}


RCS_LINKED_LIST *
RCS_STAT_CHANNEL::stat_format_chain_setup(NML_FORMAT_PTR f_ptr)
{
  RCS_LINKED_LIST *fc =new RCS_LINKED_LIST();
  fc->store_at_head ((void *) f_ptr, 0, 0);
  fc->store_at_head ((void *) RCS_STAT_MSG_format, 0, 0);
  return fc;
}

#ifdef LINUXCNC_LIBNML_COMPAT
RCS_STAT_CHANNEL::RCS_STAT_CHANNEL (NML_FORMAT_PTR f_ptr, 
				    const char *name,
				    const char *process, 
				    const char *file,
				    int set_to_server):
  NML (
       stat_format_chain_setup(f_ptr),
       name, process, file,NML_GENERIC_CHANNEL_TYPE, set_to_server,0,
       NML_NORMAL_CONNECTION_MODE)
{
  channel_type = RCS_STAT_CHANNEL_TYPE;
  sizeof_message_header = sizeof (RCS_STAT_MSG);
  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      cms->update_stat_msg_base = 0;
      cms->update_stat_msg_base_in_format = 1;
    }
  spawn_and_register_server_if_needed(set_to_server);
}

#endif  // LINUXCNC_LIBNML_COMPAT

RCS_STAT_CHANNEL::RCS_STAT_CHANNEL (NML_FORMAT_PTR f_ptr, 
				    const char *name,
				    const char *process, 
				    const char *file,
				    int set_to_server, 
				    int set_to_master,
				    enum NML_CONNECTION_MODE _connect_mode):
  NML (
       stat_format_chain_setup(f_ptr),
       name, process, file,RCS_STAT_CHANNEL_TYPE, set_to_server,set_to_master,_connect_mode)
{
  channel_type = RCS_STAT_CHANNEL_TYPE;
  sizeof_message_header = sizeof (RCS_STAT_MSG);
  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
    }
  spawn_and_register_server_if_needed(set_to_server);
}


RCS_STAT_CHANNEL::~RCS_STAT_CHANNEL ()
{
}
