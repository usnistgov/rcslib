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
#include "cmd_msg.hh"
#include "linklist.hh"		// class RCS_LINKED_LIST

RCS_CMD_MSG::RCS_CMD_MSG (NMLTYPE t, long sz):
  NMLmsg (t, sz),
  serial_number(0)
{
  serial_number = 0;
}

void
RCS_CMD_MSG::update_cmd_msg_base (CMS * cms)
{
  if (!cms)
    return;

  if (cms->update_cmd_msg_base)
    {
      cms->beginClass ("RCS_CMD_MSG", "NMLmsg");
      cms->update_with_name ("serial_number", serial_number);
      cms->endClass ("RCS_CMD_MSG", "NMLmsg");
    }
}

int
RCS_CMD_MSG_format (NMLTYPE t, void * buf, CMS * cms)
{

#ifdef LINUXCNC_LIBNML_COMPAT
  cms->update_cmd_msg_base_in_format = 1;
  cms->update_cmd_msg_base = 0;
#endif

  if (cms->update_cmd_msg_base_in_format)
    {
      cms->update_with_name ("serial_number",
			     ((RCS_CMD_MSG *) buf)->serial_number);
    }

  switch (t)
    {
    case RCS_GENERIC_CMD_TYPE:
      ((RCS_GENERIC_CMD *) buf)->update (cms);
      return (1);

    default:
      return (0);
    }
  return (0);
}

RCS_GENERIC_CMD::RCS_GENERIC_CMD ():
  RCS_CMD_MSG (RCS_GENERIC_CMD_TYPE, sizeof (RCS_GENERIC_CMD)),
  gen_id(0)
{
// Just avoiding an inline function.
}

void
RCS_GENERIC_CMD::update (CMS * cms)
{
  cms->update (gen_id);
}

RCS_LINKED_LIST *
RCS_CMD_CHANNEL::cmd_format_chain_setup(NML_FORMAT_PTR f_ptr)
{
  RCS_LINKED_LIST *fc =new RCS_LINKED_LIST();
  fc->store_at_head ((void *) f_ptr, 0, 0);
  fc->store_at_head ((void *) RCS_CMD_MSG_format, 0, 0);
  return fc;
}

#ifdef LINUXCNC_LIBNML_COMPAT
RCS_CMD_CHANNEL::RCS_CMD_CHANNEL (NML_FORMAT_PTR f_ptr, const char *name, const char *process, const char *file,
				  int set_to_server):
  NML (
       cmd_format_chain_setup(f_ptr),
       name, process, file,NML_GENERIC_CHANNEL_TYPE,set_to_server,0,NML_NORMAL_CONNECTION_MODE)
{
  
  channel_type = RCS_CMD_CHANNEL_TYPE;
  sizeof_message_header = sizeof (RCS_CMD_MSG);
  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
      cms->update_cmd_msg_base = 0;
      cms->update_cmd_msg_base_in_format = 1;
    }

  spawn_and_register_server_if_needed(set_to_server);
}


#endif  // LINUXCNC_LIBNML_COMPAT

RCS_CMD_CHANNEL::RCS_CMD_CHANNEL (NML_FORMAT_PTR f_ptr,
				  const char *name,
				  const char *process, 
				  const char *file,
				  int set_to_server,
				  int set_to_master,
				  enum NML_CONNECTION_MODE _connect_mode):
  NML (
       cmd_format_chain_setup(f_ptr),
       name, process, file,RCS_CMD_CHANNEL_TYPE,set_to_server,set_to_master,_connect_mode)
{
  
  channel_type = RCS_CMD_CHANNEL_TYPE;
  sizeof_message_header = sizeof (RCS_CMD_MSG);
  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
    }

  spawn_and_register_server_if_needed(set_to_server);
}



RCS_CMD_CHANNEL::~RCS_CMD_CHANNEL ()
{
  // Something funny happens to gdb without this being explicitly defined.
}
