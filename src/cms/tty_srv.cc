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

/****************************************************************************
* File: tty_srv.cc
* Purpose: Provides the functions for the class CMS_SERVER_REMOTE_T_PORT
*  which provides TTY specific overrides of the CMS_SERVER_REMOTE_PORT class.
****************************************************************************/
#define CMS_NETWORK_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if (defined(ENABLE_RCS_SERVER) && defined(ENABLE_RCS_TTY))

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "tty_srv_no_config.h"
#endif

#include "sokintrf.h"		/* dl_ioctl() */
#include "ttyintf.h"
#include "cms.hh"		/* class CMS */
#include "nml.hh"		// class NML
#include "tty_srv.hh"		/* class CMS_SERVER_REMOTE_TTY_PORT */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "timer.hh"		// esleep()
#include "rem_msg.hh"


CMS_SERVER_REMOTE_TTY_PORT::CMS_SERVER_REMOTE_TTY_PORT (CMS_SERVER *
							_cms_server):
CMS_SERVER_REMOTE_PORT (_cms_server),
  dtimeout(0),
  handle(0),
  request(0),
  serial_number(0),
  settings()
{
  handle = (RCS_SERIAL_PORT_HANDLE) - 1;
  memset (devName, 0, 0x100);
  memset(&settings,0,sizeof(settings));
}

CMS_SERVER_REMOTE_TTY_PORT::~CMS_SERVER_REMOTE_TTY_PORT ()
{
  unregister_port ();
}

void
CMS_SERVER_REMOTE_TTY_PORT::unregister_port ()
{
  close_serial_communications_port (handle);
}

int
CMS_SERVER_REMOTE_TTY_PORT::accept_local_port_cms (CMS * _cms)
{
  if (NULL == _cms)
    {
      return 0;
    }
  if (_cms->remote_port_type != CMS_TTY_REMOTE_PORT_TYPE)
    {
      return 0;
    }
  if (NULL != _cms)
    {
#ifdef UNIX_LIKE_PLAT
      const char *default_dev_name = "/dev/ttyb";
#else
      const char *default_dev_name = "COM2:";
#endif
      char ttyDevName[80];
      char *devNameEq = strstr (_cms->ProcessLine, "serialPortDevName=");
      if (NULL != devNameEq)
	{
	  strncpy (ttyDevName, devNameEq + 18, 80);
	}
      else
	{
	  strncpy (ttyDevName, default_dev_name, 80);
	}
      if (devName[0] == 0)
	{
	  strncpy (devName, clean_string (ttyDevName, 80), 80);
	  settings.baud_rate = 9600;
	  settings.data_bits = 8;
	  settings.stop_bits = 1;
	  settings.use_parity = 0;

	  if (strstr (_cms->BufferLine, "evenparity"))
	    {
	      settings.use_parity = 1;
	      settings.even_parity = 1;
	    }
	  if (strstr (_cms->BufferLine, "oddparity"))
	    {
	      settings.use_parity = 1;
	      settings.even_parity = 0;
	    }

	  char *baud_rate_eq = strstr (_cms->BufferLine, "baud_rate=");
	  if (NULL != baud_rate_eq)
	    {
	      settings.baud_rate = atol (baud_rate_eq + 10);
	    }
	  char *data_bits_eq = strstr (_cms->BufferLine, "data_bits=");
	  if (NULL != data_bits_eq)
	    {
	      settings.data_bits = atol (data_bits_eq + 10);
	    }
	  char *stop_bits_eq = strstr (_cms->BufferLine, "stop_bits=");
	  if (NULL != stop_bits_eq)
	    {
	      settings.stop_bits = atol (stop_bits_eq + 10);
	    }
	  return 1;
	}
      else
	{
	  if (!strcmp (devName, ttyDevName))
	    {
	      return 1;
	    }
	}
    }
  return 0;
}

void
CMS_SERVER_REMOTE_TTY_PORT::register_port ()
{

  handle = open_serial_communications_port (devName);
  if (handle < 0)
    {
      return;
    }
  if (set_serial_port_configuration (handle, &settings) < 0)
    {
      return;
    }
  port_registered = 1;
}


void
CMS_SERVER_REMOTE_TTY_PORT::run ()
{
  cms_server_count++;
  while (1)
    {
      if (readn_serial_communications_port (handle, temp_buffer, 20) < 20)
	{
	  rcs_print_error ("Can not read from serial port.\n");
	}
      handle_request ();
    }
}


void
CMS_SERVER_REMOTE_TTY_PORT::handle_request ()
{

#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  DWORD pid = GetCurrentProcessId ();
  DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
  int pid = taskIdSelf ();

  int tid = 0;
#else
  pid_t pid = getpid ();
  pid_t tid = 0;
#endif
#endif

  CMS_SERVER *server;
  server = find_server (pid, tid);
  if (NULL == server)
    {
      rcs_print_error
	("CMS_SERVER_REMOTE_TTY_PORT::handle_request() Cannot find server object for pid = %d.\n",
	 pid);
      return;
    }

  u_long request_type, buffer_number, received_serial_number;
  received_serial_number = ntohl (*((u_long *) temp_buffer));
  request_type = ntohl (*((u_long *) temp_buffer + 1));
  buffer_number = ntohl (*((u_long *) temp_buffer + 2));
  serial_number = received_serial_number;
  serial_number++;

  int total_subdivisions = 1;

  switch (request_type)
    {
    case REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE:
      {
	REMOTE_GET_BUF_NAME_REQUEST namereq;
	namereq.buffer_number = buffer_number;
	REMOTE_GET_BUF_NAME_REPLY *namereply = NULL;
	namereply =
	  (REMOTE_GET_BUF_NAME_REPLY *) server->process_request (&namereq);
	memset (temp_buffer, 0, 40);
	if (NULL != namereply)
	  {
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) = htonl (namereply->status);
	    strncpy (temp_buffer + 8, namereply->name, 31);
	    if (write_serial_communications_port (handle, temp_buffer, 40) <
		0)
	      {
		return;
	      }
	  }
	else
	  {
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) =
	      htonl ((unsigned) CMS_SERVER_SIDE_ERROR);
	    if (write_serial_communications_port (handle, temp_buffer, 40) <
		0)
	      {
		return;
	      }
	  }
      }
      break;


    case REMOTE_CMS_READ_REQUEST_TYPE:
      {
	if(server->read_req_ptr == 0)
	  {
	    server->read_req_ptr = new REMOTE_READ_REQUEST();
	  }
	server->read_req_ptr->buffer_number = buffer_number;
	server->read_req_ptr->access_type = ntohl (*((u_long *) temp_buffer + 3));
	server->read_req_ptr->last_id_read =
	  ntohl (*((u_long *) temp_buffer + 4));
	REMOTE_READ_REPLY *read_reply=
	  (REMOTE_READ_REPLY *) server->process_request (server->read_req_ptr);
	if (max_total_subdivisions > 1)
	  {
	    total_subdivisions = server->get_total_subdivisions (buffer_number);
	  }
	if (total_subdivisions > 1)
	  {
	    if (readn_serial_communications_port
		(handle, (char *) (((u_long *) temp_buffer) + 5), 4) < 0)
	      {
		rcs_print_error ("Can not read from serial port\n");
		return;
	      }
	    server->read_req_ptr->subdiv = ntohl (*((u_long *) temp_buffer + 5));
	  }
	else
	  {
	    server->read_req_ptr->subdiv = 0;
	  }
	if (NULL == read_reply)
	  {
	    rcs_print_error ("Server could not process request.\n");
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) =
	      htonl ((unsigned) CMS_SERVER_SIDE_ERROR);
	    *((u_long *) temp_buffer + 2) = htonl (0);	/* size */
	    *((u_long *) temp_buffer + 3) = htonl (0);	/* write_id */
	    *((u_long *) temp_buffer + 4) = htonl (0);	/* was_read */
	    write_serial_communications_port (handle, temp_buffer, 20);
	    return;
	  }
	*((u_long *) temp_buffer) = htonl (serial_number);
	*((u_long *) temp_buffer + 1) = htonl (read_reply->status);
	*((u_long *) temp_buffer + 2) = htonl (read_reply->size);
	*((u_long *) temp_buffer + 3) = htonl (read_reply->write_id);
	*((u_long *) temp_buffer + 4) = htonl (read_reply->was_read);
	if (read_reply->size < (0x2000 - 20)
	    && read_reply->size > 0)
	  {
	    memcpy (temp_buffer + 20, read_reply->data,
		    read_reply->size);
	    if (write_serial_communications_port
		(handle, temp_buffer, 20 + read_reply->size) < 0)
	      {
		return;
	      }
	  }
	else
	  {
	    if (write_serial_communications_port (handle, temp_buffer, 20) < 0)
	      {
		return;
	      }
	    if (read_reply->size > 0)
	      {
		if (write_serial_communications_port
		    (handle, (char *) read_reply->data,
		     read_reply->size) < 0)
		  {
		    return;
		  }
	      }
	  }
      }
      break;

    case REMOTE_CMS_WRITE_REQUEST_TYPE:
      {
	if(server->write_req_ptr == 0)
	  {
	    server->write_req_ptr = new REMOTE_WRITE_REQUEST();
	  }
	server->write_req_ptr->buffer_number = buffer_number;
	server->write_req_ptr->access_type =
	  ntohl (*((u_long *) temp_buffer + 3));
	server->write_req_ptr->size = ntohl (*((u_long *) temp_buffer + 4));
	total_subdivisions = 1;
	if (max_total_subdivisions > 1)
	  {
	    total_subdivisions = server->get_total_subdivisions (buffer_number);
	  }
	if (total_subdivisions > 1)
	  {
	    if (readn_serial_communications_port
		(handle, (char *) (((u_long *) temp_buffer) + 5), 4) < 0)
	      {
		return;
	      }
	    server->write_req_ptr->subdiv = ntohl (*((u_long *) temp_buffer + 5));
	  }
	else
	  {
	    server->write_req_ptr->subdiv = 0;
	  }
	if (server->write_req_ptr->size > 0)
	  {
	    if (readn_serial_communications_port
		(handle, (char *) server->write_req_ptr->data,
		 server->write_req_ptr->size) < 0)
	      {
		return;
	      }
	  }
	REMOTE_WRITE_REPLY *write_reply =
	  (REMOTE_WRITE_REPLY *) server->process_request (server->write_req_ptr);
	if ( (min_compatible_version < 2.58 && min_compatible_version > 1e-6)
	    || confirm_write)
	  {
	    if (NULL == write_reply)
	      {
		rcs_print_error ("Server could not process request.\n");
		*((u_long *) temp_buffer) = htonl (serial_number);
		*((u_long *) temp_buffer + 1) =
		  htonl ((unsigned) CMS_SERVER_SIDE_ERROR);
		*((u_long *) temp_buffer + 2) = htonl (0);	/* was_read */
		write_serial_communications_port (handle, temp_buffer, 12);
		return;
	      }
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) =
	      htonl (write_reply->status);
	    *((u_long *) temp_buffer + 2) =
	      htonl (write_reply->was_read);
	    if (write_serial_communications_port (handle, temp_buffer, 12) < 0)
	      {
	      }
	  }
	else
	  {
	    if (NULL == write_reply)
	      {
		rcs_print_error ("Server could not process request.\n");
	      }
	  }
      }
      break;

    case REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE:
      {
	if(server->check_if_read_req_ptr == 0)
	  {
	    server->check_if_read_req_ptr = new REMOTE_CHECK_IF_READ_REQUEST();
	  }
	server->check_if_read_req_ptr->buffer_number = buffer_number;
	server->check_if_read_req_ptr->subdiv =
	  ntohl (*((u_long *) temp_buffer + 3));
	REMOTE_CHECK_IF_READ_REPLY *check_if_read_reply =
	  (REMOTE_CHECK_IF_READ_REPLY *) server->process_request (server->
								  check_if_read_req_ptr);
	if (NULL == check_if_read_reply)
	  {
	    rcs_print_error ("Server could not process request.\n");
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) =
	      htonl ((unsigned) CMS_SERVER_SIDE_ERROR);
	    *((u_long *) temp_buffer + 2) = htonl (0);	/* was_read */
	    write_serial_communications_port (handle, temp_buffer, 12);
	    return;
	  }
	*((u_long *) temp_buffer) = htonl (serial_number);
	*((u_long *) temp_buffer + 1) =
	  htonl (check_if_read_reply->status);
	*((u_long *) temp_buffer + 2) =
	  htonl (check_if_read_reply->was_read);
	if (write_serial_communications_port (handle, temp_buffer, 12) < 0)
	  {
	  }
      }
      break;

    case REMOTE_CMS_CLEAR_REQUEST_TYPE:
      {
	if(server->clear_req_ptr == 0)
	  {
	    server->clear_req_ptr = new REMOTE_CLEAR_REQUEST();
	  }
	server->clear_req_ptr->buffer_number = buffer_number;
	server->clear_req_ptr->subdiv = ntohl (*((u_long *) temp_buffer + 3));
	REMOTE_CLEAR_REPLY *clear_reply =
	  (REMOTE_CLEAR_REPLY *) server->process_request (server->clear_req_ptr);
	if (NULL == clear_reply)
	  {
	    rcs_print_error ("Server could not process request.\n");
	    *((u_long *) temp_buffer) = htonl (serial_number);
	    *((u_long *) temp_buffer + 1) =
	      htonl ((unsigned) CMS_SERVER_SIDE_ERROR);
	    write_serial_communications_port (handle, temp_buffer, 8);
	    return;
	  }
	*((u_long *) temp_buffer) = htonl (serial_number);
	*((u_long *) temp_buffer + 1) = htonl (clear_reply->status);
	if (write_serial_communications_port (handle, temp_buffer, 8) < 0)
	  {
	  }
      }
      break;

    default:
      rcs_print_error ("Unrecognized request type received.(%ld)\n",
		       request_type);
      break;
    }
}

CMS_SERVER_REMOTE_TTY_PORT::CMS_SERVER_REMOTE_TTY_PORT(
						       __unused_parameter__ const CMS_SERVER_REMOTE_TTY_PORT &_csrtp):
  CMS_SERVER_REMOTE_PORT(0),
  dtimeout(0),
  handle(0),
  request(0),
  serial_number(0),
  settings()
{
  rcs_print_error("CMS_SERVER_REMOTE_TTY_PORT copy constructor should never be called.\n");
}

CMS_SERVER_REMOTE_TTY_PORT &
CMS_SERVER_REMOTE_TTY_PORT::operator=(
				      __unused_parameter__ const CMS_SERVER_REMOTE_TTY_PORT &_csrtp)
{
  rcs_print_error("CMS_SERVER_REMOTE_TTY_PORT::operator= should never be called.\n");
  return(*this);
}

//  (defined(ENABLE_RCS_SERVER) && defined(ENABLE_RCS_TTY))
#else
#include "rcs_empty_source"
#endif
