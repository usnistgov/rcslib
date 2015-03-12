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

#define CMS_NETWORK_SOURCE 1

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1
#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if (defined(ENABLE_RCS_TTY))

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#ifdef UNIX_LIKE_PLAT
#include <netinet/in.h>
#endif
#endif


#include "cms.hh"

#include "ttymem.hh"
#include "ttyintf.h"
#include "sokintrf.h"		// u_long,htonl(), dl_ntohl()
#include "rem_msg.hh"
#include "rcs_prnt.hh"		// rcs_print_error()

#include <stdlib.h>
#include <string.h>		// memset(),strstr(), atol()


TTYMEM::TTYMEM (const char *bufline, const char *procline):
  CMS (bufline, procline),
  handle(0),settings(),serial_number(0),
  returned_serial_number(0),message_size(0),id(0)
{
  serial_number = 0;
  returned_serial_number = 0;
  memset (temp_buffer, 0, 0x2000);
  memset (&settings, 0,sizeof(settings));

#ifdef UNIX_LIKE_PLAT
  const char *default_dev_name = "/dev/ttyb";
#else
  const char *default_dev_name = "COM2:";
#endif

  const char *devNameEq = strstr (procline, "serialPortDevName=");
  if (NULL != devNameEq)
    {
      strncpy (ttyDevName,devNameEq + 18,sizeof(ttyDevName));
      clean_string(ttyDevName,sizeof(ttyDevName));
    }
  else
    {
      strncpy (ttyDevName, default_dev_name, sizeof(ttyDevName));
    }

  handle = open_serial_communications_port (ttyDevName);
  if (handle <= 0)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  settings.baud_rate = 9600;
  settings.data_bits = 8;
  settings.stop_bits = 1;
  settings.use_parity = 0;

  if (strstr (bufline, "evenparity"))
    {
      settings.use_parity = 1;
      settings.even_parity = 1;
    }
  if (strstr (bufline, "oddparity"))
    {
      settings.use_parity = 1;
      settings.even_parity = 0;
    }

  const char *baud_rate_eq = strstr (bufline, "baud_rate=");
  if (NULL != baud_rate_eq)
    {
      settings.baud_rate = atol (baud_rate_eq + 10);
    }
  const char *data_bits_eq = strstr (bufline, "data_bits=");
  if (NULL != data_bits_eq)
    {
      settings.data_bits = atol (data_bits_eq + 10);
    }
  const char *stop_bits_eq = strstr (bufline, "stop_bits=");
  if (NULL != stop_bits_eq)
    {
      settings.stop_bits = atol (stop_bits_eq + 10);
    }
  if (set_serial_port_configuration (handle, &settings) < 0)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  verify_bufname ();
}


TTYMEM::~TTYMEM ()
{
  close_serial_communications_port (handle);
}


void
TTYMEM::verify_bufname ()
{
  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  if (write_serial_communications_port (handle, temp_buffer, 20) < 0)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  serial_number++;
  if (readn_serial_communications_port (handle, temp_buffer, 40) < 40)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  returned_serial_number = (CMS_STATUS) ntohl (*((u_long *) temp_buffer));
  if (returned_serial_number != serial_number)
    {
      rcs_print_error
	("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	 returned_serial_number, serial_number);
      status = CMS_MISC_ERROR;
      return;
    }
  status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
  if (status < 0)
    {
      return;
    }
  if (strncmp (temp_buffer + 8, BufferName, 31))
    {
      rcs_print_error
	("TTYMEM: The buffer (%s) is registered with buffer number %ld.\n",
	 ((char *) temp_buffer + 8), buffer_number);
      rcs_print_error
	("TTYMEM: However, this process (%s) is attempting to connect to the buffer %s at the same location.\n",
	 ProcessName, BufferName);
      status = CMS_RESOURCE_CONFLICT_ERROR;
      return;
    }
}


void
TTYMEM::reconnect ()
{
}

void
TTYMEM::disconnect ()
{
}

CMS_STATUS TTYMEM::read ()
{

  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_READ_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  *((u_long *) temp_buffer + 3) = htonl ((u_long) CMS_READ_ACCESS);
  *((u_long *) temp_buffer + 4) = htonl ((u_long) in_buffer_id);

  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      *((u_long *) temp_buffer + 5) = htonl ((u_long) current_subdivision);
      send_header_size = 24;
    }
  if (write_serial_communications_port (handle, temp_buffer, send_header_size)
      < 0)
    {
      rcs_print_error ("TTYMEM: Can't send READ request to server.\n");
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (readn_serial_communications_port (handle, temp_buffer, 20) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  returned_serial_number = (CMS_STATUS) ntohl (*((u_long *) temp_buffer));
  if (returned_serial_number != serial_number)
    {
      rcs_print_error
	("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	 returned_serial_number, serial_number);
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
  message_size = ntohl (*((u_long *) temp_buffer + 2));
  id = ntohl (*((u_long *) temp_buffer + 3));
  header.was_read = ntohl (*((u_long *) temp_buffer + 4));
  if (message_size > max_encoded_message_size)
    {
      rcs_print_error ("Recieved message is too big. (%d > %ld)\n",
		       message_size, max_encoded_message_size);
      return (status = CMS_MISC_ERROR);
    }
  if (message_size > 0)
    {
      if (readn_serial_communications_port
	  (handle, (char *) encoded_data, message_size) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
    }
  check_id (id);
  return (status);
}




CMS_STATUS TTYMEM::peek ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_READ_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  *((u_long *) temp_buffer + 3) = htonl ((u_long) CMS_PEEK_ACCESS);
  *((u_long *) temp_buffer + 4) = htonl ((u_long) in_buffer_id);

  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      *((u_long *) temp_buffer + 5) = htonl ((u_long) current_subdivision);
      send_header_size = 24;
    }
  if (write_serial_communications_port (handle, temp_buffer, send_header_size)
      < 0)
    {
      rcs_print_error ("TTYMEM: Can't send READ request to server.\n");
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (readn_serial_communications_port (handle, temp_buffer, 20) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  returned_serial_number = (CMS_STATUS) ntohl (*((u_long *) temp_buffer));
  if (returned_serial_number != serial_number)
    {
      rcs_print_error
	("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	 returned_serial_number, serial_number);
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
  message_size = ntohl (*((u_long *) temp_buffer + 2));
  id = ntohl (*((u_long *) temp_buffer + 3));
  header.was_read = ntohl (*((u_long *) temp_buffer + 4));
  if (message_size > max_encoded_message_size)
    {
      rcs_print_error ("Recieved message is too big. (%d > %ld)\n",
		       message_size, max_encoded_message_size);
      return (status = CMS_MISC_ERROR);
    }
  if (message_size > 0)
    {
      if (readn_serial_communications_port
	  (handle, (char *) encoded_data, message_size) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
    }
  check_id (id);
  return (status);
}


CMS_STATUS TTYMEM::blocking_read (double _blocking_timeout)
{
  rcs_print_error (" Can not call blocking_read(%f) when using TTYMEM.\n",
		   _blocking_timeout);
  return (status = CMS_NO_BLOCKING_SEM_ERROR);
}


CMS_STATUS TTYMEM::write (void *user_data)
{

  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_WRITE_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  *((u_long *) temp_buffer + 3) = htonl ((u_long) CMS_WRITE_ACCESS);
  *((u_long *) temp_buffer + 4) = htonl ((u_long) header.in_buffer_size);
  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      *((u_long *) temp_buffer + 5) = htonl ((u_long) current_subdivision);
      send_header_size = 24;
    }
  if (header.in_buffer_size < 0x2000 - 20 && header.in_buffer_size > 0)
    {
      memcpy (temp_buffer + send_header_size, user_data,
	      header.in_buffer_size);
      if (write_serial_communications_port
	  (handle, temp_buffer, header.in_buffer_size + send_header_size) < 0)
	{
	  rcs_print_error
	    ("TTYMEM: Failed to send message of size %ld + header of size %d  to the server.\n",
	     header.in_buffer_size, send_header_size);
	  return (status = CMS_MISC_ERROR);
	}
    }
  else
    {
      if (write_serial_communications_port
	  (handle, temp_buffer, send_header_size) < 0)
	{
	  rcs_print_error ("TTYMEM: Failed to send header to server.\n");
	  return (status = CMS_MISC_ERROR);
	}
      if (header.in_buffer_size > 0)
	{
	  if (write_serial_communications_port
	      (handle, (char *) user_data, header.in_buffer_size) < 0)
	    {
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  serial_number++;
  if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
      || confirm_write)
    {
      if (readn_serial_communications_port (handle, temp_buffer, 12) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
      returned_serial_number =
	(CMS_STATUS) ntohl (*((u_long *) temp_buffer));
      if (returned_serial_number != serial_number)
	{
	  rcs_print_error
	    ("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	     returned_serial_number, serial_number);
	  return (status = CMS_MISC_ERROR);
	}
      status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
      header.was_read = ntohl (*((u_long *) temp_buffer + 2));
    }
  else
    {
      header.was_read = 0;
      status = CMS_WRITE_OK;
      returned_serial_number = serial_number;
    }
  return (status);
}



CMS_STATUS TTYMEM::write_if_read (void *user_data)
{

  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_WRITE_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  *((u_long *) temp_buffer + 3) = htonl ((u_long) CMS_WRITE_ACCESS);
  *((u_long *) temp_buffer + 4) = htonl ((u_long) header.in_buffer_size);
  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      *((u_long *) temp_buffer + 5) = htonl ((u_long) current_subdivision);
      send_header_size = 24;
    }
  if (header.in_buffer_size < 0x2000 - 20 && header.in_buffer_size > 0)
    {
      memcpy (temp_buffer + send_header_size, user_data,
	      header.in_buffer_size);
      if (write_serial_communications_port
	  (handle, temp_buffer, header.in_buffer_size + send_header_size) < 0)
	{
	  rcs_print_error
	    ("TTYMEM: Failed to send message of size %ld + header of size %d  to the server.\n",
	     header.in_buffer_size, send_header_size);
	  return (status = CMS_MISC_ERROR);
	}
    }
  else
    {
      if (write_serial_communications_port
	  (handle, temp_buffer, send_header_size) < 0)
	{
	  rcs_print_error ("TTYMEM: Failed to send header to server.\n");
	  return (status = CMS_MISC_ERROR);
	}
      if (header.in_buffer_size > 0)
	{
	  if (write_serial_communications_port
	      (handle, (char *) user_data, header.in_buffer_size) < 0)
	    {
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  serial_number++;
  if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
      || confirm_write)
    {
      if (readn_serial_communications_port (handle, temp_buffer, 12) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
      returned_serial_number =
	(CMS_STATUS) ntohl (*((u_long *) temp_buffer));
      if (returned_serial_number != serial_number)
	{
	  rcs_print_error
	    ("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	     returned_serial_number, serial_number);
	  return (status = CMS_MISC_ERROR);
	}
      status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
      header.was_read = ntohl (*((u_long *) temp_buffer + 2));
    }
  else
    {
      header.was_read = 0;
      status = CMS_WRITE_OK;
      returned_serial_number = serial_number;
    }
  return (status);
}


int
TTYMEM::check_if_read ()
{

  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      *((u_long *) temp_buffer + 3) = htonl ((u_long) current_subdivision);
    }
  if (write_serial_communications_port (handle, temp_buffer, send_header_size)
      < 0)
    {
      status = CMS_MISC_ERROR;
      return (0);
    }
  serial_number++;
  if (readn_serial_communications_port (handle, temp_buffer, 12) < 0)
    {
      status = CMS_MISC_ERROR;
      return 0;
    }
  returned_serial_number = (CMS_STATUS) ntohl (*((u_long *) temp_buffer));
  if (returned_serial_number != serial_number)
    {
      rcs_print_error
	("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	 returned_serial_number, serial_number);
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
  header.was_read = ntohl (*((u_long *) temp_buffer + 2));
  return (header.was_read);
}


CMS_STATUS TTYMEM::clear ()
{
  *((u_long *) temp_buffer) = htonl ((u_long) serial_number);
  *((u_long *) temp_buffer + 1) =
    htonl ((u_long) REMOTE_CMS_CLEAR_REQUEST_TYPE);
  *((u_long *) temp_buffer + 2) = htonl ((u_long) buffer_number);
  *((u_long *) temp_buffer + 3) = htonl ((u_long) current_subdivision);

  if (write_serial_communications_port (handle, temp_buffer, 20) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (readn_serial_communications_port (handle, temp_buffer, 8) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  returned_serial_number = (CMS_STATUS) ntohl (*((u_long *) temp_buffer));
  if (returned_serial_number != serial_number)
    {
      rcs_print_error
	("TTYMEM: Returned serial number(%d) does not match expected serial number(%d).\n",
	 returned_serial_number, serial_number);
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntohl (*((u_long *) temp_buffer + 1));
  header.was_read = ntohl (*((u_long *) temp_buffer + 2));
  return (status);
}

//  (defined(ENABLE_RCS_TTY))
#else
#include "rcs_empty_source"
#endif
