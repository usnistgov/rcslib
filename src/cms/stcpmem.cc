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



#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "stcpmem_no_config.h"
#endif

#include "rem_msg.hh"		/* REMOTE_CMS_READ_REQUEST_TYPE, etc. */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "dbg_mem.h"		// DEBUG_FREE,DEBUG_MALLOC

#define REQUEST_TIMEOUT (2.0)
#include "timer.hh"		/* esleep() */

#include "stcpmem.hh"

#include "recvline.h"		/* recvline() */
#include "sendline.h"		/* sendline() */
#include "stcpopts.hh"		/* SET_TCP_NODELAY */
#include "sokintrf.h"		/* dl_socket(), dl_modified_gethostbyname() */

#ifdef ENABLE_RCS_CRYPT2
#include "crypt2.hh"		// crypt()
#endif

static const char *stcpmem_xml_end_string="-->]]><*END*>";
#define STCPMEM_XML_END_STRING_LENGTH (13)

STCPMEM::STCPMEM (const char *_bufline, const char *_procline):
  CMS (_bufline, _procline,-1),
  serial_number(0),returned_serial_number(0),
  server_socket_address_ptr(0),socket_fd(0),temp_buffer(0),
  temp_buffer_size(0),read_request_issued(0),read_request_time(0.0),
  polling(0),lines_to_skip(0),bytes_at_beginning_of_line(0),sockerrno(0),
  sockerrstr(0),last_request_was_write(false)
{
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      status = CMS_LIBRARY_UNAVAILABLE_ERROR;
    }
  socket_fd = 0;
  read_request_issued = 0;
  temp_buffer = NULL;
  polling = 0;
  lines_to_skip = 0;
  bytes_at_beginning_of_line = 0;
  last_request_was_write=false;

  if (CMS_DISPLAY_ASCII_ENCODING != neutral_encoding_method &&
      CMS_XML_ENCODING != neutral_encoding_method )
    {
      rcs_print_error
	("The neutral_encoding_method must be CMS_DISPLAY_ASCII_ENCODING or CMS_XML_ENCODING to use STCPMEM.\n");
      status = CMS_CONFIG_ERROR;
    }


  temp_buffer =
    (char *) DEBUG_MALLOC (cms_encoded_data_explosion_factor * size);
  if (temp_buffer == NULL)
    {
      rcs_print_error ("Out of memory!\n");
      status = CMS_CREATE_ERROR;
      return;
    }
  temp_buffer_size = cms_encoded_data_explosion_factor * size;
  serial_number = 0;

  server_socket_address_ptr = 
    dl_create_sa(BufferHost,stcp_port_number,use_ipv6);

  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Using server on %s with IP address %s and port %d.\n",
		   BufferHost,
		   dl_sa_get_host(server_socket_address_ptr),
		   dl_sa_get_port(server_socket_address_ptr));

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Creating socket . . .\n");

  socket_fd = (int) dl_tcp_socket (use_ipv6);
  if (socket_fd < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("STCPMEM: Error from socket() (errno = %d:%s)\n",
		       sockerrno, sockerrstr);
      status = CMS_CREATE_ERROR;
      return;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Setting socket options . . . \n");
  if (set_stcp_socket_options (socket_fd) < 0)
    {
      return;
    }
  struct dl_sa *cli_addr;
  char *bind_to_host = 
    getenv("NML_BINDTO_HOST");
  cli_addr = dl_create_sa((bind_proc_host?ProcessHost:bind_to_host),0,use_ipv6);
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Binding . . . \n");
  if (dl_bind (socket_fd, dl_sa_addr(cli_addr), dl_sa_len(cli_addr)) <
      0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("STCPMEM: bind error %d = %s\n", 
		       sockerrno,sockerrstr);
      status = CMS_CREATE_ERROR;
    }

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Connecting . . .\n");
  int connect_ret = -15;

  connect_ret = dl_connect_in_with_timeout(socket_fd, /* socket */
					   server_socket_address_ptr, /* svr address/port */
					   timeout, /* timeout */
					   1, /* print errors */
					   !stop_when_connection_refused, /* reconnect after connection refused */
					   0, /* errcode ptr */
					   0, /* timedout ptr */
					   0, /* interrupt_op_ptr */
					   sockerrbuf, /* buffer for error info */
					   sizeof(sockerrbuf)); /* size of sockerr buf */
  if(connect_ret != 0)
    {
      if(ignore_connect_err)
	{
	  rcs_print_error("STCPMEM connect to BufferHost %s failed.\n",BufferHost);
	  status = CMS_NO_SERVER_ERROR;
	}
      return;
    }
  
  dl_free_sa(cli_addr);

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "connect_ret=%d,socket_fd=%d\n",
		   connect_ret,socket_fd);

  polling = (NULL != strstr (ProcessLine, "poll"));
  if (polling)
    {
      timeout = 0;
    }
}

extern bool nml_cleanup_started;

STCPMEM::~STCPMEM ()
{
  esleep(0.05);
  if(!delete_totally
     && last_request_was_write
     && !interrupting_operation 
     && !nml_cleanup_started 
     && socket_fd > 0)
    {
      if(timeout > 5.0 || timeout < 0)
	{
	  timeout = 5.0;
	}
      check_if_read();
      esleep(0.05);
    }
  last_request_was_write=false;
  if (socket_fd > 0)
    {
      clean_prev_read_info (socket_fd);
      dl_closesocket (socket_fd);
      socket_fd = 0;
    }
  if (NULL != temp_buffer)
    {
      DEBUG_FREE (temp_buffer);
      temp_buffer = NULL;
    }
  if(server_socket_address_ptr)
    {
      dl_free_sa(server_socket_address_ptr);
      server_socket_address_ptr=0;
    }
  unload_socket_interface ();
}

CMS_STATUS STCPMEM::skip_lines ()
{

  while (lines_to_skip > 0)
    {
      if (recvline
	  (socket_fd, ((char *) encoded_data),
	   cms_encoded_data_explosion_factor * size, 0, timeout, NULL) < 0)
	{
	  if (recvline_timedout)
	    {
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      bytes_at_beginning_of_line = 0;
	      return (status = CMS_MISC_ERROR);
	    }
	}
      bytes_at_beginning_of_line = 0;
      lines_to_skip--;
    }
  bytes_at_beginning_of_line = 0;
  return (status);
}

CMS_STATUS STCPMEM::get_readpeek_reply()
{
  long line_size;
  bool all_lines_received;
  char *line_begin_ptr;

  all_lines_received=false;
  line_begin_ptr = ((char *) encoded_data);
  line_size =0;

  read_request_issued = 0;
  while(!all_lines_received)
    {
      if ((line_size =
	   recvline (socket_fd,
		     line_begin_ptr,
		     (encoded_data_size - bytes_at_beginning_of_line), 
		     0, timeout,
		     &bytes_at_beginning_of_line)) < 0)
	{
	  if (recvline_timedout)
	    {
	      if (polling)
		{
		  read_request_issued = 1;
		  return (status = CMS_READ_OLD);
		}
	      else
		{
		  lines_to_skip++;
		  bytes_at_beginning_of_line = 0;
		  return (status = CMS_TIMED_OUT);
		}
	    }
	  else
	    {
	      bytes_at_beginning_of_line = 0;
	      return (status = CMS_MISC_ERROR);
	    }
	}
      if(CMS_XML_ENCODING != neutral_encoding_method)
	{
	  all_lines_received=true;
	  break;
	}
      else if(!strncmp(line_begin_ptr,stcpmem_xml_end_string,STCPMEM_XML_END_STRING_LENGTH))
	{
	  memset(line_begin_ptr,0,STCPMEM_XML_END_STRING_LENGTH);
	  all_lines_received=true;
	  break;
	}
      if (!strncmp ((char *) encoded_data, "ERR", 3) ||
	  !strncmp ((char *) line_begin_ptr, "ERR", 3))
      {
	break;
      }
      if(line_size >0)
	{
	  line_begin_ptr += line_size;
	}
      if(CMS_XML_ENCODING == neutral_encoding_method &&
	 line_begin_ptr + STCPMEM_XML_END_STRING_LENGTH >= ((char*) encoded_data) +encoded_data_size)
	{
	  rcs_print_error("xml end tag not found. bytes_at_beginning_of_line=%d, encoded_data_size=%ld\n",bytes_at_beginning_of_line,encoded_data_size);
	  break;
	}
      else if(line_begin_ptr >= ((char*) encoded_data) + encoded_data_size)
	{
	  rcs_print_error("bytes_at_beginning_of_line=%d greater than encoded_data_size=%ld\n",bytes_at_beginning_of_line,encoded_data_size);
	  break;
	}
      if(CMS_XML_ENCODING == neutral_encoding_method && line_size > 0)
	{
	  *line_begin_ptr='\n';
	  line_begin_ptr++;
	}
    }
  bytes_at_beginning_of_line = 0;
  if (!strncmp ((char *) encoded_data, "ERR", 3))
    {
      long
	errcode;
      errcode = strtol (((char *) encoded_data) + 4, NULL, 0);
      if (errcode == 0)
	{
	  return (status = CMS_READ_OLD);
	}
      else
	{
	  return (status = CMS_MISC_ERROR);
	}
    }
  return(status);
}

CMS_STATUS STCPMEM::read ()
{
  long id;
  int read_request_issued_last_time;
  char request_string[32];

  id = in_buffer_id;
  last_request_was_write=false;
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "read(%d):", (int) buffer_number);
  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (((int) skip_lines ()) < 0)
    {
      return status;
    }

  read_request_issued_last_time = read_request_issued;

  if (!read_request_issued_last_time || !polling)
    {
      if (sendline (socket_fd, request_string, 0, timeout) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
      read_request_issued = 1;
      read_request_time = etime ();
    }

  if (read_request_issued_last_time || !polling)
    {
      get_readpeek_reply();
      if(status >= 0)
	{
	  id++;
	}
    }
  header.was_read = 1;
  check_id (id);
  return (status);
}


CMS_STATUS STCPMEM::peek ()
{
  long id;
  int read_request_issued_last_time;
  char request_string[32];

  id = in_buffer_id;
  read_request_issued_last_time = read_request_issued;

  last_request_was_write=false;
  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::peek: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "peek(%d):", (int) buffer_number);
  if (((int) skip_lines ()) < 0)
    {
      return status;
    }

  if (!read_request_issued_last_time || !polling)
    {
      if (sendline (socket_fd, request_string, 0, timeout) < 0)
	{
	  return (status = CMS_MISC_ERROR);
	}
      read_request_issued = 1;
      read_request_time = etime ();
    }
  if (read_request_issued_last_time || !polling)
    {
      get_readpeek_reply();
      if(status >= 0)
	{
	  id++;
	}
    }
  header.was_read = 1;
  check_id (id);
  return (status);
}




CMS_STATUS STCPMEM::write (
			   __unused_parameter__ void *)
{
  last_request_was_write=true;
  read_request_issued = 0;
  char request_string[32];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "write(%d): ", (int) buffer_number);

  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  memset (temp_buffer, 0, temp_buffer_size);
  strcpy (temp_buffer, request_string);
  strcat (temp_buffer, (char *) encoded_data);
  if (sendline (socket_fd, temp_buffer, 0, timeout) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (neutral_encoding_method == CMS_XML_ENCODING)
    {
      sendline(socket_fd,stcpmem_xml_end_string,0,timeout);
    }
  status = CMS_WRITE_OK;
  return (status);
}

CMS_STATUS STCPMEM::write_if_read (
				   __unused_parameter__ void *)
{
  last_request_was_write=true;
  read_request_issued = 0;
  char request_string[32];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "write_if_read(%d): ", (int) buffer_number);
  static int
    poll_error_message_sent =
    0;
  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (polling)
    {
      if (poll_error_message_sent < 100)
	{
	  rcs_print_error
	    ("STCPMEM: Can not write_if_read when polling is enabled.\n");
	}
      poll_error_message_sent++;
      return (status = CMS_MISC_ERROR);
    }

  memset (temp_buffer, 0, temp_buffer_size);
  strcpy (temp_buffer, request_string);
  strcat (temp_buffer, (char *) encoded_data);
  if (sendline (socket_fd, temp_buffer, 0, timeout) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (neutral_encoding_method == CMS_XML_ENCODING)
    {
      sendline(socket_fd,stcpmem_xml_end_string,0,timeout);
    }
  if (recvline (socket_fd, temp_buffer, temp_buffer_size, 0, timeout, NULL) <
      0)
    {
      rcs_print_error ("write_if_read: timed out.\n");
      return (status = CMS_TIMED_OUT);
    }
  if (!strncmp (temp_buffer, "write_if_read_succeeded", 22))
    {
      return (status = CMS_WRITE_OK);
    }
  rcs_print_error ("write_if_read_failed %s", temp_buffer);
  return (status = CMS_MISC_ERROR);
}

int
STCPMEM::check_if_read ()
{
  last_request_was_write=false;
  read_request_issued = 0;
  char request_string[32];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "check_if_read(%d): ", (int) buffer_number);
  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (sendline (socket_fd, request_string, 0, timeout) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (recvline (socket_fd, temp_buffer, temp_buffer_size, 0, timeout, NULL) <
      0)
    {
      status = CMS_TIMED_OUT;
      return 0;
    }
  if (!strncmp (temp_buffer, "was_read", 8))
    {
      return (1);
    }
  return (0);
}

int
STCPMEM::get_queue_length ()
{
  last_request_was_write=false;
  read_request_issued = 0;
  char request_string[32];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "get_queue_length(%d): ", (int) buffer_number);
  if (socket_fd <= 0)
    {
      rcs_print_error ("STCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (sendline (socket_fd, request_string, 0, timeout) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (recvline (socket_fd, temp_buffer, temp_buffer_size, 0, timeout, NULL) <
      0)
    {
      status = CMS_TIMED_OUT;
      return 0;
    }
  if (!strncmp (temp_buffer, "queue_length=", 13))
    {
      return (atoi(temp_buffer+13));
    }
  return (0);
}


CMS_STATUS STCPMEM::clear ()
{
  last_request_was_write=false;
  // FIXME: unimplemented function.
  return (status);
}

int
STCPMEM::login (
#ifdef ENABLE_RCS_CRYPT2
		const char *name, 
		const char *passwd
#else
		__unused_parameter__ const char *, 
		__unused_parameter__ const char *
#endif
		)
{
  last_request_was_write=false;
#ifdef ENABLE_RCS_CRYPT2
  status = CMS_STATUS_NOT_SET;
  double start_time = etime ();
  // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
  while (read_request_issued
	 && (status == CMS_STATUS_NOT_SET || status == CMS_TIMED_OUT
	     || status == CMS_READ_OLD) && etime() - start_time < 60.0)
    {
      peek ();
    }
  read_request_issued = 0;
  char request_string[32];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "get_keys(%d):%s", (int) buffer_number, name);

  if (sendline (socket_fd, request_string, 0, 30.0) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (recvline (socket_fd, temp_buffer, temp_buffer_size, 0, 30.0, NULL) < 0)
    {
      rcs_print_error ("login: timed out.\n");
      return (0);
    }
  if (strncmp (temp_buffer, "keys:", 5))
    {
      rcs_print_error
	("Bad reply from server on request for login keys: %s\n",
	 temp_buffer);
      return 0;
    }
  char key1[8];
  memset (key1, 0, 8);
  int i = 0;
  for (i = 0; i < 8 && temp_buffer[i + 5] != ':'; i++)
    {
      key1[i] = temp_buffer[i + 5];
    }
  int key2_offset = i + 6;
  char key2[8];
  memset (key2, 0, 8);
  for (i = 0; i < 8 && temp_buffer[i + key2_offset] != ':'; i++)
    {
      key2[i] = temp_buffer[i + key2_offset];
    }
  char passwd_pass1[16];
  char *crypt1_ret;
  crypt1_ret = rcs_crypt (passwd, key1);
  if (NULL == crypt1_ret)
    {
      rcs_print_error ("STCPMEM:login -- crypt failed.\n");
      return 0;
    }
  strncpy (passwd_pass1, crypt1_ret, 16);
  char passwd_pass2[16];
  char *crypt2_ret;
  crypt2_ret = rcs_crypt (passwd_pass1, key2);
  if (NULL == crypt2_ret)
    {
      rcs_print_error ("STCPMEM:login -- crypt failed.\n");
      return 0;
    }
  strncpy (passwd_pass2, crypt2_ret, 16);
  SNPRINTF_FUNC ( SNPRINTF_ARGS(request_string,sizeof(request_string)),
		  "login(%d):%s:%s", 
		  (int) buffer_number, name,
		  passwd_pass2);
  if (sendline (socket_fd, request_string, 0, 30.0) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (recvline (socket_fd, temp_buffer, temp_buffer_size, 0, 30.0, NULL) < 0)
    {
      rcs_print_error ("login: timed out.\n");
      return (0);
    }
  if (strncmp (temp_buffer, "login", 5))
    {
      rcs_print_error ("Bad reply from server on request for login: %s\n",
		       temp_buffer);
      return 0;
    }
  return !strcmp (temp_buffer, "login succeeded");
#else
  rcs_print_error("RCS library compiled without crypt so login is impossible.\n");
  return 0;
#endif
}


  /* This is not implemented, it is only added here so we can have a more
     informative error message, use TCP rather than STCP if you need blocking. */
CMS_STATUS 
STCPMEM::blocking_read (
			__unused_parameter__ double)
{
  last_request_was_write=false;
  rcs_print_error("STCP does not support a blocking read. Use TCP= instead.\n");
  return(status = CMS_NO_IMPLEMENTATION_ERROR);
}

