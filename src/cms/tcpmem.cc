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

#define CMS_NETWORK_SOURCE
#define CMS_DERIVED_CLASS 1

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#else
#include "tcpmem_no_config.h"
#endif
// HAVE_CONFIG_H

#include "rem_msg.hh"		/* REMOTE_CMS_READ_REQUEST_TYPE, etc. */
#include "linklist.hh"		// RCS_LINKED_LIST
#include "rcs_prnt.hh"

#ifdef ENABLE_RCS_DIAG
#include "cmsdiag.hh"
#endif

#define DEFAULT_MAX_CONSECUTIVE_TIMEOUTS (-1)

#include "timer.hh"		/* esleep() */
#include "tcpmem.hh"
#include "recvn.h"		/* recvn() */
#include "sendn.h"		/* sendn() */
#include "tcp_opts.hh"		/* SET_TCP_NODELAY */
#include "sokintrf.h"		/* dl_socket(), dl_modified_gethostbyname() */

#ifdef ENABLE_RCS_CRYPT2
#include "crypt2.hh"
#endif

#include "ntohhton.hh"

int tcpmem_sigpipe_count = 0;
int last_sig = 0;


void
tcpmem_sigpipe_handler (int sig)
{
  last_sig = sig;
  tcpmem_sigpipe_count++;
}


TCPMEM::TCPMEM (const char *_bufline, const char *_procline):
  CMS (_bufline, _procline,-1),
  recvd_bytes(0),serial_number(0),returned_serial_number(0),
  subscription_type(CMS_NO_SUBSCRIPTION),
  poll_interval_millis(0),
  server_socket_address_ptr(0),
  socket_fd(0),timedout_request(NO_REMOTE_CMS_REQUEST),last_request_type(NO_REMOTE_CMS_REQUEST),
  bytes_to_throw_away(0),polling(0),write_socket_fd(0),read_socket_fd(0),write_serial_number(0),
  read_serial_number(0),timedout_request_status(CMS_STATUS_NOT_SET),timedout_request_writeid(0),
  max_consecutive_timeouts(0),waiting_for_message(0),waiting_message_size(0),
  waiting_message_id(0),autoreconnect(0),reconnect_needed(0),sigpipe_count(0),
  old_handler(0),subscription_count(0),sockerrno(0),sockerrstr(0),interrupting_operation_int(0),
  reconnect_count(0),
  orig_pid(0),
  connect_pid(0),
  cli_addr(0)
{
  if (load_socket_interface () < 0)
    {
      cms_print_error ("Can't load socket interface.\n");
      status = CMS_LIBRARY_UNAVAILABLE_ERROR;
    }
  max_consecutive_timeouts = DEFAULT_MAX_CONSECUTIVE_TIMEOUTS;
  char *max_consecutive_timeouts_string;
  max_consecutive_timeouts_string = strstr (ProcessLine, "max_timeouts=");
  polling = (NULL != strstr (proclineupper, "POLL"));
  socket_fd = 0;
  reconnect_needed = 0;
  autoreconnect = 1;
  old_handler = (void (*)(int)) SIG_ERR;
  sigpipe_count = 0;
  subscription_count = 0;
  read_serial_number = 0;
  write_serial_number = 0;
  read_socket_fd = 0;
  write_socket_fd = 0;
  interrupting_operation_int=0;
  timedout_request = NO_REMOTE_CMS_REQUEST;
  last_request_type = NO_REMOTE_CMS_REQUEST;
  reconnect_count=0;
  orig_pid=0;
  connect_pid=0;
  
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  orig_pid = GetCurrentProcessId ();
#else
#ifdef VXWORKS
  orig_pid = taskIdSelf ();
#else
  orig_pid = getpid ();
#endif
#endif

  if (NULL != max_consecutive_timeouts_string)
    {
      max_consecutive_timeouts_string += strlen ("max_timeouts=");
      if (!strncmp (max_consecutive_timeouts_string, "INF", 3))
	{
	  max_consecutive_timeouts = -1;
	}
      else
	{
	  max_consecutive_timeouts =
	    strtol (max_consecutive_timeouts_string, (char **) NULL, 0);
	}
    }

  char *sub_info_string = NULL;
  poll_interval_millis = 30000;
  subscription_type = CMS_NO_SUBSCRIPTION;
  sub_info_string = strstr (ProcessLine, "sub=");
  if (NULL != sub_info_string)
    {
      if (!strncmp (sub_info_string + 4, "none", 4))
	{
	  subscription_type = CMS_NO_SUBSCRIPTION;
	}
      else if (!strncmp (sub_info_string + 4, "var", 3))
	{
	  subscription_type = CMS_VARIABLE_SUBSCRIPTION;
	}
      else
	{
	  poll_interval_millis =
	    ((int) (atof (sub_info_string + 4) * 1000.0));
	  subscription_type = CMS_POLLED_SUBSCRIPTION;
	}
    }
  if (NULL != strstr (ProcessLine, "noreconnect"))
    {
      autoreconnect = 0;
    }


  /* Set up the socket address stucture. */
  server_socket_address_ptr = dl_create_sa(BufferHost,tcp_port_number,use_ipv6);

  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Using server on %s with IP address %s and port %d.\n",
		   BufferHost,
		   dl_sa_get_host(server_socket_address_ptr),
		   tcp_port_number);


  reconnect ();

  if (status >= 0 && status != CMS_TIMED_OUT &&
      !no_verify_buf &&
      (min_compatible_version > 2.58 || min_compatible_version < 1e-6))
    {
      verify_bufname ();
      if (status < 0)
	{
	  cms_print_error ("TCPMEM: verify_bufname() failed.\n");
	}
    }

#ifdef ENABLE_RCS_DIAG
  if (status >= 0 && enable_diagnostics &&
      (min_compatible_version > 3.71 || min_compatible_version < 1e-6))
    {
      send_diag_info ();
    }
#endif

}


void
TCPMEM::send_diag_info ()
{
#ifdef ENABLE_RCS_DIAG
  if (polling)
    {
      return;
    }
  if (NULL == dpi)
    {
      return;
    }
  disable_sigpipe ();

  set_socket_fds (read_socket_fd);
  memset (diag_info_buf, 0, 88);
  hton_uint32_array_set(diag_info_buf,0,(unsigned long) serial_number);
  hton_uint32_array_set(diag_info_buf,1,(unsigned long) REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE);
  hton_uint32_array_set(diag_info_buf,2,(unsigned long) buffer_number);  
  last_request_type = REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE;
  strncpy (diag_info_buf + 20, dpi->name, 16);
  strncpy (diag_info_buf + 36, dpi->host_sysinfo, 32);
  hton_uint32_array_set(diag_info_buf,17,(unsigned long) dpi->pid);
  hton_uint32_array_set(diag_info_buf,18,(unsigned long) connection_number);
  memcpy (diag_info_buf + 76, &(dpi->rcslib_ver), 8);
    *((unsigned int *)(diag_info_buf + 84)) = 0x11223344;
  if (sendn (socket_fd, diag_info_buf, 88, 0, timeout) < 0)
    {
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      status = CMS_MISC_ERROR;
      return;
    }
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long) last_request_type,
		   buffer_number);
  reenable_sigpipe ();
#endif
}

void
TCPMEM::verify_bufname ()
{
  if (polling)
    {
      return;
    }
  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed && ignore_connect_err)
    {
      return;
    }
  disable_sigpipe ();

  set_socket_fds (read_socket_fd);

  last_request_type = REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  if (sendn (socket_fd, temp_buffer, 20, 0, timeout) < 0)
    {
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      status = CMS_MISC_ERROR;
      return;
    }
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long)last_request_type, buffer_number);
  if (recvn (socket_fd, temp_buffer, 40, 0, timeout, &recvd_bytes,1) < 0)
    {
      if (recvn_timedout)
	{
	  bytes_to_throw_away = 40;
	  reenable_sigpipe ();
	  status = CMS_TIMED_OUT;
	  return;
	}
      else
	{
	  reenable_sigpipe ();
	  status = CMS_MISC_ERROR;
	  return;
	}
    }

  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      status = CMS_MISC_ERROR;
      reenable_sigpipe ();
      return;
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(temp_buffer,1);
  if (status < 0)
    {
      reenable_sigpipe ();
      return;
    }
  if (strncmp (temp_buffer + 8, BufferName, 31))
    {
      cms_print_error
	("TCPMEM: The buffer (%s) is registered on TCP port %d of host %s with buffer number %ld.\n",
	 ((char *) temp_buffer + 8), tcp_port_number, BufferHost, 
	 buffer_number);
      cms_print_error
	("TCPMEM: However, this process (%s) is attempting to connect to the buffer %s at the same location.\n",
	 ProcessName, BufferName);
      status = CMS_RESOURCE_CONFLICT_ERROR;
      reenable_sigpipe ();
      return;
    }
  reenable_sigpipe ();
}


CMS_DIAGNOSTICS_INFO *
TCPMEM::get_diagnostics_info ()
{
#ifdef ENABLE_RCS_DIAG
  if (polling)
    {
      return (NULL);
    }
  disable_sigpipe ();

  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return (NULL);
    }

  set_socket_fds (read_socket_fd);

  last_request_type = REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  if (sendn (socket_fd, temp_buffer, 20, 0, timeout) < 0)
    {
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      status = CMS_MISC_ERROR;
      return (NULL);
    }
  memset (temp_buffer, 0, 0x2000);
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long) last_request_type,
		   buffer_number);
  if (recvn (socket_fd, temp_buffer, 32, 0, -1.0, &recvd_bytes,1) < 0)
    {
      if (recvn_timedout)
	{
	  bytes_to_throw_away = 32;
	}
      return (NULL);
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      status = CMS_MISC_ERROR;
      return (NULL);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(temp_buffer,1);
  if (status < 0)
    {
      return (NULL);
    }
  if (NULL == di)
    {
      di = new CMS_DIAGNOSTICS_INFO ();
      di->dpis = new RCS_LINKED_LIST ();
    }
  else
    {
      di->dpis->delete_members ();
    }
  di->last_writer_dpi = NULL;
  di->last_reader_dpi = NULL;
  di->last_writer = ntoh_uint32_array_get(temp_buffer,2);
  di->last_reader = ntoh_uint32_array_get(temp_buffer,3);
  double server_time;
  memcpy (&server_time, temp_buffer + 16, 8);
  double local_time = etime ();
  double diff_time = local_time - server_time;
  int dpi_count = ntoh_uint32_array_get(temp_buffer,6);
  int dpi_max_size = ntoh_uint32_array_get(temp_buffer,7);
  if (dpi_max_size > 32 && dpi_max_size < 0x2000)
    {
      if (recvn
	  (socket_fd, temp_buffer + 32, dpi_max_size - 32, 0, -1.0,
	   &recvd_bytes,1) < 0)
	{
	  if (recvn_timedout)
	    {
	      bytes_to_throw_away = dpi_max_size - 32;
	      return (NULL);
	    }
	}
      recvd_bytes = 0;
      int dpi_offset = 32;
      CMS_DIAG_PROC_INFO cms_dpi;
      for (int i = 0; i < dpi_count && dpi_offset < dpi_max_size; i++)
	{
	  memset (&cms_dpi, 0, sizeof (CMS_DIAG_PROC_INFO));
	  memcpy (cms_dpi.name, temp_buffer + dpi_offset, 16);
	  dpi_offset += 16;
	  memcpy (cms_dpi.host_sysinfo, temp_buffer + dpi_offset, 32);
	  dpi_offset += 32;
	  cms_dpi.pid = 
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  memcpy (&(cms_dpi.rcslib_ver), temp_buffer + dpi_offset, 8);
	  dpi_offset += 8;
	  cms_dpi.access_type =
	    (CMS_INTERNAL_ACCESS_TYPE)
	     ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  cms_dpi.msg_id = 
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  cms_dpi.msg_size = 
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  cms_dpi.msg_type =
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  cms_dpi.number_of_accesses =
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  cms_dpi.number_of_new_messages =
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  memcpy (&(cms_dpi.bytes_moved), temp_buffer + dpi_offset, 8);
	  dpi_offset += 8;
	  memcpy (&(cms_dpi.bytes_moved_across_socket),
		  temp_buffer + dpi_offset, 8);
	  dpi_offset += 8;
	  memcpy (&(cms_dpi.last_access_time), temp_buffer + dpi_offset, 8);
	  if (cmsdiag_timebias_set)
	    {
	      cms_dpi.last_access_time += diff_time - cmsdiag_timebias;
	    }
	  dpi_offset += 8;
	  memcpy (&(cms_dpi.first_access_time), temp_buffer + dpi_offset, 8);
	  if (cmsdiag_timebias_set)
	    {
	      cms_dpi.first_access_time += diff_time - cmsdiag_timebias;
	    }
	  dpi_offset += 8;
	  memcpy (&(cms_dpi.min_difference), temp_buffer + dpi_offset, 8);
	  dpi_offset += 8;
	  memcpy (&(cms_dpi.max_difference), temp_buffer + dpi_offset, 8);
	  dpi_offset += 8;
	  di->dpis->store_at_tail (&cms_dpi, sizeof (CMS_DIAG_PROC_INFO), 1);
	  int is_last_writer =
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  if (is_last_writer)
	    {
	      di->last_writer_dpi =
		(CMS_DIAG_PROC_INFO *) di->dpis->get_tail ();
	    }
	  int is_last_reader =
	    ntoh_uint32_array_get(((char*)temp_buffer)+dpi_offset,0);
	  dpi_offset += 4;
	  if (is_last_reader)
	    {
	      di->last_reader_dpi =
		(CMS_DIAG_PROC_INFO *) di->dpis->get_tail ();
	    }
	}
    }
  reenable_sigpipe ();
  return di;
#else
  cms_print_error("RCS compiled without DIAG support.\n");
  return 0;

#endif
  // #ifdef ENABLE_RCS_DIAG
}

CMS_STATUS
TCPMEM::setup_subscription(double _subscription_period) {
  subscription_type = CMS_POLLED_SUBSCRIPTION;
  poll_interval_millis = (int) (_subscription_period*1000.0);
  last_request_type = REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) subscription_type);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) poll_interval_millis);
  if (sendn (socket_fd, temp_buffer, 20, 0, 30) < 0) {
    cms_print_error ("Can`t setup subscription.\n");
    subscription_type = CMS_NO_SUBSCRIPTION;
    return(status=CMS_MISC_ERROR);
  }
  else
    {
      serial_number++;
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		       socket_fd, serial_number,
		       (long)last_request_type,
		       buffer_number);
      memset (temp_buffer, 0, 20);
      recvd_bytes = 0;
      if (recvn (socket_fd, temp_buffer, 8, 0, 30, &recvd_bytes,1) < 0)
	{
	  cms_print_error ("Can`t setup subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	}
      if (!ntoh_uint32_array_get(temp_buffer,1))
	{
	  cms_print_error ("Can`t setup subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	}

      bytes_to_throw_away = 8 - recvd_bytes;
      if (bytes_to_throw_away < 0 || bytes_to_throw_away > 8)
	{
	  bytes_to_throw_away = 0;
	}
      recvd_bytes = 0;
    }
  memset (temp_buffer, 0, 20);
  polling=1;
  setup_polling();
  return (status=CMS_SETUP_SUBSCRIPTION_OK);
}

void
TCPMEM::reconnect ()
{
  if (socket_fd > 0)
    {
      disconnect ();
    }
  subscription_count = 0;
  timedout_request = NO_REMOTE_CMS_REQUEST;
  bytes_to_throw_away = 0;
  recvd_bytes = 0;
  socket_fd = 0;
  waiting_for_message = 0;
  waiting_message_size = 0;
  waiting_message_id = 0;
  serial_number = 0;
  reconnect_count++;

#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  connect_pid = GetCurrentProcessId ();
#else
#ifdef VXWORKS
  connect_pid = taskIdSelf ();
#else
  connect_pid = getpid ();
#endif
#endif

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Creating socket . . .\n");

  socket_fd = (int) dl_tcp_socket (use_ipv6);
  if (socket_fd < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      cms_print_error ("TCPMEM: Error from socket() (errno = %d:%s)\n",
		       sockerrno, sockerrstr);
      status = CMS_CREATE_ERROR;
      return;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Setting socket options . . . \n");
  if (set_tcp_socket_options (socket_fd) < 0)
    {
      return;
    }
  if(0 == cli_addr)
    {
      char *bind_to_host = getenv("NML_BINDTO_HOST");
      cli_addr = dl_create_sa((bind_proc_host?ProcessHost:bind_to_host),
			      0,use_ipv6);
    }

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Binding . . . \n");
  if (dl_bind (socket_fd, dl_sa_addr(cli_addr), dl_sa_len(cli_addr)) <
      0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      cms_print_error ("TCPMEM: bind error %d = %s\n", 
		       sockerrno,sockerrstr);
      status = CMS_CREATE_ERROR;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "TCPMEM: this=%p,socket_fd=%d,cli_addr=%s:%d\n",
		   (void*)this,socket_fd,
		   dl_sa_get_host(cli_addr),
		   dl_sa_get_port(cli_addr));
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Connecting . . .\n");
  int connect_ret = -15;
  int timedout;
  interrupting_operation_int = (int) interrupting_operation;

  connect_ret = dl_connect_in_with_timeout(socket_fd, /* socket */
					   server_socket_address_ptr, /* svr address/port */
					   connect_timeout, /* timeout */
					   (!ignore_connect_err), /* print errors */
					   (reconnect_count == 1) &&
					   !stop_when_connection_refused, 
					   /* reconnect after connection refused */
					   &sockerrno, /* errcode ptr */
					   &timedout, /* timedout ptr */
					   &interrupting_operation_int, /* interrupt_op_ptr */
					   sockerrbuf, /* buffer for error info */
					   sizeof(sockerrbuf)); /* size of sockerr buf */
  if(connect_ret != 0)
    {
      if(!ignore_connect_err)
	{
	  if(interrupting_operation)
	    {
	      status=CMS_INTERRUPTED_OPERATION;
	    }
	  else if(timedout)
	    {
	      status = CMS_TIMED_OUT;
	    }
	  else
	    {
	      status = CMS_NO_SERVER_ERROR;
	    }
	  cms_print_error("TCPMEM connect to BufferHost %s failed.\n",BufferHost);

	}
      reconnect_needed=1;
      disconnect();
      reconnect_needed=1;
      return;
    }
  read_socket_fd = socket_fd;

  memset (temp_buffer, 0, 32);
  if (total_subdivisions > 1)
    {
      subscription_type = CMS_NO_SUBSCRIPTION;
    }

  if (subscription_type != CMS_NO_SUBSCRIPTION)
    {
      if(!no_verify_buf)
	{
	  verify_bufname ();
	  if (status < 0)
	    {
	      cms_print_error ("TCPMEM: verify_bufname() failed\n");
	      return;
	    }
	}
      setup_subscription(poll_interval_millis*0.001);
    }
  if (subscription_type != CMS_NO_SUBSCRIPTION)
    {
      polling = 1;
    }


  if (polling)
    {
      setup_polling();
    }
  else
    {
      write_socket_fd = read_socket_fd;
    }
  reconnect_needed = 0;
  fatal_error_occurred = false;
}

extern bool nml_cleanup_started;

void
TCPMEM::setup_polling()
{
  struct timeval timeval_timeout;
  int socket_ret;
  double start_time, current_time;
  fd_set fds;

  make_tcp_socket_nonblocking (socket_fd);
  write_socket_fd = (int) dl_tcp_socket (use_ipv6);
  if (write_socket_fd < 0)
    {
      sockerrno = dl_get_last_socket_error_int( write_socket_fd );
      sockerrstr = dl_get_last_socket_error_string(write_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      cms_print_error ("TCPMEM: Error from socket() (errno = %d:%s)\n",
		       sockerrno, sockerrstr );
      status = CMS_CREATE_ERROR;
      return;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Setting socket options . . . \n");
  if (set_tcp_socket_options (write_socket_fd) < 0)
    {
      return;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Binding . . . \n");
  if (dl_bind
      (write_socket_fd, dl_sa_addr(cli_addr),
       dl_sa_len(cli_addr)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      cms_print_error ("TCPMEM: bind error %d -- %s\n", 
		       sockerrno, sockerrstr);
      status = CMS_CREATE_ERROR;
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Connecting . . .\n");
  if (dl_connect
      (write_socket_fd, dl_sa_addr(server_socket_address_ptr),
       dl_sa_len(server_socket_address_ptr)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      if ( dl_socket_error_was_would_block( socket_fd , sockerrno) )
	{
	  FD_ZERO (&fds);
	  RCS_FD_SET (write_socket_fd, &fds);
	  start_time = etime ();
#if MS_WINDOWS_API
	  timeval_timeout.tv_sec = 0;
	  timeval_timeout.tv_usec = 0;
#else
	  timeval_timeout.tv_sec = (long) timeout;
	  // fixed Jan 8, 2006  tv_sec replaced with tv_usec - thanks to email subject tcpmcm.cc reconnect() from  Paul <bdi-emc@ntlworld.com>
	  timeval_timeout.tv_usec = (long) (fmod (timeout, 1.0) * 1e6);
#endif
	  while (!(socket_ret = dl_select (write_socket_fd + 1,
					   (fd_set *) NULL,
					   &fds, (fd_set *) NULL, &timeval_timeout)))
	    {
	      if(interrupting_operation)
		{
		  status=CMS_INTERRUPTED_OPERATION;
		  return;
		}
	      RCS_FD_SET (write_socket_fd, &fds);
	      esleep (0.001);
	      current_time = etime ();
	      double timeleft = start_time + timeout - current_time;
	      if (timeleft <= 0.0 && timeout >= 0.0)
		{
		  cms_print_timeout_error
		    ("TCPMEM: Timed out waiting for connection.\n");
		  status = CMS_NO_SERVER_ERROR;
		  return;
		}
#if MS_WINDOWS_API
	      timeval_timeout.tv_sec = 0;
	      timeval_timeout.tv_usec = 0;
#else
	      timeval_timeout.tv_sec = (long) timeleft;
	      // fixed Jan 8, 2006  tv_sec replaced with tv_usec - thanks to email subject tcpmcm.cc reconnect() from  Paul <bdi-emc@ntlworld.com>
	      timeval_timeout.tv_usec = (long) (fmod (timeleft, 1.0) * 1e6);
#endif
	    }
	  if (dl_select_ret_is_error(socket_ret))
	    {
	      sockerrno = dl_get_last_socket_error_int( socket_fd );
	      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      cms_print_error ("select error: %d -- %s\n", 
			       sockerrno, sockerrstr);
	      cms_print_error ("TCPMEM: Couldn't connect.\n");
	      status = CMS_NO_SERVER_ERROR;
	      return;
	    }
	}
      else
	{
	  sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  cms_print_error ("connect error: %d -- %s\n", 
			   sockerrno,sockerrstr);
	  cms_print_error
	    ("TCPMEM: Error trying to connect to TCP port %d of host %s.\n",
	     dl_sa_get_port(server_socket_address_ptr), BufferHost);
	}
    }
  timeout = 0;
  read_timeout = 0;
}

TCPMEM::~TCPMEM ()
{
  long current_pid;

#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  current_pid = GetCurrentProcessId ();
#else
#ifdef VXWORKS
  current_pid = taskIdSelf ();
#else
  current_pid = getpid ();
#endif
#endif
  
  bool pid_unchanged = (orig_pid > 0 && current_pid > 0 && connect_pid > 0
			&& orig_pid == current_pid && connect_pid == current_pid);

  rcs_print_debug(PRINT_MISC,"TCPMEM::~TCPMEM() called.  buffer_number=%ld,BufferName=%s,serial_number=%ld,current_pid=%ld,orig_pid=%ld,socket_fd=%d\n",buffer_number,BufferName,serial_number,current_pid,orig_pid,socket_fd);
  if(!confirm_write && !delete_totally &&
     (min_compatible_version > 2.58 || min_compatible_version < 1e-6)
     && last_request_type == REMOTE_CMS_WRITE_REQUEST_TYPE
     && !interrupting_operation 
     && !nml_cleanup_started
     && pid_unchanged)
    {
      if(timeout > 0.5 || timeout < 0)
	{
	  timeout = 0.5;
	}
      check_if_read();
    }
  interrupting_operation=true;
  disconnect ();

  if(cli_addr)
    {
      dl_free_sa(cli_addr);
      cli_addr=0;
    }
  write_socket_fd = -1;
  read_socket_fd = -1;
  socket_fd = -1;
  autoreconnect=false;
  if(server_socket_address_ptr)
    {
      dl_free_sa(server_socket_address_ptr);
      server_socket_address_ptr=0;
    }
  unload_socket_interface ();
  rcs_print_debug(PRINT_MISC,"TCPMEM::~TCPMEM() returning.  buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);
}

CMS_STATUS
TCPMEM::cancel_subscription()
{
  last_request_type = REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  if (sendn (socket_fd, temp_buffer, 12, 0, 30) < 0) {
    cms_print_error ("Can`t cancel subscription.\n");
    subscription_type = CMS_NO_SUBSCRIPTION;
    return(status=CMS_MISC_ERROR);
  }
  else
    {
      serial_number++;
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		       socket_fd, serial_number,
		       (long)last_request_type,
		       buffer_number);
      memset (temp_buffer, 0, 20);
      recvd_bytes = 0;
      if (recvn (socket_fd, temp_buffer, 8, 0, 30, &recvd_bytes,1) < 0)
	{
	  cms_print_error ("Can`t cancel subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	}
      if (!ntoh_uint32_array_get(temp_buffer,1))
	{
	  cms_print_error ("Can`t cancel subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	}

      bytes_to_throw_away = 8 - recvd_bytes;
      if (bytes_to_throw_away < 0 || bytes_to_throw_away > 8)
	{
	  bytes_to_throw_away = 0;
	}
      recvd_bytes = 0;
    }
  memset (temp_buffer, 0, 20);
  subscription_type=CMS_NO_SUBSCRIPTION;
  polling=0;
  return (status=CMS_SETUP_SUBSCRIPTION_OK);
}

void
TCPMEM::disconnect ()
{
  if(subscription_type != CMS_NO_SUBSCRIPTION)
    {
      cancel_subscription();
    }

  if (write_socket_fd > 0 && write_socket_fd != socket_fd)
    {
      if (status != CMS_CONFIG_ERROR && status != CMS_CREATE_ERROR)
	{
	  if (delete_totally)
	    {
	      last_request_type = REMOTE_CMS_CLEAN_REQUEST_TYPE;
	      hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	      hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
	      hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
	      sendn (write_socket_fd, temp_buffer, 20, 0, -1);
	    }
	}
      dl_closesocket (write_socket_fd);
    }

  if (socket_fd > 0)
    {
      if (status != CMS_CONFIG_ERROR && status != CMS_CREATE_ERROR)
	{
	  if (delete_totally)
	    {
	      last_request_type = REMOTE_CMS_CLEAN_REQUEST_TYPE;
	      hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	      hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
	      hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
	      sendn (socket_fd, temp_buffer, 20, 0, -1);
	    }
	}
      dl_closesocket (socket_fd);
     }
  socket_fd = 0;
  write_socket_fd = 0;
  read_socket_fd =0;
  subscription_count = 0;
  timedout_request = NO_REMOTE_CMS_REQUEST;
  bytes_to_throw_away = 0;
  recvd_bytes = 0;
  socket_fd = 0;
  waiting_for_message = 0;
  waiting_message_size = 0;
  waiting_message_id = 0;
  serial_number = 0;
}


CMS_STATUS TCPMEM::handle_old_replies ()
{
  long
    message_size;

  if(interrupting_operation)
    {
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }

  timedout_request_writeid = 0;
  status = CMS_STATUS_NOT_SET;
  switch (timedout_request)
    {
    case REMOTE_CMS_READ_REQUEST_TYPE:
      if (!waiting_for_message)
	{
	  if (recvn (socket_fd, temp_buffer, 20, 0, timeout, &recvd_bytes,!polling) <
	      0)
	    {
	      if (recvn_timedout)
		{
		  if (polling)
		    {
		      return status;
		    }
		  else
		    {
		      consecutive_timeouts++;
		      if (consecutive_timeouts > max_consecutive_timeouts &&
			  max_consecutive_timeouts > 0)
			{
			  cms_print_error
			    ("CMS: %d consecutive timeouts have occurred. -- Stop trying.\n",
			     consecutive_timeouts);
			  fatal_error_occurred = true;
			  reconnect_needed = 1;
			}
		      return (status = CMS_TIMED_OUT);
		    }
		}
	      else
		{
		  recvd_bytes = 0;
		  fatal_error_occurred = true;
		  return (status = CMS_MISC_ERROR);
		}
	    }
	  recvd_bytes = 0;
	  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
	  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
			   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
			   socket_fd, returned_serial_number, buffer_number);
	  if (returned_serial_number != serial_number)
	    {
	      cms_print_error
		("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
		 returned_serial_number, serial_number);
	      if (subscription_type == CMS_NO_SUBSCRIPTION)
		{
		  fatal_error_occurred = true;
		  reconnect_needed = 1;
		  return (status = CMS_MISC_ERROR);
		}
	      else
		{
		  serial_number = returned_serial_number;
		}
	    }
	  timedout_request_status = (CMS_STATUS) ntoh_uint32_array_get(temp_buffer,1);
	  message_size =  ntoh_uint32_array_get(temp_buffer,2);
	  timedout_request_writeid = ntoh_uint32_array_get(temp_buffer,3);
	  header.was_read = ntoh_uint32_array_get(temp_buffer,4);
	  if (message_size > max_encoded_message_size)
	    {
	      cms_print_error ("Recieved message is too big. (%ld > %ld)\n",
			       message_size, max_encoded_message_size);
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      return (status = CMS_INSUFFICIENT_SPACE_ERROR);
	    }
	}
      else
	{
	  message_size = waiting_message_size;
	}
      if (message_size > 0)
	{
	  if (recvn
	      (socket_fd, encoded_data, message_size, 0, timeout,
	       &recvd_bytes,!polling) < 0)
	    {
	      if (recvn_timedout)
		{
		  if (!waiting_for_message)
		    {
		      waiting_message_id = timedout_request_writeid;
		      waiting_message_size = message_size;
		    }
		  waiting_for_message = 1;
		  timedout_request_writeid = 0;
		  if (polling)
		    {
		      return status;
		    }
		  else
		    {
		      consecutive_timeouts++;
		      if (consecutive_timeouts > max_consecutive_timeouts &&
			  max_consecutive_timeouts > 0)
			{
			  cms_print_error
			    ("CMS: %d consecutive timeouts have occurred. -- Stop trying.\n",
			     consecutive_timeouts);
			  fatal_error_occurred = true;
			  reconnect_needed = 1;
			}
		      return (status = CMS_TIMED_OUT);
		    }
		}
	      else
		{
		  recvd_bytes = 0;
		  fatal_error_occurred = true;
		  reconnect_needed = 1;
		  return (status = CMS_MISC_ERROR);
		}
	    }
	  recvd_bytes = 0;
	  if (waiting_for_message)
	    {
	      timedout_request_writeid = waiting_message_id;
	    }
	}
      break;

    case REMOTE_CMS_WRITE_REQUEST_TYPE:
    case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
    case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE:
    case REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE:
    case REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE:
    case REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE:
    case REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE:
    case REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE:
      if (timedout_request == REMOTE_CMS_WRITE_REQUEST_TYPE &&
	  (min_compatible_version > 2.58 || min_compatible_version < 1e-6 ||
	   confirm_write))
	{
	  break;
	}
      if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      consecutive_timeouts++;
	      if (consecutive_timeouts > max_consecutive_timeouts &&
		  max_consecutive_timeouts > 0)
		{
		  cms_print_error
		    ("CMS: %d consecutive timeouts have occurred. -- Stop trying.\n",
		     consecutive_timeouts);
		  reconnect_needed = 1;
		  fatal_error_occurred = true;
		}
	      reconnect_needed = 1;
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      return (status = CMS_MISC_ERROR);
	    }
	}
      recvd_bytes = 0;
      returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		       socket_fd, returned_serial_number, buffer_number);
      if (returned_serial_number != serial_number)
	{
	  cms_print_error
	    ("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	     returned_serial_number, serial_number);
	  reconnect_needed = 1;
	  if (subscription_type == CMS_NO_SUBSCRIPTION)
	    {
	      return (status = CMS_MISC_ERROR);
	    }
	}
      break;

    case REMOTE_CMS_CLEAR_REQUEST_TYPE:
      if (recvn (socket_fd, temp_buffer, 4, 0, timeout, &recvd_bytes,1) < 0)
	{
	  if (recvn_timedout)
	    {
	      consecutive_timeouts++;
	      reconnect_needed = 1;
	      if (consecutive_timeouts > max_consecutive_timeouts &&
		  max_consecutive_timeouts > 0)
		{
		  cms_print_error
		    ("CMS: %d consecutive timeouts have occurred. -- Stop trying.\n",
		     consecutive_timeouts);
		  fatal_error_occurred = true;
		}
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      reconnect_needed = 1;
	      fatal_error_occurred = 1;
	      return (status = CMS_MISC_ERROR);
	    }
	}
      recvd_bytes = 0;
      returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		       socket_fd, returned_serial_number, buffer_number);
      if (returned_serial_number != serial_number)
	{
	  cms_print_error
	    ("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	     returned_serial_number, serial_number);
	  reconnect_needed = 1;
	  if (subscription_type == CMS_NO_SUBSCRIPTION)
	    {
	      return (status = CMS_MISC_ERROR);
	    }
	}
      break;



    case NO_REMOTE_CMS_REQUEST:
    default:
      break;
    }
  if (bytes_to_throw_away > 0)
    {
      if (recvn
	  (socket_fd, encoded_data, bytes_to_throw_away, 0, timeout,
	   &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      consecutive_timeouts++;
	      if (consecutive_timeouts > max_consecutive_timeouts &&
		  max_consecutive_timeouts > 0)
		{
		  cms_print_timeout_error
		    ("CMS: %d consecutive timeouts have occurred. -- Stop trying.\n",
		     consecutive_timeouts);
		  fatal_error_occurred = true;
		  reconnect_needed = 1;
		}
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      recvd_bytes = 0;
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      return (status = CMS_MISC_ERROR);
	    }
	}
      recvd_bytes = 0;
    }
  bytes_to_throw_away = 0;
  timedout_request = NO_REMOTE_CMS_REQUEST;
  consecutive_timeouts = 0;
  waiting_for_message = 0;
  waiting_message_size = 0;
  waiting_message_id = 0;
  recvd_bytes = 0;
  return status;
}

CMS_STATUS TCPMEM::read ()
{
  long
    message_size,
    id;
  REMOTE_CMS_REQUEST_TYPE
    last_timedout_request;

  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      cms_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      if(ignore_connect_err)
	{
	  return(status = CMS_READ_OLD);
	}
      return (status = CMS_MISC_ERROR);
    }
  disable_sigpipe ();

  if (subscription_type != CMS_NO_SUBSCRIPTION)
    {
      set_socket_fds (read_socket_fd);
      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
      if (subscription_count < 1)
	{
	  serial_number++;
	}
      handle_old_replies ();
      check_id (timedout_request_writeid);
      if (status == CMS_READ_OK)
	{
	  serial_number++;
	}
      subscription_count++;
      reenable_sigpipe ();
      return status;
    }

  if (timedout_request == NO_REMOTE_CMS_REQUEST)
    {
      set_socket_fds (read_socket_fd);
    }
  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      reenable_sigpipe ();
      return (status);
    }
  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      fatal_error_occurred = true;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  last_timedout_request = timedout_request;
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return status;
    }
  if (polling && last_timedout_request == REMOTE_CMS_READ_REQUEST_TYPE)
    {
      check_id (timedout_request_writeid);
      reenable_sigpipe ();
      return status;
    }
  set_socket_fds (read_socket_fd);

  last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) CMS_READ_ACCESS);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) in_buffer_id);

  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,5,(unsigned long) current_subdivision);
      send_header_size = 24;
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, read_timeout) < 0)
    {
      cms_print_error ("TCPMEM: Can't send READ request to server.\n");
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long) last_request_type,
		   buffer_number);

  if (recvn (socket_fd, temp_buffer, 20, 0, read_timeout, &recvd_bytes,!polling) < 20)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	  if (polling)
	    {
	      return (status = CMS_READ_OLD);
	    }
	  else
	    {
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	}
      else
	{
	  recvd_bytes = 0;
	  reconnect_needed = 1;
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);

  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      if (subscription_type == CMS_NO_SUBSCRIPTION)
	{
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  message_size = ntoh_uint32_array_get(temp_buffer,2);
  id = ntoh_uint32_array_get(temp_buffer,3);
  header.was_read = ntoh_uint32_array_get(temp_buffer,4);

  if (message_size > max_encoded_message_size)
    {
      cms_print_error ("Recieved message is too big. (%ld > %ld)\n",
		       message_size, max_encoded_message_size);
      fatal_error_occurred = true;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (message_size > 0)
    {
      if (recvn
	  (socket_fd, encoded_data, message_size, 0, read_timeout,
	   &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      if (!waiting_for_message)
		{
		  waiting_message_id = id;
		  waiting_message_size = message_size;
		}
	      waiting_for_message = 1;
	      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
	      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	      if (polling)
		{
		  reenable_sigpipe ();
		  return (status = CMS_READ_OLD);
		}
	      else
		{
		  reenable_sigpipe ();
		  return (status = CMS_TIMED_OUT);
		}
	    }
	  else
	    {
	      recvd_bytes = 0;
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  recvd_bytes = 0;
  check_id (id);
  reenable_sigpipe ();
  return (status);
}


CMS_STATUS TCPMEM::blocking_read (double _blocking_timeout)
{
  blocking_timeout = _blocking_timeout;
  long
    message_size,
    id;
  REMOTE_CMS_REQUEST_TYPE
    last_timedout_request;
  long
    timeout_millis;
  int
    orig_print_recvn_timeout_errors =
    print_recvn_timeout_errors;
  print_recvn_timeout_errors = 0;

/* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      cms_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  if (blocking_timeout < 0)
    {
      timeout_millis = -1;
    }
  else
    {
      timeout_millis = (long) (blocking_timeout * 1000.0);
    }

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  disable_sigpipe ();

  if (subscription_type != CMS_NO_SUBSCRIPTION)
    {
      if (blocking_timeout < -1e-6 || blocking_timeout > 1e-6)
	{
	  make_tcp_socket_blocking (read_socket_fd);
	  timeout = blocking_timeout;
	}
      set_socket_fds (read_socket_fd);
      if (subscription_count < 1)
	{
	  serial_number++;
	}
      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
      handle_old_replies ();
      if(interrupting_operation)
	{
	  reenable_sigpipe ();
	  status=CMS_INTERRUPTED_OPERATION;
	  return(status);
	}
      check_id (timedout_request_writeid);
      if (status == CMS_READ_OK)
	{
	  serial_number++;
	}
      subscription_count++;
      reenable_sigpipe ();
      if (blocking_timeout < -1e-6 || blocking_timeout > 1e-6)
	{
	  make_tcp_socket_nonblocking (read_socket_fd);
	  timeout = orig_timeout;
	}
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return status;
    }

  if (timedout_request == NO_REMOTE_CMS_REQUEST)
    {
      set_socket_fds (read_socket_fd);
    }
  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status);
    }
  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      fatal_error_occurred = true;
      reconnect_needed = 1;
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  last_timedout_request = timedout_request;
  if (((int) handle_old_replies ()) < 0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return status;
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }

  if (polling && last_timedout_request == REMOTE_CMS_READ_REQUEST_TYPE)
    {
      check_id (timedout_request_writeid);
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return status;
    }
  set_socket_fds (read_socket_fd);

  last_request_type = REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) CMS_READ_ACCESS);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) in_buffer_id);
  hton_uint32_array_set(temp_buffer,5,(unsigned long) timeout_millis);

  int
    send_header_size =
    24;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,6,(unsigned long) current_subdivision);
      send_header_size = 28;
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, blocking_timeout) <
      0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
      cms_print_error
	("TCPMEM: Can't send BLOCKING_READ request to server.\n");
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, "
		   "request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long) last_request_type,
		   buffer_number);
  if (recvn (socket_fd, temp_buffer, 20, 0, blocking_timeout, &recvd_bytes,!polling) <
      0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
	print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
	if (recvn_timedout)
	  {
	    timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	    if (polling)
	    {
	      return (status = CMS_READ_OLD);
	    }
	  else
	    {
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	}
      else
	{
	  recvd_bytes = 0;
	  reconnect_needed = 1;
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }
  print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);

  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      if (subscription_type == CMS_NO_SUBSCRIPTION)
	{
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  message_size = ntoh_uint32_array_get(temp_buffer,2);
  id = ntoh_uint32_array_get(temp_buffer,3);
  header.was_read = ntoh_uint32_array_get(temp_buffer,4);

  if (message_size > max_encoded_message_size)
    {
      cms_print_error ("Recieved message is too big. (%ld > %ld)\n",
		       message_size, max_encoded_message_size);
      fatal_error_occurred = true;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (message_size > 0 && status == CMS_READ_OK)
    {
      if(interrupting_operation)
	{
	  reenable_sigpipe ();
	  status=CMS_INTERRUPTED_OPERATION;
	  return(status);
	}
      if (recvn
	  (socket_fd, encoded_data, message_size, 0, blocking_timeout,
	   &recvd_bytes,!polling) < 0)
	{
	  if(interrupting_operation)
	    {
	      reenable_sigpipe ();
	      status=CMS_INTERRUPTED_OPERATION;
	      return(status);
	    }
	  if (recvn_timedout)
	    {
	      if (!waiting_for_message)
		{
		  waiting_message_id = id;
		  waiting_message_size = message_size;
		}
	      waiting_for_message = 1;
	      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
	      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	      if (polling)
		{
		  reenable_sigpipe ();
		  return (status = CMS_READ_OLD);
		}
	      else
		{
		  reenable_sigpipe ();
		  return (status = CMS_TIMED_OUT);
		}
	    }
	  else
	    {
	      recvd_bytes = 0;
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }
  recvd_bytes = 0;
  check_id (id);
  reenable_sigpipe ();
  return (status);
}

void
TCPMEM::reenable_sigpipe ()
{
#if !defined(MSDOS) && !defined(MS_WINDOWS_API)
  if (old_handler != ((void (*)(int)) SIG_ERR))
    {
      signal (SIGPIPE, old_handler);
    }
  old_handler = (void (*)(int)) SIG_ERR;
  if (tcpmem_sigpipe_count > sigpipe_count)
    {
      sigpipe_count = tcpmem_sigpipe_count;
      reconnect_needed = 1;
    }
#endif
}

void
TCPMEM::disable_sigpipe ()
{
#if !defined(MSDOS) && !defined(MS_WINDOWS_API)
  if (!autoreconnect)
    {
      return;
    }
  old_handler = signal (SIGPIPE, tcpmem_sigpipe_handler);
  if (tcpmem_sigpipe_count > sigpipe_count)
    {
      sigpipe_count = tcpmem_sigpipe_count;
    }
#endif
}


CMS_STATUS TCPMEM::peek ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      cms_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      if(ignore_connect_err)
	{
	  return(status = CMS_READ_OLD);
	}
      return (status = CMS_MISC_ERROR);
    }
  disable_sigpipe ();

  long
    message_size,
    id;
  REMOTE_CMS_REQUEST_TYPE
    last_timedout_request;
  if (subscription_type != CMS_NO_SUBSCRIPTION)
    {
      set_socket_fds (read_socket_fd);
      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
      if (subscription_count < 1)
	{
	  serial_number++;
	}
      handle_old_replies ();
      check_id (timedout_request_writeid);
      if (status == CMS_READ_OK)
	{
	  serial_number++;
	}
      reenable_sigpipe ();
      subscription_count++;
      return status;
    }

  if (timedout_request == NO_REMOTE_CMS_REQUEST)
    {
      set_socket_fds (read_socket_fd);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      reenable_sigpipe ();
      return (status);
    }
  if (socket_fd <= 0)
    {
      reconnect_needed = 1;
      cms_print_error ("TCPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  last_timedout_request = timedout_request;
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return status;
    }
  if (polling && last_timedout_request == REMOTE_CMS_READ_REQUEST_TYPE)
    {
      check_id (timedout_request_writeid);
      reenable_sigpipe ();
      return status;
    }
  set_socket_fds (read_socket_fd);

  last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) CMS_PEEK_ACCESS);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) in_buffer_id);

  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,5,(unsigned long) current_subdivision);
      send_header_size = 24;
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, read_timeout) < 0)
    {
      cms_print_error ("TCPMEM: Can't send PEEK request to server.\n");
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 20, 0, read_timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	  if (polling)
	    {
	      reenable_sigpipe ();
	      return (status = CMS_READ_OLD);
	    }
	  else
	    {
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  reconnect_needed = 1;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);

  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      if (subscription_type == CMS_NO_SUBSCRIPTION)
	{
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }

  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  message_size = ntoh_uint32_array_get(temp_buffer,2);
  id = ntoh_uint32_array_get(temp_buffer,3);
  header.was_read = ntoh_uint32_array_get(temp_buffer,4);

  if (message_size > max_encoded_message_size)
    {
      reconnect_needed = 1;
      cms_print_error ("Recieved message is too big. (%ld > %ld)\n",
		       message_size, max_encoded_message_size);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (message_size > 0)
    {
      if (recvn
	  (socket_fd, encoded_data, message_size, 0, read_timeout,
	   &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      if (!waiting_for_message)
		{
		  waiting_message_id = id;
		  waiting_message_size = message_size;
		}
	      waiting_for_message = 1;
	      last_request_type = REMOTE_CMS_READ_REQUEST_TYPE;
	      timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	      if (polling)
		{
		  reenable_sigpipe ();
		  return (status = CMS_READ_OLD);
		}
	      else
		{
		  reenable_sigpipe ();
		  return (status = CMS_TIMED_OUT);
		}
	    }
	  else
	    {
	      reconnect_needed = 1;
	      recvd_bytes = 0;
	      fatal_error_occurred = true;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  recvd_bytes = 0;
  check_id (id);
  reenable_sigpipe ();
  return (status);
}

CMS_STATUS TCPMEM::write (void *user_data)
{
  rcs_print_debug(PRINT_MISC,"TCPMEM::write() called. buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);

  if (!write_permission_flag)

    {
      cms_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return status;
    }
  set_socket_fds (write_socket_fd);


  last_request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
  if(priority_set)
    {
      last_request_type = REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE;
    }
  else if(bitwise_op == CMS_BITWISE_AND_OP || bitwise_op == CMS_BITWISE_OR_OP)
    {
      last_request_type = REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE;
    }

  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) CMS_WRITE_ACCESS);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) header.in_buffer_size);

  int
    send_header_size =
    20;
  if(priority_set)
    {
      hton_uint32_array_set(temp_buffer,5,(unsigned long) priority);
      send_header_size =24;
    }
  else if(bitwise_op == CMS_BITWISE_AND_OP || bitwise_op == CMS_BITWISE_OR_OP)
    {
      hton_uint32_array_set(temp_buffer,5,(unsigned long) bitwise_op);
      send_header_size =24;
    }

  if (total_subdivisions > 1)
    {
      if(send_header_size == 20)
	{
	  hton_uint32_array_set(temp_buffer,5,(unsigned long) current_subdivision);
	  send_header_size = 24;
	}
      else 
	{
	  hton_uint32_array_set(temp_buffer,6,(unsigned long) current_subdivision);
	  send_header_size = 28;
	}
    }
  if (header.in_buffer_size < 0x2000 - 32 && header.in_buffer_size > 0)
    {
      memcpy (temp_buffer + send_header_size, user_data,
	      header.in_buffer_size);
      if (sendn
	  (socket_fd, temp_buffer, header.in_buffer_size + send_header_size,
	   0, write_timeout) < 0)
	{
	  cms_print_error
	    ("TCPMEM: Failed to send message of size %ld + header of size %d  to the server.\n",
	     header.in_buffer_size, send_header_size);
	  reconnect_needed = 1;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  else
    {
      if (sendn (socket_fd, temp_buffer, send_header_size, 0, write_timeout) < 0)
	{
	  cms_print_error ("TCPMEM: Failed to send header to server.\n");
	  reconnect_needed = 1;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
      if (header.in_buffer_size > 0)
	{
	  if (sendn (socket_fd, user_data, header.in_buffer_size, 0, timeout)
	      < 0)
	    {
	      reconnect_needed = 1;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  serial_number++;
  if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
      || confirm_write)
    {
      if (recvn (socket_fd, temp_buffer, 12, 0, write_timeout, &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      last_request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
	      timedout_request = REMOTE_CMS_WRITE_REQUEST_TYPE;
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      recvd_bytes = 0;
	      reconnect_needed = 1;
	      fatal_error_occurred = true;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
      recvd_bytes = 0;
      returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		       socket_fd, returned_serial_number, buffer_number);

      if (returned_serial_number != serial_number)
	{
	  cms_print_error
	    ("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	     returned_serial_number, serial_number);
	  reconnect_needed = 1;
	  if (subscription_type == CMS_NO_SUBSCRIPTION)
	    {
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
      status = (CMS_STATUS) ntoh_uint32_array_get(temp_buffer,1);
      header.was_read = ntoh_uint32_array_get(temp_buffer,2);
    }
  else
    {
      header.was_read = 0;
      status = CMS_WRITE_OK;
      returned_serial_number = serial_number;
    }
  reenable_sigpipe ();
  return (status);
}

CMS_STATUS TCPMEM::write_if_read (void *user_data)
{

  if (!write_permission_flag)

    {
      cms_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }
  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }
  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return status;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) CMS_WRITE_IF_READ_ACCESS);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) header.in_buffer_size);

  int
    send_header_size =
    20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,5,(unsigned long) current_subdivision);
      send_header_size = 24;
    }
  if (header.in_buffer_size < 0x2000 - 20 && header.in_buffer_size > 0)
    {
      memcpy (temp_buffer + 20, user_data, header.in_buffer_size);
      if (sendn
	  (socket_fd, temp_buffer, header.in_buffer_size + send_header_size,
	   0, write_timeout) < 0)
	{
	  reconnect_needed = 1;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  else
    {
      if (sendn (socket_fd, temp_buffer, send_header_size, 0, write_timeout) < 0)
	{
	  reconnect_needed = 1;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
      if (header.in_buffer_size > 0)
	{
	  if (sendn (socket_fd, user_data, header.in_buffer_size, 0, write_timeout)
	      < 0)
	    {
	      reconnect_needed = 1;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
    }
  serial_number++;
  if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6) ||
      confirm_write)
    {
      if (recvn (socket_fd, temp_buffer, 12, 0, write_timeout, 
		 &recvd_bytes,!polling) < 0)
	{
	  if (recvn_timedout)
	    {
	      last_request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
	      timedout_request = REMOTE_CMS_WRITE_REQUEST_TYPE;
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	  else
	    {
	      recvd_bytes = 0;
	      fatal_error_occurred = true;
	      reconnect_needed = 1;
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
      recvd_bytes = 0;
      returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		       "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		       socket_fd, returned_serial_number, buffer_number);
      if (returned_serial_number != serial_number)
	{
	  cms_print_error
	    ("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	     returned_serial_number, serial_number);
	  reconnect_needed = 1;
	  if (subscription_type == CMS_NO_SUBSCRIPTION)
	    {
	      reenable_sigpipe ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
      status = (CMS_STATUS) ntoh_uint32_array_get(temp_buffer,1);
      header.was_read = ntoh_uint32_array_get(temp_buffer,2);
    }
  else
    {
      header.was_read = 0;
      status = CMS_WRITE_OK;
      returned_serial_number = 0;
    }
  reenable_sigpipe ();
  return (status);
}

int
TCPMEM::check_if_read ()
{
  rcs_print_debug(PRINT_MISC,"TCPMEM::check_if_read() called. buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error
	("TCPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return 0;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, timeout) < 0)
    {
      status = CMS_MISC_ERROR;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (0);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type =REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  status = CMS_TIMED_OUT;
	  reenable_sigpipe ();
	  return 0;
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  status = CMS_MISC_ERROR;
	  reenable_sigpipe ();
	  return 0;
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }

  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  header.was_read = ntoh_uint32_array_get(temp_buffer,2);

  reenable_sigpipe ();
  return (header.was_read);
}


int
TCPMEM::get_queue_length ()
{
  rcs_print_debug(PRINT_MISC,"TCPMEM::get_queue_length() called. buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error
	("TCPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return 0;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, timeout) < 0)
    {
      status = CMS_MISC_ERROR;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (0);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  status = CMS_TIMED_OUT;
	  reenable_sigpipe ();
	  return 0;
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  status = CMS_MISC_ERROR;
	  reenable_sigpipe ();
	  return 0;
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  queuing_header.queue_length = ntoh_uint32_array_get(temp_buffer,2);

  reenable_sigpipe ();
  return (queuing_header.queue_length);
}

int
TCPMEM::get_msg_count ()
{
  rcs_print_debug(PRINT_MISC,"TCPMEM::get_msg_count() called. buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error
	("TCPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return 0;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, timeout) < 0)
    {
      status = CMS_MISC_ERROR;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (0);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  status = CMS_TIMED_OUT;
	  reenable_sigpipe ();
	  return 0;
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  status = CMS_MISC_ERROR;
	  reenable_sigpipe ();
	  return 0;
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  header.write_id = ntoh_uint32_array_get(temp_buffer,2);

  reenable_sigpipe ();
  return (header.write_id);
}

long
TCPMEM::get_msg_type ()
{
  rcs_print_debug(PRINT_MISC,"TCPMEM::get_msg_type() called. buffer_number=%ld,BufferName=%s,serial_number=%ld\n",buffer_number,BufferName,serial_number);

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error
	("TCPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return 0;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, timeout) < 0)
    {
      status = CMS_MISC_ERROR;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (0);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  status = CMS_TIMED_OUT;
	  reenable_sigpipe ();
	  return 0;
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  status = CMS_MISC_ERROR;
	  reenable_sigpipe ();
	  return 0;
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  long t = ntoh_uint32_array_get(temp_buffer,2);

  reenable_sigpipe ();
  return (t);
}


int
TCPMEM::get_space_available ()
{
  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }

  disable_sigpipe ();

  if (socket_fd <= 0)
    {
      cms_print_error
	("TCPMEM:: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      reenable_sigpipe ();
      return 0;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  int send_header_size = 20;
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }

  if (sendn (socket_fd, temp_buffer, send_header_size, 0, timeout) < 0)
    {
      status = CMS_MISC_ERROR;
      reconnect_needed = 1;
      reenable_sigpipe ();
      return (0);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 12, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  status = CMS_TIMED_OUT;
	  reenable_sigpipe ();
	  return 0;
	}
      else
	{
	  recvd_bytes = 0;
	  fatal_error_occurred = true;
	  status = CMS_MISC_ERROR;
	  reenable_sigpipe ();
	  return 0;
	}
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reenable_sigpipe ();
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  free_space = ntoh_uint32_array_get(temp_buffer,2);

  reenable_sigpipe ();
  return (free_space);
}


CMS_STATUS TCPMEM::clear ()
{
  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      return (status = CMS_MISC_ERROR);
    }

  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }
  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::clear: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      reconnect_needed = 1;
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
      return status;
    }

  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_CLEAR_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  if(total_subdivisions > 0)
    {
      hton_uint32_array_set(temp_buffer,3,(unsigned long) current_subdivision);
    }

  if (sendn (socket_fd, temp_buffer, 20, 0, timeout) < 0)
    {
      reconnect_needed = 1;
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 8, 0, timeout, &recvd_bytes,!polling) < 0)
    {
      if (recvn_timedout)
	{
	  last_request_type = REMOTE_CMS_CLEAR_REQUEST_TYPE;
	  timedout_request = REMOTE_CMS_CLEAR_REQUEST_TYPE;
	  consecutive_timeouts = 1;
	  return (status = CMS_TIMED_OUT);
	}
      else
	{
	  fatal_error_occurred = true;
	  reconnect_needed = 1;
	  return (status = CMS_MISC_ERROR);
	}
    }
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);

  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  header.was_read = ntoh_uint32_array_get(temp_buffer,2);

  return (status);
}


int
TCPMEM::login (
#ifdef ENABLE_RCS_CRYPT2
		const char *name, 
		const char *passwd
#else
		__unused_parameter__ const char *, 
		__unused_parameter__ const char *
#endif
		)
{
#ifdef ENABLE_RCS_CRYPT2
  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      return (status);
    }
  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  int handle_old_reply_ret = 0;

  while (timedout_request != NO_REMOTE_CMS_REQUEST && !handle_old_reply_ret)
    {
      if(interrupting_operation)
	{
	  status=CMS_INTERRUPTED_OPERATION;
	  return(status);
	}
      handle_old_reply_ret = handle_old_replies ();
    }
  if (handle_old_reply_ret < 0)
    {
      return 0;
    }
  set_socket_fds (write_socket_fd);

  last_request_type = REMOTE_CMS_GET_KEYS_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);

  if (sendn (socket_fd, temp_buffer, 20, 0, 30.0) < 0)
    {
      return 0;
    }
  memset (temp_buffer, 0, 20);
  strncpy (((char *) temp_buffer), name, 16);
  if (sendn (socket_fd, temp_buffer, 16, 0, 30.0) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 20, 0, 30.0, &recvd_bytes) < 0)
    {
      return 0;
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      return (0);
    }
  char *crypt1_ret = rcs_crypt (passwd, ((char *) temp_buffer) + 4);
  if (NULL == crypt1_ret)
    {
      cms_print_error ("TCPMEM::login() crypt function failed.\n");
      return 0;
    }
  char passwd_pass1[16];
  strncpy (passwd_pass1, crypt1_ret, 16);
  char *crypt2_ret = rcs_crypt (passwd_pass1, ((char *) temp_buffer) + 12);
  if (NULL == crypt2_ret)
    {
      cms_print_error ("TCPMEM::login() crypt function failed.\n");
      return (0);
    }
  char passwd_pass2[16];
  strncpy (passwd_pass2, crypt2_ret, 16);

  last_request_type = REMOTE_CMS_LOGIN_REQUEST_TYPE;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  if (sendn (socket_fd, temp_buffer, 20, 0, 30.0) < 0)
    {
      return 0;
    }
  memset (temp_buffer, 0, 20);
  strncpy (((char *) temp_buffer), name, 16);
  if (sendn (socket_fd, temp_buffer, 16, 0, 30.0) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  if (sendn (socket_fd, passwd_pass2, 16, 0, 30.0) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  if (recvn (socket_fd, temp_buffer, 8, 0, 30.0, &recvd_bytes) < 0)
    {
      return 0;
    }
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);
  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      return (status = CMS_MISC_ERROR);
    }

  int success = ntoh_uint32_array_get(temp_buffer,1);
  return (success);
#else
  cms_print_error("RCS Library compile without crypto support so login is not supported.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR) ;
#endif

}

void
TCPMEM::set_socket_fds (int new_fd)
{
  if (socket_fd == read_socket_fd)
    {
      read_serial_number = serial_number;
    }
  if (socket_fd == write_socket_fd)
    {
      write_serial_number = serial_number;
    }
  socket_fd = new_fd;
  if (socket_fd == read_socket_fd)
    {
      serial_number = read_serial_number;
    }
  if (socket_fd == write_socket_fd)
    {
      serial_number = write_serial_number;
    }
}

void TCPMEM::interrupt_operation(void)
{
  interrupting_operation=true;
  interrupting_operation_int=1;
  disconnect();
}

void TCPMEM::clear_interrupt_operation(void)
{
  interrupting_operation=false;
  interrupting_operation_int=0;
  reconnect();
}


int TCPMEM::do_wait_for(REMOTE_CMS_REQUEST_TYPE t, long lparam1, long lparam2, double _blocking_timeout)
{
  int orig_print_recvn_timeout_errors =
    print_recvn_timeout_errors;
  blocking_timeout = _blocking_timeout;
  print_recvn_timeout_errors = 0;

  if (reconnect_needed && autoreconnect)
    {
      reconnect ();
    }

  if (reconnect_needed)
    {
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  disable_sigpipe ();

  if (timedout_request == NO_REMOTE_CMS_REQUEST)
    {
      set_socket_fds (read_socket_fd);
    }
  if (fatal_error_occurred)
    {
      if (status >= 0)
	{
	  status = CMS_MISC_ERROR;
	}
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status);
    }
  if (socket_fd <= 0)
    {
      cms_print_error ("TCPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      fatal_error_occurred = true;
      reconnect_needed = 1;
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  if (((int) handle_old_replies ()) < 0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return status;
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }

  set_socket_fds (read_socket_fd);

  last_request_type = t;
  hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
  hton_uint32_array_set(temp_buffer,1,(unsigned long) last_request_type);
  hton_uint32_array_set(temp_buffer,2,(unsigned long) buffer_number);
  hton_uint32_array_set(temp_buffer,3,(unsigned long) lparam1);
  hton_uint32_array_set(temp_buffer,4,(unsigned long) lparam2);

  int send_header_size=20;
  if (total_subdivisions > 1)
    {
      cms_print_error("Can not call a wait_for function when subdivisions are being used.\n");
      return(status =CMS_MISC_ERROR);
    }
  if (sendn (socket_fd, temp_buffer, send_header_size, 0, blocking_timeout) <
      0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
      cms_print_error
	("TCPMEM: Can't send WAIT_FOR_WRITE request to server.\n");
      reconnect_needed = 1;
      fatal_error_occurred = true;
      reenable_sigpipe ();
      print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
      return (status = CMS_MISC_ERROR);
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }
  serial_number++;
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM sending request: fd = %d, serial_number=%ld, "
		   "request_type=%ld, buffer_number=%ld\n",
		   socket_fd, serial_number,
		   (long) last_request_type,
		   buffer_number);
  if (recvn (socket_fd, temp_buffer, 12, 0, blocking_timeout, &recvd_bytes,
	     !polling) <
      0)
    {
        if(interrupting_operation)
	  {
	    reenable_sigpipe ();
	    status=CMS_INTERRUPTED_OPERATION;
	    return(status);
	  }
	print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
	if (recvn_timedout)
	  {
	    timedout_request = REMOTE_CMS_READ_REQUEST_TYPE;
	    if (polling)
	    {
	      return (status = CMS_READ_OLD);
	    }
	  else
	    {
	      consecutive_timeouts = 1;
	      reenable_sigpipe ();
	      return (status = CMS_TIMED_OUT);
	    }
	}
      else
	{
	  recvd_bytes = 0;
	  reconnect_needed = 1;
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  if(interrupting_operation)
    {
      reenable_sigpipe ();
      status=CMS_INTERRUPTED_OPERATION;
      return(status);
    }
  print_recvn_timeout_errors = orig_print_recvn_timeout_errors;
  recvd_bytes = 0;
  returned_serial_number = ntoh_uint32_array_get(temp_buffer,0);
  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
		   "TCPMEM recieved_reply: fd = %d, serial_number=%ld, buffer_number=%ld\n",
		   socket_fd, returned_serial_number, buffer_number);

  if (returned_serial_number != serial_number)
    {
      cms_print_error
	("TCPMEM: Returned serial number(%ld) does not match expected serial number(%ld).\n",
	 returned_serial_number, serial_number);
      reconnect_needed = 1;
      if (subscription_type == CMS_NO_SUBSCRIPTION)
	{
	  fatal_error_occurred = true;
	  reenable_sigpipe ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  status = (CMS_STATUS)  ntoh_uint32_array_get(temp_buffer,1);
  reenable_sigpipe ();
  return (status);
}

int TCPMEM::wait_for_anything(double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE, 0,0,timeout_parameter);
}

int TCPMEM::wait_for_read(double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE, 0,0,timeout_parameter);
}

int TCPMEM::wait_for_clear(double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE, 0,0,timeout_parameter);
}

int TCPMEM::wait_for_write(double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE, 0,0,timeout_parameter);
}


int TCPMEM::wait_for_queue_length_over(int l, double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE, 1,l,timeout_parameter);
}

int TCPMEM::wait_for_queue_length_under(int l, double timeout_parameter)
{
  return do_wait_for(REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE, 0,l,timeout_parameter);
}

