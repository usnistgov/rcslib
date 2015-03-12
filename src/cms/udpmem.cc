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

// This is neccessary to avoid muliple definitions of fd_set, etc when both
// RPC via PCNFS and Windows Sockets are to be available

#define CMS_NETWORK_SOURCE
#define CMS_DERIVED_CLASS 1

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "udpmem_no_config.h"
#endif

#include "udpmem.hh"
#include "udp_opts.hh"		/* SET_UDP_NODELAY */
#include "_timer.h"
#include "rem_msg.hh"		/* REMOTE_CMS_READ_REQUEST_TYPE, etc. */
#include "rcs_prnt.hh"
#include "sendmsgt.h"		/* sendmsgt() */
#include "recvmsgt.h"		/* recvmsgt() */
#include "sokintrf.h"		/* load_socket_interface(), unload_socket_interface() */

#include "msghdr.h"		// struct iovec, msghdr
#include "ntohhton.hh"

#ifndef MAX_UDP_BUF_SIZE
#define MAX_UDP_BUF_SIZE (16384)
#endif

class UDPMEM_NONPORT_INTERNALS
{
  friend class UDPMEM;

  struct dl_sa *cli_addr;
  struct iovec request_iov2[2];
  struct iovec reply_iov2[2];
  struct msghdr request_message_header;
  struct msghdr reply_message_header;
#ifndef VXWORKS
  struct hostent *server_host_entry;
  struct hostent *broadcast_server_host_entry;
#endif
  struct dl_sa *server_socket_address;
  struct sockaddr_in broadcast_server_socket_address;
  struct dl_sa *throwaway_socket_address;
  struct iovec throwaway_iov2[2];
  struct msghdr throwaway_header;

  size_t buf_size;
  char *throwaway_buf1;
  char *throwaway_buf2;
  unsigned char *sendmsgt_collection_buffer;
  unsigned char *recvmsgt_collection_buffer;
};


static int number_of_udpmem_objects;

UDPMEM::UDPMEM (const char *_bufline, const char *_procline):
  CMS (_bufline, _procline,-1),
  polling(0),
  get_reply(0),
  last_reply_timed_out(0),
  reply_size(0),
  request_size(0),
  retry_timeout(0),
  serial_number(0),
  returned_serial_number(0),
  socket_fd(0),
  send_broadcast(0),
  send_broadcast_addr(0),
  recieve_broadcast(0),
  recieve_broadcast_port(0),
  subscription_type(0),
  poll_interval_millis(0),
  subscription_id(0),
  send_request(0),
  broadcast_subscriptions(0),
  broadcast_clnt_port(0),
  sockerrno(0),
  sockerrstr(0),
  internals(0)
{
  if (load_socket_interface () < 0)
    {
      cms_print_error ("UDPMEM: Can not load socket interface.\n");
      status = CMS_LIBRARY_UNAVAILABLE_ERROR;
      return;
    }
  internals = new UDPMEM_NONPORT_INTERNALS();
  memset(internals,0, sizeof(*internals));
  internals->buf_size = size+128 -(size%64);

  if(internals->buf_size > MAX_UDP_BUF_SIZE)
    {
      internals->buf_size = (size_t) MAX_UDP_BUF_SIZE;
    }

  internals->throwaway_buf1 =
    (char *) malloc(internals->buf_size);
  internals->throwaway_buf2 =
    (char *) malloc(internals->buf_size);
  internals->sendmsgt_collection_buffer =
    (unsigned char *) malloc(internals->buf_size);
  internals->recvmsgt_collection_buffer =
    (unsigned char *) malloc(internals->buf_size);


  init_variables ();
  send_request = 1;
  send_broadcast = 0;
  number_of_udpmem_objects++;
  polling = (((char *) NULL) != strstr (ProcessLine, "poll"));
  memset (reply_buffer, 0, 32);
  memset (request_buffer, 0, 32);

  char *broadcast_eq = 0;
  broadcast_eq = strstr (ProcessLine, "broadcast_to_svr=");
  if (NULL != broadcast_eq)
    {
      send_broadcast_addr = dl_inet_addr (broadcast_eq + 17);
      send_broadcast = 1;
    }
  else if (0 != strstr (ProcessLine, "broadcast_to_svr"))
    {
      char localhostname[80];
      if (dl_gethostname (localhostname, 80) < 0)
	{
	  if (strcmp (BufferHost, "localhost") != 0)
	    {
	      strncpy (localhostname, BufferHost, 80);
	    }
	  else
	    {
	      strncpy (localhostname, ProcessHost, 80);
	    }
	}
#ifndef VXWORKS
      dl_modified_gethostbyname (localhostname, &internals->broadcast_server_host_entry,1);
      if (NULL == internals->broadcast_server_host_entry)
	{
	  status = CMS_CONFIG_ERROR;
	  cms_print_error ("UDPMEM: Couldn't get host address for (%s).\n",
			   localhostname);
	  return;
	}
#ifdef __MSDOS__
      internals->broadcast_server_socket_address.sin_addr.s_addr =
	*((u_long *) internals->broadcast_server_host_entry->h_addr_list[0]);
#else
      internals->broadcast_server_socket_address.sin_addr.s_addr =
	*((int *) internals->broadcast_server_host_entry->h_addr_list[0]);
#endif
      internals->broadcast_server_socket_address.sin_family =
	internals->broadcast_server_host_entry->h_addrtype;
#else
      internals->broadcast_server_socket_address.sin_addr.s_addr =
	hostGetByName (localhostname);
      if (internals->broadcast_server_socket_address.sin_addr.s_addr == 
	  ((unsigned)ERROR))
	{
	  cms_print_error ("UDPMEM: Couldn't get host address for (%s).\n",
			   localhostname);
	  status = CMS_CONFIG_ERROR;
	}
#endif
      hton_uint32_array_set(&internals->broadcast_server_socket_address.sin_addr.s_addr,
			   0,
			   ntoh_uint32_array_get(&internals->broadcast_server_socket_address.sin_addr.s_addr,0) | 0xFF);
      // internals->broadcast_server_socket_address.sin_addr.s_addr |= dl_hton l (0xff);
      send_broadcast_addr = internals->broadcast_server_socket_address.sin_addr.s_addr;
      send_broadcast = 1;
    }

  socket_fd = 0;
#ifndef VXWORKS
  internals->server_host_entry = NULL;
#endif
  serial_number = 0;

  /* Get the IP address of the server using it's BufferHost. */
  if (send_broadcast)
    {
      internals->broadcast_server_socket_address.sin_family = AF_INET;
      internals->broadcast_server_socket_address.sin_port =
	dl_htons (((u_short) udp_port_number));
      internals->broadcast_server_socket_address.sin_addr.s_addr = send_broadcast_addr;;
      rcs_print_debug (PRINT_SOCKET_CONNECT,
		       "Broadcasting to IP address %s.\n",
		       dl_inet_ptr_ntoa
		       (&internals->broadcast_server_socket_address.sin_addr));
    }
  else {
    internals->server_socket_address = dl_create_sa(BufferHost,
						    udp_port_number,
						    use_ipv6);
    
    rcs_print_debug (PRINT_SOCKET_CONNECT,
		     "Using server on %s with IP address %s.\n", BufferHost,
		     dl_sa_get_host(internals->server_socket_address));
  }
  socket_fd = (int) dl_udp_socket(use_ipv6);
  if (socket_fd < 0)
    {
      cms_print_error ("UDPMEM: Error from socket() (errno = %d:%s)\n",
		       errno, strerror (errno));
      status = CMS_CREATE_ERROR;
      return;
    }
  if (set_udp_socket_options (socket_fd) < 0)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  char *sub_info_string = NULL;
  poll_interval_millis = 30000;
  subscription_type = CMS_NO_SUBSCRIPTION;
  internals->cli_addr = dl_create_sa(0,0,use_ipv6);

  if ((min_compatible_version > 3.13 || min_compatible_version < 1e-6)
      && !send_broadcast)
    {
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
      if (poll_interval_millis < ceil (clk_tck () * 1000.0))
	{
	  poll_interval_millis = (int) ceil (clk_tck () * 1000.0);
	}
      char *broadcast_clnt_port_eq = strstr (buflineupper, "BROADCAST_PORT=");
      if (broadcast_clnt_port_eq != NULL)
	{
	  broadcast_subscriptions = 1;
	  broadcast_clnt_port = strtol (broadcast_clnt_port_eq + 15, 0, 0);
	  dl_sa_set_port(internals->cli_addr,broadcast_clnt_port);
	}
    }

  if (send_broadcast)
    {
      if (make_udp_socket_broadcast (socket_fd) < 0)
	{
	  return;
	}
    }

  if (dl_bind (socket_fd, dl_sa_addr(internals->cli_addr), dl_sa_len(internals->cli_addr)) <
      0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      cms_print_error ("UDPMEM: bind error %d = %s\n", 
		       sockerrno, sockerrstr);
    }
  memset (&internals->request_message_header, 0, sizeof (internals->request_message_header));
  internals->request_message_header.msg_name = (caddr_t) 
    dl_sa_addr(internals->server_socket_address);
  internals->request_message_header.msg_namelen = 
    dl_sa_len(internals->server_socket_address);

  internals->request_iov2[0].iov_base = request_buffer;
  internals->request_iov2[0].iov_len = 20;
  if (min_compatible_version > 3.13 || min_compatible_version < 1e-6)
    {
      internals->request_iov2[0].iov_len = 24;
    }
  if (min_compatible_version > 3.43 || min_compatible_version < 1e-6)
    {
      internals->request_iov2[0].iov_len = 28;
    }
  else
    {
      total_subdivisions = 1;
    }
  internals->request_iov2[1].iov_base = (char *) encoded_data;
  internals->request_iov2[1].iov_len = max_encoded_message_size;
  internals->request_message_header.msg_iov = internals->request_iov2;
  internals->request_message_header.msg_iovlen = 2;
  memset (&internals->reply_message_header, 0, sizeof (internals->reply_message_header));
  internals->reply_message_header.msg_name = (caddr_t) 
    dl_sa_addr(internals->server_socket_address);
  internals->reply_message_header.msg_namelen = 
    dl_sa_len(internals->server_socket_address);

  internals->reply_iov2[0].iov_base = reply_buffer;
  internals->reply_iov2[0].iov_len = 20;
  internals->reply_iov2[1].iov_base = (char *) encoded_data;
  internals->reply_iov2[1].iov_len = max_encoded_message_size;
  internals->reply_message_header.msg_iov = internals->reply_iov2;
  internals->reply_message_header.msg_iovlen = 2;

  memset(internals->throwaway_iov2,0,sizeof(internals->throwaway_iov2));
  memset(&internals->throwaway_header,0,sizeof(internals->throwaway_header));
  memset(internals->throwaway_buf1,0,internals->buf_size);
  memset(internals->throwaway_buf2,0,internals->buf_size);
  internals->throwaway_iov2[0].iov_base = internals->throwaway_buf1;
  internals->throwaway_iov2[0].iov_len = internals->buf_size;
  internals->throwaway_iov2[1].iov_base = internals->throwaway_buf2;
  internals->throwaway_iov2[1].iov_len = internals->buf_size;
  internals->throwaway_header.msg_iov = internals->throwaway_iov2;
  internals->throwaway_header.msg_iovlen = 2;
  internals->throwaway_socket_address = 0;

  if(0 != internals->server_socket_address) {
    dl_sa_copy(&internals->throwaway_socket_address,
	       internals->server_socket_address);
    internals->throwaway_header.msg_name = (caddr_t) 
      dl_sa_addr(internals->throwaway_socket_address);
    internals->reply_message_header.msg_namelen = 
      dl_sa_len(internals->throwaway_socket_address);
  }

  char *retry_string;
  if ((retry_string = strstr (ProcessLine, "retry=")) != NULL)
    {
      retry_string += strlen ("retry=");
      retry_timeout = strtod (retry_string, (char **) NULL);
    }
  if (retry_timeout > timeout)
    retry_timeout = timeout;
  reply_size = 0;


  if ((min_compatible_version > 3.13 || min_compatible_version < 1e-6)
      && !send_broadcast)
    {
      if (!no_verify_buf && (verify_bufname () < 0))
	{
	  //status = CMS_STATUS_NOT_SET;
	  return;
	}
      if (subscription_type != CMS_NO_SUBSCRIPTION &&
	  poll_interval_millis > 0)
	{
	  setup_subscription (poll_interval_millis/1000.0);
	}
    }

  if (polling)
    {
      make_udp_socket_nonblocking (socket_fd);
      timeout = 0;
      retry_timeout = 0;
    }
  last_reply_timed_out = 0;
  if(max_encoded_message_size  > 
     ((long) internals->buf_size - 32))
    {
      max_encoded_message_size = 
 	((long) internals->buf_size - 32);
    }
  if(max_message_size  > 
     ((long) internals->buf_size - 32))
    {
      if(!queuing_enabled)
	{
	  set_rcs_print_tag(BufferName);
	  rcs_print_warning("max_message_size reduced from %ld to %ld. (Suggest change from UDP to TCP?)\n",
			max_message_size,
			    ((long) (internals->buf_size-32)));
	  set_rcs_print_tag(BufferName);
	}
      max_message_size = 
 	((long) internals->buf_size - 32);
    }
  if(guaranteed_message_space  > 
     ((long) internals->buf_size - 32))
    {
       guaranteed_message_space = 
	 ((long) internals->buf_size - 32);
    }
}

int
UDPMEM::init_variables ()
{
  polling = 0;
  get_reply = 1;
  last_reply_timed_out = 0;
  reply_size = 20;
  request_size = 24;
  retry_timeout = 0.02;

  serial_number = 0;
  returned_serial_number = 0;
#ifndef VXWORKS
  if(internals)
    {
      internals->server_host_entry = 0;
      internals->broadcast_server_host_entry = 0;
    }
#endif
  if(internals)
    {
      internals->server_socket_address = 0;
      memset (&internals->broadcast_server_socket_address, 0, sizeof (struct sockaddr_in));
      memset (&internals->request_message_header, 0, sizeof (struct msghdr));
      memset (&internals->reply_message_header, 0, sizeof (struct msghdr));
      internals->cli_addr=0;
      memset (&internals->request_iov2, 0, 2 * sizeof (struct iovec));
      memset (&internals->reply_iov2, 0, 2 * sizeof (struct iovec));
    }
  socket_fd = -1;
  memset (request_buffer, 0, 32);
  memset (reply_buffer, 0, 32);
  send_broadcast = 0;
  send_broadcast_addr = 0xFF;
  recieve_broadcast = 0;
  recieve_broadcast_port = 0;
  subscription_type = CMS_NO_SUBSCRIPTION;
  poll_interval_millis = 30;
  subscription_id = -1;
  send_request = 1;
  broadcast_subscriptions = 0;
  broadcast_clnt_port = 0;
  return 0;
}



UDPMEM::~UDPMEM ()
{
  if (socket_fd > 0)
    {
      if (subscription_type != CMS_NO_SUBSCRIPTION)
	{
	  cancel_subscription ();
	}

      if (delete_totally)
	{
	  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_CLEAN_REQUEST_TYPE);
	  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
	  internals->request_iov2[0].iov_len = 28;
	  internals->request_message_header.msg_iovlen = 1;
	  sendmsgt (socket_fd, &internals->request_message_header, 0, timeout,
		    internals->sendmsgt_collection_buffer,
		    internals->buf_size);
	}

      hton_uint32_array_set(request_buffer,0,
			    (unsigned long) REMOTE_CMS_CLOSE_CHANNEL_REQUEST_TYPE);
      hton_uint32_array_set(request_buffer,1,
			    (unsigned long) buffer_number);
      internals->request_iov2[0].iov_len = 28;
      internals->request_message_header.msg_iovlen = 1;
      sendmsgt (socket_fd, &internals->request_message_header, 0, timeout,
		internals->sendmsgt_collection_buffer,
		internals->buf_size);
      dl_closesocket (socket_fd);
      socket_fd = 0;
    }
#if 0 
  //MS_WINDOWS_API
  if (number_of_udpmem_objects == 1)
    {
      free_sendmsg_collection_buffer ();
      free_recvmsg_collection_buffer ();
    }
#endif
  number_of_udpmem_objects--;
  if(internals)
    {
      if(internals->throwaway_buf1)
	{
	  free(internals->throwaway_buf1);
	  internals->throwaway_buf1=0;
	}
      if(internals->throwaway_buf2)
	{
	  free(internals->throwaway_buf2);
	  internals->throwaway_buf2=0;
	}
      if(internals->recvmsgt_collection_buffer)
	{
	  free(internals->recvmsgt_collection_buffer);
	  internals->recvmsgt_collection_buffer=0;
	}
      if(internals->sendmsgt_collection_buffer)
	{
	  free(internals->sendmsgt_collection_buffer);
	  internals->sendmsgt_collection_buffer=0;
	}
      if(internals->throwaway_socket_address)
	{
	  dl_free_sa(internals->throwaway_socket_address);
	  internals->throwaway_socket_address=0;
	}
      if(internals->server_socket_address)
	{
	  dl_free_sa(internals->server_socket_address);
	  internals->server_socket_address=0;
	}
      if(internals->cli_addr)
	{
	  dl_free_sa(internals->cli_addr);
	  internals->cli_addr=0;
	}
      delete internals;
      internals=0;
    }
  unload_socket_interface ();
}

CMS_STATUS UDPMEM::read ()
{
  long
    message_size,
    id;
  get_reply = 1;
  send_request = (subscription_type == CMS_NO_SUBSCRIPTION);
  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (!last_reply_timed_out)
    {
      serial_number++;
    }
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_READ_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_READ_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) in_buffer_id);

  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = max_encoded_message_size;
  internals->reply_iov2[1].iov_base = (caddr_t) encoded_data;
  internals->reply_message_header.msg_iovlen = 2;
  timeout=read_timeout;
  if (call_on_server () < 0)
    {
      timeout=orig_timeout;
      cms_print_error ("UDPMEM: read failed.\n");
      internals->reply_message_header.msg_name = (caddr_t) 
	dl_sa_addr(internals->cli_addr);
      internals->reply_message_header.msg_namelen = 
	dl_sa_len(internals->cli_addr);
      if(last_reply_timed_out || status == CMS_TIMED_OUT)
	{
	  return (status = CMS_TIMED_OUT);
	}
      return (status = CMS_MISC_ERROR);
    }
  timeout=orig_timeout;
  internals->reply_message_header.msg_name = (caddr_t) 
    dl_sa_addr(internals->cli_addr);
  internals->reply_message_header.msg_namelen = 
    dl_sa_len(internals->cli_addr);

  if (!last_reply_timed_out)
    {
      status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
      message_size = ntoh_uint32_array_get(reply_buffer,2);
      if (message_size + 20 != reply_size && !polling)
	{
	  cms_print_error ("UDPMEM: message_size+20 = %ld\n",
			   message_size + 20);
	  cms_print_error ("UDPMEM: reply_size = %ld\n", reply_size);
	}
      id = ntoh_uint32_array_get(reply_buffer,3);
      header.was_read = ntoh_uint32_array_get(reply_buffer,4);
      check_id (id);
    }
  else
    {
      status = CMS_TIMED_OUT;
      if (polling)
	{
	  status = CMS_READ_OLD;
	}
    }
  return (status);
}

CMS_STATUS UDPMEM::blocking_read (double _blocking_timeout)
{
  long
    message_size,
    id;
  get_reply = 1;
  blocking_timeout = _blocking_timeout;
  timeout = blocking_timeout;
  int
    orig_polling =
    polling;
  if (polling)
    {
      polling = 0;
      make_udp_socket_blocking (socket_fd);
    }
  int
    orig_subscription_type =
    subscription_type;
  if (CMS_NO_SUBSCRIPTION == subscription_type)
    {
      subscription_type = CMS_POLLED_SUBSCRIPTION;
      if (blocking_timeout > 0.0)
	{
	  // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
	  poll_interval_millis = (int) (blocking_timeout * 1000);
	  if (poll_interval_millis < 10)
	    {
	      poll_interval_millis = 10;
	    }
	  if (poll_interval_millis > 1000)
	    {
	      poll_interval_millis = 1000;
	    }
	}
      else
	{
	  poll_interval_millis = 30;
	}
      setup_subscription (poll_interval_millis/1000.0);
    }
  double
    orig_retry_timeout =
    retry_timeout;
  retry_timeout = timeout = blocking_timeout;
  send_request = 0;
  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      retry_timeout = orig_retry_timeout;
      return (status = CMS_MISC_ERROR);
    }
  if (!last_reply_timed_out)
    {
      serial_number++;
    }
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_READ_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_READ_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) in_buffer_id);

  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = max_encoded_message_size;
  internals->reply_iov2[1].iov_base = (caddr_t) encoded_data;
  internals->reply_message_header.msg_iovlen = 2;
  status = CMS_READ_OLD;
  double
    start_time =
    0.0;
  if (timeout > 1e-6)
    {
      start_time = etime ();
    }
  double
    time_diff =
    0.0;
  while (status == CMS_READ_OLD && (timeout < 0.0 || time_diff > timeout))
    {
      if (call_on_server () < 0)
	{
	  cms_print_error ("UDPMEM: read failed.\n");
	  timeout = orig_timeout;
	  polling = orig_polling;
	  retry_timeout = orig_retry_timeout;
	  internals->reply_message_header.msg_name = (caddr_t) 
	    dl_sa_addr(internals->cli_addr);
	  internals->reply_message_header.msg_namelen = 
	    dl_sa_len(internals->cli_addr);
	  if (orig_subscription_type == CMS_NO_SUBSCRIPTION)
	    {
	      cancel_subscription ();
	    }
	  subscription_type = orig_subscription_type;
	  if(last_reply_timed_out || status == CMS_TIMED_OUT)
	    {
	      return (status = CMS_TIMED_OUT);
	    }
	  return (status = CMS_MISC_ERROR);
	}
      internals->reply_message_header.msg_name = (caddr_t) 
	dl_sa_addr(internals->cli_addr);
      internals->reply_message_header.msg_namelen = 
	dl_sa_len(internals->cli_addr);
      if (!last_reply_timed_out)
	{
	  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
	  message_size = ntoh_uint32_array_get(reply_buffer,2);
	  if (message_size + 20 != reply_size && !polling)
	    {
	      cms_print_error ("UDPMEM: message_size+20 = %ld\n",
			       message_size + 20);
	      cms_print_error ("UDPMEM: reply_size = %ld\n", reply_size);
	      return(status = CMS_MISC_ERROR);
	    }
	  id = ntoh_uint32_array_get(reply_buffer,3);
	  header.was_read = ntoh_uint32_array_get(reply_buffer,4);
	  check_id (id);
	}
      if (timeout > 1e-6)
	{
	  time_diff = etime () - start_time;
	}
    }
  retry_timeout = orig_retry_timeout;
  timeout = orig_timeout;
  polling = orig_polling;
  if (orig_subscription_type == CMS_NO_SUBSCRIPTION)
    {
      cancel_subscription ();
    }
  subscription_type = orig_subscription_type;
  return (status);
}

CMS_STATUS UDPMEM::peek ()
{
  long
    message_size,
    id;
  get_reply = 1;
  send_request = (subscription_type == CMS_NO_SUBSCRIPTION);
  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  if (!last_reply_timed_out)
    {
      serial_number++;
    }
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_READ_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_PEEK_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) in_buffer_id);

  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = max_encoded_message_size;
  internals->reply_iov2[1].iov_base = (caddr_t) encoded_data;
  internals->reply_message_header.msg_iovlen = 2;
  timeout=read_timeout;
  if (call_on_server () < 0)
    {
      timeout=orig_timeout;
      cms_print_error ("UDPMEM: peek failed.\n");
      internals->reply_message_header.msg_name = (caddr_t) 
	dl_sa_addr(internals->cli_addr);
      internals->reply_message_header.msg_namelen = 
	dl_sa_len(internals->cli_addr);
      if(last_reply_timed_out || status == CMS_TIMED_OUT)
	{
	  return (status = CMS_TIMED_OUT);
	}
      return (status = CMS_MISC_ERROR);
    }
  timeout=orig_timeout;
  internals->reply_message_header.msg_name = (caddr_t) 
    dl_sa_addr(internals->cli_addr);
  internals->reply_message_header.msg_namelen = 
    dl_sa_len(internals->cli_addr);
  if (!last_reply_timed_out)
    {
      status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
      message_size = ntoh_uint32_array_get(reply_buffer,2);
      if (message_size + 20 != reply_size && !polling)
	{
	  cms_print_error ("UDPMEM: message_size+20 = %ld\n",
			   message_size + 20);
	  cms_print_error ("UDPMEM: reply_size = %ld\n", reply_size);
	  return(status = CMS_MISC_ERROR);
	}
      id = ntoh_uint32_array_get(reply_buffer,3);
      header.was_read = ntoh_uint32_array_get(reply_buffer,4);
      check_id (id);
    }
  else
    {
      status = CMS_TIMED_OUT;
      if (polling)
	{
	  status = CMS_READ_OLD;
	}
    }
  return (status);
}


CMS_STATUS
UDPMEM::setup_subscription (double _period)
{
  get_reply = 1;
  send_request = 1;
  double orig_retry_timeout = retry_timeout;
  poll_interval_millis = (int) (1000.0 *_period);
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::setup_subscription: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status=CMS_MISC_ERROR);
    }
  last_reply_timed_out = 0;
  serial_number++;
  subscription_type=CMS_POLLED_SUBSCRIPTION;
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) subscription_type);
  hton_uint32_array_set(request_buffer,4,(unsigned long) poll_interval_millis);
  hton_uint32_array_set(request_buffer,5,(unsigned long) in_buffer_id);

  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) 0;
  internals->reply_message_header.msg_iovlen = 1;
  int orig_polling = polling;
  if (orig_timeout <= 0.0)
    {
      timeout = 5.0;
    }
  polling = 0;
  if (call_on_server () < 0)
    {
      retry_timeout = orig_retry_timeout;
      cms_print_error ("UDPMEM: Can't setup subscription.\n");
      polling = orig_polling;
      timeout = orig_timeout;
      last_reply_timed_out = 0;
      subscription_type = CMS_NO_SUBSCRIPTION;
      subscription_id = -1;
      serial_number++;
      return (status=CMS_MISC_ERROR);
    }
  retry_timeout = orig_retry_timeout;
  last_reply_timed_out = 0;
  serial_number++;
  polling = orig_polling;
  timeout = orig_timeout;
  if (!last_reply_timed_out)
    {
      long success = ntoh_uint32_array_get(reply_buffer,1);
      if (success == 1)
	{
	  subscription_id = ntoh_uint32_array_get(reply_buffer,2);
	}
      else
	{
	  cms_print_error ("UDPMEM: Can't setup subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	  subscription_id = -1;
	}
    }
  polling = subscription_type != CMS_NO_SUBSCRIPTION;
  if (polling)
    {
      make_udp_socket_nonblocking (socket_fd);
      timeout = 0;
      retry_timeout = 0;
    }
  return (status=CMS_SETUP_SUBSCRIPTION_OK);
}

CMS_STATUS
UDPMEM::cancel_subscription ()
{
  get_reply = 1;
  send_request = 1;
  double orig_retry_timeout = retry_timeout;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::cancel_subscription: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      retry_timeout = orig_retry_timeout;
      return (status=CMS_MISC_ERROR);
    }
  make_udp_socket_blocking (socket_fd);
  last_reply_timed_out = 0;
  serial_number += 10000 / poll_interval_millis;
  serial_number += 10;

  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) subscription_id);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) 0;
  internals->reply_message_header.msg_iovlen = 1;
  int orig_polling = polling;
  if (orig_timeout <= 10.0)
    {
      timeout = 10.0;
    }
  polling = 0;
  if (call_on_server () < 0)
    {
      retry_timeout = orig_retry_timeout;
      cms_print_error ("UDPMEM: Can't cancel subscription.\n");
      polling = orig_polling;
      timeout = orig_timeout;
      last_reply_timed_out = 0;
      subscription_type = CMS_NO_SUBSCRIPTION;
      subscription_id = -1;
      serial_number++;
      return (status=CMS_MISC_ERROR);
    }
  retry_timeout = orig_retry_timeout;
  last_reply_timed_out = 0;
  serial_number++;
  polling = orig_polling;
  timeout = orig_timeout;

  if (!last_reply_timed_out)
    {
      long success = ntoh_uint32_array_get(reply_buffer,1);
      if (success == 1)
	{
	  if (subscription_id != (long)ntoh_uint32_array_get(reply_buffer,2))
	    {
	      cms_print_error
		("UDPMEM: Can't cancel subscription. (Incorrect subscription id returned.)\n");
	      subscription_type = CMS_NO_SUBSCRIPTION;
	      subscription_id = -1;
	    }
	}
      else
	{
	  cms_print_error ("UDPMEM: Can't cancel subscription.\n");
	  subscription_type = CMS_NO_SUBSCRIPTION;
	  subscription_id = -1;
	}
    }
  return (status=CMS_SETUP_SUBSCRIPTION_OK);
}


int
UDPMEM::verify_bufname ()
{
  long message_size;
  get_reply = 1;
  send_request = 1;
  char bufname_from_server[80];
  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::read: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      status = CMS_MISC_ERROR;
      return (-1);
    }
  last_reply_timed_out = 0;
  serial_number++;
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_PEEK_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) in_buffer_id);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 80;
  internals->reply_iov2[1].iov_base = (caddr_t) bufname_from_server;
  internals->reply_message_header.msg_iovlen = 2;
  int orig_polling = polling;
  if (orig_timeout <= 0.0)
    {
      timeout = 5.0;
    }
  polling = 0;
  if (call_on_server () < 0)
    {
      cms_print_error
	("UDPMEM: Failed to verify BufferName with server (Server may not be running.)\n");
      polling = orig_polling;
      timeout = orig_timeout;
      last_reply_timed_out = 0;
      serial_number++;
      status = CMS_NO_SERVER_ERROR;
      return (-1);
    }
  last_reply_timed_out = 0;
  serial_number++;
  polling = orig_polling;
  timeout = orig_timeout;
  if (!last_reply_timed_out)
    {
      status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
      message_size = ntoh_uint32_array_get(reply_buffer,2);
      if (message_size > 100)
	{
	  cms_print_error
	    ("UDPMEM: message_size = %ld exceeds maximum for GET_BUF_NAME reply.\n",
	     message_size + 20);
	  status = CMS_MISC_ERROR;
	  return (-1);
	}
      if (0 != strncmp (BufferName, bufname_from_server, 80))
	{
	  cms_print_error
	    ("UDPMEM: Buffer name retrieved from server %s at IP address %s for buffer number %ld was %s but %s was expected.\n",
	     BufferHost, 
	     dl_sa_get_host(internals->server_socket_address),
	     buffer_number,
	     ((0 != bufname_from_server[0]) ? bufname_from_server : "blank"),
	     BufferName);
	  if (0 != bufname_from_server[0] && 0 != BufferName[0])
	    {
	      status = CMS_RESOURCE_CONFLICT_ERROR;
	      return (-1);
	    }
	}
    }
  else
    {
      status = CMS_NO_SERVER_ERROR;
      return (-1);
    }
  return (0);
}


void
UDPMEM::throwaway_extra_data_on_socket (int throwaway_socket_fd)
{
  int count = 0;
  if (throwaway_socket_fd > 0)
    {
      while (recvmsgtq (throwaway_socket_fd, &(internals->throwaway_header), 0, 0.0, internals->recvmsgt_collection_buffer, 
			(long) (internals->buf_size)) > 0 && count < 10)
	count++;
    }
}

int
UDPMEM::call_on_server ()
{
  double start_time, current_time;
  start_time = etime ();
  last_reply_timed_out = 0;
  double last_time_sent = 0.0;
  double time_diff = 0.0;
  int first_time = 1;
  while (1)
    {
      if (send_broadcast)
	{
	  if (get_reply)
	    {
	      internals->request_message_header.msg_name = (caddr_t) 
		dl_sa_addr(internals->server_socket_address);
	      internals->request_message_header.msg_namelen = 
		dl_sa_len(internals->server_socket_address);
	    }
	  else
	    {
	      internals->request_message_header.msg_name =
		(caddr_t) & internals->broadcast_server_socket_address;
	      internals->request_message_header.msg_namelen =
		sizeof (internals->broadcast_server_socket_address);
	    }
	}
      if (send_request)
	{
	  rcs_print_debug(PRINT_MISC,"UDPMEM::call_on_server() sending request\n");
	  time_diff = etime () - last_time_sent;
	  if (first_time || time_diff >= retry_timeout)
	    {
	      long request_size_expected = 0;
	      for(int i = 0; i < ((int) internals->request_message_header.msg_iovlen); i++)
		{
		  request_size_expected += internals->request_message_header.msg_iov[i].iov_len;
		}
	      if ((request_size =
		   sendmsgt (socket_fd, 
			     &internals->request_message_header, 0,
			     retry_timeout,
			     internals->sendmsgt_collection_buffer,
			     internals->buf_size)) != request_size_expected)
		{
		  if(request_size > 0)
		    {
		      cms_print_error("sendmsg returned %ld when we expected to send %ld bytes.\n", request_size, request_size_expected); 
		      return(-1);
		    }
		  if (!sendmsgt_timed_out)
		    return (-1);
		}
	      last_time_sent = etime ();
	      first_time = 0;
	    }
	}
      if (!get_reply)
	{
	  last_reply_timed_out = 0;
	  return 0;
	}
      else
	{
	  rcs_print_debug(PRINT_MISC,"UDPMEM::call_on_server() getting_reply\n");
	  if ((reply_size =
	       recvmsgtq (socket_fd, &internals->reply_message_header, 0,
			  retry_timeout,
			  internals->recvmsgt_collection_buffer,
			  (long) (internals->buf_size))) < 0)
	    {
	      if (!recvmsgt_timed_out)
		return (-1);
	    }
	  returned_serial_number = ntoh_uint32_array_get(reply_buffer,0);
	  rcs_print_debug(PRINT_MISC,"UDPMEM::call_on_server() returned_serial_number=%ld, serial_number=%ld\n",returned_serial_number,serial_number);
	  if (returned_serial_number == serial_number)
	    {
	      last_reply_timed_out = 0;
	      throwaway_extra_data_on_socket (socket_fd);
	      return 0;
	    }
	  // If we have a subscription, force the messages to the correct order.
	  if (!send_request &&
	      (returned_serial_number > serial_number ||
	       (serial_number > 1000 && returned_serial_number < 500)))
	    {
	      serial_number = returned_serial_number;
	      last_reply_timed_out = 0;
	      return 0;
	    }
	  if (!polling)
	    {
	      current_time = etime ();
	      if (current_time - start_time > timeout && timeout >= 1e-6)
		{
		  status=CMS_TIMED_OUT;
		  cms_print_timeout_error
		    ("UDPMEM: time out error after %f seconds.\n",
		     current_time - start_time);
		  last_reply_timed_out = 1;
		  return -1;
		}
	    }
	  else
	    {
	      last_reply_timed_out = 1;
	      return 0;
	    }
	}
    }
  throwaway_extra_data_on_socket (socket_fd);
  last_reply_timed_out = 0;
  return (0);
}


CMS_STATUS UDPMEM::write (void *user_data)
{
  get_reply = confirm_write;
  send_request = 1;

  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;

  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_WRITE_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_WRITE_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) header.in_buffer_size);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = header.in_buffer_size;
  internals->request_iov2[1].iov_base = (caddr_t) user_data;
  internals->request_message_header.msg_iovlen = 2;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  timeout = write_timeout;
  if (call_on_server () < 0)
    {
      timeout=orig_timeout;
      cms_print_error ("UDPMEM: write failed.\n");
      if(last_reply_timed_out || status == CMS_TIMED_OUT)
	{
	  return (status = CMS_TIMED_OUT);
	}
      return (status = CMS_MISC_ERROR);
    }

  timeout=orig_timeout;
  if (confirm_write)
    {
      status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
      header.was_read = ntoh_uint32_array_get(reply_buffer,2);
      if (last_reply_timed_out)
	{
	  header.was_read = 0;
	}
    }
  else
    {
      status = CMS_WRITE_OK;
      header.was_read = 0;
    }
  return (status);
}

CMS_STATUS UDPMEM::write_if_read (void *user_data)
{
  get_reply = confirm_write;
  send_request = 1;
  if (!force_raw)
    {
      user_data = encoded_data;
    }

  if (socket_fd <= 0)
    {
      cms_print_error ("UDPMEM::write: Invalid socket descriptor. (%d)\n",
		       socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_WRITE_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  hton_uint32_array_set(request_buffer,3,(unsigned long) CMS_WRITE_IF_READ_ACCESS);
  hton_uint32_array_set(request_buffer,4,(unsigned long) header.in_buffer_size);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = header.in_buffer_size;
  internals->request_iov2[1].iov_base = (caddr_t) user_data;
  internals->request_message_header.msg_iovlen = 2;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  timeout = write_timeout;
  if (call_on_server () < 0)
    {
      timeout = orig_timeout;
      cms_print_error ("UDPMEM: write_if_read failed.\n");
      if(last_reply_timed_out || status == CMS_TIMED_OUT)
	{
	  return (status = CMS_TIMED_OUT);
	}
      return (status = CMS_MISC_ERROR);
    }
  timeout=orig_timeout;

  if (confirm_write)
    {
      status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
      header.was_read = ntoh_uint32_array_get(reply_buffer,2);
      if (last_reply_timed_out)
	{
	  header.was_read = 0;
	}
    }
  else
    {
      status = CMS_WRITE_OK;
      header.was_read = 0;
    }
  return (status);
}

int
UDPMEM::check_if_read ()
{
  get_reply = 1;
  send_request = 1;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;

  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      cms_print_error ("UDPMEM: check_if_read failed.\n");
      return (status = CMS_MISC_ERROR);
    }
  
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  header.was_read = ntoh_uint32_array_get(reply_buffer,2);
  if (last_reply_timed_out)
    {
      header.was_read = 0;
    }
  return (header.was_read);
}

long
UDPMEM::get_msg_type ()
{
  get_reply = 1;
  send_request = 1;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::get_msg_type: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;

  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      cms_print_error ("UDPMEM: get_msg_type failed.\n");
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  return ntoh_uint32_array_get(reply_buffer,2);
}

int
UDPMEM::get_msg_count ()
{
  get_reply = 1;
  send_request = 1;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::get_msg_count: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;

  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }

  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      cms_print_error ("UDPMEM: get_msg_count failed.\n");
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  header.write_id = ntoh_uint32_array_get(reply_buffer,2);
  return (header.write_id);
}

int
UDPMEM::get_queue_length ()
{
  get_reply = 1;
  send_request = 1;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::get_queue_length: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      cms_print_error ("UDPMEM: get_queue_length failed.\n");
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  queuing_header.queue_length = ntoh_uint32_array_get(reply_buffer,2);
  return (queuing_header.queue_length);
}



int
UDPMEM::get_space_available ()
{
  get_reply = 1;
  send_request = 1;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::get_space_available: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  serial_number++;
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_len = 0;
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      cms_print_error ("UDPMEM: get_space_available failed.\n");
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  free_space = ntoh_uint32_array_get(reply_buffer,2);
  return (free_space);
}


CMS_STATUS UDPMEM::clear ()
{
  send_request = 1;
  get_reply = 0;
  if (socket_fd <= 0)
    {
      cms_print_error
	("UDPMEM::check_if_read: Invalid socket descriptor. (%d)\n",
	 socket_fd);
      return (status = CMS_MISC_ERROR);
    }
  hton_uint32_array_set(request_buffer,0,(unsigned long) REMOTE_CMS_CLEAR_REQUEST_TYPE);
  hton_uint32_array_set(request_buffer,1,(unsigned long) buffer_number);
  hton_uint32_array_set(request_buffer,2,(unsigned long) serial_number);
  if (total_subdivisions > 1)
    {
      hton_uint32_array_set(request_buffer,6,(unsigned long) current_subdivision);
    }
  internals->request_iov2[1].iov_base = (caddr_t) NULL;
  internals->request_message_header.msg_iovlen = 1;
  internals->reply_iov2[1].iov_len = 0;
  internals->reply_iov2[1].iov_base = (caddr_t) NULL;
  internals->reply_message_header.msg_iovlen = 1;
  if (call_on_server () < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  status = (CMS_STATUS) ntoh_uint32_array_get(reply_buffer,1);
  return (status);
}


