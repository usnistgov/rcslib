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
#include "gdrs_im_srv_no_config.h"
#endif

#include "recvmsgt.h"		/* recvmsgt() */
#include "sendmsgt.h"		/* sendmsgt() */
#include "sokintrf.h"		/* dl_sendmsg() */

#include "cms.hh"		/* class CMS */
#include "cms_srv.hh"		// class CMS_SERVER
#include "gdrs_im_srv.hh"		/* class CMS_SERVER_REMOTE_GDRS_IM_PORT */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "udp_opts.hh"		/* SET_UDP_NODELAY */
#include "timer.hh"		// etime(), clk_tck()
#include "_timer.h"
#include "msghdr.h"		// struct iovec, struct msghdr
#include "rem_msg.hh"
#include "ntohhton.hh"

class GDRS_IM_BROADCAST_DATA
{
public:
  int buffer_number;
  struct sockaddr_in broadcast_address;
  int broadcast_clnt_port;
};

class GDRS_IM_BUFFER_SUBSCRIPTION_INFO
{
public:
  GDRS_IM_BUFFER_SUBSCRIPTION_INFO ();
  ~GDRS_IM_BUFFER_SUBSCRIPTION_INFO ();
  int buffer_number;
  int subdiv;
  int min_last_id;
  unsigned long max_serial_number;
  int list_id;
  double last_update_time;
  double min_update_interval;
  class GDRS_IM_BROADCAST_DATA *broadcast_data;
  class RCS_LINKED_LIST *sub_clnt_info;

private:
  //Prevent copying.
  GDRS_IM_BUFFER_SUBSCRIPTION_INFO(const GDRS_IM_BUFFER_SUBSCRIPTION_INFO &);
  GDRS_IM_BUFFER_SUBSCRIPTION_INFO &operator=(const GDRS_IM_BUFFER_SUBSCRIPTION_INFO &);

};

class GDRS_IM_CLIENT_SUBSCRIPTION_INFO
{
public:
  GDRS_IM_CLIENT_SUBSCRIPTION_INFO ();
  ~GDRS_IM_CLIENT_SUBSCRIPTION_INFO ();
  int subscription_type;
  int poll_interval_millis;
  double last_sub_sent_time;
  int subscription_list_id;
  int buffer_number;
  int subdiv;
  int subscription_paused;
  int last_id_read;
  int subscription_id;
  class CLIENT_GDRS_IM_PORT *clnt_port;
  class GDRS_IM_BUFFER_SUBSCRIPTION_INFO *sub_buf_info;

private:
  //Prevent copying.
  GDRS_IM_CLIENT_SUBSCRIPTION_INFO(const GDRS_IM_CLIENT_SUBSCRIPTION_INFO &);
  GDRS_IM_CLIENT_SUBSCRIPTION_INFO &operator=(const GDRS_IM_CLIENT_SUBSCRIPTION_INFO &);

};

class CLIENT_GDRS_IM_PORT
{
public:
  CLIENT_GDRS_IM_PORT ();
  int subscription_id;
  int last_read_id;
  unsigned long serial_number;
  int errors, max_errors;
  struct sockaddr_in address;
  int socket_fd;
  class RCS_LINKED_LIST *subscriptions;
  struct msghdr reply_message_header;
  struct dl_sa *client_socket_address;
  struct iovec reply_iov2[2];
  int client_list_id;

#if MS_WINDOWS_API
  DWORD tid;
  DWORD pid;
#else
#ifdef VXWORKS
  int tid;
  int pid;
#else
  pid_t tid;
  pid_t pid;
#endif
#endif
  int blocking;

private:
  CLIENT_GDRS_IM_PORT(const CLIENT_GDRS_IM_PORT &_cup);
  CLIENT_GDRS_IM_PORT &operator=(const CLIENT_GDRS_IM_PORT &_cup);
};


int gdrs_im_server_count = 0;

CMS_SERVER_REMOTE_GDRS_IM_PORT::CMS_SERVER_REMOTE_GDRS_IM_PORT (CMS_SERVER * _cms_server):
  CMS_SERVER_REMOTE_PORT (_cms_server),
  dtimeout(0),
#ifndef VXWORKS
  read_fd_set(), write_fd_set(),
#endif
  maxfdpl(0),
  request_length(0),
  client_ports(0),
  connection_socket(0),
  connection_port(0),
  ptr_to_server_socket_address(0),
  request(0),
  server(0),
  ptr_to_client_address(0),
  ptr_to_message_header(0),
  client_addresslen(0),
  polling_period(0),
  ptr_to_iov2(0),
  subscription_buffers(0),
  current_poll_interval_millis(0),
  polling_enabled(0),
  ptr_to_select_timeout(0),
  last_subscription_id(0),
  broadcast_ports(0),
  request_header_size(0),
  reply_header_size(0),
  ptr_to_broadcast_address(0),
  broadcast_address_set(0),
#ifndef VXWORKS
  broadcast_server_host_entry(0),
#endif
  broadcast_subscriptions(0),
  sockerrno(0),
  sockerrstr(0)
{
  client_ports = (RCS_LINKED_LIST *) NULL;
  gdrs_im_server_count++;
  dtimeout = -1.0;
  last_subscription_id = 1000 * gdrs_im_server_count + 1;
  connection_socket = 0;
  broadcast_address_set = 0;
  connection_port = 0;
  maxfdpl = 0;
  polling_enabled = 0;
  ptr_to_message_header=0;
  ptr_to_select_timeout=0;
  ptr_to_server_socket_address=0;
  ptr_to_broadcast_address=0;
  ptr_to_client_address=0;

  if(getenv("IPV6") || getenv("NML_IPV6") || getenv("CMS_IPV6"))
    {
      use_ipv6=1;
    }

  ptr_to_select_timeout = new struct timeval;

  ptr_to_message_header = new struct msghdr;
  memset (ptr_to_message_header, 0, sizeof (struct msghdr));
  ((struct msghdr*)ptr_to_message_header)->msg_name = (caddr_t) NULL;
  ((struct msghdr*)ptr_to_message_header)->msg_namelen = 0;
  ((struct msghdr*)ptr_to_message_header)->msg_iov = (struct iovec *) NULL;
  ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 0;
  strcpy (temp_buffer, "UNINITIALIZED");

  ptr_to_server_socket_address = dl_create_sa(0,0,use_ipv6);
  broadcast_subscriptions = 0;
  broadcast_ports = NULL;

  subscription_buffers = NULL;
  client_ports = new RCS_LINKED_LIST;
  if (NULL == client_ports)
    {
      rcs_print_error ("Can not create linked list for client ports.\n");
      return;
    }
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      return;
    }

  request_header_size = 20;
  if ((min_compatible_version > 3.13 || min_compatible_version < 1e-6))
    {
      request_header_size = 24;
    }
  reply_header_size = 20;
}

CMS_SERVER_REMOTE_GDRS_IM_PORT::~CMS_SERVER_REMOTE_GDRS_IM_PORT ()
{
  unregister_port ();
  if (NULL != client_ports)
    {
      delete client_ports;
      client_ports = (RCS_LINKED_LIST *) NULL;
    }
  if (NULL != broadcast_ports)
    {
      GDRS_IM_BROADCAST_DATA *broadcast_data = (GDRS_IM_BROADCAST_DATA *)
	broadcast_ports->get_head ();
      while (NULL != broadcast_data)
	{
	  delete broadcast_data;
	  broadcast_ports->delete_current_node ();
	  broadcast_data = (GDRS_IM_BROADCAST_DATA *)
	    broadcast_ports->get_next ();
	}
      delete broadcast_ports;
      broadcast_ports = NULL;
    }
  if (NULL != subscription_buffers)
    {
      delete subscription_buffers;
      subscription_buffers = NULL;
    }
  if(ptr_to_server_socket_address)
    {
      dl_free_sa(ptr_to_server_socket_address);
      ptr_to_server_socket_address=0;
    }
  if(ptr_to_client_address)
    {
      dl_free_sa(ptr_to_client_address);
      ptr_to_client_address=0;
    }
  if(ptr_to_message_header)
    {
      delete ptr_to_message_header;
      ptr_to_message_header=0;
    }
  if(ptr_to_iov2)
    {
      delete ptr_to_iov2;
      ptr_to_iov2=0;
    }
  if(ptr_to_broadcast_address)
    {
      delete ptr_to_broadcast_address;
      ptr_to_broadcast_address=0;
    }
  if(ptr_to_select_timeout)
    {
      delete ptr_to_select_timeout;
      ptr_to_select_timeout=0;
    }      
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::unregister_port ()
{
  if (connection_socket > 0)
    {
      dl_closesocket (connection_socket);
      connection_socket = 0;
    }
}


void
CMS_SERVER_REMOTE_GDRS_IM_PORT::set_broadcast_address (CMS * _cms)
{
  if(0 == ptr_to_broadcast_address)
    {
      ptr_to_broadcast_address = new struct sockaddr_in;
    }
  memset (ptr_to_broadcast_address, 0, sizeof (struct sockaddr_in));
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_family = AF_INET;
  hton_uint32_array_set(&(((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr),0,(unsigned long) INADDR_ANY);
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_port = 0;
  char localhostname[80];
  if (dl_gethostname (localhostname, 80) < 0)
    {
      if (strcmp (_cms->BufferHost, "localhost") != 0)
	{
	  strncpy (localhostname, _cms->BufferHost, 80);
	}
      else
	{
	  strncpy (localhostname, _cms->ProcessHost, 80);
	}
    }
#ifndef VXWORKS
  dl_modified_gethostbyname (localhostname, &broadcast_server_host_entry,1);
  if (NULL == broadcast_server_host_entry)
    {
      rcs_print_error ("GDRS_IMMEM: Couldn't get host address for (%s).\n",
		       localhostname);
      return;
    }
#ifdef __MSDOS__
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr =
    *((u_long *) broadcast_server_host_entry->h_addr_list[0]);
#else
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr =
    *((int *) broadcast_server_host_entry->h_addr_list[0]);
#endif
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_family = broadcast_server_host_entry->h_addrtype;
#else
  ((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr = hostGetByName (localhostname);
  if (((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr == 
      ((unsigned)ERROR))
    {
      rcs_print_error ("GDRS_IMMEM: Couldn't get host address for (%s).\n",
		       localhostname);
      return;
    }
#endif
  hton_uint32_array_set(&((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr,
			0,
			ntoh_uint32_array_get(&((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr,0) | 0xFF);
  
  //((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr.s_addr |= dl_hton l (0xff);
  rcs_print_debug (PRINT_SOCKET_CONNECT, "Broadcasting to IP address %s.\n",
		   dl_inet_ptr_ntoa (&((struct sockaddr_in*)ptr_to_broadcast_address)->sin_addr));
  broadcast_address_set = 1;
}

int
CMS_SERVER_REMOTE_GDRS_IM_PORT::accept_local_port_cms (CMS * _cms)
{
  int retval = 0;
  if (NULL == _cms)
    {
      return 0;
    }
  if (_cms->remote_port_type != CMS_GDRS_IM_REMOTE_PORT_TYPE)
    {
      return 0;
    }

  if (NULL != _cms)
    {
      if (min_compatible_version < 1e-6 ||
	  (min_compatible_version > _cms->min_compatible_version &&
	   _cms->min_compatible_version > 1e-6))
	{
	  min_compatible_version = _cms->min_compatible_version;
	}
      if (_cms->confirm_write)
	{
	  confirm_write = _cms->confirm_write;
	}
    }

  if (dl_sa_get_port(ptr_to_server_socket_address) == 0)
    {
      if(ptr_to_server_socket_address && use_ipv6 != _cms->use_ipv6)
	{
	  dl_free_sa(ptr_to_server_socket_address);
	  ptr_to_server_socket_address=0;
	}
      if(!ptr_to_server_socket_address)
	{
	  ptr_to_server_socket_address = dl_create_sa(0,_cms->gdrs_im_port_number, _cms->use_ipv6);
	}
      use_ipv6 =  _cms->use_ipv6;
      dl_sa_set_port(ptr_to_server_socket_address, _cms->gdrs_im_port_number);
      connection_port = _cms->gdrs_im_port_number;
      set_broadcast_address (_cms);
      char *broadcast_clnt_port_eq =
	strstr (_cms->buflineupper, "BROADCAST_PORT=");
      if (broadcast_clnt_port_eq != NULL)
	{
	  broadcast_subscriptions = 1;
	  int broadcast_clnt_port =
	    strtol (broadcast_clnt_port_eq + 15, 0, 0);
	  if (_cms->gdrs_im_port_number == broadcast_clnt_port)
	    {
	      rcs_print_error
		("Can't broadcast on the same port used to accept requests. (%d)\n",
		 _cms->gdrs_im_port_number);
	      return -1;
	    }
	  GDRS_IM_BROADCAST_DATA *broadcast_data = new GDRS_IM_BROADCAST_DATA ();
	  memcpy (&(broadcast_data->broadcast_address), ptr_to_broadcast_address,
		  sizeof (sockaddr_in));
	  broadcast_data->buffer_number = _cms->buffer_number;
	  broadcast_data->broadcast_clnt_port = broadcast_clnt_port;
	  broadcast_data->broadcast_address.sin_port =
	    dl_htons ((u_short) broadcast_data->broadcast_clnt_port);
	  if (NULL == broadcast_ports)
	    {
	      broadcast_ports = new RCS_LINKED_LIST ();
	    }
	  broadcast_ports->store_at_tail (broadcast_data,
					  sizeof (GDRS_IM_BROADCAST_DATA), 0);
	}
      return 1;
    }
  else if (dl_sa_get_port(ptr_to_server_socket_address) ==
	   _cms->gdrs_im_port_number)
    {
      int new_broadcast_subscriptions = 0;
      int new_broadcast_clnt_port = 0;
      char *broadcast_clnt_port_eq =
	strstr (_cms->buflineupper, "BROADCAST_PORT=");
      if (broadcast_clnt_port_eq != NULL)
	{
	  new_broadcast_subscriptions = 1;
	  new_broadcast_clnt_port =
	    strtol (broadcast_clnt_port_eq + 15, 0, 0);
	}
      if (new_broadcast_subscriptions)
	{
	  if (_cms->gdrs_im_port_number == new_broadcast_clnt_port)
	    {
	      rcs_print_error
		("Can't broadcast on the same port used to accept requests. (%d)\n",
		 _cms->gdrs_im_port_number);
	      return -1;
	    }
	  if (NULL != broadcast_ports)
	    {
	      GDRS_IM_BROADCAST_DATA *broadcast_data = (GDRS_IM_BROADCAST_DATA *)
		broadcast_ports->get_head ();
	      while (NULL != broadcast_data)
		{
		  if (broadcast_data->buffer_number == _cms->buffer_number)
		    {
		      return 1;
		    }
		  if (broadcast_data->broadcast_clnt_port ==
		      new_broadcast_clnt_port)
		    {
		      rcs_print_error
			("Can't broadcast data for both buffer %d and  buffer %ld on port %d.\n",
			 broadcast_data->buffer_number, _cms->buffer_number,
			 new_broadcast_clnt_port);
		      return -1;
		    }
		  broadcast_data = (GDRS_IM_BROADCAST_DATA *)
		    broadcast_ports->get_next ();
		}
	    }
	  if (!broadcast_address_set)
	    {
	      set_broadcast_address (_cms);
	    }
	  GDRS_IM_BROADCAST_DATA *broadcast_data = new GDRS_IM_BROADCAST_DATA ();
	  memcpy (&(broadcast_data->broadcast_address), ptr_to_broadcast_address,
		  sizeof (sockaddr_in));
	  broadcast_data->buffer_number = _cms->buffer_number;
	  broadcast_data->broadcast_clnt_port = new_broadcast_clnt_port;
	  broadcast_data->broadcast_address.sin_port =
	    dl_htons ((u_short) broadcast_data->broadcast_clnt_port);
	  if (NULL == broadcast_ports)
	    {
	      broadcast_ports = new RCS_LINKED_LIST ();
	    }
	  broadcast_ports->store_at_tail (broadcast_data,
					  sizeof (GDRS_IM_BROADCAST_DATA), 0);
	}
      port_num = _cms->gdrs_im_port_number;
      return 1;
    }


  return retval;
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::register_port ()
{
  port_registered = 0;
  if (dl_sa_get_port(ptr_to_server_socket_address) == 0)
    {
      rcs_print_error ("server can not register on port number 0.\n");
      return;
    }
  if ((connection_socket = dl_udp_socket(use_ipv6)) < 0)
    {
      rcs_print_error ("Server can not open stream socket.\n");
      return;
    }

  if (set_udp_socket_options (connection_socket) < 0)
    {
      return;
    }
  if (broadcast_subscriptions)
    {
      if (make_udp_socket_broadcast (connection_socket) < 0)
	{
	  return;
	}
    }
  if (dl_bind (connection_socket, dl_sa_addr(ptr_to_server_socket_address),
	       dl_sa_len(ptr_to_server_socket_address)) < 0)
    {
      rcs_print_error ("Server can not bind the connection socket.\n");
      return;
    }
  port_registered = 1;
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::run ()
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
  server = find_server (pid, tid);
  if (NULL == server)
    {
      rcs_print_error ("Can`t run. (server = NULL)\n");
      exit (-1);
    }
  if (NULL == server->write_req_ptr || NULL == server->write_req_ptr->data)
    {
      rcs_print_error ("Can`t run. (server->write_req_ptr->data = NULL)\n");	/*  */
      server->clean (2);
      exit (-1);
    }
  if (NULL == client_ports)
    {
      rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
      server->clean (2);
      exit (-1);
    }
  ptr_to_client_address = dl_create_sa(0,0,use_ipv6);
  ((struct msghdr*)ptr_to_message_header)->msg_name = (caddr_t) 
    dl_sa_addr(ptr_to_client_address);
  ((struct msghdr*)ptr_to_message_header)->msg_namelen = client_addresslen = 
    dl_sa_len(ptr_to_client_address);

  request_header_size = 20;
  if ((min_compatible_version > 3.13 || min_compatible_version < 1e-6))
    {
      request_header_size = 24;
    }
  if ((min_compatible_version > 3.43 || min_compatible_version < 1e-6))
    {
      request_header_size = 28;
    }
  cms_server_count++;

  ptr_to_iov2 = new struct iovec[2];

  double last_recv_time =etime();
  double cur_time=etime();
  double dtimeout2;
 
  while (1)
    {
      ptr_to_iov2[0].iov_base = temp_buffer;
      ptr_to_iov2[0].iov_len = request_header_size;
      ptr_to_iov2[1].iov_base = (char *) server->write_req_ptr->data;
      ptr_to_iov2[1].iov_len = server->maximum_cms_size * 4;
      ((struct msghdr*)ptr_to_message_header)->msg_iov = ptr_to_iov2;
      ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 2;
      if (polling_enabled)
	{
	  cur_time=etime();
	  dtimeout2 = dtimeout - (cur_time - last_recv_time);
	  if(dtimeout2 < 0.0005 || dtimeout2 > dtimeout)
	    {
	      dtimeout2=dtimeout;
	    }
	  request_length = recvmsgtq (connection_socket,
				      ptr_to_message_header, 0, dtimeout2);
	  last_recv_time=etime();
	  //printf("dtimeot2-%f,diff=%f\n",dtimeout2,(last_recv_time-cur_time));
	  if (request_length == 0 && polling_enabled)
	    {
	      update_subscriptions ();
	      continue;
	    }
	}
      else
	{
	  request_length = recvmsgt (connection_socket,
				     ptr_to_message_header, 0, -1.0);
	}
      if (request_length < 0)
	{
	  sockerrno = dl_get_last_socket_error_int( connection_socket ); 
	  sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("server: recvmsg error: %d %s\n",
			   sockerrno,sockerrstr);
	  continue;
	}

      if (request_length < request_header_size)
	{
	  rcs_print_error
	    ("server: recvmsg error: recieved only %d bytes when atleast %d were expected.\n",
	     request_length, request_header_size);
	  continue;
	}
      handle_request ();
      if (polling_enabled)
	{
	  update_subscriptions ();
	}
    }
}

CLIENT_GDRS_IM_PORT *
CMS_SERVER_REMOTE_GDRS_IM_PORT::get_client_port (struct dl_sa *cli_addr)
{
  if (NULL == client_ports)
    {
      client_ports = new RCS_LINKED_LIST ();
    }
  CLIENT_GDRS_IM_PORT *temp_port = NULL;
  temp_port = (CLIENT_GDRS_IM_PORT *) client_ports->get_head ();
  while (NULL != temp_port)
    {
      if (!dl_sa_compare(temp_port->client_socket_address,cli_addr))
	{
	  return temp_port;
	}
      temp_port = (CLIENT_GDRS_IM_PORT *) client_ports->get_next ();
    }
  temp_port = new CLIENT_GDRS_IM_PORT ();
  
  dl_sa_copy(&(temp_port->client_socket_address), cli_addr);
  temp_port->client_list_id =
    client_ports->store_at_tail (temp_port, sizeof (CLIENT_GDRS_IM_PORT), 0);
  return temp_port;
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::handle_request ()
{
  u_long request_type, buffer_number, subdiv, serial_number;
  long reply_length;
  request_type = ntoh_uint32_array_get(temp_buffer,0);
  buffer_number = ntoh_uint32_array_get(temp_buffer,1);
  serial_number = ntoh_uint32_array_get(temp_buffer,2);

  subdiv = 0;
  if (min_compatible_version > 3.43 || min_compatible_version < 1e-6)
    {
      subdiv = ntoh_uint32_array_get(temp_buffer,6);
    }
  hton_uint32_array_set(temp_buffer,0,serial_number);
  ptr_to_iov2[0].iov_base = temp_buffer;
  ptr_to_iov2[0].iov_len = reply_header_size;

  switch (request_type)
    {
    case REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE:
      {
	REMOTE_GET_BUF_NAME_REQUEST namereq;
	namereq.buffer_number = buffer_number;
	REMOTE_GET_BUF_NAME_REPLY *namereply = NULL;
	namereply =
	  (REMOTE_GET_BUF_NAME_REPLY *) server->process_request (&namereq);
	memset (temp_buffer, 0, 32);
	if (NULL == namereply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == namereply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    hton_uint32_array_set(temp_buffer,3,(unsigned long) 0);
	    hton_uint32_array_set(temp_buffer,4,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	long name_data_len = strlen (namereply->name) + 1;
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) namereply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) name_data_len);
	ptr_to_iov2[1].iov_base = (char *) namereply->name;
	ptr_to_iov2[1].iov_len = name_data_len;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 2;
	reply_length = sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	if (reply_length != name_data_len + 20)
	  {
	    rcs_print_error ("reply_length = %ld, name_data_len=%ld\n",
			     reply_length, name_data_len);
	  }
      }
      break;


    case REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE:
      {
	int subscription_type = ntoh_uint32_array_get(temp_buffer,3);
	int poll_interval_millis = ntoh_uint32_array_get(temp_buffer,4);
	int last_read_id = ntoh_uint32_array_get(temp_buffer,5);
	CLIENT_GDRS_IM_PORT *new_clnt_port = get_client_port (ptr_to_client_address);
	dl_sa_copy(&(new_clnt_port->client_socket_address),
		   ptr_to_client_address);
	memcpy (&(new_clnt_port->reply_message_header), ptr_to_message_header,
		sizeof (struct msghdr));
	new_clnt_port->reply_message_header.msg_name = (caddr_t)
	  dl_sa_addr(new_clnt_port->client_socket_address);
	new_clnt_port->reply_message_header.msg_namelen =
	  dl_sa_len(new_clnt_port->client_socket_address);

	new_clnt_port->reply_iov2[0].iov_base = temp_buffer;
	new_clnt_port->reply_iov2[0].iov_len = 20;
	new_clnt_port->reply_iov2[1].iov_base =
	  (char *) server->write_req_ptr->data;
	new_clnt_port->reply_iov2[1].iov_len = server->maximum_cms_size * 4;
	new_clnt_port->reply_message_header.msg_iov =
	  new_clnt_port->reply_iov2;
	new_clnt_port->reply_message_header.msg_iovlen = 2;
	new_clnt_port->serial_number = serial_number + 1;
	new_clnt_port->last_read_id = last_read_id;
	add_subscription_client (buffer_number, subdiv,
				 subscription_type,
				 poll_interval_millis, new_clnt_port);

	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) 1);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) new_clnt_port->subscription_id);
	new_clnt_port->reply_iov2[1].iov_base = 0;
	new_clnt_port->reply_iov2[1].iov_len = 0;
	new_clnt_port->reply_message_header.msg_iovlen = 1;
	reply_length =
	  sendmsgt (connection_socket, &(new_clnt_port->reply_message_header),
		    0, -1.0);
	if (reply_length != 20)
	  {
	    rcs_print_error ("reply_length = %ld, (expected 20) \n",
			     reply_length);
	  }
      }
      break;

    case REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE:
      {
	int subscription_id = ntoh_uint32_array_get(temp_buffer,3);

	remove_subscription (subscription_id, buffer_number, subdiv);
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) 1);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) subscription_id);
	ptr_to_iov2[1].iov_base = 0;
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	reply_length =
	  sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	if (reply_length != 20)
	  {
	    rcs_print_error ("reply_length = %ld, (expected 20) \n",
			     reply_length);
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
	server->read_req_ptr->access_type = ntoh_uint32_array_get(temp_buffer,3);
	server->read_req_ptr->last_id_read =  ntoh_uint32_array_get(temp_buffer,4);
	server->read_req_ptr->subdiv = subdiv;
	REMOTE_READ_REPLY *read_reply =
	  (REMOTE_READ_REPLY *) server->process_request (server->read_req_ptr);
	if (NULL == read_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == read_reply)\n");

	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    hton_uint32_array_set(temp_buffer,3,(unsigned long) 0);
	    hton_uint32_array_set(temp_buffer,4,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) read_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) read_reply->size);
	hton_uint32_array_set(temp_buffer,3,(unsigned long) read_reply->write_id);
	hton_uint32_array_set(temp_buffer,4,(unsigned long) read_reply->was_read);
	ptr_to_iov2[1].iov_base = (char *) read_reply->data;
	ptr_to_iov2[1].iov_len = read_reply->size;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 2;
	reply_length = sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	if (reply_length != read_reply->size + 20)
	  {
	    rcs_print_error
	      ("reply_length = %ld, read_reply->size+20=%d\n",
	       reply_length, read_reply->size + 20);
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
	server->write_req_ptr->access_type = ntoh_uint32_array_get(temp_buffer,3);
	server->write_req_ptr->size =  ntoh_uint32_array_get(temp_buffer,4);
	server->write_req_ptr->subdiv = subdiv;
	REMOTE_WRITE_REPLY *write_reply =
	  (REMOTE_WRITE_REPLY *) server->process_request (server->write_req_ptr);
	if (request_length < server->write_req_ptr->size + 20)
	  {
	    rcs_print_error
	      ("request_length = %d, server->write_req_ptr->size+20 = %d\n",
	       request_length, server->write_req_ptr->size + 20);
	  }
	if (NULL == write_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == write_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	if (write_reply->confirm_write)
	  {
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) write_reply->status);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) write_reply->was_read);
	    ptr_to_iov2[1].iov_len = 0;
	    ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
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
	server->check_if_read_req_ptr->subdiv = subdiv;
	REMOTE_CHECK_IF_READ_REPLY *check_if_read_reply = (REMOTE_CHECK_IF_READ_REPLY *)
	  server->process_request (server->check_if_read_req_ptr);
	if (NULL == check_if_read_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == check_if_read_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }

	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) check_if_read_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) check_if_read_reply->was_read);
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
      }
      break;


    case REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE:
      {
	if(server->get_msg_count_req_ptr == 0)
	  {
	    server->get_msg_count_req_ptr = new REMOTE_GET_MSG_COUNT_REQUEST();
	  }
	server->get_msg_count_req_ptr->buffer_number = buffer_number;
	server->get_msg_count_req_ptr->subdiv = subdiv;
	REMOTE_GET_MSG_COUNT_REPLY *get_msg_count_reply = (REMOTE_GET_MSG_COUNT_REPLY *)
	  server->process_request (server->get_msg_count_req_ptr);
	if (NULL == get_msg_count_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == get_msg_count_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) get_msg_count_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) get_msg_count_reply->count);
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
      }
      break;

    case REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE:
      {
	if(server->get_queue_length_req_ptr == 0)
	  {
	    server->get_queue_length_req_ptr = new REMOTE_GET_QUEUE_LENGTH_REQUEST();
	  }
	server->get_queue_length_req_ptr->buffer_number = buffer_number;
	server->get_queue_length_req_ptr->subdiv = subdiv;
	REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply = (REMOTE_GET_QUEUE_LENGTH_REPLY *)
	  server->process_request (server->get_queue_length_req_ptr);
	if (NULL == get_queue_length_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == get_queue_length_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) get_queue_length_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) get_queue_length_reply->queue_length);
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
      }
      break;

    case REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE:
      {
	if(server->get_space_available_req_ptr == 0)
	  {
	    server->get_space_available_req_ptr = new REMOTE_GET_SPACE_AVAILABLE_REQUEST();
	  }
	server->get_space_available_req_ptr->buffer_number = buffer_number;
	server->get_space_available_req_ptr->subdiv = subdiv;
	REMOTE_GET_SPACE_AVAILABLE_REPLY *get_space_available_reply = (REMOTE_GET_SPACE_AVAILABLE_REPLY *)
	  server->process_request (server->get_space_available_req_ptr);
	if (NULL == get_space_available_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == get_space_available_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
	    return;
	  }
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) get_space_available_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) get_space_available_reply->space_available);
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	sendmsgt (connection_socket, ptr_to_message_header, 0, -1.0);
      }
      break;

    case REMOTE_CMS_CLEAR_REQUEST_TYPE:
      {
	if(server->clear_req_ptr == 0)
	  {
	    server->clear_req_ptr = new REMOTE_CLEAR_REQUEST();
	  }
	server->clear_req_ptr->buffer_number = buffer_number;
	server->clear_req_ptr->subdiv = subdiv;
	REMOTE_CLEAR_REPLY *clear_reply =
	  (REMOTE_CLEAR_REPLY *) server->process_request (server->clear_req_ptr);
	if (NULL == clear_reply)
	  {
	    rcs_print_error ("Server could not process request.(NULL == clear_reply)\n");
	    hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	    hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	    ptr_to_iov2[1].iov_len = 0;
	    sendmsgt (connection_socket, ptr_to_message_header, 0, -1);
	    return;
	  }
	hton_uint32_array_set(temp_buffer,0,(unsigned long) serial_number);
	hton_uint32_array_set(temp_buffer,1,(unsigned long) clear_reply->status);
	hton_uint32_array_set(temp_buffer,2,(unsigned long) 0);
	ptr_to_iov2[1].iov_len = 0;
	((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
	//sendmsgt(connection_socket, ptr_to_message_header, 0, -1);
      }
      break;

    case REMOTE_CMS_CLEAN_REQUEST_TYPE:
      server->set_spawner_pid_to_server_pid();
      server->kill_server ();
      break;

    case REMOTE_CMS_CLOSE_CHANNEL_REQUEST_TYPE:
      hton_uint32_array_set(temp_buffer,1,(unsigned long) CMS_CLEAR_OK);
      ptr_to_iov2[1].iov_len = 0;
      ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 1;
      //sendmsgt(connection_socket, ptr_to_message_header, 0, -1);
      break;

    default:
      rcs_print_error ("Unrecognized request type received.(%ld)\n",
		       request_type);
      break;
    }
}


void
CMS_SERVER_REMOTE_GDRS_IM_PORT::add_subscription_client (int buffer_number,
						     int subdiv,
						     int subscription_type,
						     int poll_interval_millis,
						     CLIENT_GDRS_IM_PORT * clnt)
{
  if (NULL == subscription_buffers)
    {
      subscription_buffers = new RCS_LINKED_LIST ();
    }
  if (NULL == subscription_buffers)
    {
      rcs_print_error ("Can`t create subscription_buffers list.\n");
    }

  GDRS_IM_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info)
    {
      if (buf_info->buffer_number == buffer_number &&
	  buf_info->subdiv == subdiv)
	{
	  break;
	}
      buf_info =
	(GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }
  if (NULL == buf_info)
    {
      buf_info = new GDRS_IM_BUFFER_SUBSCRIPTION_INFO ();
      buf_info->buffer_number = buffer_number;
      buf_info->subdiv = subdiv;
      buf_info->last_update_time = etime ();
      buf_info->sub_clnt_info = new RCS_LINKED_LIST ();
      buf_info->list_id =
	subscription_buffers->store_at_tail (buf_info, sizeof (*buf_info), 0);
      buf_info->min_last_id = clnt->last_read_id;
      if (NULL != broadcast_ports)
	{
	  GDRS_IM_BROADCAST_DATA *broadcast_data = (GDRS_IM_BROADCAST_DATA *)
	    broadcast_ports->get_head ();
	  while (NULL != broadcast_data)
	    {
	      if (broadcast_data->buffer_number == buffer_number)
		{
		  buf_info->broadcast_data = broadcast_data;
		  break;
		}
	      broadcast_data = (GDRS_IM_BROADCAST_DATA *)
		broadcast_ports->get_next ();
	    }
	}
    }
  if (buf_info->max_serial_number < clnt->serial_number)
    {
      buf_info->max_serial_number = clnt->serial_number;
    }
  if (buf_info->min_last_id > clnt->last_read_id)
    {
      buf_info->min_last_id = clnt->last_read_id;
    }
  if (NULL == clnt->subscriptions)
    {
      clnt->subscriptions = new RCS_LINKED_LIST ();
    }
  GDRS_IM_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_head ();
  while (temp_clnt_info != NULL)
    {
      if (temp_clnt_info->buffer_number == buffer_number &&
	  temp_clnt_info->subdiv == subdiv)
	{
	  break;
	}
      temp_clnt_info =
	(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_next ();
    }
  if (NULL == temp_clnt_info)
    {
      temp_clnt_info = new GDRS_IM_CLIENT_SUBSCRIPTION_INFO ();
      temp_clnt_info->last_sub_sent_time = etime ();
      temp_clnt_info->buffer_number = buffer_number;
      temp_clnt_info->subdiv = subdiv;
      temp_clnt_info->subscription_paused = 0;
      temp_clnt_info->last_id_read = 0;
      temp_clnt_info->sub_buf_info = buf_info;
      temp_clnt_info->clnt_port = clnt;
      temp_clnt_info->last_sub_sent_time = etime ();
      temp_clnt_info->subscription_list_id =
	clnt->subscriptions->store_at_tail (temp_clnt_info,
					    sizeof (*temp_clnt_info), 0);
      buf_info->sub_clnt_info->store_at_tail (temp_clnt_info,
					      sizeof (*temp_clnt_info), 0);
      temp_clnt_info->subscription_id = clnt->subscription_id =
	++last_subscription_id;
    }
  temp_clnt_info->subscription_type = subscription_type;
  temp_clnt_info->poll_interval_millis = poll_interval_millis;
  recalculate_polling_interval ();
}


void
CMS_SERVER_REMOTE_GDRS_IM_PORT::remove_subscription (int subscription_id,
						 int buffer_number,
						 int subdiv)
{
  if (NULL == subscription_buffers)
    {
      return;
    }

  GDRS_IM_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info)
    {
      if (buf_info->buffer_number == buffer_number &&
	  buf_info->subdiv == subdiv)
	{
	  break;
	}
      buf_info =
	(GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }

  if (NULL != buf_info)
    {
      if (NULL != buf_info->sub_clnt_info)
	{
	  GDRS_IM_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
	    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *)
	    buf_info->sub_clnt_info->get_head ();
	  while (temp_clnt_info != NULL)
	    {
	      if (temp_clnt_info->subscription_id == subscription_id
		  && NULL != temp_clnt_info->clnt_port)
		{
		  remove_subscription_client (temp_clnt_info->clnt_port,
					      buffer_number, subdiv);
		  break;
		}
	      if (NULL == buf_info)
		{
		  break;
		}
	      if (NULL == buf_info->sub_clnt_info)
		{
		  break;
		}
	      temp_clnt_info =
		(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *)
		buf_info->sub_clnt_info->get_next ();
	    }
	}
    }
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::remove_subscription_client (CLIENT_GDRS_IM_PORT *
							clnt,
							int buffer_number,
							int subdiv)
{
  if (NULL == clnt)
    {
      return;
    }
  if (NULL == clnt->subscriptions)
    {
      return;
    }
  GDRS_IM_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_head ();
  while (temp_clnt_info != NULL)
    {
      if (temp_clnt_info->buffer_number == buffer_number &&
	  temp_clnt_info->subdiv == subdiv)
	{
	  if (NULL != temp_clnt_info->sub_buf_info)
	    {
	      if (NULL != temp_clnt_info->sub_buf_info->sub_clnt_info)
		{
		  temp_clnt_info->sub_buf_info->sub_clnt_info->
		    delete_node (temp_clnt_info->subscription_list_id);
		  if (temp_clnt_info->sub_buf_info->sub_clnt_info->
		      list_size == 0)
		    {
		      subscription_buffers->delete_node (temp_clnt_info->
							 sub_buf_info->
							 list_id);
		      if (subscription_buffers->list_size == 0)
			{
			  delete subscription_buffers;
			  subscription_buffers = NULL;
			}
		      delete temp_clnt_info->sub_buf_info->sub_clnt_info;
		      temp_clnt_info->sub_buf_info->sub_clnt_info = NULL;
		      delete temp_clnt_info->sub_buf_info;
		      temp_clnt_info->sub_buf_info = NULL;
		    }
		}
	    }
	  clnt->subscriptions->delete_current_node ();
	  delete temp_clnt_info;
	  temp_clnt_info = NULL;
	  break;
	}
      temp_clnt_info =
	(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_next ();
    }
  temp_clnt_info =
    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_head ();
  while (temp_clnt_info != NULL)
    {
      if (temp_clnt_info->buffer_number == buffer_number &&
	  temp_clnt_info->subdiv == subdiv)
	{
	  delete temp_clnt_info;
	  temp_clnt_info = NULL;
	  break;
	}
      temp_clnt_info =
	(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_next ();
    }

  if (clnt->subscriptions->list_size == 0)
    {
      delete clnt->subscriptions;
      clnt->subscriptions = NULL;
      if (NULL != client_ports)
	{
	  client_ports->delete_node (clnt->client_list_id);
	  delete clnt;
	  clnt = NULL;
	}
      if (client_ports->list_size == 0)
	{
	  delete client_ports;
	  client_ports = NULL;
	}
    }
  recalculate_polling_interval ();
}

void
CMS_SERVER_REMOTE_GDRS_IM_PORT::recalculate_polling_interval ()
{
  int min_poll_interval_millis = 30000;
  polling_enabled = 0;
  dtimeout = -1.0;
  if (NULL == subscription_buffers)
    {
      if(0 == ptr_to_select_timeout)
	{
	  ptr_to_select_timeout = new struct timeval;
	}
      current_poll_interval_millis = min_poll_interval_millis;
      ((struct timeval*)ptr_to_select_timeout)->tv_sec = 30;
      ((struct timeval*)ptr_to_select_timeout)->tv_usec = 0;
      return;
    }
  GDRS_IM_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info)
    {
      buf_info->min_update_interval = 3600;
      GDRS_IM_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
	(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) buf_info->sub_clnt_info->get_head ();
      while (temp_clnt_info != NULL)
	{
	  if (temp_clnt_info->poll_interval_millis < min_poll_interval_millis
	      && temp_clnt_info->subscription_type == CMS_POLLED_SUBSCRIPTION)
	    {
	      min_poll_interval_millis = temp_clnt_info->poll_interval_millis;
	      polling_enabled = 1;
	    }
	  if (temp_clnt_info->poll_interval_millis / 1000.0 <
	      buf_info->min_update_interval)
	    {
	      buf_info->min_update_interval =
		temp_clnt_info->poll_interval_millis / 1000.0;
	    }
	  temp_clnt_info =
	    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *)
	    buf_info->sub_clnt_info->get_next ();
	}
      buf_info =
	(GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }
  if (min_poll_interval_millis >= ((int) (clk_tck () * 1000.0)))
    {
      current_poll_interval_millis = min_poll_interval_millis;
      ((struct timeval*)ptr_to_select_timeout)->tv_sec = current_poll_interval_millis / 1000;
      ((struct timeval*)ptr_to_select_timeout)->tv_usec = (current_poll_interval_millis % 1000) * 1000;
      dtimeout = (current_poll_interval_millis) / 1000.0;
    }
  else
    {
      current_poll_interval_millis = ((int) (clk_tck () * 1000.0));
      if (((int) (clk_tck () * 1E6) % 1000) != 0)
	{
	  current_poll_interval_millis++;
	}
      ((struct timeval*)ptr_to_select_timeout)->tv_sec = current_poll_interval_millis / 1000;
      ((struct timeval*)ptr_to_select_timeout)->tv_usec = (long) (ceil (clk_tck () * 1E6));
      dtimeout = clk_tck ();
    }
}


void
CMS_SERVER_REMOTE_GDRS_IM_PORT::update_subscriptions ()
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
  CMS_SERVER *current_svr;
  current_svr = find_server (pid, tid);
  if (NULL == current_svr)
    {
      rcs_print_error ("Cannot find server object.\n");
      return;
    }
  if (NULL == subscription_buffers)
    {
      return;
    }
  double cur_time = etime ();
  GDRS_IM_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info)
    {
      if (cur_time - buf_info->last_update_time + 0.019 <
	  buf_info->min_update_interval)
	{
	  buf_info =
	    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      buf_info->last_update_time = cur_time;
      if(current_svr->read_req_ptr == 0)
	{
	  current_svr->read_req_ptr = new REMOTE_READ_REQUEST();
	}
      current_svr->read_req_ptr->buffer_number = buf_info->buffer_number;
      current_svr->read_req_ptr->subdiv = buf_info->subdiv;
      current_svr->read_req_ptr->access_type = CMS_READ_ACCESS;
      current_svr->read_req_ptr->last_id_read = buf_info->min_last_id;
      REMOTE_READ_REPLY *read_reply =
	(REMOTE_READ_REPLY *) current_svr->process_request (current_svr->read_req_ptr);
      //printf("read_reply=%p\n",read_reply);
      if (NULL == read_reply)
	{
	  rcs_print_error ("Server could not process request.\n");
	  buf_info =
	    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      hton_uint32_array_set(temp_buffer,0,(unsigned long) 0);
      hton_uint32_array_set(temp_buffer,1,(unsigned long) read_reply->status);
      hton_uint32_array_set(temp_buffer,2,(unsigned long) read_reply->size);
      hton_uint32_array_set(temp_buffer,3,(unsigned long) read_reply->write_id);
      hton_uint32_array_set(temp_buffer,4,(unsigned long) read_reply->was_read);
      if (read_reply->size < 1)
	{
	  buf_info =
	    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      if (read_reply->write_id == buf_info->min_last_id)
	{
	  buf_info =
	    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      buf_info->min_last_id = read_reply->write_id;
      if (buf_info->broadcast_data != NULL)
	{
	  buf_info->max_serial_number++;
	  hton_uint32_array_set(temp_buffer,0,(unsigned long) buf_info->max_serial_number);
	  ptr_to_iov2[0].iov_len = reply_header_size;
	  ptr_to_iov2[1].iov_base = (caddr_t) read_reply->data;
	  ptr_to_iov2[1].iov_len = read_reply->size;
	  ((struct msghdr*)ptr_to_message_header)->msg_iovlen = 2;
	  ((struct msghdr*)ptr_to_message_header)->msg_name =
	    (caddr_t) & (buf_info->broadcast_data->broadcast_address);
	  if (sendmsgt (connection_socket, ptr_to_message_header, 0, dtimeout) < 0)
	    {
	      //rcs_print_sys_error(ERRNO_ERROR_SOURCE,"sendmsg error");
	    }
	  buf_info =
	    (GDRS_IM_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  ((struct msghdr*)ptr_to_message_header)->msg_name = (caddr_t) 
	    dl_sa_addr(ptr_to_client_address);
	  ((struct msghdr*)ptr_to_message_header)->msg_namelen = 
	    dl_sa_len(ptr_to_client_address);
	  continue;
	}
      GDRS_IM_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
	(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *) buf_info->sub_clnt_info->get_head ();
      while (temp_clnt_info != NULL)
	{
	  if (NULL == temp_clnt_info->clnt_port)
	    {
	      temp_clnt_info =
		(GDRS_IM_CLIENT_SUBSCRIPTION_INFO *)
		buf_info->sub_clnt_info->get_next ();
	      continue;
	    }
	  double time_diff = cur_time - temp_clnt_info->last_sub_sent_time;
	  int time_diff_millis = (int) ((double) time_diff * 1000.0);
	  rcs_print_debug (PRINT_SERVER_SUBSCRIPTION_ACTIVITY,
			   "Subscription time_diff_millis=%d\n",
			   time_diff_millis);
	  if (((temp_clnt_info->subscription_type == CMS_POLLED_SUBSCRIPTION
		&& time_diff_millis +19 >=
		temp_clnt_info->poll_interval_millis)
	       || temp_clnt_info->subscription_type ==
	       CMS_VARIABLE_SUBSCRIPTION)
	      && temp_clnt_info->last_id_read != read_reply->write_id)
	    {
	      temp_clnt_info->last_id_read = read_reply->write_id;
	      temp_clnt_info->last_sub_sent_time = cur_time;
	      temp_clnt_info->clnt_port->serial_number++;
	      CLIENT_GDRS_IM_PORT *cup = temp_clnt_info->clnt_port;
	      hton_uint32_array_set(temp_buffer,0,(unsigned long) cup->serial_number);
	      temp_clnt_info->clnt_port->reply_iov2[1].iov_base =
		(caddr_t) read_reply->data;
	      temp_clnt_info->clnt_port->reply_iov2[1].iov_len =
		read_reply->size;
	      temp_clnt_info->clnt_port->reply_message_header.msg_iovlen = 2;
	      if (sendmsgt
		  (connection_socket,
		   &(temp_clnt_info->clnt_port->reply_message_header), 0,
		   dtimeout) < 0)
		{
		  temp_clnt_info->clnt_port->errors++;
		  return;
		}
	    }
	  if (temp_clnt_info->last_id_read < buf_info->min_last_id)
	    {
	      buf_info->min_last_id = temp_clnt_info->last_id_read;
	    }
	  // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
	  if (temp_clnt_info->clnt_port->serial_number >
	      buf_info->max_serial_number)
	    {
	      buf_info->max_serial_number =
		temp_clnt_info->clnt_port->serial_number;
	    }
	  temp_clnt_info =
	    (GDRS_IM_CLIENT_SUBSCRIPTION_INFO *)
	    buf_info->sub_clnt_info->get_next ();
	}
      buf_info =
	(GDRS_IM_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }
}


GDRS_IM_BUFFER_SUBSCRIPTION_INFO::GDRS_IM_BUFFER_SUBSCRIPTION_INFO ():
  buffer_number(0),
  subdiv(0),
  min_last_id(0),
  max_serial_number(0),
  list_id(0),
  last_update_time(0),
  min_update_interval(0),
  broadcast_data(0),
  sub_clnt_info(0)
{
  buffer_number = -1;
  subdiv = 0;
  min_last_id = 0;
  list_id = -1;
  sub_clnt_info = NULL;
  max_serial_number = 0;
  broadcast_data = NULL;
  min_update_interval = 0.0;
  last_update_time = 0.0;
}

GDRS_IM_BUFFER_SUBSCRIPTION_INFO::~GDRS_IM_BUFFER_SUBSCRIPTION_INFO ()
{
  buffer_number = -1;
  subdiv = 0;
  min_last_id = 0;
  list_id = -1;
  if (NULL != sub_clnt_info)
    {
      delete sub_clnt_info;
      sub_clnt_info = NULL;
    }
}

GDRS_IM_CLIENT_SUBSCRIPTION_INFO::GDRS_IM_CLIENT_SUBSCRIPTION_INFO ():
  subscription_type(0),
  poll_interval_millis(0),
  last_sub_sent_time(0),
  subscription_list_id(0),
  buffer_number(0),
  subdiv(0),
  subscription_paused(0),
  last_id_read(0),
  subscription_id(0),
  clnt_port(0),
  sub_buf_info(0)
{
  subscription_type = CMS_NO_SUBSCRIPTION;
  poll_interval_millis = 30000;
  last_sub_sent_time = 0.0;
  subscription_list_id = -1;
  buffer_number = -1;
  subdiv = 0;
  subscription_paused = 0;
  last_id_read = 0;
  sub_buf_info = NULL;
  clnt_port = NULL;
}

GDRS_IM_CLIENT_SUBSCRIPTION_INFO::~GDRS_IM_CLIENT_SUBSCRIPTION_INFO ()
{
  subscription_type = CMS_NO_SUBSCRIPTION;
  poll_interval_millis = 30000;
  last_sub_sent_time = 0.0;
  subscription_list_id = -1;
  buffer_number = -1;
  subdiv = 0;
  subscription_paused = 0;
  last_id_read = 0;
  sub_buf_info = NULL;
  clnt_port = NULL;
}


CLIENT_GDRS_IM_PORT::CLIENT_GDRS_IM_PORT ():
    subscription_id(0),
    last_read_id(0),
    serial_number(0),
    errors(0), max_errors(0),
#ifndef VXWORKS
    address(),
#endif
    socket_fd(0),
    subscriptions(0),
#ifndef VXWORKS
    reply_message_header(),
    client_socket_address(0),
#endif
    client_list_id(0),
    tid(0),pid(0),
    blocking(0)
{
  serial_number = 0;
  errors = 0;
  max_errors = 50;
  address.sin_port = 0;
  address.sin_family = AF_INET;
  hton_uint32_array_set(&address.sin_addr.s_addr,0,(unsigned long) INADDR_ANY);
  socket_fd = -1;
  subscriptions = NULL;
  tid = 0;
  pid = 0;
}

