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

/****************************************************************************
* File: stcpsvr.cc
* Purpose: Provides the functions for the class CMS_SERVER_REMOTE_STCP_PORT
*  which provides STCP specific overrides of the CMS_SERVER_REMOTE_PORT class.
****************************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "stcpsvr_no_config.h"
#endif

#include "sokintrf.h"		/* dl_ioctl() */
#include "timer.hh"		// esleep()
#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE
#include "cms.hh"		/* class CMS */
#include "cms_srv.hh"		// class CMS_SERVER
#include "stcpsvr.hh"		/* class CMS_SERVER_REMOTE_STCP_PORT */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "stcpopts.hh"		/* SET_STCP_NODELAY */

#include "rem_msg.hh"

extern "C"
{
#include "recvline.h"		/* recvn() */
#include "sendline.h"		/* sendn() */
}

class CLIENT_STCP_PORT
{
public:
  CLIENT_STCP_PORT ():
    errors(0),max_errors(50),
#ifndef VXWORKS
    address(0),
#endif
    socket_fd(-1),last_write_id(-1),
#ifdef MS_WINDOWS_API
    tid(0),
#endif
    pid(0)
  {
    errors = 0;
    max_errors = 50;
    last_write_id = -1;
    socket_fd = -1;
  }

  ~CLIENT_STCP_PORT () {
    if(address)
      {
	dl_free_sa(address);
	address=0;
      }
    if(socket_fd > 0)
      {
	dl_closesocket (socket_fd);
	socket_fd=-1;
      }
  }

  int errors, max_errors;
  struct dl_sa *address;
  int socket_fd;
  int last_write_id;
#if MS_WINDOWS_API
  unsigned long tid;
  unsigned long pid;
#else
#ifdef VXWORKS
  int pid;
#else
  pid_t pid;
#endif
#endif
private:
  CLIENT_STCP_PORT (const CLIENT_STCP_PORT &);
  CLIENT_STCP_PORT &operator=(const CLIENT_STCP_PORT &);
  
};

static const char *stcp_xml_end_string="-->]]><*END*>";
#define STCP_XML_END_STRING_LENGTH (13)

CMS_SERVER_REMOTE_STCP_PORT::CMS_SERVER_REMOTE_STCP_PORT (CMS_SERVER *
							  _cms_server):
  CMS_SERVER_REMOTE_PORT (_cms_server),
#ifndef VXWORKS
  read_fd_set(),write_fd_set(),
#endif
  maxfdpl(0),client_ports(0),connection_socket(0),
  buffer_number(0),connection_port(0),server_socket_address_ptr(0),
  request(0),temp_buffer(0),temp_buffer_size(0),
  handle_request_error_occured(0),sockerrno(0),sockerrstr(0)
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_STCP_PORT::CMS_SERVER_REMOTE_STCP_PORT (CMS_SERVER *_cms_server=%p) called. this=%p\n",
		  (void*)_cms_server,
		  (void*)this);

  client_ports = (RCS_LINKED_LIST *) NULL;
  connection_socket = 0;
  connection_port = 0;
  maxfdpl = 0;
  temp_buffer = NULL;
  temp_buffer_size = 0;
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      return;
    }
  server_socket_address_ptr = dl_create_sa(0,0,use_ipv6);
  client_ports = new RCS_LINKED_LIST;
  if (NULL == client_ports)
    {
      rcs_print_error ("Can not create linked list for client ports.\n");
      return;
    }

}

CMS_SERVER_REMOTE_STCP_PORT::~CMS_SERVER_REMOTE_STCP_PORT ()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_STCP_PORT::~CMS_SERVER_REMOTE_STCP_PORT() called. this=%p\n",
		  (void*)this);

  unregister_port ();
  if (NULL != client_ports)
    {
      delete client_ports;
      client_ports = (RCS_LINKED_LIST *) NULL;
    }
  if (NULL != temp_buffer)
    {
      delete temp_buffer;
      temp_buffer = NULL;
    }
  if(server_socket_address_ptr)
    {
      dl_free_sa(server_socket_address_ptr);
      server_socket_address_ptr=0;
    }
  unload_socket_interface ();
}

void
CMS_SERVER_REMOTE_STCP_PORT::unregister_port ()
{
  CLIENT_STCP_PORT *client;
  int clients_to_close = 0;
  client = (CLIENT_STCP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      rcs_print ("\nPlease shutdown the client on %s.\n",
		 dl_sa_get_host(client->address));
      if (client->socket_fd > 0)
	{
	  sendline (client->socket_fd, "ERR:Shutdown NOW!!!", 0, 1.0);
	}
      clients_to_close++;
      client = (CLIENT_STCP_PORT *) client_ports->get_next ();
    }
  if (clients_to_close > 0)
    {
      rcs_print
	("Waiting 15 seconds for you to close %d remote client connection(s).\n",
	 clients_to_close);
      esleep (15.0);
    }
  client = (CLIENT_STCP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      if (client->socket_fd > 0)
	{
	  clean_prev_read_info (client->socket_fd);
	  dl_closesocket (client->socket_fd);
	}
      delete client;
      client_ports->delete_current_node ();
      client = (CLIENT_STCP_PORT *) client_ports->get_next ();
    }
  if (clients_to_close > 0)
    {
      esleep (2.0);
    }
  if (connection_socket > 0)
    {
      dl_closesocket (connection_socket);
      connection_socket = 0;
    }
}

int
CMS_SERVER_REMOTE_STCP_PORT::accept_local_port_cms (CMS * _cms)
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_STCP_PORT::accept_local_port_cms(_cms=%p) called. this=%p\n",
		  (void*)_cms,(void*)this);

  if (NULL == _cms)
    {
      return 0;
    }
  if (_cms->remote_port_type != CMS_STCP_REMOTE_PORT_TYPE)
    {
      return 0;
    }

  if (server_socket_address_ptr == 0 || 
      dl_sa_get_port(server_socket_address_ptr) == 0)
    {
      if(use_ipv6 != _cms->use_ipv6 && server_socket_address_ptr)
	{
	  dl_free_sa(server_socket_address_ptr);
	  server_socket_address_ptr=0;
	}
      use_ipv6 = _cms->use_ipv6;
      if(server_socket_address_ptr == 0)
	{
	  server_socket_address_ptr = dl_create_sa((_cms->bind_proc_host?_cms->ProcessHost:0), 
						   _cms->stcp_port_number,
						   _cms->use_ipv6);
	}
      connection_port = _cms->stcp_port_number;
      dl_sa_set_port(server_socket_address_ptr,_cms->stcp_port_number);
      buffer_number = _cms->buffer_number;
      temp_buffer =
	(char *) DEBUG_MALLOC (cms_encoded_data_explosion_factor *
			       _cms->size);
      if (temp_buffer != NULL)
	{
	  temp_buffer_size = cms_encoded_data_explosion_factor * _cms->size;
	}
      else
	{
	  rcs_print_error ("Out of memory.\n");
	}
      if (CMS_DISPLAY_ASCII_ENCODING != _cms->neutral_encoding_method &&
	  CMS_XML_ENCODING != _cms->neutral_encoding_method )
	{
	  rcs_print_error
	    ("The neutral_encoding_method must be CMS_DISPLAY_ASCII_ENCODING or CMS_XML_ENCODING to use the simplified STCP protocol.\n");
	  rcs_print_error
	    (" This server will not provide access to the %s buffer.\n",
	     _cms->BufferName);
	  return 0;
	}
      return 1;
    }

  if (server_socket_address_ptr &&
      dl_sa_get_port(server_socket_address_ptr) == _cms->stcp_port_number)
    {
      return 1;
    }

  return 0;
}

void
CMS_SERVER_REMOTE_STCP_PORT::register_port ()
{
  port_registered = 0;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Registering server on STCP port %d.\n",
		   dl_sa_get_port(server_socket_address_ptr));
  if (!server_socket_address_ptr || dl_sa_get_port(server_socket_address_ptr) == 0)
    {
      rcs_print_error ("server can not register on port number 0.\n");
      return;
    }
  if ((connection_socket = (int) dl_tcp_socket(use_ipv6)) < 0)
    {
      rcs_print_error ("Server can not open stream socket.\n");
      return;
    }

  int old_stcp_nonblocking = stcp_nonblocking;
  stcp_nonblocking = 0;
  if (set_stcp_socket_options (connection_socket) < 0)
    {
      stcp_nonblocking = old_stcp_nonblocking;
      return;
    }
  stcp_nonblocking = old_stcp_nonblocking;

  if (dl_bind (connection_socket, 
	       dl_sa_addr(server_socket_address_ptr),
	       dl_sa_len(server_socket_address_ptr)) < 0)
    {
      sockerrno= dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("Can not bind port %d. errno = %d -- %s\n",
		       dl_sa_get_port(server_socket_address_ptr),
		       sockerrno, sockerrstr);
      dl_closesocket (connection_socket);
      connection_socket = 0;
      return;
    }
  if (dl_listen (connection_socket, 50) < 0)
    {
      rcs_print_error ("STCP Server: error on call to listen.\n");
      return;
    }
  port_registered = 1;

}

void
CMS_SERVER_REMOTE_STCP_PORT::run ()
{
  unsigned long bytes_ready;
  int ready_descriptors;
  int unfinished = 0;
  if (NULL == client_ports)
    {
      rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
    }
  CLIENT_STCP_PORT *client_port_to_check;
  FD_ZERO (&read_fd_set);
  FD_ZERO (&write_fd_set);
  RCS_FD_SET (connection_socket, &read_fd_set);
  maxfdpl = connection_socket + 1;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "running server for STCP port %d (connection_socket = %d).\n",
		   dl_sa_get_port(server_socket_address_ptr),
		   connection_socket);
  cms_server_count++;
  if(cms_svr_sfunc) {
    svr_start_func stmp = cms_svr_sfunc;
    cms_svr_sfunc=0;
    rcs_print("calling cms_svr_sfunc()\n");
    (*stmp)();
    rcs_print("\nfinished cms_svr_sfunc()\n");
  }

  while (1)
    {
      ready_descriptors =
	dl_select (maxfdpl, &read_fd_set, &write_fd_set, (fd_set *) NULL,
		   (timeval *) NULL);
      if (ready_descriptors < 0)
	{
	  sockerrno = dl_get_last_socket_error_int( -1 );
	  sockerrstr = dl_get_last_socket_error_string(-1,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("server: select error.(errno = %d | %s)\n",
			   sockerrno,sockerrstr);
	}
      unfinished = 0;
      client_port_to_check = (CLIENT_STCP_PORT *) client_ports->get_head ();
      while (NULL != client_port_to_check)
	{
	  if (dl_fd_isset (client_port_to_check->socket_fd, &read_fd_set))
	    {
	      dl_ioctlsocket_fionread_ulp(client_port_to_check->socket_fd,&bytes_ready);
	      if (bytes_ready == 0)
		{
		  rcs_print_debug (PRINT_SOCKET_CONNECT,
				   "Socket closed by host with IP address %s using connection %d.\n",
				   dl_sa_get_host(client_port_to_check->address),
				   client_port_to_check->socket_fd);
		  clean_prev_read_info (client_port_to_check->socket_fd);
		  if(client_port_to_check->socket_fd > 0) 
		    {
		      dl_closesocket (client_port_to_check->socket_fd);
		      RCS_FD_CLR (client_port_to_check->socket_fd, &read_fd_set);
		      client_port_to_check->socket_fd=-1;
		    }
		  delete client_port_to_check;
		  client_ports->delete_current_node ();
		}
	      else
		{
		  handle_request (client_port_to_check);
		  if (!handle_request_error_occured)
		    {
		      if (get_bytes_already_read
			  (client_port_to_check->socket_fd) > 0)
			{
			  unfinished = 1;
			}
		    }
		}
	      ready_descriptors--;
	    }
	  else
	    {
	      RCS_FD_SET (client_port_to_check->socket_fd, &read_fd_set);
	    }
	  client_port_to_check =
	    (CLIENT_STCP_PORT *) client_ports->get_next ();
	}
      if (dl_fd_isset (connection_socket, &read_fd_set)
	  && ready_descriptors > 0)
	{
	  ready_descriptors--;
	  CLIENT_STCP_PORT *new_client_port = new CLIENT_STCP_PORT();  
	  
	  new_client_port->errors = 0;
	  new_client_port->max_errors = 2;
	  new_client_port->last_write_id = -1;
	  new_client_port->socket_fd = -1;
	  new_client_port->address = dl_create_sa(0,0,use_ipv6);
	  int client_address_length = dl_sa_len(new_client_port->address);
	  new_client_port->socket_fd = dl_accept (connection_socket,
						 dl_sa_addr(new_client_port->address),
						 &client_address_length);
	  clean_prev_read_info (new_client_port->socket_fd);
	  if (new_client_port->socket_fd < 0)
	    {
	      sockerrno = dl_get_last_socket_error_int( connection_socket );
	      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("server: accept error %d -- %s\n", 
			       sockerrno, sockerrstr );
	      delete new_client_port;
	      continue;
	    }
	  rcs_print_debug (PRINT_SOCKET_CONNECT,
			   "Socket opened to host with IP address %s using connection %d.\n",
			   dl_sa_get_host(new_client_port->address),
			   new_client_port->socket_fd);
	  if (NULL != client_ports)
	    {
	      client_ports->store_at_tail (new_client_port,
					   sizeof (new_client_port), 0);
	    }
	  if (maxfdpl < new_client_port->socket_fd + 1)
	    {
	      maxfdpl = new_client_port->socket_fd + 1;
	    }
	  RCS_FD_SET (new_client_port->socket_fd, &read_fd_set);
	}
      else
	{
	  RCS_FD_SET (connection_socket, &read_fd_set);
	}
      if (0 != ready_descriptors)
	{
	  rcs_print_error ("%d descriptors ready but not serviced.\n",
			   ready_descriptors);
	}
      int unfinished_cycles = 0;
      while (unfinished && unfinished_cycles < 10)
	{
	  unfinished_cycles++;
	  client_port_to_check =
	    (CLIENT_STCP_PORT *) client_ports->get_head ();
	  unfinished = 0;
	  while (NULL != client_port_to_check)
	    {
#if MS_WINDOWS_API
	      dl_ioctlsocket (client_port_to_check->socket_fd, FIONREAD,
			      &bytes_ready);
#else
#ifndef VXWORKS
	      ioctl (client_port_to_check->socket_fd, FIONREAD,
		     (caddr_t) & bytes_ready);
#else
	      ioctl (client_port_to_check->socket_fd, FIONREAD,
		     (int) &bytes_ready);
#endif
#endif
	      if (get_bytes_already_read (client_port_to_check->socket_fd) > 0
		  || bytes_ready > 0)
		{
		  handle_request (client_port_to_check);
		  if (!handle_request_error_occured)
		    {
		      if (get_bytes_already_read
			  (client_port_to_check->socket_fd) > 0)
			{
			  unfinished = 1;
			}
		    }
		}
	      client_port_to_check =
		(CLIENT_STCP_PORT *) client_ports->get_next ();
	    }
	}
    }
}

void
CMS_SERVER_REMOTE_STCP_PORT::handle_request (CLIENT_STCP_PORT *
					     _client_stcp_port)
{
  int bytes_sent =0;
  int bytes_sent_last=-1;
  CLIENT_STCP_PORT *client_port_to_check;
  handle_request_error_occured = 0;
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
	("Cannot find server object associated with this process/task ( %ld (0x%lX), %ld (0x%lX)).\n",
	 (long)pid, (unsigned long) pid, (long)tid, (unsigned long)tid);
      print_servers ();
      return;
    }


  if (_client_stcp_port->errors >= _client_stcp_port->max_errors)
    {
      int temp_socket_fd = _client_stcp_port->socket_fd;
      rcs_print_error ("Too many errors - closing connection(%d)\n",
		       _client_stcp_port->socket_fd);
      client_port_to_check = (CLIENT_STCP_PORT *) client_ports->get_head ();
      while (NULL != client_port_to_check)
	{
	  if (client_port_to_check->socket_fd == temp_socket_fd)
	    {
	      if(client_port_to_check->socket_fd > 0)
		{
		  dl_closesocket (client_port_to_check->socket_fd);
		  RCS_FD_CLR (client_port_to_check->socket_fd, &read_fd_set);
		  client_port_to_check->socket_fd=-1;
		}
	      delete client_port_to_check;
	      client_ports->delete_current_node ();
	    }
	  client_port_to_check =
	    (CLIENT_STCP_PORT *) client_ports->get_next ();
	}
      handle_request_error_occured = 1;
      return;
    }

  if (NULL == temp_buffer)
    {
      rcs_print_error ("temp_buffer is NULL");
      _client_stcp_port->errors++;
      handle_request_error_occured = 1;
      return;
    }

  if (recvline
      (_client_stcp_port->socket_fd, temp_buffer, temp_buffer_size, 0, -1,
       NULL) < 0)
    {
      rcs_print_error ("Can not read from client port.\n");
      _client_stcp_port->errors++;
      handle_request_error_occured = 1;
      return;
    }
  if (server->using_passwd_file)
    {
      current_user_info = get_connected_user (_client_stcp_port->socket_fd);
    }

  if (!strncmp (temp_buffer, "read", 4))
    {
      if (temp_buffer[4] == '(')
	{
	  buffer_number = strtol (temp_buffer + 5, NULL, 0);
	}
      if(0 == server->read_req_ptr)
	{
	  server->read_req_ptr = new REMOTE_READ_REQUEST();
	}
      server->read_req_ptr->buffer_number = buffer_number;
      server->read_req_ptr->access_type = CMS_READ_ACCESS;
      server->read_req_ptr->last_id_read = _client_stcp_port->last_write_id;
      REMOTE_READ_REPLY *read_reply = 
	(REMOTE_READ_REPLY *)server->process_request (server->read_req_ptr);
      if (NULL == read_reply)
	{
	  rcs_print_error ("Server could not process request.\n");
	  sendline (_client_stcp_port->socket_fd,
		    "ERR: -1 = Process Request Error", 0, 1.0);
	  return;
	}
      _client_stcp_port->last_write_id = read_reply->write_id;
      if (read_reply->size > 0)
	{	
	  bytes_sent =0;
	  bytes_sent_last = -1;
	  while(bytes_sent < read_reply->size &&
		bytes_sent >  bytes_sent_last)
	    {
	      bytes_sent_last = bytes_sent;
	      if (bytes_sent += sendline
		  (_client_stcp_port->socket_fd,
		   (char *) read_reply->data+bytes_sent, 0, -1) < 0)
		{
		  _client_stcp_port->errors++;
		  handle_request_error_occured = 1;
		  return;
		}
	    }
	  CMS_SERVER_LOCAL_PORT *read_lp = server->find_local_port(buffer_number);
	  if(read_lp->cms->neutral_encoding_method == CMS_XML_ENCODING)
	    {
	      sendline(_client_stcp_port->socket_fd,stcp_xml_end_string,0,-1);
	      // This sends a string to mark the end of the message
	      // It was chosen because it can not occur inside 
	      // an XML MSG.
	    }
	}
      else
	{
	  sendline (_client_stcp_port->socket_fd, "ERR: 0 = No data.", 0,
		    1.0);
	}
    }
  else if (!strncmp (temp_buffer, "peek", 4))
    {
      if (temp_buffer[4] == '(')
	{
	  buffer_number = strtol (temp_buffer + 5, NULL, 0);
	}
      if(0 == server->read_req_ptr)
	{
	  server->read_req_ptr = new REMOTE_READ_REQUEST();
	}
      server->read_req_ptr->buffer_number = buffer_number;
      server->read_req_ptr->access_type = CMS_PEEK_ACCESS;
      server->read_req_ptr->last_id_read = _client_stcp_port->last_write_id;
      REMOTE_READ_REPLY *peek_reply =
	(REMOTE_READ_REPLY *) server->process_request (server->read_req_ptr);
      if (NULL == peek_reply)
	{
	  rcs_print_error ("Server could not process request.\n");
	  sendline (_client_stcp_port->socket_fd,
		    "ERR: -1 = Process Request Error", 0, 1.0);
	  return;
	}
      _client_stcp_port->last_write_id = peek_reply->write_id;
      if (peek_reply->size > 0)
	{
	  bytes_sent =0;
	  bytes_sent_last = -1;
	  while(bytes_sent < peek_reply->size &&
		bytes_sent >  bytes_sent_last)
	    {
	      bytes_sent_last = bytes_sent;
	      if (bytes_sent += sendline
		  (_client_stcp_port->socket_fd,
		   (char *) peek_reply->data+bytes_sent, 0, -1) < 0)
		{
		  _client_stcp_port->errors++;
		  handle_request_error_occured = 1;
		  return;
		}
	    }
	  CMS_SERVER_LOCAL_PORT *peek_lp = server->find_local_port(buffer_number);
	  if(peek_lp->cms->neutral_encoding_method == CMS_XML_ENCODING)
	    {
	      sendline(_client_stcp_port->socket_fd,stcp_xml_end_string,0,-1);
	      // This sends a string to mark the end of the message
	      // It was chosen because it can not occur inside 
	      // an XML MSG.
	    }
	}
      else
	{
	  sendline (_client_stcp_port->socket_fd, "ERR: 0 = No data", 0, 1.0);
	}

    }
  else if (!strncmp (temp_buffer, "write_if_read",13))
    {
      if (temp_buffer[13] == '(')
	{
	  buffer_number = strtol (temp_buffer + 14, NULL, 0);
	}
      if(0 == server->write_req_ptr)
	{
	  server->write_req_ptr = new REMOTE_WRITE_REQUEST();
	}
      server->write_req_ptr->buffer_number = buffer_number;
      server->write_req_ptr->access_type = CMS_WRITE_IF_READ_ACCESS;
      char *write_if_read_data = strchr (temp_buffer, ':') + 1;
      if(write_if_read_data == NULL)
	{
	  rcs_print_error ("data must begin with \':\'\n");
	}
      CMS_SERVER_LOCAL_PORT *write_if_read_lp = server->find_local_port(buffer_number);
      if(write_if_read_lp->cms->neutral_encoding_method == CMS_XML_ENCODING)
	{
	  char *last_line = write_if_read_data;
	  last_line += strlen(write_if_read_data);
	  int bytes_read=0;
	  while(last_line)
	    {
	      if ((bytes_read = recvline
		  (_client_stcp_port->socket_fd, last_line, 
		   temp_buffer_size-(last_line-temp_buffer), 0, -1,
		   NULL)) <= 0)
		{
		  rcs_print_error ("Can not read from client port.\n");
		  _client_stcp_port->errors++;
		  handle_request_error_occured = 1;
		  return;
		}
	      if(!strncmp(last_line,stcp_xml_end_string,STCP_XML_END_STRING_LENGTH))
		{
		  memset(last_line,0,STCP_XML_END_STRING_LENGTH);
		  break;
		}
	      last_line += bytes_read;
	    }
	}
      server->write_req_ptr->size = (int) strlen (write_if_read_data);
      strcpy ((char *) server->write_req_ptr->data, write_if_read_data);
      REMOTE_WRITE_REPLY *write_if_read_reply =
	(REMOTE_WRITE_REPLY *) server->process_request (server->write_req_ptr);
      if (NULL == write_if_read_reply)
	{
	  rcs_print_error ("Server could not process write request.\n");
	  temp_buffer[temp_buffer_size-1] = 0;
	  rcs_print_error ("request was %s\n",temp_buffer);
	  sendline (_client_stcp_port->socket_fd, "write_if_read_failed:1", 0, 1.0);
	}
      else
	{
	  if (write_if_read_reply->was_read)
	    {
	      sendline (_client_stcp_port->socket_fd,
			"write_if_read_succeeded:1", 0, 1.0);
	    }
	  else
	    {
	      sendline (_client_stcp_port->socket_fd,
			"write_if_read_succeeded:0", 0, 1.0);
	    }
	}
    }
  else if (!strncmp (temp_buffer, "write", 5))
    {
      if (temp_buffer[5] == '(')
	{
	  buffer_number = strtol (temp_buffer + 6, NULL, 0);
	}
      if(0 == server->write_req_ptr)
	{
	  server->write_req_ptr = new REMOTE_WRITE_REQUEST();
	}
      server->write_req_ptr->buffer_number = buffer_number;
      server->write_req_ptr->access_type = CMS_WRITE_ACCESS;
      char *write_data = strchr (temp_buffer, ':');
      if (NULL == write_data)
	{
	  rcs_print_error ("Server could not process write request.\n");
	  temp_buffer[temp_buffer_size-1] = 0;
	  rcs_print_error ("request was %s\n",temp_buffer);
	  rcs_print_error ("data must begin with \':\'\n");
	}
      else
	{
	  write_data++;		// Skip past colon
	  CMS_SERVER_LOCAL_PORT *write_lp = server->find_local_port(buffer_number);
	  if(write_lp->cms->neutral_encoding_method == CMS_XML_ENCODING)
	    {
	      char *last_line = write_data;
	      last_line += strlen(write_data);
	      int bytes_read=0;
	      while(last_line)
		{
		  if ((bytes_read = recvline
		      (_client_stcp_port->socket_fd, last_line, 
		       temp_buffer_size-(last_line-temp_buffer), 0, -1,
		       NULL)) <= 0)
		    {
		      rcs_print_error ("Can not read from client port.\n");
		      _client_stcp_port->errors++;
		      handle_request_error_occured = 1;
		      return;
		    }
		  if(!strncmp(last_line,stcp_xml_end_string,STCP_XML_END_STRING_LENGTH))
		    {
		      memset(last_line,0,STCP_XML_END_STRING_LENGTH);
		      break;
		    }
		  last_line += bytes_read;
		}
	    }
	  server->write_req_ptr->size = (int) strlen (write_data);
	  strcpy ((char *) server->write_req_ptr->data, write_data);
	  REMOTE_WRITE_REPLY *write_reply = 
	    (REMOTE_WRITE_REPLY *) server->process_request (server->
							    write_req_ptr);
	  if (NULL == write_reply)
	    {
	      rcs_print_error ("Server could not process write request.\n");
	    }
	}
    }
  else if (!strncmp (temp_buffer, "check_if_read", STCP_XML_END_STRING_LENGTH))
    {
      if (temp_buffer[STCP_XML_END_STRING_LENGTH] == '(')
	{
	  buffer_number = strtol (temp_buffer + 14, NULL, 0);
	}
      if(0 == server->check_if_read_req_ptr)
	{
	  server->check_if_read_req_ptr = new REMOTE_CHECK_IF_READ_REQUEST();
	}
      server->check_if_read_req_ptr->buffer_number = buffer_number;
      REMOTE_CHECK_IF_READ_REPLY *check_if_read_reply
	= (REMOTE_CHECK_IF_READ_REPLY *)
	server->process_request (server->check_if_read_req_ptr);
      if (NULL == check_if_read_reply)
	{
	  rcs_print_error
	    ("Server could not process check_if_read request.\n");
	  sendline (_client_stcp_port->socket_fd, "check_if_read_failed", 0,
		    1.0);
	}
      else
	{
	  if (check_if_read_reply->was_read)
	    {
	      sendline (_client_stcp_port->socket_fd, "was_read", 0, 1.0);
	    }
	  else
	    {
	      sendline (_client_stcp_port->socket_fd, "not_read", 0, 1.0);
	    }
	}
    }
  else if (!strncmp (temp_buffer, "get_queue_lenth", STCP_XML_END_STRING_LENGTH))
    {
      if (temp_buffer[STCP_XML_END_STRING_LENGTH] == '(')
	{
	  buffer_number = strtol (temp_buffer + 14, NULL, 0);
	}
      if(0 == server->get_queue_length_req_ptr)
	{
	  server->get_queue_length_req_ptr = new REMOTE_GET_QUEUE_LENGTH_REQUEST();
	}
      server->get_queue_length_req_ptr->buffer_number = buffer_number;
      REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply
	= (REMOTE_GET_QUEUE_LENGTH_REPLY *)
	server->process_request (server->get_queue_length_req_ptr);
      if (NULL == get_queue_length_reply)
	{
	  rcs_print_error
	    ("Server could not process get_queue_length request.\n");
	  sendline (_client_stcp_port->socket_fd, "get_queue_length_failed", 0,
		    1.0);
	}
      else
	{
	  snprintf(temp_buffer,temp_buffer_size,"queue_length=%ld",
		   get_queue_length_reply->queue_length);
	  sendline (_client_stcp_port->socket_fd, temp_buffer, 0, 1.0);
	}
    }
  else if (!strncmp (temp_buffer, "get_keys", 8))
    {
      if (temp_buffer[9] == '(')
	{
	  buffer_number = strtol (temp_buffer + 10, NULL, 0);
	}
      if(0 == server->get_keys_req_ptr)
	{
	  server->get_keys_req_ptr = new REMOTE_GET_KEYS_REQUEST();
	}
      server->get_keys_req_ptr->buffer_number = buffer_number;
      char *get_keys_data = strchr (temp_buffer, ':') + 1;
      while (*get_keys_data == ' ')
	{
	  get_keys_data++;
	}
      strncpy ((char *) server->get_keys_req_ptr->name, get_keys_data, 16);
      REMOTE_GET_KEYS_REPLY *get_keys_reply=
	(REMOTE_GET_KEYS_REPLY *) server->process_request (server->
							   get_keys_req_ptr);
      if (NULL == get_keys_reply)
	{
	  rcs_print_error ("Server could not process request.\n");
	  memset (temp_buffer, 0, temp_buffer_size);
	  server->gen_random_key (((char *) temp_buffer), 2);
	  server->gen_random_key (((char *) temp_buffer) + 8, 2);
	  if (temp_buffer_size < 64)
	    {
	      sendline (_client_stcp_port->socket_fd, "keys:sd:b8", 0, 1.0);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS((((char *)temp_buffer)+16),(temp_buffer_size-17)), 
			     "keys:%s:%s",
			     ((char *) temp_buffer), ((char *) temp_buffer) + 8);
	      sendline (_client_stcp_port->socket_fd,
			((char *) temp_buffer) + 16, 0, 1.0);
	    }
	  return;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS((((char *)temp_buffer)+16),(temp_buffer_size-17)), 
		      "keys:%s:%s",
		      get_keys_reply->key1, get_keys_reply->key2);
      sendline (_client_stcp_port->socket_fd, ((char *) temp_buffer) + 16, 0,
		1.0);
      return;
    }
  else if (!strncmp (temp_buffer, "list", 4))
    {
      if (server->cms_local_ports)
	{
	  CMS_SERVER_LOCAL_PORT *lp = (CMS_SERVER_LOCAL_PORT *)
	    server->cms_local_ports->get_head ();
	  sendline(_client_stcp_port->socket_fd, "# begin list", 0,
		   1.0);
	  while (lp)
	    {
	      sendline(_client_stcp_port->socket_fd, lp->cms->BufferLine, 0,
			1.0);
	      lp = (CMS_SERVER_LOCAL_PORT *)
		server->cms_local_ports->get_next ();
	    }
	  sendline(_client_stcp_port->socket_fd, "# end list", 0,
		   1.0);
	}
    }
  else if (!strncmp (temp_buffer, "login", 5))
    {
      if (temp_buffer[6] == '(')
	{
	  buffer_number = strtol (temp_buffer + 7, NULL, 0);
	}
      if(0 == server->login_req_ptr)
	{
	  server->login_req_ptr = new REMOTE_LOGIN_REQUEST();
	}
      server->login_req_ptr->buffer_number = buffer_number;
      char *login_data = NULL;
      login_data = strchr (temp_buffer, ':') + 1;
      if (NULL != login_data)
	{
	  while (*login_data == ' ')
	    {
	      login_data++;
	    }
	  strncpy ((char *) server->login_req_ptr->name, login_data, 16);
	  char *bad_char = strpbrk (server->login_req_ptr->name, "\r\n\t :");
	  if (NULL != bad_char)
	    {
	      *bad_char = 0;
	    }
	  login_data = strchr (login_data, ':') + 1;
	  while (*login_data == ' ')
	    {
	      login_data++;
	    }
	  strncpy ((char *) server->login_req_ptr->passwd, login_data, 16);
	  bad_char = strpbrk (server->login_req_ptr->passwd, "\r\n\t :");
	  if (NULL != bad_char)
	    {
	      *bad_char = 0;
	    }
	  REMOTE_LOGIN_REPLY *login_reply=
	    (REMOTE_LOGIN_REPLY *) server->process_request (server->
							    login_req_ptr);
	  if (NULL == login_reply)
	    {
	      rcs_print_error ("Server could not process request.\n");
	      sendline (_client_stcp_port->socket_fd, "login failed", 0, 1.0);
	      return;
	    }
	  if (login_reply->success)
	    {
	      sendline (_client_stcp_port->socket_fd, "login succeeded", 0,
			1.0);
	      return;
	    }
	  else
	    {
	      sendline (_client_stcp_port->socket_fd, "login failed", 0, 1.0);
	      return;
	    }
	}
      else
	{
	  sendline (_client_stcp_port->socket_fd, "login failed", 0, 1.0);
	  return;
	}

      return;
    }
  else if (!strncmp (temp_buffer, "help", 4))
    {
      sendline (_client_stcp_port->socket_fd, "read(<buffer_number>)", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "peek(<buffer_number>)", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "check_if_read(<buffer_number>)", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "get_queue_length(<buffer_number>)", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "write(<buffer_number>) : <message>", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "write_if_read(<buffer_number>) : <message>", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "END XML messages with", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, stcp_xml_end_string, 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "list", 0, 1.0);
      sendline (_client_stcp_port->socket_fd, "help", 0, 1.0);
      
    }
  else
    {
      sendline (_client_stcp_port->socket_fd, "ERR: -1 = Unrecognized request.", 0, 1.0);
      temp_buffer[sizeof(temp_buffer)-1] = 0;
      rcs_print_error("Unrecognized request : %s\n",temp_buffer);
    }    
}

