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
* File: http_srv.cc
* Purpose: Provides the functions for the class CMS_SERVER_REMOTE_T_PORT
*  which provides HTTP specific overrides of the CMS_SERVER_REMOTE_PORT class.
****************************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if (defined(ENABLE_RCS_SERVER) && defined(ENABLE_RCS_HTTP))


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "http_srv_no_config.h"
#endif				// HAVE_CONFIG_H

#include "cms.hh"		/* class CMS */
#include "http_srv.hh"		/* class CMS_SERVER_REMOTE_HTTP_PORT */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "tcp_opts.hh"		/* SET_TCP_NODELAY */
#include "timer.hh"		// esleep()
#include "_timer.h"
#include "sokintrf.h"		/* dl_ioctl() */

#include "rem_msg.hh"

#ifdef RCS_ENABLE_DIAG
#include "cmsdiag.hh"		// class CMS_DIAGNOSTICS_INFO
#endif

extern "C"
{
#include "recvn.h"		/* recvn() */
#include "sendn.h"		/* sendn() */
}

class CLIENT_HTTP_PORT
{
public:
  CLIENT_HTTP_PORT (int _use_ipv6=0);
  ~CLIENT_HTTP_PORT ();
  long serial_number;
  int errors, max_errors;
  struct dl_sa * ptr_to_address;
  int socket_fd;
  RCS_LINKED_LIST *subscriptions;

  int blocking;
  
  //   HTTPSVR_BLOCKING_READ_REQUEST *blocking_read_req;
  char unprocessed_request[2048];
  char *ptr_to_end_recieved_request;

  REMOTE_SET_DIAG_INFO_REQUEST *diag_info;

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
#if defined(sunos5) && !defined(NO_THREADS)
  thread_t threadId;
#else
#ifdef POSIX_THREADS
  pthread_t threadId;
#else
  int threadId;
#endif
#endif
  int use_ipv6;

private:
  CLIENT_HTTP_PORT(const CLIENT_HTTP_PORT &_chp);
  CLIENT_HTTP_PORT &operator=(const CLIENT_HTTP_PORT &_chp);

};

#if 0

int httpsvr_threads_created = 0;
int httpsvr_threads_killed = 0;
int httpsvr_threads_exited = 0;
int httpsvr_threads_returned_early = 0;

HTTPSVR_BLOCKING_READ_REQUEST::HTTPSVR_BLOCKING_READ_REQUEST ()
{
  access_type = CMS_READ_ACCESS;	/* read or just peek */
  last_id_read = 0;		/* The server can compare with id from buffer */
  /* to determine if the buffer is new */
  /* to this client */
  timeout_millis = -1;		/* Milliseconds for blocking_timeout or -1
				 * to wait forever */
  _client_http_port = NULL;
  remport = NULL;
  server = NULL;
  _nml = NULL;
  _reply = NULL;
  _data = NULL;
  read_reply = NULL;
}


static inline double
http_svr_reverse_double (double in)
{
  double out;
  char *c1, *c2;

  c1 = ((char *) &in) + 7;
  c2 = (char *) &out;
  for (int i = 0; i < 8; i++)
    {
      *c2 = *c1;
      c1--;
      c2++;
    }
  return out;
}


HTTPSVR_BLOCKING_READ_REQUEST::~HTTPSVR_BLOCKING_READ_REQUEST ()
{
  if (NULL != _nml)
    {
      NML *nmlcopy = (NML *) _nml;
      _nml = NULL;
      delete nmlcopy;
    }
  if (NULL != _data)
    {
      void *_datacopy = _data;
      if (NULL != read_reply)
	{
	  if (_data == read_reply->data)
	    {
	      read_reply->data = NULL;
	    }
	}
      _data = NULL;
      DEBUG_FREE (_datacopy);
    }
  if (NULL != _reply)
    {
      DEBUG_FREE (_reply);
      _reply = NULL;
      read_reply = NULL;
    }
  if (NULL != read_reply)
    {
      if (NULL != read_reply->data)
	{
	  DEBUG_FREE (read_reply->data);
	  read_reply->data = NULL;
	}
      delete read_reply;
      read_reply = NULL;
    }
}

#endif

CMS_SERVER_REMOTE_HTTP_PORT::CMS_SERVER_REMOTE_HTTP_PORT (CMS_SERVER * _cms_server):
  CMS_SERVER_REMOTE_PORT (_cms_server),
  dtimeout(0.0),
#ifndef VXWORKS
  read_fd_set(), write_fd_set(),
#endif
  maxfdpl(0),
  client_ports(0),
  subscription_buffers(0),
  connection_socket(0),
  connection_port(0),
  ptr_to_server_socket_address(0),
  request(0),
  current_poll_interval_millis(0),
  polling_enabled(0),
  ptr_to_select_timeout(0),
  sockerrno(0),
  sockerrstr(0),
  use_ipv6(0)
{
  client_ports = (RCS_LINKED_LIST *) NULL;
  connection_socket = 0;
  connection_port = 0;
  maxfdpl = 0;
  dtimeout = 20.0;
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      return;
    }
  ptr_to_server_socket_address = dl_create_sa(0,0,use_ipv6);

  client_ports = new RCS_LINKED_LIST;
  if (NULL == client_ports)
    {
      rcs_print_error ("Can not create linked list for client ports.\n");
      return;
    }
  polling_enabled = 0;
  ptr_to_select_timeout = new struct timeval;
  memset (ptr_to_select_timeout, 0, sizeof (struct timeval));
  ((struct timeval*)ptr_to_select_timeout)->tv_sec = 30;
  ((struct timeval*)ptr_to_select_timeout)->tv_usec = 30;
  subscription_buffers = NULL;
  current_poll_interval_millis = 30000;
  memset (&read_fd_set, 0, sizeof (read_fd_set));
  memset (&write_fd_set, 0, sizeof (write_fd_set));
}

CMS_SERVER_REMOTE_HTTP_PORT::~CMS_SERVER_REMOTE_HTTP_PORT ()
{
  unregister_port ();
  if (NULL != client_ports)
    {
      delete client_ports;
      client_ports = (RCS_LINKED_LIST *) NULL;
    }
  if(ptr_to_server_socket_address)
    {
      dl_free_sa(ptr_to_server_socket_address);
      ptr_to_server_socket_address=0;
    }
  if(ptr_to_select_timeout)
    {
      free(ptr_to_select_timeout);
      ptr_to_select_timeout=0;
    }
  unload_socket_interface();
}

#if 0

static void
http_blocking_thread_kill (long int id)
{

  if (id <= 0)
    {
      return;
    }
#if defined(sunos5) && !defined(NO_THREADS)
  thr_kill (id, SIGINT);
  thr_join (id, NULL, NULL);
#endif
#ifdef POSIX_THREADS
  pthread_kill (id, SIGINT);
  pthread_join (id, NULL);
#endif
#ifdef VXWORKS
  if (taskIdVerify (id) != OK)
    {
      return;
    }
  kill (id, SIGINT);
  taskDelay (1);
  if (taskIdVerify (id) == OK)
    {
      taskDelete (id);
    }
#endif
#if defined(NO_THREADS) || defined(SGI)
  kill (id, SIGINT);
  waitpid (id, NULL, 0);
#endif
#if MS_WINDOWS_API && HAVE_TERMINATETHREAD
  TerminateThread ((HANDLE) id, -1);
#endif
  httpsvr_threads_killed++;
}

#endif

void
CMS_SERVER_REMOTE_HTTP_PORT::unregister_port ()
{
  CLIENT_HTTP_PORT *client;
  int number_of_connected_clients = 0;
  client = (CLIENT_HTTP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      if( NULL != client->ptr_to_address)
	{
	  rcs_print ("Exiting even though client on %s is still connected.\n",
		     dl_sa_get_host(client->ptr_to_address));

	}
      client = (CLIENT_HTTP_PORT *) client_ports->get_next ();
      number_of_connected_clients++;
    }
  client = (CLIENT_HTTP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      delete client;
      client_ports->delete_current_node ();
      client = (CLIENT_HTTP_PORT *) client_ports->get_next ();
    }
#if 0
  if (NULL != subscription_buffers)
    {
      HTTP_BUFFER_SUBSCRIPTION_INFO *sub_info =
	(HTTP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
      while (NULL != sub_info)
	{
	  delete sub_info;
	  sub_info =
	    (HTTP_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	}
      delete subscription_buffers;
      subscription_buffers = NULL;
    }
#endif
  if (number_of_connected_clients > 0)
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
CMS_SERVER_REMOTE_HTTP_PORT::accept_local_port_cms (CMS * _cms)
{
  if (NULL == _cms)
    {
      return 0;
    }
  if (_cms->remote_port_type != CMS_HTTP_REMOTE_PORT_TYPE)
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
  if (_cms->total_subdivisions > max_total_subdivisions)
    {
      max_total_subdivisions = _cms->total_subdivisions;
    }
  if (dl_sa_get_port(ptr_to_server_socket_address) == 0)
    {
      dl_sa_set_port(ptr_to_server_socket_address,(short)_cms->http_port_number);
      return 1;
    }
  if (dl_sa_get_port(ptr_to_server_socket_address) == (short) _cms->http_port_number)
    {
      port_num = _cms->http_port_number;
      return 1;
    }
  return 0;
}

void
CMS_SERVER_REMOTE_HTTP_PORT::register_port ()
{
  port_registered = 0;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Registering server on HTTP port %d.\n",
		   dl_sa_get_port(ptr_to_server_socket_address));

  if (dl_sa_get_port(ptr_to_server_socket_address)  == 0)
    {
      rcs_print_error ("server can not register on port number 0.\n");
      return;
    }
  connection_socket=-1;

  connection_socket = dl_tcp_socket (use_ipv6);

  if (connection_socket < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("socket error: %d -- %s\n", sockerrno, sockerrstr);
      rcs_print_error ("Server can not open stream socket.\n");
      return;
    }

  if (set_tcp_socket_options (connection_socket,use_ipv6) < 0)
    {
      return;
    }

  if (dl_bind (connection_socket, dl_sa_addr(ptr_to_server_socket_address),
	       dl_sa_len(ptr_to_server_socket_address)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("bind error: %d -- %s\n", sockerrno, sockerrstr);
      rcs_print_error
	("Server can not bind the connection socket on port %d.\n",
	 dl_sa_get_port(ptr_to_server_socket_address));
      return;
    }
  if (dl_listen (connection_socket, 50) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("listen error: %d -- %s\n", sockerrno, sockerrstr);
      rcs_print_error ("HTTP Server: error on call to listen for port %d.\n",
		       dl_sa_get_port(ptr_to_server_socket_address));
      return;
    }
  port_registered = 1;
}


#if !defined(DOS_WINDOWS) && !defined(MS_WINDOWS_API)
static int last_pipe_signum = 0;

static void
handle_pipe_error (int signum)
{
  last_pipe_signum = signum;
  rcs_print_error ("SIGPIPE intercepted.\n");
}
#endif

void
CMS_SERVER_REMOTE_HTTP_PORT::run ()
{
  unsigned long bytes_ready;
  int ready_descriptors;
  if (NULL == client_ports)
    {
      rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
      return;
    }
  CLIENT_HTTP_PORT *new_client_port=0;
  CLIENT_HTTP_PORT *client_port_to_check=0;

  FD_ZERO (&read_fd_set);
  FD_ZERO (&write_fd_set);
  RCS_FD_SET (connection_socket, &read_fd_set);
  maxfdpl = connection_socket + 1;
#if !defined(DOS_WINDOWS) && !defined(MS_WINDOWS_API)
  signal (SIGPIPE, handle_pipe_error);
#endif
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "running server for HTTP port %d (connection_socket = %d).\n",
		   dl_sa_get_port(ptr_to_server_socket_address),
		   connection_socket);

  cms_server_count++;
  fd_set read_fd_set_copy, write_fd_set_copy;
  FD_ZERO (&read_fd_set_copy);
  FD_ZERO (&write_fd_set_copy);
  RCS_FD_SET (connection_socket, &read_fd_set_copy);

  while (1)
    {
      if (polling_enabled)
	{
	  memcpy (&read_fd_set_copy, &read_fd_set, sizeof (fd_set));
	  memcpy (&write_fd_set_copy, &write_fd_set, sizeof (fd_set));
	  ((struct timeval*)ptr_to_select_timeout)->tv_sec = current_poll_interval_millis / 1000;
	  ((struct timeval*)ptr_to_select_timeout)->tv_usec =
	    (current_poll_interval_millis % 1000) * 1000;
	  ready_descriptors =
	    dl_select (maxfdpl, &read_fd_set, &write_fd_set, (fd_set *) NULL,
		       (timeval *) ptr_to_select_timeout);
	  if (ready_descriptors == 0)
	    {
	      //	      update_subscriptions ();
	      memcpy (&read_fd_set, &read_fd_set_copy, sizeof (fd_set));
	      memcpy (&write_fd_set, &write_fd_set_copy, sizeof (fd_set));
	      continue;
	    }
	}
      else
	{
	  ready_descriptors =
	    dl_select (maxfdpl, &read_fd_set, &write_fd_set, (fd_set *) NULL,
		       (timeval *) NULL);

	}
      if (ready_descriptors < 0)
	{
	  sockerrno = dl_get_last_socket_error_int( -1 );
	  sockerrstr = dl_get_last_socket_error_string(-1,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("server: select error.(errno = %d | %s)\n",
			   sockerrno, sockerrstr);
	}
      if (NULL == client_ports)
	{
	  rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
	  return;
	}
      client_port_to_check = (CLIENT_HTTP_PORT *) client_ports->get_head ();
      while (NULL != client_port_to_check)
	{
	  if (dl_fd_isset (client_port_to_check->socket_fd, &read_fd_set))
	    {
	      dl_ioctlsocket_fionread_ulp(client_port_to_check->socket_fd,&bytes_ready);
	      if (bytes_ready <= 0)
		{
		  rcs_print_debug (PRINT_SOCKET_CONNECT,
				   "Socket closed by host with IP address %s.\n",
				   dl_sa_get_host(client_port_to_check->ptr_to_address));

#if 0
		  if (NULL != client_port_to_check->subscriptions)
		    {
		      HTTP_CLIENT_SUBSCRIPTION_INFO *clnt_sub_info =
			(HTTP_CLIENT_SUBSCRIPTION_INFO *)
			client_port_to_check->subscriptions->get_head ();
		      while (NULL != clnt_sub_info)
			{
			  if (NULL != clnt_sub_info->sub_buf_info &&
			      clnt_sub_info->subscription_list_id >= 0)
			    {
			      if (NULL !=
				  clnt_sub_info->sub_buf_info->sub_clnt_info)
				{
				  clnt_sub_info->sub_buf_info->sub_clnt_info->
				    delete_node (clnt_sub_info->
						 subscription_list_id);
				  if (clnt_sub_info->sub_buf_info->
				      sub_clnt_info->list_size < 1)
				    {
				      delete clnt_sub_info->sub_buf_info->
					sub_clnt_info;
				      clnt_sub_info->sub_buf_info->
					sub_clnt_info = NULL;
				      if (NULL != subscription_buffers
					  && clnt_sub_info->sub_buf_info->
					  list_id >= 0)
					{
					  subscription_buffers->delete_node
					    (clnt_sub_info->sub_buf_info->
					     list_id);
					  delete clnt_sub_info->sub_buf_info;
					  clnt_sub_info->sub_buf_info = NULL;
					}
				    }
				  clnt_sub_info->sub_buf_info = NULL;
				}
			      delete clnt_sub_info;
			      clnt_sub_info =
				(HTTP_CLIENT_SUBSCRIPTION_INFO *)
				client_port_to_check->subscriptions->
				get_next ();
			    }
			  delete client_port_to_check->subscriptions;
			  client_port_to_check->subscriptions = NULL;
			  // recalculate_polling_interval ();
			}
		    }
		  if (client_port_to_check->threadId > 0
		      && client_port_to_check->blocking)
		    {
		      http_blocking_thread_kill (client_port_to_check->
						 threadId);
		    }
#endif // if 0

		  dl_closesocket (client_port_to_check->socket_fd);
		  RCS_FD_CLR (client_port_to_check->socket_fd, &read_fd_set);
		  client_port_to_check->socket_fd = -1;
		  delete client_port_to_check;
		  client_ports->delete_current_node ();
		}
	      else
		{
#if 0
		  if (client_port_to_check->blocking)
		    {
		      if (client_port_to_check->threadId > 0)
			{
			  rcs_print_debug (PRINT_SERVER_THREAD_ACTIVITY,
					   "Data recieved from %s:%d when it should be blocking (bytes_ready=%d).\n",
					   dl_sa_get_host(client_port_to_check->ptr_to_address),
					   client_port_to_check->socket_fd,
					   bytes_ready);
			  rcs_print_debug (PRINT_SERVER_THREAD_ACTIVITY,
					   "Killing handler %d.\n",
					   client_port_to_check->threadId);

			  http_blocking_thread_kill
			    (client_port_to_check->threadId);
			  client_port_to_check->threadId = 0;
			  client_port_to_check->blocking = 0;
			}
		    }
#endif
		  handle_request (client_port_to_check);
		}
	      ready_descriptors--;
	    }
	  else
	    {
	      RCS_FD_SET (client_port_to_check->socket_fd, &read_fd_set);
	    }
	  client_port_to_check =
	    (CLIENT_HTTP_PORT *) client_ports->get_next ();
	}
      if (dl_fd_isset (connection_socket, &read_fd_set)
	  && ready_descriptors > 0)
	{
	  ready_descriptors--;
	  new_client_port = new CLIENT_HTTP_PORT ();
	  int client_address_length = 
	    dl_sa_len(new_client_port->ptr_to_address);
	  new_client_port->socket_fd = dl_accept (connection_socket,
						  dl_sa_addr(new_client_port->ptr_to_address),
						  &client_address_length);
	  current_clients++;
	  if (current_clients > max_clients)
	    {
	      max_clients = current_clients;
	    }
	  if (new_client_port->socket_fd < 0)
	    {
	      sockerrno = dl_get_last_socket_error_int( new_client_port->socket_fd );
	      sockerrstr = dl_get_last_socket_error_string(new_client_port->socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("server: accept error -- %d %s \n", 
			       sockerrno, sockerrstr);
	    }
	  rcs_print_debug (PRINT_SOCKET_CONNECT,
			   "Socket opened by host with IP address %s.\n",
				   dl_sa_get_host(client_port_to_check->ptr_to_address));
	  new_client_port->serial_number = 0;
	  new_client_port->blocking = 0;
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
      //      update_subscriptions ();
    }
}

void
CMS_SERVER_REMOTE_HTTP_PORT::handle_request (CLIENT_HTTP_PORT *
					     _client_http_port)
{
  long buffer_number;
  char *uri = 0;
  char xmlfile[256];
  CMS_SERVER *server = 0;
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
      rcs_print_error
	("CMS_SERVER_REMOTE_HTTP_PORT::handle_request() Cannot find server object for pid = %ld.\n",
	 (long)pid);
      return;
    }

  if (server->using_passwd_file)
    {
      current_user_info = get_connected_user (_client_http_port->socket_fd);
    }

  if (_client_http_port->errors >= _client_http_port->max_errors)
    {
      rcs_print_error ("Too many errors - closing connection(%d)\n",
		       _client_http_port->socket_fd);
      CLIENT_HTTP_PORT *client_port_to_check = NULL;
      client_port_to_check = (CLIENT_HTTP_PORT *) client_ports->get_head ();
      while (NULL != client_port_to_check)
	{
	  if (client_port_to_check->socket_fd == _client_http_port->socket_fd)
	    {
	      delete client_port_to_check;
	      client_ports->delete_current_node ();
	    }
	  client_port_to_check =
	    (CLIENT_HTTP_PORT *) client_ports->get_next ();
	}
      dl_closesocket (_client_http_port->socket_fd);
      current_clients--;
      RCS_FD_CLR (_client_http_port->socket_fd, &read_fd_set);
      _client_http_port->socket_fd = -1;
    }

  unsigned long bytes_ready = 0;
  dl_ioctlsocket_fionread_ulp(_client_http_port->socket_fd,
			      &bytes_ready);

  int bytes_to_read = bytes_ready;
  if (bytes_to_read >
      ((_client_http_port->unprocessed_request +
	sizeof (_client_http_port->unprocessed_request)) -
       _client_http_port->ptr_to_end_recieved_request) - 1)
    {
      bytes_to_read =
	(_client_http_port->unprocessed_request +
	 sizeof (_client_http_port->unprocessed_request)) -
	_client_http_port->ptr_to_end_recieved_request - 1;
    }

  int bytes_read = 0;
  memset (_client_http_port->ptr_to_end_recieved_request, 0,
	  (_client_http_port->unprocessed_request +
	   sizeof (_client_http_port->unprocessed_request)) -
	  _client_http_port->ptr_to_end_recieved_request);
  bytes_read =
    recvn (_client_http_port->socket_fd,
	   _client_http_port->ptr_to_end_recieved_request, bytes_to_read, 0,
	   -1, NULL,1);
  if (bytes_read < 0)
    {
      rcs_print_error ("Can not read from client port (%d) from %s\n",
		       _client_http_port->socket_fd,
		       dl_sa_get_host(_client_http_port->ptr_to_address));
      _client_http_port->errors++;
      return;
    }
  int urifd = -1;
  if (bytes_read > 0)
    {
      _client_http_port->ptr_to_end_recieved_request += bytes_read;
      int firstlinelen =
	strcspn (_client_http_port->unprocessed_request, "\r\n");
      char *endfirstline;
      if (firstlinelen > 0
	  && ((size_t)firstlinelen) < sizeof (_client_http_port->unprocessed_request))
	{
	  endfirstline =
	    _client_http_port->unprocessed_request + firstlinelen;
	}
      else
	{
	  endfirstline =
	    _client_http_port->unprocessed_request +
	    strlen (_client_http_port->unprocessed_request);
	}

      char *cptr = _client_http_port->unprocessed_request;
      while ((*cptr == ' ' || *cptr == '\t') && cptr < endfirstline)
	{
	  cptr++;
	}
      char *method = cptr;
      while (!(*cptr == ' ' || *cptr == '\t') && cptr < endfirstline)
	{
	  cptr++;
	}
      char *endmethod = cptr;
      if (!strncmp (method, "GET", (endmethod - method)))
	{
	  if(0 == server->read_req_ptr)
	    {
	      server->read_req_ptr = new REMOTE_READ_REQUEST();
	    }
	  while ((*cptr == ' ' || *cptr == '\t') && cptr < endfirstline)
	    {
	      cptr++;
	    }
	  uri = cptr;
	  while (!(*cptr == ' ' || *cptr == '\t') && cptr < endfirstline)
	    {
	      cptr++;
	    }
	  char *enduri = cptr;
	  if (enduri > uri)
	    {
	      *enduri = 0;
	      if (!strcmp (uri, "/"))
		{
		  char dirbuf[4096];
		  strcpy (dirbuf,
			  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n<HTML>\n<HEAD><TITLE>Index of /</TITLE></head>\n<BODY>\n<H1>Index of /</H1>\n<H2>Communications Channels</H2><UL>\n");
		  char *enddirbuf = dirbuf + strlen (dirbuf);
		  if (server->cms_local_ports)
		    {
		      CMS_SERVER_LOCAL_PORT *lp = (CMS_SERVER_LOCAL_PORT *)
			server->cms_local_ports->get_head ();
		      while (lp && enddirbuf < dirbuf + sizeof (dirbuf) - 40)
			{
			  if (lp->cms)
			    {

			      SNPRINTF_FUNC ( SNPRINTF_ARGS(enddirbuf,
					((dirbuf + sizeof (dirbuf)) -
					 enddirbuf)),
					"<LI><A HREF=\"%s.xml\">%s.xml</A></LI>\n",
					lp->cms->BufferName,
					lp->cms->BufferName);
			      enddirbuf += strlen (enddirbuf);
			      SNPRINTF_FUNC ( SNPRINTF_ARGS(enddirbuf,
					((dirbuf + sizeof (dirbuf)) -
					 enddirbuf)),
					"<LI><A HREF=\"diff--%s.xml\">diff--%s.xml</A></LI>\n",
					lp->cms->BufferName,
					lp->cms->BufferName);
			      enddirbuf += strlen (enddirbuf);
			      if (lp->cms->transfer_alias_list)
				{
				  CMS_TRANSFER_ALIAS *a =
				    (CMS_TRANSFER_ALIAS *) lp->cms->
				    transfer_alias_list->get_head ();
				  while (a)
				    {
				      if (a->name)
					{
					  SNPRINTF_FUNC ( SNPRINTF_ARGS(enddirbuf,
						    ((dirbuf +
						      sizeof (dirbuf)) -
						     enddirbuf)),
						    "<LI><A HREF=\"%s.xml\">%s.xml</A></LI>\n",
						    a->name, a->name);
					  enddirbuf += strlen (enddirbuf);
					  SNPRINTF_FUNC ( SNPRINTF_ARGS(enddirbuf,
						    ((dirbuf +
						      sizeof (dirbuf)) -
						     enddirbuf)),
						    "<LI><A HREF=\"diff--%s.xml\">diff--%s.xml</A></LI>\n",
						    a->name, a->name);
					  enddirbuf += strlen (enddirbuf);
					}
				      a = (CMS_TRANSFER_ALIAS *)
					lp->cms->transfer_alias_list->
					get_next ();
				    }
				}
			    }
			  lp = (CMS_SERVER_LOCAL_PORT *)
			    server->cms_local_ports->get_next ();
			}
		    }
		  if(cms_http_show_files == 1)
		    {		      
		      strcpy (enddirbuf, "</UL>\n<h2>Files</h2>\n<UL>\n");
		      enddirbuf += strlen (enddirbuf);
		      char cwdbuf[256];
		      DIR *d = opendir (getcwd (cwdbuf, sizeof (cwdbuf)));
		      struct dirent *d2 = 0;
		      while ((0 != (d2 = readdir (d)))
			     && enddirbuf < dirbuf + sizeof (dirbuf) - 40)
			{
			  if (d2->d_name[0] != '.')
			    {
			      SNPRINTF_FUNC ( SNPRINTF_ARGS(enddirbuf,
							    ((dirbuf + sizeof (dirbuf)) - enddirbuf)),
						       "<LI><A HREF=\"%s\">%s</A></LI>\n",
						       d2->d_name, d2->d_name);
			      enddirbuf += strlen (enddirbuf);
			    }
			}
		    }
		  strcpy (enddirbuf, "</UL>\n</BODY>\n</HTML>\n\n\n");
		  sendn (_client_http_port->socket_fd, dirbuf,
			 strlen (dirbuf), 0, -1.0);
		  dl_closesocket (_client_http_port->socket_fd);
		  RCS_FD_CLR (_client_http_port->socket_fd, &read_fd_set);
		  current_clients--;
		  _client_http_port->socket_fd = -1;
		  delete _client_http_port;
		  client_ports->delete_current_node ();
		  return;
		}
	      if (server->cms_local_ports)
		{
		  CMS_SERVER_LOCAL_PORT *lp = (CMS_SERVER_LOCAL_PORT *)
		    server->cms_local_ports->get_head ();
		  while (lp)
		    {
		      if (lp->cms)
			{
			  int this_is_it = 0;
			  SNPRINTF_FUNC ( SNPRINTF_ARGS(xmlfile,sizeof(xmlfile)),
					  "/%s.xml", lp->cms->BufferName);
			  if (!strcmp (uri, xmlfile))
			    {
			      this_is_it = 1;
			      lp->cms->current_alias = 0;
			      lp->cms->enable_xml_differencing = 0;
			      lp->enable_xml_differencing = 0;
			      server->read_req_ptr->clientid.long_id[0] =
				_client_http_port->socket_fd;
			      server->read_req_ptr->clientid.long_id[1] =
				(long) 
				dl_sa_get_port(ptr_to_server_socket_address);
			      server->read_req_ptr->clientid.use_me = 0;
			    }
			  SNPRINTF_FUNC ( SNPRINTF_ARGS(xmlfile,sizeof(xmlfile)), 
				   "/diff--%s.xml",
				   lp->cms->BufferName);
			  if (!strcmp (uri, xmlfile))
			    {
			      this_is_it = 1;
			      lp->cms->enable_xml_differencing = 1;
			      lp->enable_xml_differencing = 1;
			      lp->cms->current_alias = 0;
			      server->read_req_ptr->clientid.long_id[0] =
				_client_http_port->socket_fd;
			      server->read_req_ptr->clientid.long_id[1] =
				(long) 
				dl_sa_get_port(ptr_to_server_socket_address);
			      server->read_req_ptr->clientid.use_me = 1;
			    }
			  if (lp->cms->transfer_alias_list && !this_is_it)
			    {
			      CMS_TRANSFER_ALIAS *a = (CMS_TRANSFER_ALIAS *)
				lp->cms->transfer_alias_list->get_head ();
			      while (a && !this_is_it)
				{
				  if (a->name)
				    {
				      SNPRINTF_FUNC ( SNPRINTF_ARGS(xmlfile,sizeof(xmlfile)),
					       "/%s.xml", a->name);
				      if (!strcmp (uri, xmlfile))
					{
					  lp->cms->current_alias = a;
					  this_is_it = 1;
					  lp->cms->enable_xml_differencing =
					    0;
					  lp->enable_xml_differencing = 0;
					  server->read_req_ptr->clientid.
					    long_id[0] =
					    _client_http_port->socket_fd;
					  server->read_req_ptr->clientid.
					    long_id[1] =
					    (long) 
					    dl_sa_get_port(ptr_to_server_socket_address);
					    
					  server->read_req_ptr->clientid.use_me =
					    0;
					  break;
					}
				      SNPRINTF_FUNC ( SNPRINTF_ARGS(xmlfile,sizeof(xmlfile)),
					       "/diff--%s.xml",
					       a->name);
				      if (!strcmp (uri, xmlfile))
					{
					  lp->cms->current_alias = a;
					  this_is_it = 1;
					  lp->cms->enable_xml_differencing =
					    1;
					  lp->enable_xml_differencing = 1;
					  server->read_req_ptr->clientid.
					    long_id[0] =
					    _client_http_port->socket_fd;
					  server->read_req_ptr->clientid.
					    long_id[1] =
					    (long) 
					    dl_sa_get_port(ptr_to_server_socket_address);					    
					  
					  server->read_req_ptr->clientid.use_me =
					    1;
					  break;
					}
				    }
				  a = (CMS_TRANSFER_ALIAS *)
				    lp->cms->transfer_alias_list->get_next ();
				}
			    }
			  if (this_is_it)
			    {
			      buffer_number = lp->buffer_number;

			      if(0 == server->read_req_ptr)
				{
				  server->read_req_ptr = new REMOTE_READ_REQUEST();
				}
			      server->read_req_ptr->buffer_number = buffer_number;
			      server->read_req_ptr->access_type = CMS_READ_ACCESS;
			      _client_http_port->serial_number = -2;
			      server->read_req_ptr->last_id_read =
				_client_http_port->serial_number;
			      
			      REMOTE_READ_REPLY *read_reply=
				(REMOTE_READ_REPLY *) server->
				process_request (server->read_req_ptr);

			      server->read_req_ptr->clientid.use_me = 0;
			      server->read_req_ptr->subdiv = 0;
			      if (NULL == read_reply)
				{
				  rcs_print_error
				    ("Server could not process request.\n");
				}
			      else
				{
				  if (read_reply->size > 0)
				    {
				      ((char *) read_reply->
				       data)[read_reply->size - 1] =
	   '\n';
				      if (sendn
					  (_client_http_port->socket_fd,
					   read_reply->data,
					   read_reply->size, 0,
					   dtimeout) < 0)
					{
					  _client_http_port->errors++;
					  return;
					}
				    }
				}
			      RCS_FD_CLR (_client_http_port->socket_fd,
					  &read_fd_set);
			      dl_closesocket (_client_http_port->socket_fd);
			      current_clients--;
			      _client_http_port->socket_fd = -1;
			      delete _client_http_port;
			      client_ports->delete_current_node ();
			      lp->cms->current_alias = 0;
			      return;
			    }
			  lp = (CMS_SERVER_LOCAL_PORT *)
			    server->cms_local_ports->get_next ();
			}
		    }
		}
	      if(cms_http_show_files)
		{
		  if (uri[0] == '/')
		    {
		      urifd = open (uri + 1, O_RDONLY);
		    }
		  else
		    {
		      urifd = open (uri, O_RDONLY);
		    }
		  char fbuf[2048];
		  if (urifd > 0)
		    {
		      bytes_read = read (urifd, fbuf, sizeof (fbuf));
		      while (bytes_read > 0)
			{
			  sendn (_client_http_port->socket_fd, fbuf, bytes_read,
				 0, -1.0);
			  bytes_read = read (urifd, fbuf, sizeof (fbuf));
			}
		      RCS_FD_CLR (_client_http_port->socket_fd, &read_fd_set);
		      dl_closesocket (_client_http_port->socket_fd);
		      current_clients--;
		      _client_http_port->socket_fd = -1;
		      delete _client_http_port;
		      client_ports->delete_current_node ();
		      return;
		    }
		}
	    }

	}
      char errmsg[4096];
      SNPRINTF_FUNC ( SNPRINTF_ARGS(errmsg,sizeof(errmsg)), 
	       "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<HTML><HEAD>\n<TITLE>404 Not Found</TITLE>\n</HEAD><BODY>\n<H1>Not Found</H1>\nThe requested URL %s was not found on this server.<P>\n<HR> <ADDRESS>Server at %s Port %d</ADDRESS>\n</BODY></HTML>\n\n", uri,
	       dl_sa_get_host(_client_http_port->ptr_to_address),
	       dl_sa_get_port(ptr_to_server_socket_address));

      sendn (_client_http_port->socket_fd, errmsg, strlen (errmsg), 0, -1.0);
      dl_closesocket (_client_http_port->socket_fd);
      RCS_FD_CLR (_client_http_port->socket_fd, &read_fd_set);
      current_clients--;
      _client_http_port->socket_fd = -1;
      delete _client_http_port;
      client_ports->delete_current_node ();
    }
  return;
}

CLIENT_HTTP_PORT::CLIENT_HTTP_PORT ( int _use_ipv6):
    serial_number(0),
    errors(0), max_errors(0),
    ptr_to_address(0),
    socket_fd(0),
    subscriptions(0),
    blocking(0),
    ptr_to_end_recieved_request(0),
    diag_info(0),
    tid(0),pid(0),
    threadId(0),
    use_ipv6(_use_ipv6)
{
  serial_number = 0;
  errors = 0;
  memset (unprocessed_request, 0, sizeof (unprocessed_request));
  ptr_to_end_recieved_request = unprocessed_request;
  max_errors = 50;
  ptr_to_address = dl_create_sa(0,0,_use_ipv6);
  socket_fd = -1;
  subscriptions = NULL;

#if !defined(MS_WINDOWS_API)
  tid = -1;
  pid = -1;
#else
  tid = 0;
  pid = 0;
#endif
  threadId = 0;
  diag_info = NULL;
}

CLIENT_HTTP_PORT::~CLIENT_HTTP_PORT ()
{
  if (socket_fd > 0)
    {
      dl_closesocket (socket_fd);
      socket_fd = -1;
    }
  if(ptr_to_address)
    {
      dl_free_sa(ptr_to_address);
      ptr_to_address = 0;
    }

#if 0
  if (NULL != subscriptions)
    {
      HTTP_CLIENT_SUBSCRIPTION_INFO *sub_info =
	(HTTP_CLIENT_SUBSCRIPTION_INFO *) subscriptions->get_head ();
      while (NULL != sub_info)
	{
	  delete sub_info;
	  sub_info =
	    (HTTP_CLIENT_SUBSCRIPTION_INFO *) subscriptions->get_next ();
	}
      delete subscriptions;
      subscriptions = NULL;
    }

#ifdef NO_THREADS
  if (NULL != blocking_read_req)
    {
      delete blocking_read_req;
      blocking_read_req = NULL;
    }
#endif

#endif 
// if 0

  if (NULL != diag_info)
    {
      delete diag_info;
      diag_info = NULL;
    }
}

CMS_SERVER_REMOTE_HTTP_PORT::CMS_SERVER_REMOTE_HTTP_PORT(
							 __unused_parameter__ const CMS_SERVER_REMOTE_HTTP_PORT &_csrhp):
  CMS_SERVER_REMOTE_PORT (0),
  dtimeout(0.0),
  read_fd_set(), write_fd_set(),
  maxfdpl(0),
  client_ports(0),
  subscription_buffers(0),
  connection_socket(0),
  connection_port(0),
  ptr_to_server_socket_address(0),
  request(0),
  current_poll_interval_millis(0),
  polling_enabled(0),
  ptr_to_select_timeout(0),
  sockerrno(0),
  sockerrstr(0)
{
  rcs_print_error("CMS_SERVER_REMOTE_HTTP_PORT copy constructor should never be called.\n");
}

CMS_SERVER_REMOTE_HTTP_PORT &
CMS_SERVER_REMOTE_HTTP_PORT::operator=(
				       __unused_parameter__ const CMS_SERVER_REMOTE_HTTP_PORT &_csrhp)
{
  rcs_print_error("CMS_SERVER_REMOTE_HTTP_PORT::operator= should never be called.\n");
  return(*this);
}

//  (defined(ENABLE_RCS_SERVER) && defined(ENABLE_RCS_HTTP))

#else
#include "rcs_empty_source"
#endif


