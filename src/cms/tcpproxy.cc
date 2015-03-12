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
* File: tcpproxy.cc
****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "tcpproxy_no_config.h"
#endif

#include "sokintrf.h"		/* dl_ioctl() */
#include "linklist.hh"		/* RCS_LINKED_LIST */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "tcp_opts.hh"		/* SET_TCP_NODELAY */
#include "timer.hh"

extern "C"
{
#include "recvn.h"		/* recvn() */
#include "sendn.h"		/* sendn() */
}



#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

class TCP_PROXY_ENTRY
{
public:
  TCP_PROXY_ENTRY():client_socket(0),server_socket(0),client_address(),client_record_fd(-1),server_record_fd(-1),client_bytes(0),server_bytes(0) {};

  SOCKET client_socket;
  SOCKET server_socket;
  struct sockaddr_in client_address;
  int client_record_fd;
  int server_record_fd;
  int client_bytes;
  int server_bytes;
};

static int last_pipe_error_id = 0;

static void
handle_pipe_error (int id)
{
  last_pipe_error_id = id;
  rcs_print_error ("SIGPIPE intercepted.\n");
}

int tcpproxy_quit = 0;
static int last_sigint_id = 0;

static void
handle_sigint (int id)
{
  last_sigint_id = id;
  tcpproxy_quit = 1;
}

#ifndef VXWORKS
struct hostent *server_host_entry;
#endif

static bool record_mode=false;
static bool no_text=false;
static bool verbose=false;

#ifdef VXWORKS
extern "C" int tcpproxy (long local_port, char *remote_host,
			 long remote_port);

int
tcpproxy (long local_port, char *remote_host, long remote_port)
{
#else


int
main (int argc, char **argv)
{
  set_rcs_print_destination (RCS_PRINT_TO_STDOUT);

  if(getenv("TCPPROXY_RECORD"))
    {
      record_mode=true;
      printf("record_mode=true\n");
    }

  if(getenv("TCPPROXY_NO_TEXT"))
    {
      no_text=true;
      printf("no_text=true\n");
    }

  if(getenv("TCPPROXY_VERBOSE"))
    {
      verbose=true;
      printf("verbose=true\n");
    }
  

  if (argc < 3)
    {
      rcs_print_error ("insufficient arguments (%d)\n", argc);
      rcs_print_error
	("usage: tcpproxy local-port remote-host [remote-port]\n");
      rcs_print_error ("  Some interesting ports to try:\n");
      rcs_print_error ("    \tEcho:   \t7\n");
      rcs_print_error ("    \tFTP:    \t21\n");
      rcs_print_error ("    \tTelnet: \t23\n");
      rcs_print_error ("    \tHTTP:   \t80\n");
      exit (-1);
    }

  long local_port = strtol (argv[1], NULL, 0);
  char *remote_host = argv[2];
  long remote_port = 0;


  if (argc > 3)
    {
      remote_port = strtol (argv[3], NULL, 0);
    }
  else
    {
      remote_port = local_port;
    }

#endif

  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];

  RCS_LINKED_LIST *proxy_sockets = NULL;
  struct sockaddr_in proxy_socket_address;
  struct sockaddr_in proxy_client_socket_address;
  struct sockaddr_in server_socket_address;

  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      exit (-1);
    }
  proxy_sockets = new RCS_LINKED_LIST;


  /* Get the IP address of the server using it's remote_host. */
  server_socket_address.sin_family = AF_INET;
#ifndef VXWORKS
  dl_modified_gethostbyname (remote_host, &server_host_entry,1);
  if (NULL == server_host_entry)
    {
      rcs_print_error ("Couldn't get host address for (%s).\n", remote_host);
      exit (-1);
    }
#ifdef __MSDOS__
  server_socket_address.sin_addr.s_addr =
    *((u_long *) server_host_entry->h_addr_list[0]);
#else
  server_socket_address.sin_addr.s_addr =
    *((int *) server_host_entry->h_addr_list[0]);
#endif
#else
  server_socket_address.sin_addr.s_addr = hostGetByName (remote_host);
  if (server_socket_address.sin_addr.s_addr == ERROR)
    {
      rcs_print_error ("TCPMEM: Couldn't get host address for (%s).\n",
		       remote_host);
      exit (-1);
    }
#endif
  server_socket_address.sin_port = dl_htons ((unsigned short) remote_port);

  memset (&proxy_socket_address, 0, sizeof (proxy_socket_address));
  proxy_socket_address.sin_family = AF_INET;
  proxy_socket_address.sin_addr.s_addr = htonl (INADDR_ANY);
  proxy_socket_address.sin_port = dl_htons (local_port);

  if(verbose)
    {
      printf("Registering server on TCP port %d.\n",
	     dl_ntohs (proxy_socket_address.sin_port));
    }

  if (proxy_socket_address.sin_port == 0)
    {
      rcs_print_error ("server can not register on port number 0.\n");
      exit (-1);
    }
  SOCKET connection_socket;

  if ((long) (connection_socket = dl_tcp_socket (0)) <= 0)
    {
      rcs_print_error ("socket error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error ("Server can not open stream socket.\n");
      exit (-1);
    }

  if (set_tcp_socket_options (connection_socket) < 0)
    {
      exit (-1);
    }
  if (dl_bind (connection_socket, (struct sockaddr *) &proxy_socket_address,
	       sizeof (proxy_socket_address)) < 0)
    {
      rcs_print_error ("bind error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error
	("Server can not bind the connection socket on port %d.\n",
	 dl_ntohs (proxy_socket_address.sin_port));
      exit (-1);
    }
  if (dl_listen (connection_socket, 50) < 0)
    {
      rcs_print_error ("listen error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error ("TCP Server: error on call to listen for port %d.\n",
		       dl_ntohs (server_socket_address.sin_port));
      exit (-1);
    }

  int bytes_ready;
  int ready_descriptors;
  fd_set read_fd_set, write_fd_set;
  FD_ZERO (&read_fd_set);
  FD_ZERO (&write_fd_set);
  RCS_FD_SET (connection_socket, &read_fd_set);
  SOCKET maxfdpl;
  maxfdpl = connection_socket + 1;
  TCP_PROXY_ENTRY *current_entry;

#ifndef DOS_WINDOWS
  signal (SIGPIPE, handle_pipe_error);
#endif
  signal (SIGINT, handle_sigint);
  rcs_print
    ("Running TCP proxy  for:\n (TCP port %d) \n(connection_socket = %d)\n(remote_host = %s)\n(server_IP_address = %s)\n(remote_port = %d)",
     dl_ntohs (proxy_socket_address.sin_port), connection_socket, remote_host,
     dl_inet_ptr_ntoa (&server_socket_address.sin_addr), remote_port);

  char temp_buffer[4097];
  int client_count=0;

  double diff, cur_time,last_time;
  
  cur_time=etime();
  last_time = cur_time;


  while (!tcpproxy_quit)
    {
      ready_descriptors =
	dl_select (maxfdpl,
		   &read_fd_set,
		   &write_fd_set, (fd_set *) NULL, (timeval *) NULL);
      if (ready_descriptors < 0)
	{
	  rcs_print_error
	    ("select(%d,%d,%d,NULL,NULL) error.(errno = %d | %s)\n", maxfdpl,
	     read_fd_set, write_fd_set, errno, strerror (errno));
	  continue;
	}
      current_entry = (TCP_PROXY_ENTRY *) proxy_sockets->get_head ();
      while (NULL != current_entry)
	{
	  if (dl_fd_isset (current_entry->client_socket, &read_fd_set))
	    {
	      ready_descriptors--;
	      dl_ioctlsocket_fionread(current_entry->client_socket,&bytes_ready);

	      //	      dl_ioctlsocket (current_entry->client_socket, FIONREAD,
	      //		      &bytes_ready);
	      
	      cur_time=etime();
	      diff = cur_time-last_time;
	      last_time = cur_time;
	      if (bytes_ready == 0)
		{
		  rcs_print ("Socket closed by host with IP address %s port=%d at %f,%f.\n",
			     dl_inet_ptr_ntoa (&current_entry->client_address.
					   sin_addr),
			     dl_ntohs(current_entry->client_address.sin_port),
			     cur_time,diff);
		  dl_closesocket (current_entry->server_socket);
		  dl_closesocket (current_entry->client_socket);
		  if(record_mode)
		    {
		      close(current_entry->client_record_fd);
		      close(current_entry->server_record_fd);
		    }

		  if (dl_fd_isset
		      (current_entry->server_socket, &read_fd_set))
		    {
		      RCS_FD_CLR (current_entry->server_socket, &read_fd_set);
		    }
		  if (dl_fd_isset
		      (current_entry->client_socket, &read_fd_set))
		    {
		      RCS_FD_CLR (current_entry->client_socket, &read_fd_set);
		    }
		  proxy_sockets->delete_current_node ();
		  current_entry =
		    (TCP_PROXY_ENTRY *) proxy_sockets->get_next ();
		  continue;
		}
	      else
		{
		  while (bytes_ready > 0)
		    {
		      if (bytes_ready > 4096)
			{
			  recvn (current_entry->client_socket, temp_buffer,
				 4096, 0, -1, NULL);
			  sendn (current_entry->server_socket, temp_buffer,
				 4096, 0, -1);
			  current_entry->client_bytes += 4096;
			  if(record_mode)
			    {
			      write(current_entry->client_record_fd,temp_buffer,4096);
			      fsync(current_entry->client_record_fd);
			      fprintf(stderr,"client %d %d\n",current_entry->client_bytes,4096);
			    }
			  cur_time=etime();
			  diff = cur_time-last_time;
			  last_time = cur_time;
			  if(verbose)
			    {
			      temp_buffer[4096]=0;
			      if(no_text)
				{
				  printf("client 4096 --\t");
				}
			      else
				{
				  printf("client %f: 4096 %s -- \t",diff,temp_buffer);
				}
			      for(int i = 0; i < 4096; i++)
				{
				  unsigned int ui = ((unsigned int) temp_buffer[i] & 0xff);
				  if(i%4== 0)
				    {
				      printf(" ");
				    }
				  printf("%2.2x",ui);
				}
			      printf("\n");
			      fflush(stdout);
			    }
			  bytes_ready -= 4096;
			}
		      else
			{
			  recvn (current_entry->client_socket, temp_buffer,
				 bytes_ready, 0, -1, NULL);
			  sendn (current_entry->server_socket, temp_buffer,
				 bytes_ready, 0, -1);
			  current_entry->client_bytes += bytes_ready;

			  if(record_mode)
			    {
			      write(current_entry->client_record_fd,temp_buffer,bytes_ready);
			      fsync(current_entry->client_record_fd);
			      fprintf(stderr,"client %d %d\n",current_entry->client_bytes,bytes_ready);

			    }
			  cur_time=etime();
			  diff = cur_time-last_time;
			  last_time = cur_time;
			  if(verbose)
			    {
			      temp_buffer[bytes_ready]=0;
			      if(no_text)
				{
				  printf("client %d --\t",bytes_ready);
				}
			      else
				{
				  printf("client %f: %d %s -- \t",diff,bytes_ready, temp_buffer);
				}
			      for(int i = 0; i < bytes_ready; i++)
				{
				  unsigned int ui = ((unsigned int) temp_buffer[i] & 0xff);
				  if(i%4== 0)
				    {
				      printf(" ");
				    }
				  printf("%2.2x",ui);
				}
			      printf("\n");
			      fflush(stdout);
			    }
			  bytes_ready = 0;
			  break;
			}
		    }
		}
	    }
	  else
	    {
	      RCS_FD_SET (current_entry->client_socket, &read_fd_set);
	    }
	  if (dl_fd_isset (current_entry->server_socket, &read_fd_set))
	    {
	      ready_descriptors--;
	      dl_ioctlsocket_fionread(current_entry->server_socket,&bytes_ready);

#if 0
#if MS_WINDOWS_API
	      //      dl_ioctlsocket (current_entry->server_socket, FIONREAD,
	      //	      &bytes_ready);
#else
#ifndef VXWORKS
	  // ioctl (current_entry->server_socket, FIONREAD,
	  //	     (caddr_t) & bytes_ready);
#else
      //  ioctl (current_entry->server_socket, FIONREAD,
      //	     (int) &bytes_ready);
	      //VXWORKS
#endif
	      //MS_WINDOWS_API
#endif
	      // 0
#endif

	      if (bytes_ready == 0)
		{
		  rcs_print ("Socket closed by host with IP address %s.\n",
			     dl_inet_ptr_ntoa (&server_socket_address.sin_addr));
		  dl_closesocket (current_entry->server_socket);
		  dl_closesocket (current_entry->client_socket);
		  if(record_mode)
		    {
		      if(current_entry->client_record_fd > 0)
			{
			  fsync(current_entry->client_record_fd);
			  close(current_entry->client_record_fd);
			  current_entry->client_record_fd=-1;
			}
		      if(current_entry->server_record_fd > 0)
			{
			  fsync(current_entry->server_record_fd);
			  close(current_entry->server_record_fd);
			  current_entry->server_record_fd=-1;
			}
		    }
		      
		  if (dl_fd_isset
		      (current_entry->server_socket, &read_fd_set))
		    {
		      RCS_FD_CLR (current_entry->server_socket, &read_fd_set);
		    }
		  if (dl_fd_isset
		      (current_entry->client_socket, &read_fd_set))
		    {
		      RCS_FD_CLR (current_entry->client_socket, &read_fd_set);
		    }
		  proxy_sockets->delete_current_node ();
		  current_entry =
		    (TCP_PROXY_ENTRY *) proxy_sockets->get_next ();
		  continue;
		}
	      else
		{
		  while (bytes_ready > 0)
		    {
		      if (bytes_ready > 4096)
			{
			  recvn (current_entry->server_socket, temp_buffer,
				 4096, 0, -1, NULL);
			  sendn (current_entry->client_socket, temp_buffer,
				 4096, 0, -1);
			  if(record_mode)
			    {
			      write(current_entry->server_record_fd,temp_buffer,4096);
			      fsync(current_entry->server_record_fd);
			    }
			  cur_time=etime();
			  diff = cur_time-last_time;
			  last_time = cur_time;
			  if(verbose)
			    {
			      temp_buffer[4095]=0;
			      if(no_text)
				{
				  printf("server 4096 --\t");
				}
			      else
				{
				  printf("server %f: 4096 %s -- \t",diff,temp_buffer);
				}
			      for(int i = 0; i < 4096; i++)
				{
				  unsigned int ui = ((unsigned int) temp_buffer[i] & 0xff);
				  if(i%4== 0)
				    {
				      printf(" ");
				    }
				  printf("%2.2x",ui);
				}
			      printf("\n");
			      fflush(stdout);
			    }
			  bytes_ready -= 4096;
			}
		      else
			{
			  recvn (current_entry->server_socket, temp_buffer,
				 bytes_ready, 0, -1, NULL);
			  sendn (current_entry->client_socket, temp_buffer,
				 bytes_ready, 0, -1);
			  if(record_mode)
			    {
			      write(current_entry->server_record_fd,temp_buffer,bytes_ready);
			      fsync(current_entry->server_record_fd);
			    }
			  cur_time=etime();
			  diff = cur_time-last_time;
			  last_time = cur_time;
			  if(verbose)
			    {
			      temp_buffer[bytes_ready]=0;
			      if(no_text)
				{
				  printf("server %d -- \t",bytes_ready);
				}
			      else
				{
				  printf("server %f: %d %s -- \t",diff,bytes_ready,temp_buffer);
				}
			      for(int i = 0; i < bytes_ready; i++)
				{
				  unsigned int ui = ((unsigned int) temp_buffer[i] & 0xff);
				  if(i%4== 0)
				    {
				      printf(" ");
				    }
				  printf("%2.2x",ui);
				}
			      printf("\n");
			      fflush(stdout);
			    }
			  bytes_ready = 0;
			  break;
			}
		    }
		}
	    }
	  else
	    {
	      RCS_FD_SET (current_entry->server_socket, &read_fd_set);
	    }
	  current_entry = (TCP_PROXY_ENTRY *) proxy_sockets->get_next ();
	}
      if (dl_fd_isset (connection_socket, &read_fd_set)
	  && ready_descriptors > 0)
	{
	  ready_descriptors--;
	  current_entry = new TCP_PROXY_ENTRY;
	  if (NULL == current_entry)
	    {
	      continue;
	    }
	  current_entry->server_socket = dl_tcp_socket(0);
	  if (((long) current_entry->server_socket) <= 0)
	    {
	      rcs_print_error ("Error from socket() (errno = %d:%s)\n",
			       errno, strerror (errno));
	      delete current_entry;
	      continue;
	    }
	  proxy_client_socket_address.sin_family = AF_INET;
	  proxy_client_socket_address.sin_addr.s_addr = htonl (INADDR_ANY);
	  proxy_client_socket_address.sin_port = dl_htons (0);

	  if (dl_bind (current_entry->server_socket,
		       (struct sockaddr *) &proxy_client_socket_address,
		       sizeof (proxy_client_socket_address)) < 0)
	    {	
	      sockerrno= dl_get_last_socket_error_int( current_entry->server_socket );
	      sockerrstr = dl_get_last_socket_error_string(current_entry->server_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error (" bind error %d = %s\n", 
			       sockerrno,sockerrstr);
	      delete current_entry;
	      continue;
	    }
	  if (dl_connect (current_entry->server_socket,
			  (struct sockaddr *) &server_socket_address,
			  sizeof (server_socket_address)) < 0)
	    {
	      sockerrno= dl_get_last_socket_error_int( current_entry->server_socket );
	      sockerrstr = dl_get_last_socket_error_string(current_entry->server_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Couldn't connect. Error = (%d:%s)\n",
			       sockerrno, sockerrstr);
	      delete current_entry;
	      current_entry = NULL;
	      continue;
	    }
	  int client_address_length;
	  client_address_length = sizeof (current_entry->client_address);
	  current_entry->client_socket = dl_accept (connection_socket,
						    (struct sockaddr *)
						    &
						    (current_entry->
						     client_address),
						    &client_address_length);
	  client_count++;
	  if(record_mode)
	    {
	      char fname[256];
	      SNPRINTF_FUNC( SNPRINTF_ARGS(fname,sizeof(fname)),
			    "tcpproxy_client_%s_%s_%s_%d_%d.dat",
			    argv[1],argv[2],
			    dl_inet_ptr_ntoa (&current_entry->client_address.sin_addr),	      
			    getpid(),client_count);
	      current_entry->client_record_fd = open(fname,O_CREAT | O_WRONLY | O_TRUNC ,0666);
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(fname,sizeof(fname)),
			      "tcpproxy_server_%s_%s_%s_%d_%d.dat",
			      argv[1],argv[2],
			      dl_inet_ptr_ntoa (&current_entry->client_address.sin_addr),	      
			      getpid(),client_count);
	      current_entry->server_record_fd = open(fname,O_CREAT | O_WRONLY | O_TRUNC, 0666);
	    }

	  if (((long) current_entry->client_socket) <= 0)
	    {
	      sockerrno= dl_get_last_socket_error_int( current_entry->client_socket );
	      sockerrstr = dl_get_last_socket_error_string(current_entry->client_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("accept error = (%d:%s)\n",
			       sockerrno, sockerrstr);
	      continue;
	    }
	  cur_time=etime();
	  diff = cur_time-last_time;
	  last_time = cur_time;
	  rcs_print ("Socket opened by host with IP address %s port=%d at %f,%f.\n",
		     dl_inet_ptr_ntoa (&current_entry->client_address.sin_addr),
		     dl_ntohs(current_entry->client_address.sin_port),
		     cur_time,diff);
	  if (NULL != proxy_sockets)
	    {
	      proxy_sockets->store_at_tail (current_entry,
					    sizeof (TCP_PROXY_ENTRY), 0);
	    }
	  if (maxfdpl < current_entry->client_socket + 1)
	    {
	      maxfdpl = current_entry->client_socket + 1;
	    }
	  if (maxfdpl < current_entry->server_socket + 1)
	    {
	      maxfdpl = current_entry->server_socket + 1;
	    }
	  RCS_FD_SET (current_entry->server_socket, &read_fd_set);
	  RCS_FD_SET (current_entry->client_socket, &read_fd_set);
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
    }


  current_entry = (TCP_PROXY_ENTRY *) proxy_sockets->get_head ();
  while (NULL != current_entry)
    {
      dl_closesocket (current_entry->server_socket);
      dl_closesocket (current_entry->client_socket);
      if(record_mode)
	{
	  if(current_entry->client_record_fd > 0)
	    {
	      fsync(current_entry->client_record_fd);
	      close(current_entry->client_record_fd);
	      current_entry->client_record_fd=-1;
	    }
	  if(current_entry->server_record_fd > 0)
	    {
	      fsync(current_entry->server_record_fd);
	      close(current_entry->server_record_fd);
	      current_entry->server_record_fd=-1;
	    }
	}
      current_entry = (TCP_PROXY_ENTRY *) proxy_sockets->get_next ();
    }
  dl_closesocket (connection_socket);
  delete proxy_sockets;
  unload_socket_interface();
  sync();
  fprintf(stderr,"\ntcpproxy %s %s exiting.\n",argv[1],argv[2]);
  fflush(stdout);
  fflush(stderr);
  exit (0);
}
