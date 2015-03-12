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

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_HTTP)

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "httpnml_no_config.h"
#endif

#include "httpnml.hh"
#include "tcp_opts.hh"		/* SET_TCP_NODELAY */
#include "sokintrf.h"		/* dl_closesocket(), dl_gethostbyname(), dl_sock*/
#include "rcs_prnt.hh"
#include "timer.hh"
#include "sendn.h"
#include "recvn.h"

HttpXmlNml::HttpXmlNml( NML_FORMAT_PTR f_ptr, const char *url) : 
  NML(f_ptr,0,0,0),
  hostname(0),
  hostname_len(0),
  get_msg(0),
  get_msg_len(0),
  server_socket_address_ptr(0),
  tcp_port_number(0),
  socket_fd(0),
  buffer(0),
  buffer_size(0),
  sockerrno(0),
  sockerrstr(0),
  use_ipv6(0)
{
  hostname=0;
  hostname_len=0;
  get_msg=0;
  get_msg_len=0;
  tcp_port_number=80;
  socket_fd=-1;
  buffer  =0 ;
  buffer_size =0;

  if(strncmp(url,"http://",7))
    {
      rcs_print_error("HttpXmlNml bad URL=%s\n",url);
      return;
    }
  hostname_len = strcspn(url+7,":/?");
  if(hostname_len <= 0)
    {
      rcs_print_error("HttpXmlNml bad URL=%s\n",url);
      return;
    }
  hostname = (char *) malloc(hostname_len + 1);
  memcpy(hostname,url+7,hostname_len);
  hostname[hostname_len] = 0;
  if(*(url+7+hostname_len) == ':')
    {
      tcp_port_number = strtol(url+8+hostname_len,0,0);
    }
  server_socket_address_ptr = 
    dl_create_sa(hostname,
		 (short) tcp_port_number, 
		 use_ipv6);
  if(!server_socket_address_ptr)
    {
      rcs_print_error("HttpXmlNml bad URL=%s\n",url);
      return;
    }

  const char *pathstart = strchr(url+7,'/');
  if(!pathstart)
    {
      rcs_print_error("HttpXmlNml bad URL=%s\n",url);
      return;
    }
  const size_t get_msg_max_size = strlen(pathstart) + 10;
  get_msg = (char *) malloc(get_msg_max_size);
  SNPRINTF_FUNC ( SNPRINTF_ARGS(get_msg,get_msg_max_size),
		  "GET %s\r\n",pathstart);
  get_msg_len = strlen(get_msg);


  buffer_size = 8192;
  buffer = (char *) malloc(buffer_size);

  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Using server on %s with IP address %s and port %d.\n",
		   hostname,
		   dl_sa_get_host(server_socket_address_ptr),
		   tcp_port_number);
}

HttpXmlNml::~HttpXmlNml()
{
  if(get_msg)
    {
      free(get_msg);
      get_msg=0;
    }
  if(server_socket_address_ptr)
    {
      dl_free_sa(server_socket_address_ptr);
      server_socket_address_ptr=0;
    }
  if(socket_fd > 0)
    {
      dl_closesocket(socket_fd);
      socket_fd=-1;
    }
}
  

NMLmsg *
HttpXmlNml::readMsg(void)
{
  if(!buffer)
    {
      rcs_print_error("HttpXmlNML: not initialized.\n");
    }

  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Creating socket . . .\n");
  socket_fd = dl_tcp_socket (use_ipv6);
  if (socket_fd < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("HTTPNML: Error from socket() (errno = %d:%s)\n",
		       sockerrno, sockerrstr);
      return(0);
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Setting socket options . . . \n");
  if (set_tcp_socket_options (socket_fd) < 0)
    {
      return(0);
    }
  double timeout = 10.0;
  struct timeval timeval_timeout;
  int socket_ret;
  double start_time, current_time;
  fd_set fds;

  struct dl_sa *cli_addr_ptr = dl_create_sa(0,0,use_ipv6);  
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Binding . . . \n");
  if (dl_bind (socket_fd, dl_sa_addr(cli_addr_ptr), dl_sa_len(cli_addr_ptr)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("HTTPNML: bind error %d = %s\n", 
		       sockerrno, sockerrstr);
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Connecting . . .\n");
  if (dl_connect (socket_fd, dl_sa_addr(server_socket_address_ptr),
		  dl_sa_len(server_socket_address_ptr)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      if ( dl_socket_error_was_would_block( socket_fd ,sockerrno))
	{
#if MS_WINDOWS_API
	  timeval_timeout.tv_sec = 0;
	  timeval_timeout.tv_usec = 0;
#else
	  timeval_timeout.tv_sec = (long) timeout;
	  timeval_timeout.tv_sec = (long) (fmod (timeout, 1.0) * 1e6);
#endif
	  FD_ZERO (&fds);
	  RCS_FD_SET (socket_fd, &fds);
	  start_time = etime ();
	  while (!(socket_ret = dl_select (socket_fd + 1,
					   (fd_set *) NULL,
					   &fds, (fd_set *) NULL, &timeval_timeout)))
	    {
	      RCS_FD_SET (socket_fd, &fds);
	      esleep (0.001);
	      current_time = etime ();
	      double timeleft = start_time + timeout - current_time;
	      if (timeleft <= 0.0 && timeout >= 0.0)
		{
		  rcs_print_error
		    ("HTTPXMLNML: Timed out waiting for connection.\n");
		  return(0);
		}
#if MS_WINDOWS_API
	      timeval_timeout.tv_sec = 0;
	      timeval_timeout.tv_usec = 0;
#else
	      timeval_timeout.tv_sec = (long) timeleft;
	      timeval_timeout.tv_sec = (long) (fmod (timeleft, 1.0) * 1e6);
#endif
	    }

	  if (dl_select_ret_is_error(socket_ret))
	    {
	      sockerrno = dl_get_last_socket_error_int( socket_fd );
	      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("select error: %d -- %s\n", 
			       sockerrno, sockerrstr);
	      rcs_print_error ("HTTPNML: Couldn't connect.\n");
	      return(0);
	    }
	}
      else
	{
	  sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("connect error: %d -- %s\n", 
			   sockerrno,sockerrstr);
	  rcs_print_error
	    ("HTTPXMLNML: Error trying to connect to TCP port %d of host %s(%s).\n",
	     dl_sa_get_port(server_socket_address_ptr),
	     hostname,
	     dl_sa_get_host(server_socket_address_ptr));
	  return(0);
	}
    }
  if(socket_fd < 0)
    {
      return 0;
    }

  if(sendn(socket_fd,get_msg,get_msg_len,0,-1) < 0)
    {
      return 0;
    }
  
  buffer[0]=0;
  char *ptr = buffer;
  int recvn_return;
  do 
    {
      recvn_return = recv(socket_fd,ptr,buffer_size - (ptr-buffer), 0,1);
      if(recvn_return > 0)
	{
	  ptr += recvn_return;
	}
      if(buffer_size - (ptr-buffer) < 4096)
	{
	  buffer_size += 8192;
	  buffer = (char *) realloc(buffer,buffer_size);
	}
    }
  while(recvn_return > 0) ;

  
  if(xml2msg(buffer) > 0)
    {
      dl_closesocket(socket_fd);
      socket_fd=0;
      if(cli_addr_ptr)
	{
	  dl_free_sa(cli_addr_ptr);
	  cli_addr_ptr=0;
	}
      return get_address();
    }
  
  dl_closesocket(socket_fd);
  if(cli_addr_ptr)
    {
      dl_free_sa(cli_addr_ptr);
      cli_addr_ptr=0;
    }
  socket_fd =0;
  return 0;
}


//  defined(ENABLE_RCS_HTTP)
#else
#include "rcs_empty_source"
#endif
