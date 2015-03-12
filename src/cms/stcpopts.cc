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

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "stcpopts_no_config.h"
#endif

#include "stcpopts.hh"
#include "rcs_prnt.hh"		/* rcs_print_error()  */
#include "sokintrf.h"		// all the socket defs

int stcp_nonblocking = 1;

int
set_stcp_socket_options (int socket_fd)
{
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];

  if (socket_fd <= 0)
    {
      return -1;
    }
  int optval = 1;
#ifdef TCP_NODELAY
  if (dl_setsockopt (socket_fd, IPPROTO_TCP, TCP_NODELAY,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", sockerrno, sockerrstr);
      return -1;
    }
#endif
  optval = 1;

#ifdef SO_REUSEADDR
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_REUSEADDR,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("errno = %d = %s\n", sockerrno, sockerrstr);
      return -1;
    }
#endif

  if (stcp_nonblocking)
    {
#if MS_WINDOWS_API
      unsigned long arg = 1;
      if (dl_ioctlsocket (socket_fd, FIONBIO, &arg) < 0)
	{
	  rcs_print_error ("Can't set  socket to non-blocking.");
	}
#else
#ifndef VXWORKS
#if defined(O_NDELAY) && !defined(O_NONBLOCK)
      if (-1 == fcntl (socket_fd, F_SETFL, O_NDELAY))
	{
	  rcs_print_error ("Couldn's set flag for no delay on socket.\n");
	}
#endif
#ifdef O_NONBLOCK
      if (-1 == fcntl (socket_fd, F_SETFL, O_NONBLOCK))
	{
	  rcs_print_error ("Couldn's set flag for non-blocking on socket.\n");
	}
#endif
#endif
#endif
    }
  return (0);
}
