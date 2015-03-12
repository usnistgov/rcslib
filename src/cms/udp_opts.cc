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
#include "rcs_config_include.h"
#else
#include "udp_opts_no_config.h"
#endif

#include "sokintrf.h"		// all the socket defs
#include "udp_opts.hh"
#include "rcs_prnt.hh"		/* rcs_print_error()  */

int
set_udp_socket_options (int socket_fd)
{
  if (socket_fd <= 0)
    {
      return -1;
    }
  int optval = 1;
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_REUSEADDR,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;;
    }
#if 0
  if (-1 == fcntl (socket_fd, F_SETFL, O_NDELAY))
    {
      rcs_print_error ("Couldn's set flag for no delay on socket.\n");
    }
#endif
  return (0);
}


int
make_udp_socket_nonblocking (int socket_fd)
{
#if MS_WINDOWS_API
  unsigned long long_arg = 1;
  if (dl_ioctlsocket (socket_fd, FIONBIO, &long_arg) < 0)
    {
      rcs_print_error ("Can't set  socket to non-blocking.");
    }
#else
#ifndef VXWORKS
#ifdef MSDOS
  int int_arg = 1;
  if (-1 == tk_ioctl (socket_fd, FIONBIO, &int_arg))
    {
      rcs_print_error ("Can't set  socket to non-blocking. : %d %s\n",
		       tk_geterrno (socket_fd), tk_sperror ());
      return -1;
    }
#else


#if defined(O_NDELAY) && !defined(O_NONBLOCK)
  if (-1 == fcntl (socket_fd, F_SETFL, O_NDELAY))
    {
      rcs_print_error ("Couldn's set flag for no delay on socket.\n");
      return -1;
    }
#endif
#ifdef O_NONBLOCK
  if (-1 == fcntl (socket_fd, F_SETFL, O_NONBLOCK))
    {
      rcs_print_error ("Couldn's set flag for non-blocking on socket.\n");
      return -1;
    }
#endif
#endif
#else
// VXWORKS
  
  // See email: from rpatel@gdrs.com titled "small nml app". Sep 24,2009 
  int arg=1;
  if (-1 == ioctl (socket_fd, FIONBIO, (int) (&arg)))
    {
      rcs_print_error ("Couldn's set flag for non-blocking on socket.\n");
      return -1;
    }
#endif
#endif
  return (0);
}


int
make_udp_socket_blocking (int socket_fd)
{
#if MS_WINDOWS_API
  unsigned long long_arg = 0;
  if (dl_ioctlsocket (socket_fd, FIONBIO, &long_arg) < 0)
    {
      rcs_print_error ("Can't set  socket to non-blocking.");
    }
#else
#ifndef VXWORKS
#ifdef MSDOS
  int int_arg = 0;
  if (-1 == tk_ioctl (socket_fd, FIONBIO, &int_arg))
    {
      rcs_print_error ("Can't set  socket to non-blocking. : %d %s\n",
		       tk_geterrno (socket_fd), tk_sperror ());
      return -1;
    }
#else


#if defined(O_NONBLOCK) || defined(O_NDELAY)
  int val = fcntl (socket_fd, F_GETFL, 0);
  if (val < 0)
    {
      rcs_print_error ("fcntl error %d %s\n", errno, strerror (errno));
      return -1;
    }
#ifdef O_NONBLOCK
  val &= ~O_NONBLOCK;
#endif
#ifdef O_NDELAY
  val &= ~O_NDELAY;
#endif
  if (fcntl (socket_fd, F_SETFL, val) < 0)
    {
      rcs_print_error ("Couldn's set flag for blocking on socket.: %d,%s\n",
		       errno, strerror (errno));
      return -1;
    }
#endif
#endif

#else
// VXWORKS
  if (-1 == ioctl (socket_fd, FIONBIO, 0))
    {
      rcs_print_error ("Couldn's set flag for non-blocking on socket.\n");
      return -1;
    }
#endif
#endif
  return (0);
}



int
make_udp_socket_broadcast (int socket_fd)
{
  if (socket_fd <= 0)
    {
      return -1;
    }
  int optval = 1;
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_BROADCAST,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error ("setsockopt(%d,SOL_SOCKET,SO_\n",socket_fd);
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;;
    }
  return 0;
}
