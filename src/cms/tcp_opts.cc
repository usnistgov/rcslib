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
#include "tcpopts_no_config.h"
#endif

#include "sokintrf.h"		// all the socket defs
#include "tcp_opts.hh"
#include "rcs_prnt.hh"		/* rcs_print_error()  */

int
set_tcp_socket_options (int socket_fd, int)
{
  if (socket_fd <= 0)
    {
      return -1;
    }
  int optval = 1;
  //socklen_t optval_len=sizeof(optval);
#ifdef TCP_NODELAY
  rcs_print_debug(PRINT_MISC,"setsockopt(%d, IPPROTO_TCP=%d, TCP_NODELAY=%d,%p,%lu)\n",
		  socket_fd,
		  ((int)IPPROTO_TCP),
		  ((int)TCP_NODELAY),
		  (void*)(&optval),
		  (unsigned long) sizeof(optval));

  if (dl_setsockopt (socket_fd, IPPROTO_TCP, TCP_NODELAY,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
#endif // TCP_NODELAY

  optval = 1;

#ifdef SO_REUSEADDR
  rcs_print_debug(PRINT_MISC,"setsockopt(%d, SOL_SOCKET, SO_REUSEADDR,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_REUSEADDR,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
#endif

#if 0
#ifdef SO_SNDBUF
  optval=0;
  rcs_print_debug(PRINT_MISC,"getsockopt(%d, SOL_SOCKET, SO_SNDBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (getsockopt (socket_fd, SOL_SOCKET, SO_SNDBUF,
		     (char *) &optval, &optval_len) < 0)
    {
      rcs_print_error (" Can`t get a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  rcs_print("SO_SNDBUF=%d\n",optval);
  optval = 65536*8;
  rcs_print_debug(PRINT_MISC,"setsockopt(%d, SOL_SOCKET, SO_SNDBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_SNDBUF,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  optval=0;
  rcs_print_debug(PRINT_MISC,"getsockopt(%d, SOL_SOCKET, SO_SNDBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (getsockopt (socket_fd, SOL_SOCKET, SO_SNDBUF,
		     (char *) &optval, &optval_len) < 0)
    {
      rcs_print_error (" Can`t get a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  rcs_print("SO_SNDBUF=%d\n",optval);

#endif

#ifdef SO_RCVBUF
  optval=0;
  rcs_print_debug(PRINT_MISC,"getsockopt(%d, SOL_SOCKET, SO_RCVBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (getsockopt (socket_fd, SOL_SOCKET, SO_RCVBUF,
		     (char *) &optval, &optval_len) < 0)
    {
      rcs_print_error (" Can`t get a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  rcs_print("SO_RCVBUF=%d\n",optval);
  optval = 65536;
  rcs_print_debug(PRINT_MISC,"setsockopt(%d, SOL_SOCKET, SO_RCVBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (dl_setsockopt (socket_fd, SOL_SOCKET, SO_RCVBUF,
		     (char *) &optval, sizeof (optval)) < 0)
    {
      rcs_print_error (" Can`t set a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  optval=0;
  rcs_print_debug(PRINT_MISC,"getsockopt(%d, SOL_SOCKET, SO_RCVBUF,%p,%lu)\n",
		  socket_fd,(void*)(&optval),(unsigned long)sizeof(optval));
  if (getsockopt (socket_fd, SOL_SOCKET, SO_RCVBUF,
		     (char *) &optval, &optval_len) < 0)
    {
      rcs_print_error (" Can`t get a socket option.\n");
      rcs_print_error ("errno = %d = %s\n", errno, strerror (errno));
      return -1;
    }
  rcs_print("SO_RCVBUF=%d\n",optval);
#endif
#endif

  return 0;
}


int
make_tcp_socket_nonblocking (int socket_fd, int /* use_ipv6 */ )
{
#if MS_WINDOWS_API
  unsigned long arg = 1;
  rcs_print_debug(PRINT_MISC,"ioctlsocket(%d, FIONBIO,%p) (arg=%lu)\n",
		  socket_fd,(void*)&arg,arg);
  if (dl_ioctlsocket (socket_fd, FIONBIO, &arg) < 0)
    {
      rcs_print_error ("Can't set  socket to non-blocking.");
    }
#else
#ifndef VXWORKS
#ifdef O_NONBLOCK
  rcs_print_debug(PRINT_MISC,"fcntl(%d,F_SETFL, O_NONBLOCK)\n",
		  socket_fd);
  if (-1 == fcntl (socket_fd, F_SETFL, O_NONBLOCK))
    {
      rcs_print_error ("Couldn's set flag for non-blocking on socket.\n");
      return -1;
    }
#else
#ifdef O_NDELAY
  rcs_print_debug(PRINT_MISC,"fcntl(%d,F_SETFL, O_NDELAY)\n",
		  socket_fd);
  if (-1 == fcntl (socket_fd, F_SETFL, O_NDELAY))
    {
      rcs_print_error ("Couldn's set flag for no delay on socket.\n");
      return -1;
    }
#endif
#endif

#else
// VXWORKS
  rcs_print_debug(PRINT_MISC,"ioctl(%d,FIONBIO,1)\n",
		  socket_fd);

  // See email: rpatel@gdrs.com titled "small nml app". Sep 24,2009 
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
make_tcp_socket_blocking (int socket_fd, int /* use_ipv6 */ )
{
#if MS_WINDOWS_API
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
#endif

#if MS_WINDOWS_API
  unsigned long arg = 0;
  rcs_print_debug(PRINT_MISC,"ioctlsocket(%d, FIONBIO, %p) (arg=%lu)\n",
		  socket_fd,(void*)&arg,arg);
  if (dl_ioctlsocket (socket_fd, FIONBIO, &arg) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("Can't set  socket to blocking. %d -- %s\n",
		       sockerrno,sockerrstr);
    }
#else
#ifdef VXWORKS
  rcs_print_debug(PRINT_MISC,"ioctl(%d,FIONBIO,0)\n",
		  socket_fd);
  if (-1 == ioctl (socket_fd, FIONBIO, 0))
    {
      rcs_print_error ("Couldn's set flag for blocking on socket.: %d,%s\n",
		       errno, strerror (errno));
      return -1;
    }
#else

#if defined(O_NONBLOCK) || defined(O_NDELAY)
  int val = fcntl (socket_fd, F_GETFL, 0);
  rcs_print_debug(PRINT_MISC,"fcntl(%d,F_GETFL,0) returned %d\n",
		  socket_fd,val);
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
  rcs_print_debug(PRINT_MISC,"fcntl(%d,F_GETFL,%d)\n",
		  socket_fd,val);
  if (fcntl (socket_fd, F_SETFL, val) < 0)
    {
      rcs_print_error ("Couldn's set flag for blocking on socket.: %d,%s\n",
		       errno, strerror (errno));
      return -1;
    }
#endif
#endif
#endif
  return (0);
}
