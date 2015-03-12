/* 
The Nist RCS (Real-time Control Systems) 
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

/************************************************************************
* File: sendn.c
* Purpose: Provides a C file for the sendn function from
* the book Advanced Programming in the UNIX Environment by Richard Stevens.
* The sendn function calls the send function repeatedly until n bytes
* have been written to the file descriptor.
*************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#else
#include "sendn_no_config.h"
#endif

#include "sendn.h"		/* sendn() */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "_timer.h"		/* etime(), esleep() */
#include "sokintrf.h"		/* dl_send(), dl_select(), dl_WSAGetLastError() */
				/* struct timeval, fd_set */

int sendn_timedout = 0;
int print_sendn_timeout_errors = 1;

static int 
sendn_double_is_zero(double d)
{
  if(d > -1e-12 && d < 1e-12)
    {
      return 1;
    }
  return 0;
}

/* Write "n" bytes to a descriptor. */
int
sendn (int fd, const void *vptr, int n, int _flags, double _timeout)
{
  int nleft;
  long nwritten;
  int select_ret;
  double start_time, current_time, timeleft;
  char *ptr;
  struct timeval timeout_tv;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  fd_set send_fd_set;

  timeout_tv.tv_sec = (long) _timeout;
  timeout_tv.tv_usec = (long) (_timeout * 1000000.0);
  if (timeout_tv.tv_usec >= 1000000)
    {
      timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
    }
  FD_ZERO (&send_fd_set);
  RCS_FD_SET (fd, &send_fd_set);

  ptr = (char *) vptr;		/* can't do pointer arithmetic on void* */
  nleft = n;
  current_time = start_time = etime ();
  timeleft = _timeout;
  while (nleft > 0)
    {
      if (sendn_double_is_zero(_timeout))
	{
	  if (_timeout > 0)
	    {
	      current_time = etime ();
	      timeleft = start_time + _timeout - current_time;
	      if (timeleft <= 0.0)
		{
		  if (print_sendn_timeout_errors)
		    {
		      rcs_print_error
			("sendn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f) timed out.\n",
			 fd, vptr, n, _flags, _timeout);
		    }
		  sendn_timedout = 1;
		  return -1;
		}
	      timeout_tv.tv_sec = (long) timeleft;
	      timeout_tv.tv_usec = (long) (timeleft * 1000000.0);
	      if (timeout_tv.tv_usec >= 1000000)
		{
		  timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
		}
	      select_ret =
		dl_select (fd + 1, (fd_set *) NULL, &send_fd_set,
			   (fd_set *) NULL, &timeout_tv);
	    }
	  else
	    {
	      select_ret =
		dl_select (fd + 1, (fd_set *) NULL, &send_fd_set,
			   (fd_set *) NULL, NULL);
	    }
	  switch (select_ret)
	    {
	    case -1:
	      sockerrno = dl_get_last_socket_error_int( select_ret );
	      sockerrstr = dl_get_last_socket_error_string(select_ret,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Error in select: %d -> %s\n", 
			       sockerrno, sockerrstr);
	      rcs_print_error
		("sendn(fd=%d, vptr=%p, int n=%d, int _flags=%d, double _timeout=%f) failed.\n",
		 fd, vptr, n, _flags, _timeout);
	      return -1;

	    case 0:
	      rcs_print_error
		("sendn(fd=%d, vptr=%p, int n=%d, int _flags=%d, double _timeout=%f) timed out.\n",
		 fd, vptr, n, _flags, _timeout);
	      return -1;

	    default:
	      break;
	    }
	}
      if ((nwritten = dl_send (fd, ptr, nleft, _flags)) == -1)
	{
	  sockerrno = dl_get_last_socket_error_int( fd );
	  if(!dl_socket_error_was_would_block( fd ,sockerrno) ||  
	     sendn_double_is_zero(_timeout))
	    {
	      sockerrstr = dl_get_last_socket_error_string(fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Send error: %d = %s\n", 
			       sockerrno, sockerrstr);
	      return (-1);		/* error */
	    }
	  else
	    {
	      nwritten = 0;
	    }
	}
      nleft -= nwritten;
      ptr += nwritten;
      if (nleft > 0 && _timeout > 0.0)
	{
	  current_time = etime ();
	  if (current_time - start_time > _timeout)
	    {
	      rcs_print_error ("sendn: timed out after %f seconds.\n",
			       current_time - start_time);
	      return (-1);
	    }
	  esleep (0.001);
	}
    }
  rcs_print_debug (PRINT_SOCKET_WRITE_SIZE, "wrote %d bytes to %d\n", n, fd);
  return (n);
}
