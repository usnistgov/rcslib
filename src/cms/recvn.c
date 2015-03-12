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

/************************************************************************
* File: recvn.c
* Purpose: Provides a C file for the recvn function from
* the book Advanced Programming in the UNIX Environment  by Richard Stevens.
* The recvn function calls the recv function repeatedly until n bytes
* have been recv from the file descriptor.
* It uses select and FIONREAD checks ahead of time to guarantee that even
* if the socket is blocking the timeout will be enforced.
* To retry a socket to for the data missed during past timeouts the
* application should pass recvn the same buffer and address of a variable
* storing the number of bytes read on previous attempts.
*************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "recvn_no_config.h"
#endif

#include "recvn.h"		/* recvn(int, void *, int, double) */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "sokintrf.h"		/* dl_select(), dl_recv(), dl_WSAGetLastError() */
				/* struct timeval, fd_set */
#include "_timer.h"		/* etime(), esleep() */

int recvn_timedout = 0;
int print_recvn_timeout_errors = 1;

/* Read "n" bytes from a descriptor. */
int
recvn (int fd, void *vptr, int n, int flags, double _timeout,
       int *bytes_read_ptr,
       int _print_recvn_timeout_errors)
{
  int nleft, nrecv;
  int select_ret;
  char *ptr;
  double start_time, current_time, timeleft;
  struct timeval timeout_tv;
  fd_set recv_fd_set;
  int bytes_ready;
  int bytes_to_read;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  int count =0;

  if(!print_recvn_timeout_errors || !getRcsDoNotPrintTimeoutErrors())
    {
      _print_recvn_timeout_errors=0;
    }
  if (etime_disabled)
    {
      _timeout = -1.0;
    }

  bytes_ready = bytes_to_read = 0;
  timeout_tv.tv_sec = (long) _timeout;
  timeout_tv.tv_usec = (long) (_timeout * 1000000.0);
  if (timeout_tv.tv_usec >= 1000000)
    {
      timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
    }
  FD_ZERO (&recv_fd_set);
  RCS_FD_SET (fd, &recv_fd_set);

  recvn_timedout = 0;
  ptr = (char *) vptr;
  nleft = n;
  if (n < 1)
    {
      rcs_print_error ("recvn:n=%d\n", n);
      return -1;
    }

  if (NULL != bytes_read_ptr)
    {
      if (*bytes_read_ptr >= n)
	{
	  rcs_print_error
	    ("recvn: Invalid parameter -- (*bytes_read_ptr = %d) must be less than (n = %d).\n",
	     *bytes_read_ptr, n);
	  return -1;
	}
      if (*bytes_read_ptr < 0)
	{
	  rcs_print_error
	    ("recvn: Invalid parameter -- (*bytes_read_ptr = %d) must be greater than or equal to zero.\n",(int) (*bytes_read_ptr));
	  return -1;
	}
      ptr += *bytes_read_ptr;
      nleft -= *bytes_read_ptr;
    }

  start_time = current_time = etime ();
  timeleft = _timeout;
  while (nleft > 0)
    {
      if (_timeout >= 0.0)
	{
	  current_time = etime ();
	  timeleft = start_time + _timeout - current_time;
	  if(count ==0)
	    {
	      timeleft = _timeout;
	    }
	  else if (timeleft <= 0.0)
	    {
	      if (_print_recvn_timeout_errors && _timeout > 1e-5)
		{
		  rcs_print_error ("Recv timed out.\n");
		  if (NULL == bytes_read_ptr)
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f) failed.\n",
			 fd, vptr, n, flags, _timeout);
		    }
		  else
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f,bytes_read=%d) failed.\n",
			 fd, vptr, n, flags, _timeout, *bytes_read_ptr);
		    }
		}
	      recvn_timedout = 1;
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = (n - nleft);
		}
	      return -1;
	    }
	  timeout_tv.tv_sec = (long) timeleft;
	  timeout_tv.tv_usec = (long) (timeleft * 1000000.0);
	  if (timeout_tv.tv_usec >= 1000000)
	    {
	      timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
	    }
	  switch (select_ret =
		  dl_select (fd + 1, &recv_fd_set, (fd_set *) NULL,
			     (fd_set *) NULL, &timeout_tv))
	    {
	    case -1:
	      sockerrno = dl_get_last_socket_error_int( select_ret );
	      sockerrstr = dl_get_last_socket_error_string(select_ret,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Error in select: %d -> %s\n", 
			       sockerrno, sockerrstr);
	      if (NULL == bytes_read_ptr)
		{
		  rcs_print_error
		    ("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f) failed.\n",
		     fd, vptr, n, flags, _timeout);
		}
	      else
		{
		  rcs_print_error
		    ("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f,bytes_read=%d) failed.\n",
		     fd, vptr, n, flags, _timeout, *bytes_read_ptr);
		}
	      return -1;

	    case 0:
	      if (_print_recvn_timeout_errors)
		{
		  rcs_print_error ("Recv timed out.\n");
		  if (NULL == bytes_read_ptr)
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f) failed.\n",
			 fd, vptr, n, flags, _timeout);
		    }
		  else
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f,bytes_read=%d) failed.\n",
			 fd, vptr, n, flags, _timeout, *bytes_read_ptr);
		    }
		}
	      recvn_timedout = 1;
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = (n - nleft);
		}
	      return -1;

	    default:
	      break;
	    }
	  bytes_ready = 0;
	  dl_ioctlsocket_fionread(fd,&bytes_ready);
	  bytes_to_read = (nleft <= bytes_ready) ? nleft : bytes_ready;
	}
      else
	{
	  bytes_to_read = nleft;
	}
      nrecv = 0;
      if (bytes_to_read > 0)
	{
	  if ((nrecv = dl_recv (fd, ptr, bytes_to_read, flags)) == -1)
	    {
	      sockerrno = dl_get_last_socket_error_int( fd );
	      if ( dl_socket_error_was_would_block( fd ,sockerrno))
		{
		  if (fabs (_timeout) < 1e-6)
		    {
		      recvn_timedout = 1;
		      if (NULL != bytes_read_ptr)
			{
			  *bytes_read_ptr = (n - nleft);
			}
		      rcs_print_error ("Recvn failed.\n");
		      return -1;
		    }
		}
	      else
		{
		  sockerrstr = dl_get_last_socket_error_string(fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
		  if (NULL == bytes_read_ptr)
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f) failed -- %s.\n",
			 fd, vptr, n, flags, _timeout,sockerrstr);
		    }
		  else
		    {
		      rcs_print_error
			("recvn(fd=%d, vptr=%p, int n=%d, int flags=%d, double _timeout=%f,bytes_read=%d) failed -- %s.\n",
			 fd, vptr, n, flags, _timeout, *bytes_read_ptr,sockerrstr);
		    }
		  if (NULL != bytes_read_ptr)
		    {
		      *bytes_read_ptr = (n - nleft);
		    }
		  return (-1);	/* error, return < 0 */
		}
	      nrecv = 0;
	    }
	  else if (nrecv == 0)
	    {
	      rcs_print_error ("recvn: Premature EOF recieved.\n");
	      return (-2);
	    }
	}
      nleft -= nrecv;
      ptr += nrecv;
      if (nleft > 0 && _timeout > 0.0)
	{
	  esleep (0.001);
	  current_time = etime ();
	  if (current_time - start_time > _timeout)
	    {
	      rcs_print_error ("Recv timed out.\n");
	      recvn_timedout = 1;
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = (n - nleft);
		}
	      return (-1);
	    }
	}
      count++;
    }
  rcs_print_debug (PRINT_SOCKET_READ_SIZE, "read %d bytes from %d\n", n, fd);
  if (NULL != bytes_read_ptr)
    {
      *bytes_read_ptr = (n - nleft);
    }
  return (n - nleft);		/* return >= 0 */
}
