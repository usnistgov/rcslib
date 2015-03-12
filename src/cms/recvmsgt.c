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
#include "recvmsgt_no_config.h"
#endif

#include "recvmsgt.h"		/* Forward Prototype */
#include "_timer.h"		/* etime(), etime_disabled */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "sokintrf.h"		/* dl_recvfrom(), dl_select() */
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */
#include "msghdr.h"

static long total_bytes_read = 0;


int recvmsgt_timed_out = 0;

int recvmsgt_implementation (int _socket_fd, struct msghdr *_msg_header,
			     int _flags, double _timeout,
			     int print_timeout_errors,
			     unsigned char *collection_buffer,
			     long collection_buffer_size);

int
recvmsgt (int _socket_fd, struct msghdr *_msg_header, int _flags,
	  double _timeout,
	  unsigned char *collection_buffer,
	  long collection_buffer_size)
{
  return (recvmsgt_implementation
	  (_socket_fd, _msg_header, _flags, _timeout, 1, collection_buffer, collection_buffer_size));
}

int
recvmsgtq (int _socket_fd, struct msghdr *_msg_header, int _flags,
	   double _timeout,
	   unsigned char *collection_buffer,
	   long collection_buffer_size)
{
  return (recvmsgt_implementation
	  (_socket_fd, _msg_header, _flags, _timeout, 0, collection_buffer, collection_buffer_size));
}

int
recvmsgt_implementation (int _socket_fd, struct msghdr *_msg_header,
			 int _flags, double _timeout,
			 int print_timeout_errors,
			 __unused_parameter__ unsigned char *collection_buffer,
			 __unused_parameter__ long collection_buffer_size)
{
  struct timeval timeout_timeval;
  fd_set read_fd_set;
  int bytes_read;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
#if defined(MS_WINDOWS_API) || !defined(DISABLE_RCS_DEBUG_PRINT)
  int i = 0;
#endif

#if MS_WINDOWS_API
  long required_size, bytes_copied, bytes_to_copy;
  char *temp_pointer;
  required_size=0;
  if (_msg_header->msg_iovlen > 1)
    {
      for (i = 0, required_size = 0; i < _msg_header->msg_iovlen; i++)
	{
	  if(!_msg_header->msg_iov[i].iov_base)
	    {
	      rcs_print_error("recvmsgt: bad msg_header\n");
	      return(-1);
	    }
	  required_size += _msg_header->msg_iov[i].iov_len;
	}
      if (required_size > collection_buffer_size)
	{
	  rcs_print_error("recvmsgt collection_buffer_size(%ld) less than required size(%ld): _msg_header=%p,  _msg_header->msg_iovlen=%d, _msg_header->msg_iov[0].iov_len=%d, _msg_header->msg_iov[1].iov_len=%d\n",
			  (collection_buffer_size),
			  required_size,
			  _msg_header,
			  _msg_header->msg_iovlen, 
			  _msg_header->msg_iov[0].iov_len, 
			  _msg_header->msg_iov[1].iov_len);
	  return -1;
	}
      if (NULL == collection_buffer)
	{
	  collection_buffer_size = 0;
	  rcs_print_error ("Couldn't malloc collection buffer of size.\n");
	  return (-1);
	}
    }
#endif
  if (etime_disabled)
    {
      _timeout = -1.0;
    }

  if (_timeout > 1E-6)
    {
      timeout_timeval.tv_sec = (long) _timeout;
      timeout_timeval.tv_usec = (long) (_timeout * 1000000.0);
      if (timeout_timeval.tv_usec >= 1000000)
	{
	  timeout_timeval.tv_usec = timeout_timeval.tv_usec % 1000000;
	}
      FD_ZERO (&read_fd_set);
      RCS_FD_SET (_socket_fd, &read_fd_set);
      switch (dl_select
	      (_socket_fd + 1, &read_fd_set, NULL, NULL, &timeout_timeval))
	{
	case -1:
	  sockerrno = dl_get_last_socket_error_int( _socket_fd );
	  sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("recvmsgt: select error: %d %s\n",
			   sockerrno, sockerrstr);
	  return -1;

	case 0:
	  recvmsgt_timed_out = 1;
	  if (print_timeout_errors)
	    {
	      rcs_print_error
		("recvmgt: select timed out after %f seconds.\n", _timeout);
	    }
	  return (0);

	default:
	  break;
	}
    }
  else if (_timeout > -1E-6)
    {
      timeout_timeval.tv_sec = (long) 0;
      timeout_timeval.tv_usec = (long) 0;
      FD_ZERO (&read_fd_set);
      RCS_FD_SET (_socket_fd, &read_fd_set);
      switch (dl_select
	      (_socket_fd + 1, &read_fd_set, NULL, NULL, &timeout_timeval))
	{
	case -1:
	  sockerrno = dl_get_last_socket_error_int( _socket_fd );
	  sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("recvmsgt: select error: %d %s\n",
			   sockerrno, sockerrstr);
	  return -1;

	case 0:
	  recvmsgt_timed_out = 1;
	  return (0);

	default:
	  break;
	}
    }
#if MS_WINDOWS_API
  if (_msg_header->msg_iovlen > 1)
    {
      rcs_print_debug(PRINT_MISC,"required_size=%ld\n",required_size);
      bytes_read =
	dl_recvfrom (_socket_fd, collection_buffer, required_size, _flags,
		     (struct sockaddr *) _msg_header->msg_name,
		     &_msg_header->msg_namelen);
      if (bytes_read == -1)
	{
	  sockerrno = dl_get_last_socket_error_int( _socket_fd );
	  sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("recvmsgt: recvfrom error %d:%s\n", 
			   sockerrno, sockerrstr);
	  return (-1);
	}
      for (i = 0, temp_pointer = collection_buffer, bytes_copied = 0;
	   i < _msg_header->msg_iovlen && bytes_copied < bytes_read; i++)
	{
	  bytes_to_copy =
	    (bytes_read - bytes_copied) <
	    _msg_header->msg_iov[i].iov_len ? bytes_read -
	    bytes_copied : _msg_header->msg_iov[i].iov_len;
	  memcpy (_msg_header->msg_iov[i].iov_base, temp_pointer,
		  bytes_to_copy);
	  temp_pointer += bytes_to_copy;
	  bytes_copied += bytes_to_copy;
	}
    }
  else
    {
      bytes_read = dl_recvfrom (_socket_fd, _msg_header->msg_iov[0].iov_base,
				_msg_header->msg_iov[0].iov_len, _flags,
				(struct sockaddr *) _msg_header->msg_name,
				&_msg_header->msg_namelen);
      if (bytes_read == -1)
	{
	  sockerrno = dl_get_last_socket_error_int( _socket_fd );
	  sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("recvmsgt: recvfrom error %d:%s\n", 
			   sockerrno, sockerrstr);
	  return (-1);
	}
    }
#else
  bytes_read = recvmsg (_socket_fd, _msg_header, _flags);
  if (bytes_read < 0)
    {
      sockerrno = dl_get_last_socket_error_int( _socket_fd );
      sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("recvmsg(%d (0x%X), %p, %d (0x%X) error: %d = %s\n",
		       _socket_fd, _socket_fd, (void*)_msg_header, _flags, _flags,
		       sockerrno, sockerrstr);
    }
#endif
  total_bytes_read += bytes_read;
#ifndef DISABLE_RCS_DEBUG_PRINT
  if(rcs_debugging_enabled)
    {
      rcs_print_debug (PRINT_SOCKET_READ_SIZE, "recvmsg %d bytes from socket_fd=%d,_msg_header=%p,_msg_header->msg_name=%s:%d\n",
		       bytes_read, _socket_fd,
		       (void*)_msg_header,
		       dl_inet_ptr_ntoa(&(((struct sockaddr_in *)_msg_header->msg_name)->sin_addr)),
		       dl_ntohs(((struct sockaddr_in *)_msg_header->msg_name)->sin_port));

      rcs_print_debug (PRINT_SOCKET_READ_SIZE,"recvmsg _msg_header->msg_iovlen=%d\n", (int) _msg_header->msg_iovlen);
      for (i = 0; i < (int)_msg_header->msg_iovlen; i++)
	{
	  rcs_print_debug (PRINT_SOCKET_READ_SIZE,"recvmsg _msg_header->_msg_header->msg_iov[%d].iov_base=%p\n",
			   i,_msg_header->msg_iov[i].iov_base);
	  rcs_print_debug (PRINT_SOCKET_READ_SIZE,"recvmsg _msg_header->_msg_header->msg_iov[%d].iov_len=%lu\n",
			   i,(unsigned long)_msg_header->msg_iov[i].iov_len);
	}
    }
#endif	 
      
  return (bytes_read);
}

