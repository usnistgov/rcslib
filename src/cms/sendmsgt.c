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
#include "sendmsgt_no_config.h"
#endif

#include "dbg_mem.h"		/* DEBUG_MALLOC, DEBUG_FREE */
#include "sendmsgt.h"		/* Forward Prototype */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "sokintrf.h"		/* dl_sendto(), dl_select(), dl_WSAGetLastError() */
#include "msghdr.h"

static long total_bytes_sent = 0;

#ifndef HAVE_SENDMSG
#ifndef USE_SENDTO
#define USE_SENDTO 1
#endif
#endif

#if MS_WINDOWS_API
#ifndef USE_SENDTO
#define USE_SENDTO 1
#endif
#endif

int sendmsgt_timed_out = 0;

int
sendmsgt (int _socket_fd, struct msghdr *_msg_header, int _flags,
	  double _timeout, 
	  __unused_parameter__ unsigned char *collection_buffer, 
	  __unused_parameter__ long collection_buffer_size
	  )
{
  struct timeval timeout_timeval;
  fd_set write_fd_set;
  int bytes_sent;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
#ifdef USE_SENDTO
  int i=0;
  long required_size = 0;
  unsigned char *temp_pointer=0;
  if(!collection_buffer || 
     (collection_buffer_size) <= 0)
    {
      rcs_print_error("call to sendmst(%d,%p,%d(0x%X),%f,%p,%ld) with bad collecton_buffer.\n",
		      _socket_fd,_msg_header,_flags,_flags,
		      _timeout,collection_buffer,collection_buffer_size);
      return -1;
    }
#endif

  sendmsgt_timed_out = 0;

  if (_timeout > 1E-6)
    {
      timeout_timeval.tv_sec = (long) _timeout;
      timeout_timeval.tv_usec = (long) (_timeout * 1000000.0);
      if (timeout_timeval.tv_usec >= 1000000)
	{
	  timeout_timeval.tv_usec = timeout_timeval.tv_usec % 1000000;
	}
      FD_ZERO (&write_fd_set);
      RCS_FD_SET (_socket_fd, &write_fd_set);
      switch (dl_select
	      (_socket_fd + 1, NULL, &write_fd_set, NULL, &timeout_timeval))
	{
	case -1:
	  sockerrno = dl_get_last_socket_error_int( _socket_fd );
	  sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("sendmsgt: select error: %d %s\n",
			   sockerrno, sockerrstr);
	  return -1;

	case 0:
	  sendmsgt_timed_out = 1;
	  rcs_print_error ("sendmgt: select timed out.\n");
	  return (0);

	default:
	  break;
	}
    }
  else if (_timeout > -1E-6)
    {
      timeout_timeval.tv_sec = 0L;
      timeout_timeval.tv_usec = 0L;
      FD_ZERO (&write_fd_set);
      RCS_FD_SET (_socket_fd, &write_fd_set);
      switch (dl_select
	      (_socket_fd + 1, NULL, &write_fd_set, NULL, &timeout_timeval))
	{
	case -1:
	  rcs_print_error ("sendmsgt: select error: %d %s\n",
			   errno, strerror (errno));
	  return -1;

	case 0:
	  sendmsgt_timed_out = 1;
	  return (0);

	default:
	  break;
	}
    }
#ifdef USE_SENDTO
  if (_msg_header->msg_iovlen > 1)
    {
      for (i = 0, required_size = 0; i < (int)_msg_header->msg_iovlen; i++)
	{
	  required_size += _msg_header->msg_iov[i].iov_len;
	}
      if (required_size > (collection_buffer_size))
	{
	  rcs_print_error("sendmsgt collection_buffer_size(%ld) less than required size(%ld)\n",
			  (collection_buffer_size),
			  required_size);
	  return -1;
	}
      for (i = 0, temp_pointer = collection_buffer;
	   i < (int) _msg_header->msg_iovlen; i++)
	{
	  memcpy (temp_pointer, _msg_header->msg_iov[i].iov_base,
		  _msg_header->msg_iov[i].iov_len);
	  temp_pointer += _msg_header->msg_iov[i].iov_len;
	}
      bytes_sent =
	dl_sendto (_socket_fd, 
		   ((char *) collection_buffer), 
		   required_size, _flags,
		   (struct sockaddr *) _msg_header->msg_name,
		   _msg_header->msg_namelen);
    }
  else
    {
      bytes_sent = dl_sendto (_socket_fd, _msg_header->msg_iov[0].iov_base,
			      _msg_header->msg_iov[0].iov_len, _flags,
			      (struct sockaddr *) _msg_header->msg_name,
			      _msg_header->msg_namelen);
    }
  if (bytes_sent < 0)
    {
      sockerrno = dl_get_last_socket_error_int( _socket_fd );
      sockerrstr = 
	dl_get_last_socket_error_string(_socket_fd,
					sockerrno,
					sockerrbuf,
					sizeof(sockerrbuf));
      rcs_print_error ("sendto(%d,%p,%ld,0x%X,%s,%d) failed: %d -- %s \n",
		       _socket_fd, _msg_header->msg_iov[0].iov_base,
		       (long) _msg_header->msg_iov[0].iov_len, _flags,
		       dl_inet_ptr_ntoa (&((struct sockaddr_in *)
					   _msg_header->msg_name)->sin_addr),
		       _msg_header->msg_namelen, sockerrno,sockerrstr);
    }
#else
  bytes_sent = sendmsg (_socket_fd, _msg_header, _flags);
  if (bytes_sent < 0)
    {
      sockerrno = dl_get_last_socket_error_int( _socket_fd );
      sockerrstr = dl_get_last_socket_error_string(_socket_fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("sendmsg(%d,%p,%d) error: %d = %s\n",
		       _socket_fd, _msg_header, _flags,
		       sockerrno, sockerrstr);
      rcs_print_error (" _msg_header->msg_iovlen = %lu\n",
		       ((unsigned long) _msg_header->msg_iovlen));
      rcs_print_error ("_msg_header->msg_iov[0].iov_len = %lu\n",
		       ((unsigned long) _msg_header->msg_iov[0].iov_len));
    }
#endif
  total_bytes_sent += bytes_sent;
  rcs_print_debug (PRINT_SOCKET_WRITE_SIZE, "sendmsg %d bytes to %d\n",
		   bytes_sent, _socket_fd);
  return (bytes_sent);
}

