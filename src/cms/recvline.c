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
* File: recvline.c
*************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "recvline_no_config.h"
#endif

#include "recvline.h"		/* recvline(int, void *, int, double) */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "sokintrf.h"		/* dl_select(), dl_recv(), dl_WSAGetLastError() */
#include "_timer.h"		/* etime(), esleep() */
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */


int recvline_timedout = 0;
int print_recvline_timeout_errors = 1;
int recvline_timeout_count = 0;

struct prev_read_info_struct
{
  char *buf;
  char *ptr;
  int fd;
  int buf_size;
};

static struct prev_read_info_struct prev_read_info[256];
static int prev_read_infos = 0;
static int initialized = 0;

#ifdef VXWORKS
static int tasklock_recvline_prev_read_info = 1;
#endif

#ifdef VXWORKS
void
print_recvline_prev_read_info ()
{
  int i;

  for (i = 0; i < 256; i++)
    {
      if (prev_read_info[i].fd > 1)
	{
	  rcs_print ("fd = %d, buf = %p, ptr = %p, buf_size = %d(0x%X)\n",
		     prev_read_info[i].fd,
		     prev_read_info[i].buf,
		     prev_read_info[i].ptr,
		     prev_read_info[i].buf_size, prev_read_info[i].buf_size);
	  rcs_print ("\t buf=%s\n", prev_read_info[i].buf);
	  rcs_print ("\t ptr=%s\n", prev_read_info[i].ptr);
	}
    }
}
#endif

void
clean_prev_read_info (int fd)
{
  int max_prev_read_info = 0;
  int i;

  if (fd < 1)
    {
      return;
    }
#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskLock ();
#endif

  for (i = 0; i < 256; i++)
    {
      if (prev_read_info[i].fd == fd)
	{
	  prev_read_info[i].ptr = NULL;
	  if (NULL != prev_read_info[i].buf)
	    {
	      DEBUG_FREE (prev_read_info[i].buf);
	      prev_read_info[i].buf = NULL;
	    }
	  prev_read_info[i].buf_size = 0;
	  prev_read_info[i].fd = -1;
	  continue;
	}
      if (prev_read_info[i].fd > 0)
	{
	  max_prev_read_info = i;
	}
    }
  prev_read_infos = max_prev_read_info;

#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskUnlock ();
#endif

}


int
get_bytes_already_read (int fd)
{
  int bytes_left;
  int i;

  if (fd < 1)
    {
      return -1;
    }

#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskLock ();
#endif

  for (i = 0; i < prev_read_infos; i++)
    {
      if (prev_read_info[i].fd == fd)
	{
	  bytes_left = prev_read_info[i].buf_size -
	    ((unsigned long) prev_read_info[i].ptr
	     - (unsigned long) prev_read_info[i].buf);
#ifdef VXWORKS
	  if (tasklock_recvline_prev_read_info)
	    taskUnlock ();
#endif
	  return bytes_left;
	}
    }

#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskUnlock ();
#endif

  return 0;
}

/* Read "n" bytes from a descriptor. */
int
recvline (int fd, char *cptr, int maxbytes, int flags, double _timeout,
	  int *bytes_read_ptr)
{
  char *previous_read_ptr = NULL;
  char *previous_read_buf = NULL;
  char *nptr;
  int bytes_read = 0;
  int bytes_ready=0;
  int nrecv;
  int i;
  int select_ret;
  char *ptr;
  int bytes_left;
  double start_time, current_time, timeleft;
  struct timeval timeout_tv;
  fd_set recv_fd_set;
  int first_empty_slot = -1;
  int max_prev_read_info = 0;
  int current_prev_read_info = -1;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];

  if (etime_disabled)
    {
      _timeout = -1.0;
    }

  if (fd < 1 || cptr == NULL)
    {
      rcs_print_error ("recvline: Invalid parameter. fd=%d, cptr=%p\n", fd,
		       cptr);
      return -1;
    }
  recvline_timedout = 0;
  timeout_tv.tv_sec = (long) _timeout;
  timeout_tv.tv_usec = (long) (_timeout * 1000000.0);
  if (timeout_tv.tv_usec >= 1000000)
    {
      timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
    }
  FD_ZERO (&recv_fd_set);
  RCS_FD_SET (fd, &recv_fd_set);

#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskLock ();
#endif

  if (!initialized)
    {
      for (i = 0; i < 256; i++)
	{
	  prev_read_info[i].ptr = NULL;
	  prev_read_info[i].buf = NULL;
	  prev_read_info[i].fd = 0;
	  prev_read_info[i].buf_size = 0;
	}
      prev_read_infos = 0;
      initialized = 1;
    }

#ifdef VXWORKS
  if (tasklock_recvline_prev_read_info)
    taskUnlock ();
#endif


  for (i = 0; i < prev_read_infos; i++)
    {
      if (prev_read_info[i].fd == fd)
	{
	  previous_read_ptr = prev_read_info[i].ptr;
	  previous_read_buf = prev_read_info[i].buf;
	  if (NULL == prev_read_info[i].buf || NULL == prev_read_info[i].ptr)
	    {
	      rcs_print_error
		("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
		 prev_read_info[i].buf, prev_read_info[i].ptr,
		 prev_read_info[i].fd, prev_read_info[i].buf_size,
		 prev_read_info[i].buf_size);
	      prev_read_info[i].ptr = NULL;
	      prev_read_info[i].buf = NULL;
	      prev_read_info[i].fd = 0;
	      prev_read_info[i].buf_size = 0;
	      continue;
	    }
	  else if ((unsigned long) previous_read_ptr >
		   ((unsigned long) previous_read_buf) +
		   prev_read_info[i].buf_size)
	    {
	      rcs_print_error
		("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
		 prev_read_info[i].buf, prev_read_info[i].ptr,
		 prev_read_info[i].fd, prev_read_info[i].buf_size,
		 prev_read_info[i].buf_size);
	      prev_read_info[i].ptr = NULL;
	      prev_read_info[i].buf = NULL;
	      prev_read_info[i].fd = 0;
	      prev_read_info[i].buf_size = 0;
	      continue;
	    }
	  else if ((unsigned long) previous_read_ptr <
		   ((unsigned long) previous_read_buf))
	    {
	      rcs_print_error
		("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
		 prev_read_info[i].buf, prev_read_info[i].ptr,
		 prev_read_info[i].fd, prev_read_info[i].buf_size,
		 prev_read_info[i].buf_size);
	      prev_read_info[i].ptr = NULL;
	      prev_read_info[i].buf = NULL;
	      prev_read_info[i].fd = 0;
	      prev_read_info[i].buf_size = 0;
	      continue;
	    }

	  max_prev_read_info = i;
	  current_prev_read_info = i;
	  break;
	}
      if (prev_read_info[i].fd < 1 && first_empty_slot < 0)
	{
	  first_empty_slot = i;
	}
      else
	{
	  max_prev_read_info = i;
	}
    }
  if (first_empty_slot < 0)
    {
      first_empty_slot = prev_read_infos;
    }
  prev_read_infos = max_prev_read_info + 1;

  if (previous_read_ptr != NULL)
    {
      if(*previous_read_ptr == 0)
	{
	  bytes_left =
	    prev_read_info[current_prev_read_info].buf_size -
	    ((unsigned long) previous_read_ptr -
	     (unsigned long) previous_read_buf);
	  while (bytes_left > 0 && *previous_read_ptr == 0)
	    {
	      previous_read_ptr++;
	      bytes_left =
		prev_read_info[current_prev_read_info].buf_size -
		((unsigned long) previous_read_ptr -
		 (unsigned long) previous_read_buf);
	    }
	  if(bytes_left <= 0)
	    {
	      previous_read_ptr=0;
	      prev_read_info[current_prev_read_info].ptr = NULL;
	      prev_read_info[current_prev_read_info].fd = 0;
	      previous_read_buf = NULL;
	      if (prev_read_info[current_prev_read_info].buf != NULL)
		{
		  DEBUG_FREE (prev_read_info[current_prev_read_info].buf);
		  prev_read_info[current_prev_read_info].buf = NULL;
		}
	      prev_read_info[current_prev_read_info].buf = NULL;
	      prev_read_info[current_prev_read_info].buf_size = 0;
	    }
	}
    }

  if (previous_read_ptr != NULL)
    {
      ptr = strchr (previous_read_ptr, '\n');
      if (NULL != ptr)
	{
	  *ptr = 0;
	      bytes_left =
		prev_read_info[current_prev_read_info].buf_size -
		((unsigned long) previous_read_ptr -
		 (unsigned long) previous_read_buf);
	      if (bytes_left <= 0)
		{
		  rcs_print_error ("Problem with previous_read_ptr.\n");
		  prev_read_info[current_prev_read_info].ptr = NULL;
		  prev_read_info[current_prev_read_info].buf = NULL;
		  prev_read_info[current_prev_read_info].fd = 0;
		  prev_read_info[current_prev_read_info].buf_size = 0;
		  return -1;
		}
	      strncpy (cptr, previous_read_ptr, bytes_left);
	      previous_read_ptr = ptr;
	      previous_read_ptr++;
	      bytes_read = (int) strlen (cptr);
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = bytes_read;
		}
	      bytes_left =
		prev_read_info[current_prev_read_info].buf_size -
		((unsigned long) previous_read_ptr -
		 (unsigned long) previous_read_buf);
	      if (bytes_left > 0)
		{
		  prev_read_info[current_prev_read_info].ptr =
		    previous_read_ptr;
		  if (NULL == prev_read_info[current_prev_read_info].buf
		      || NULL == prev_read_info[current_prev_read_info].ptr)
		    {
		      rcs_print_error
			("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
			 prev_read_info[current_prev_read_info].buf,
			 prev_read_info[current_prev_read_info].ptr,
			 prev_read_info[current_prev_read_info].fd,
			 prev_read_info[current_prev_read_info].buf_size,
			 prev_read_info[current_prev_read_info].buf_size);
		      prev_read_info[current_prev_read_info].ptr = NULL;
		      prev_read_info[current_prev_read_info].buf = NULL;
		      prev_read_info[current_prev_read_info].fd = 0;
		      prev_read_info[current_prev_read_info].buf_size = 0;
		      return -1;
		    }
		  else if ((unsigned long) previous_read_ptr >
			   ((unsigned long) previous_read_buf) +
			   prev_read_info[current_prev_read_info].buf_size)
		    {
		      rcs_print_error
			("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
			 prev_read_info[current_prev_read_info].buf,
			 prev_read_info[current_prev_read_info].ptr,
			 prev_read_info[current_prev_read_info].fd,
			 prev_read_info[current_prev_read_info].buf_size,
			 prev_read_info[current_prev_read_info].buf_size);
		      prev_read_info[current_prev_read_info].ptr = NULL;
		      prev_read_info[current_prev_read_info].buf = NULL;
		      prev_read_info[current_prev_read_info].fd = 0;
		      prev_read_info[current_prev_read_info].buf_size = 0;
		      return -1;
		    }
		  else if ((unsigned long) previous_read_ptr <
			   ((unsigned long) previous_read_buf))
		    {
		      rcs_print_error
			("Invalid prev_read_info. buf =%p, ptr=%p, fd = %d, buf_size=%d(0x%X)\n",
			 prev_read_info[current_prev_read_info].buf,
			 prev_read_info[current_prev_read_info].ptr,
			 prev_read_info[current_prev_read_info].fd,
			 prev_read_info[current_prev_read_info].buf_size,
			 prev_read_info[current_prev_read_info].buf_size);
		      prev_read_info[current_prev_read_info].ptr = NULL;
		      prev_read_info[current_prev_read_info].buf = NULL;
		      prev_read_info[current_prev_read_info].fd = 0;
		      prev_read_info[current_prev_read_info].buf_size = 0;
		      return -1;
		    }
		}
	      else
		{
		  prev_read_info[current_prev_read_info].ptr = NULL;
		  prev_read_info[current_prev_read_info].fd = 0;
		  previous_read_buf = NULL;
		  if (prev_read_info[current_prev_read_info].buf != NULL)
		    {
		      DEBUG_FREE (prev_read_info[current_prev_read_info].buf);
		      prev_read_info[current_prev_read_info].buf = NULL;
		    }
		  prev_read_info[current_prev_read_info].buf = NULL;
		  prev_read_info[current_prev_read_info].buf_size = 0;
		}
	      return bytes_read;
	    }
	  else
	    {
	      strcpy (cptr, previous_read_ptr);
	      previous_read_ptr = previous_read_buf;
	      bytes_read = (int) strlen (cptr);
	      prev_read_info[current_prev_read_info].ptr = NULL;
	      prev_read_info[current_prev_read_info].fd = 0;
	      previous_read_buf = NULL;
	      if (prev_read_info[current_prev_read_info].buf != NULL)
		{
		  DEBUG_FREE (prev_read_info[current_prev_read_info].buf);
		  prev_read_info[current_prev_read_info].buf = NULL;
		}
	      prev_read_info[current_prev_read_info].buf = NULL;
	      prev_read_info[current_prev_read_info].buf_size = 0;
	      if (current_prev_read_info > first_empty_slot)
		{
		  current_prev_read_info = -1;
		}
	      cptr += bytes_read;
	    }
    }

  ptr = (char *) cptr;
  current_time = start_time = etime ();
  timeleft = _timeout;
  while (1)
    {
      if (fabs (_timeout) > 1E-6)
	{
	  if (_timeout > 0)
	    {
	      current_time = etime ();
	      timeleft = start_time + _timeout - current_time;
	      if (timeleft <= 0.0)
		{
		  if (print_recvline_timeout_errors)
		    {
		      rcs_print_error
			("recvline(fd=%d, cptr=%p, int flags=%d, double _timeout=%f) timed out for the %d time.\n",
			 fd, cptr, flags, _timeout, recvline_timeout_count);
		    }
		  recvline_timedout = 1;
		  recvline_timeout_count++;
		  return -1;
		}
	      timeout_tv.tv_sec = (long) timeleft;
	      timeout_tv.tv_usec = (long) (timeleft * 1000000.0);
	      if (timeout_tv.tv_usec >= 1000000)
		{
		  timeout_tv.tv_usec = timeout_tv.tv_usec % 1000000;
		}
	      select_ret =
		dl_select (fd + 1, &recv_fd_set, (fd_set *) NULL,
			   (fd_set *) NULL, &timeout_tv);
	    }
	  else
	    {
	      select_ret =
		dl_select (fd + 1, &recv_fd_set, (fd_set *) NULL,
			   (fd_set *) NULL, NULL);
	    }
	  switch (select_ret)
	    {
	    case -1:
	      sockerrno = dl_get_last_socket_error_int( -1 );
	      sockerrstr = dl_get_last_socket_error_string(-1,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Error in select: %d -> %s\n", 
			       sockerrno, sockerrstr);
	      return -1;

	    case 0:
	      if(_timeout >= 0)
		{
		  rcs_print_error ("Recvline timed out.\n");
		  recvline_timedout = 1;
		  recvline_timeout_count++;
		  return -1;
		}
	      else
		{
		  rcs_print_error("select returned 0 when timeout should be infinite.\n");
		}
	      break;

	    default:
	      break;
	    }
	}
      dl_ioctlsocket_fionread(fd, &bytes_ready);
      if (bytes_ready <= 0)
	{
	  if (fabs (_timeout) < 1e-6)
	    {
	      recvline_timedout = 1;
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = bytes_read;
		}
	      return (-1);
	    }
	  else
	    {
	      rcs_print_error ("recvline: Premature EOF recieved.\n");
	      return (-1);
	    }
	}
      if (bytes_read < 0)
	{
	  bytes_read = 0;
	}
      if (bytes_ready > maxbytes - bytes_read)
	{
	  bytes_ready = maxbytes - bytes_read;
	}
      if ((nrecv = dl_recv (fd, ptr, bytes_ready, flags)) == -1)
	{
	  sockerrno = dl_get_last_socket_error_int( fd );
	  if ( dl_socket_error_was_would_block( fd , sockerrno) )
	    {
	      if (fabs (_timeout) < 1e-6)
		{
		  recvline_timedout = 1;
		  if (NULL != bytes_read_ptr)
		    {
		      *bytes_read_ptr = bytes_read;
		    }
		  return (-1);
		}
	    }
	  else
	    {
	      sockerrstr = dl_get_last_socket_error_string(fd,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("Recv error: %d = %s\n", 
			       sockerrno,sockerrstr);
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = bytes_read;
		}
	      return (-1);
	    }
	  continue;
	}
      else if (nrecv == 0)
	{
	  rcs_print_error ("recvline: Premature EOF recieved.\n");
	  return (-1);
	}
      nptr = strchr (ptr, '\n');
      if (NULL != nptr)
	{
	  *nptr = 0;
	  bytes_read += (int) strlen (ptr);
	  if (NULL != bytes_read_ptr)
	    {
	      *bytes_read_ptr = bytes_read;
	    }
	  nptr++;
	  if (nrecv > ((int) (strlen (ptr) + 1)))
	    {
	      if (current_prev_read_info < 0)
		{
		  current_prev_read_info = first_empty_slot;
		}

	      prev_read_info[current_prev_read_info].buf = NULL;
	      prev_read_info[current_prev_read_info].ptr = NULL;
	      prev_read_info[current_prev_read_info].fd = 0;
	      prev_read_info[current_prev_read_info].buf_size = 0;
	      prev_read_info[current_prev_read_info].buf =
		DEBUG_MALLOC (nrecv - strlen (ptr));
	      prev_read_info[current_prev_read_info].buf_size = (int) 
		 ( nrecv - strlen (ptr) );	
	      prev_read_info[current_prev_read_info].fd = fd;
	      previous_read_buf = prev_read_info[current_prev_read_info].buf;
	      memcpy (previous_read_buf, nptr, nrecv - (strlen (ptr) + 1));
	      *(((char *) previous_read_buf) +
		prev_read_info[current_prev_read_info].buf_size - 1) = 0;
	      previous_read_ptr = previous_read_buf;
	      prev_read_info[current_prev_read_info].ptr = previous_read_ptr;
	      if (prev_read_infos < current_prev_read_info + 1)
		{
		  prev_read_infos = current_prev_read_info + 1;
		}
	    }
	  rcs_print_debug (PRINT_SOCKET_READ_SIZE,
			   "recvline:(%d bytes from %d) %s \n", bytes_read,
			   fd, cptr);
	  return bytes_read;
	}
      ptr += nrecv;
      bytes_read += nrecv;
      if (bytes_read > maxbytes)
	{
	  rcs_print_error ("recvline: Buffer size of %d exceeded.\n",
			   maxbytes);
	  return (-1);
	}
      if (_timeout > 0.0)
	{
	  esleep (0.001);
	  current_time = etime ();
	  if (current_time - start_time > _timeout)
	    {
	      rcs_print_error
		("recvline: timed out after %f seconds between %f and %f\n",
		 current_time - start_time, start_time, current_time);
	      recvline_timedout = 1;
	      if (NULL != bytes_read_ptr)
		{
		  *bytes_read_ptr = bytes_read;
		}
	      return (-1);
	    }
	}
    }
  rcs_print_debug (PRINT_SOCKET_READ_SIZE,
		   "recvline:(%d bytes from %d) %s \n", bytes_read, fd, cptr);

  if (NULL != bytes_read_ptr)
    {
      *bytes_read_ptr = bytes_read;
    }
  return (bytes_read);		/* return >= 0 */
}
