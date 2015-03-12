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

#if ( !defined(NO_RTL) || defined(rtai) || defined(linux_rtai) ) && defined(linux)

#include <stdio.h>		// sscanf

#include "rtlmem.hh"
#include "nmlmsg.hh"
#include "timer.hh"
#include "rcs_prnt.hh"

static int rtlnml_count = 0;

RTLMEM::RTLMEM (const char *bufline, const char *procline, int set_to_server,
		int set_to_master):
CMS (bufline, procline, set_to_server)
{
  if (rtlnml_count == 0)
    {
      rtlnml_init ();
    }
  rtlnml_count++;

  if (sscanf (bufline, "%*s %*s %*s %*s %*s %*s %*s %*s %*s %d", &key) != 1)
    {
      key = 0;
    }
  r = rtlnml_open (BufferName, ProcessName, NULL, size, 
		   (int) (is_local_master?1:0), key);

  if (NULL == r)
    {
      status = CMS_MISC_ERROR;
      return;
    }
  rtlnml_set_local_pointer (r, ((char *) data) + 8);
}

RTLMEM::~RTLMEM ()
{
  if (NULL != r)
    {
      rtlnml_close (r, BufferName);
    }
  rtlnml_count--;
  if (rtlnml_count == 0)
    {
      rtlnml_exit ();
    }
}

  /* Overloaded CMS functions. */
CMS_STATUS RTLMEM::read ()
{
  int
    split_read_count =
    0;
  long
    type =
    rtlnml_read (r);
  if (type > 0)
    {
      ((NMLmsg *) data)->type = type;
      return (status = CMS_READ_OK);
    }
  if (type == RTLNML_READ_OLD)
    {
      return (status = CMS_READ_OLD);
    }
  if (type == RTLNML_SPLIT_READ_ERROR)
    {
      double
	start_time =
	etime ();
      double
	now =
	start_time;
      while ((now - start_time) < timeout || timeout < 0.0
	     || split_read_count < 1)
	{
	  long
	    type =
	    rtlnml_read (r);
	  if (type > 0)
	    {
	      ((NMLmsg *) data)->type = type;
	      return (status = CMS_READ_OK);
	    }
	  if (type == RTLNML_READ_OLD)
	    {
	      return (status = CMS_READ_OLD);
	    }
	  if (type == RTLNML_SPLIT_READ_ERROR)
	    {
	      split_read_count++;
	    }
	  else
	    {
	      return (status = CMS_MISC_ERROR);
	    }
	  now = etime ();
	}
      rcs_print_error
	("Timedout after %d RTLNML_SPLIT_READ_ERRORs and %f seconds.\n",
	 split_read_count, (etime () - start_time));
      return (status = CMS_TIMED_OUT);
    }
  return (status = CMS_MISC_ERROR);
}

CMS_STATUS RTLMEM::peek ()
{
  return read ();
}

CMS_STATUS RTLMEM::write (void *data)
{
  if (rtlnml_write
      (r, ((char *) data) + 8, ((NMLmsg *) data)->type,
       header.in_buffer_size) < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  return (status = CMS_WRITE_OK);
}

#endif
