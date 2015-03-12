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

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_BBD)


/*
 * File: bbdmem.cc
 *
 * This file provides the C++ member functions for BBDMEM a wrapper
 * used to provide a CMS interface to Karl Murphy's BBD communications
 * library.
 */

/* Include Files */
extern "C"
{
#include <string.h>		/* memcpy(), memset(), strtol() */
#include <stddef.h>		/* size_t */
#include <stdlib.h>		/* calloc() */

#ifdef VXWORKS
#include "vxWorks.h"
#endif
}

#include "cms.hh"		// class CMS
#include "bbdmem.hh"		// class BBDMEM
#include "rcs_prnt.hh"		// rcs_print_error()
#include "timer.hh"		// clk_tck()

extern "C"
{
#include "bbd.h"		// Karl Murphy's original C bbd functions.
}



// Constructor
BBDMEM::BBDMEM (const char *_bufline, const char *_procline, int set_to_server,
		int set_to_master):
CMS (_bufline, _procline, set_to_server)
{
  char bbd_name[20];
  int i;
  bbd_data = 0;
  id = 0;
  reader_bbd_id = NULL;
  writer_bbd_id = NULL;

  if (neutral)
    {
      rcs_print_error ("Neutral encoded buffers not supported by BBD.\n");
      status = CMS_CONFIG_ERROR;
      return;
    }

  if (!read_permission_flag && !write_permission_flag)
    {
      status = CMS_CONFIG_ERROR;
      rcs_print_error
	("BBD requires that the process have either read pr write permission.\n");
      return;
    }

  char *bbd_name_eq = strstr (buflineupper, "BBD_NAME=");

  if (NULL != bbd_name_eq)
    {
      bbd_name_eq += 9;
      memset (bbd_name, 0, 20);
      for (i = 0; i < 20; i++)
	{
	  if (bbd_name_eq[i] == 0 ||
	      bbd_name_eq[i] == ' ' ||
	      bbd_name_eq[i] == '\t' ||
	      bbd_name_eq[i] == '\b' ||
	      bbd_name_eq[i] == '\r' || bbd_name_eq[i] == '\n')
	    {
	      break;
	    }
	  bbd_name[i] = bbd_name_eq[i];
	}
    }
  else
    {
      strncpy (bbd_name, BufferName, 20);
    }

  bbd_size = size;
  char *bbd_size_eq = strstr (buflineupper, "BBD_SIZE=");
  if (bbd_size_eq != 0);
  {
    bbd_size_eq += 9;
    bbd_size = strtol (bbd_size_eq, NULL, 0);
    if (bbd_size <= 0)
      {
	rcs_print_error ("bbd_size of %d is invalid.\n", bbd_size);
	bbd_size = size;
      }
  }


  if (read_permission_flag)
    {
      reader_bbd_id = bbdConnect (bbd_name, BBD_MULT_READER, bbd_size);
      bbd_data = NULL;
    }
  if (write_permission_flag)
    {
      char *proc_sole_writer = strstr (proclineupper, "SOLE_WRITER");
      if (NULL != proc_sole_writer)
	{
	  writer_bbd_id = bbdConnect (bbd_name, BBD_SOLE_WRITER, bbd_size);
	}
      else
	{
	  writer_bbd_id = bbdConnect (bbd_name, BBD_MULT_WRITER, bbd_size);
	}
    }
  if (NULL == reader_bbd_id && NULL == writer_bbd_id)
    {
      status = CMS_CREATE_ERROR;
    }
  bbd_timeout = NO_WAIT;
  if (timeout > 0)
    {
      bbd_timeout = (int) ((double) timeout / clk_tck ());
    }
  else if (timeout == 0)
    {
      bbd_timeout = NO_WAIT;
    }
  else
    {
      bbd_timeout = WAIT_FOREVER;
    }
  subdiv_data = data = realloc (data, size + 256);
}

BBDMEM::~BBDMEM ()
{
  // FIXME the following code creates a memory leak, but that is preferable
  // to a memPartFree error that occurs with any reasonable attempt to
  // free the memory.
  if (NULL != reader_bbd_id)
    {
      bbdDelete (reader_bbd_id);
      reader_bbd_id = NULL;
    }
  if (NULL != writer_bbd_id)
    {
      bbdDelete (writer_bbd_id);
      writer_bbd_id = NULL;
    }
  bbd_data = NULL;
}

CMS_STATUS
BBDMEM::read ()
{
  if (NULL == bbd_data)
    {
      bbd_data = (void *) (((char *) data) + sizeof_message_header);
    }

  if (!read_permission_flag)
    {
      rcs_print_error ("Read permission denied.\n");
      return (status = CMS_PERMISSIONS_ERROR);
    }
  if (bbdRead (reader_bbd_id, bbd_data, BBD_FRESH_READ, NO_WAIT) == OK)
    {
      id++;
    }
  else
    {
      //      rcs_print_error("BBD error or no new data.\n");
    }

  /*
     {
     rcs_print_error("BBD ERROR:\n");
     bbdShow(reader_bbd_id, BufferName);
     return(status = CMS_MISC_ERROR);
     }


     rId = (BBD_READER *) reader_bbd_id;
     if(NULL == rId)
     {
     rcs_print_error("Can't get writeCnt from BBD.\n");
     return(status = CMS_MISC_ERROR);
     }
     wId = rId->bbd;
     if(NULL == wId)
     {
     rcs_print_error("Can't get writeCnt from BBD.\n");
     return(status = CMS_MISC_ERROR);
     }
     id = wId->writeCnt;
   */

  check_id (id);
  return status;
}

CMS_STATUS
BBDMEM::peek ()
{
  if (NULL == bbd_data)
    {
      bbd_data = (void *) (((char *) data) + sizeof_message_header);
    }

  if (!read_permission_flag)
    {
      rcs_print_error ("Read permission denied.\n");
      return (status = CMS_PERMISSIONS_ERROR);
    }
  if (bbdRead (reader_bbd_id, bbd_data, BBD_FRESH_READ, NO_WAIT) == OK)
    {
      id++;
    }
  else
    {
      // rcs_print_error("BBD error or no new data.\n");
    }

  /*   else
     {
     rcs_print_error("BBD ERROR:\n");
     bbdShow(reader_bbd_id, BufferName);
     return(status = CMS_MISC_ERROR);
     }

     rId = (BBD_READER *) reader_bbd_id;
     if(NULL == rId)
     {
     rcs_print_error("Can't get writeCnt from BBD.\n");
     return(status = CMS_MISC_ERROR);
     }
     wId = rId->bbd;
     if(NULL == wId)
     {
     rcs_print_error("Can't get writeCnt from BBD.\n");
     return(status = CMS_MISC_ERROR);
     }
     id = wId->writeCnt;
   */

  check_id (id);
  return status;
}

CMS_STATUS
BBDMEM::write (void *user_data)
{
  bbd_data = (void *) (((char *) user_data) + sizeof_message_header);

  if (!write_permission_flag)
    {
      rcs_print_error ("Write permission denied.\n");
      return (status = CMS_PERMISSIONS_ERROR);
    }
  if (bbdWrite (writer_bbd_id, bbd_data, BBD_PLAIN_WRITE, bbd_timeout) != OK)
    {
      rcs_print_error ("BBD ERROR:\n");
      bbdShow (writer_bbd_id, BufferName);
      return (status = CMS_MISC_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

CMS_STATUS
BBDMEM::write_if_read (void *user_data)
{
  bbd_data = (void *) (((char *) user_data) + sizeof_message_header);

  if (!write_permission_flag)
    {
      rcs_print_error ("Write permission denied.\n");
      return (status = CMS_PERMISSIONS_ERROR);
    }
  if (bbdWrite (writer_bbd_id, bbd_data, BBD_WAIT_WRITE, NO_WAIT) != OK)
    {
      rcs_print_error ("BBD ERROR:\n");
      bbdShow (writer_bbd_id, BufferName);
      return (status = CMS_MISC_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

//  defined(ENABLE_RCS_BBD)

#else
#include "rcs_empty_source"
#endif
