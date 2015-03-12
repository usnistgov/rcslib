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

/*****************************************************************************
* File: cms_in.cc
* Author(s): Will Shackleford
* Purpose: Provides the internal interface of CMS to the buffer.
*
* NOTES:
* The following applies to the member functions in this file:
* They work through a handle to a shared physical memory object.
* They should only be called when this process has sole access to
 this shared physical memory object. This is normally achieved by taking
 a mutual-exclusion semaphore before calling the internal_access function
 above from the main_access function of a derived class.
* If they begin with "queue" then they are for buffers where messages are
 to be queued, other wise they are for buffers with will have only 1
 message at a time.*
* Queuing buffers store a CMS_QUEUING_HEADER at the beginning and a
 CMS_HEADER before each message. Non-queuing buffers have only a
 CMS_HEADER before the only message.
* If they end in "encoded" they are for buffers that will neutrally encoded
 in some processor architecture independant data format such as XDR
 (eXternal Data Representation), otherwise the buffer must be in the
 format used by the same compiler as this is compiled in and for the same
 processor architecture and the function name will end in "raw".
*****************************************************************************/

/* Include Files */
#include "cms.hh"		/* class CMS  */
#include "cmsdiag.hh"		// class CMS_DIAG_PROC_INFO, CMS_DIAG_HEADER
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "physmem.hh"		/* class PHYSMEM_HANDLE  */

#if ENABLE_RCS_CMS_MRPQ
#include "cms_mrpq.hh"
#endif

/* CMS Member Functions. */

int cms_print_queue_free_space = 0;
int cms_print_queue_full_messages = 1;


/*************************************************************************
* This function should be called by functions overloading main_access()
* It uses a dummy physmem handle so that reads and writes work on
* the memory at _global.
* Parameters:
* _local - Address of local buffer where user has stored messages in or will
* read messages from whithin this process.
* _global - Address of shared or global memory buffer used to communicate with
* other  process.
************************************************************************/
CMS_STATUS
  CMS::internal_access (void *_global, long _global_size, void *_local)
{
  /* Don't bother trying to create a physmem handle for a NULL pointer. */
  if (NULL == _global)
    {
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Create a dummy physmem handle if I don't already have one. */
  if (NULL == dummy_handle)
    {
      dummy_handle = new PHYSMEM_HANDLE;
    }

  /* Check for problem allocating memory. */
  if (NULL == dummy_handle)
    {
      rcs_print_error ("CMS: Couldn't create PHYSMEM_HANDLE.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  dummy_handle->set_to_ptr (_global, _global_size);
  internal_access (dummy_handle, _local);
  return (status);
}

/* This function should be called by classes which overload the */
/* main_access function. */
CMS_STATUS CMS::internal_access (PHYSMEM_HANDLE * _global, void *_local)
{
  long orig_offset;
  long offset_before_split;
  status = CMS_STATUS_NOT_SET;
  orig_offset=-1;
  offset_before_split=-1;

  if (NULL == _global)
    {
      rcs_print_error ("CMS: Handle to global memory is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  handle_to_global_data = _global;
  int start_error_count =  handle_to_global_data->get_error_count();

  if (CMS_CLEAR_ACCESS == internal_access_type &&
      !(multireader_priority_queue_enabled && mrpq))
    {
      internal_clear ();
      if(status > 0 &&
	 start_error_count !=  handle_to_global_data->get_error_count()) {
	status = CMS_MISC_ERROR;
      }
      return (status);
    }

  if (min_compatible_version > 2.58 || min_compatible_version < 1E-6)
    {
      handle_to_global_data->increment_offset( skip_area);
    }


  if (CMS_GET_DIAG_INFO_ACCESS == internal_access_type)
    {
      internal_retrieve_diag_info (handle_to_global_data, _local);
      if(status > 0 &&
	 start_error_count !=  handle_to_global_data->get_error_count()) {
	status = CMS_MISC_ERROR;
      }
      return (status);
    }

  orig_offset = handle_to_global_data->get_offset();
  if (enable_diagnostics)
    {
      handle_to_global_data->increment_offset(
	sizeof (CMS_DIAG_HEADER) +
	total_connections * sizeof (CMS_DIAG_PROC_INFO));
      handle_to_global_data->set_enable_byte_counting(1);
      pre_op_total_bytes_moved = handle_to_global_data->get_total_bytes_moved();
    }

  char
    was_read_byte;
  int
    read_only = ((internal_access_type == CMS_CHECK_IF_READ_ACCESS) ||
		 (internal_access_type == CMS_READ_ACCESS) ||
		 (internal_access_type == CMS_PEEK_ACCESS));

  offset_before_split = handle_to_global_data->get_offset();

  write_just_completed = false;

  if (total_subdivisions >= 1 && current_subdivision > 0
      && current_subdivision < total_subdivisions)
    {
      handle_to_global_data->set_size(size);      
      handle_to_global_data->increment_offset( (current_subdivision * subdiv_size));
      handle_to_global_data->set_size( ((current_subdivision + 1) * subdiv_size));
      if (handle_to_global_data->get_size() > size)
	{
	  handle_to_global_data->set_size( size);
	}
    }

#if ENABLE_RCS_CMS_MRPQ
  if(multireader_priority_queue_enabled && mrpq)
    {
      return mrpq->internal_access(handle_to_global_data,
				   _local,
				   internal_access_type,
				   priority);
    }
#endif

  if (split_buffer)
    {
      if (internal_access_type == CMS_WRITE_IF_READ_ACCESS)
	{
	  handle_to_global_data->increment_offset(1);
	  handle_to_global_data->read (&was_read_byte, 1);
	  handle_to_global_data->decrement_offset(1);
	  header.was_read = (was_read_byte == toggle_bit + 1);
	  if (!header.was_read)
	    {
	      status = CMS_WRITE_WAS_BLOCKED;
	      return (status);
	    }
	  internal_access_type = CMS_WRITE_ACCESS;
	}
      if (read_only == toggle_bit)
	{
	  handle_to_global_data->increment_offset( 2);
	  handle_to_global_data->set_size( half_size);
	}
      else
	{
	  handle_to_global_data->set_size( size);
	  handle_to_global_data->increment_offset( half_offset);
	}
    }
  if(memory_align > 1)
    {
      handle_to_global_data->align(memory_align);
    }

  if (!queuing_enabled)
    {
      if (neutral)
	{
	  switch (internal_access_type)
	    {
	    case CMS_CHECK_IF_READ_ACCESS:
	      check_if_read_encoded ();
	      break;
	    case CMS_READ_ACCESS:
	      read_encoded ();
	      break;
	    case CMS_PEEK_ACCESS:
	      peek_encoded ();
	      break;
	    case CMS_WRITE_ACCESS:
	      write_encoded ();
	      break;
	    case CMS_WRITE_IF_READ_ACCESS:
	      write_if_read_encoded ();
	      break;
	    case CMS_GET_MSG_COUNT_ACCESS:
	      get_msg_count_encoded ();
	      break;
	    case CMS_GET_READ_COUNT_ACCESS:
	      get_read_count_encoded ();
	      break;
	    case CMS_GET_IS_CLEAR_ACCESS:
	      get_is_clear_encoded ();
	      break;

	    case CMS_WAIT_FOR_READ_ACCESS:
	      check_if_read_encoded ();
	      get_msg_count_encoded ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_was_read != 0 && 0 == header.was_read)
		{
		  starting_wait_for_was_read = 0;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      else if(header.was_read != 0 &&
		      ((starting_wait_for_was_read == 0) ||
		       (starting_wait_for_write_id != header.write_id)))
		{
		  status = CMS_WAIT_FOR_READ_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      break;

	    case CMS_WAIT_FOR_WRITE_ACCESS:
	      get_msg_count_encoded ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_write_id != header.write_id)
		{
		  status = CMS_WAIT_FOR_WRITE_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		}
	      break;

	    default:
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
      else
	{
	  switch (internal_access_type)
	    {
	    case CMS_CHECK_IF_READ_ACCESS:
	      check_if_read_raw ();
	      break;
	    case CMS_READ_ACCESS:
	      read_raw ();
	      break;
	    case CMS_PEEK_ACCESS:
	      peek_raw ();
	      break;
	    case CMS_WRITE_ACCESS:
	      write_raw (_local);
	      break;
	    case CMS_WRITE_IF_READ_ACCESS:
	      write_if_read_raw (_local);
	      break;
	    case CMS_GET_MSG_COUNT_ACCESS:
	      get_msg_count_raw ();
	      break;
	    case CMS_GET_READ_COUNT_ACCESS:
	      get_read_count_raw ();
	      break;
	    case CMS_GET_IS_CLEAR_ACCESS:
	      get_is_clear_raw ();
	      break;

	    case CMS_WAIT_FOR_READ_ACCESS:
	      check_if_read_raw ();
	      get_msg_count_raw ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_was_read != 0 && 0 == header.was_read)
		{
		  starting_wait_for_was_read = 0;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      else if(header.was_read != 0 &&
		      ((starting_wait_for_was_read == 0) ||
		       (starting_wait_for_write_id != header.write_id)))
		{
		  status = CMS_WAIT_FOR_READ_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      break;

	    case CMS_WAIT_FOR_WRITE_ACCESS:
	      get_msg_count_raw ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_write_id != header.write_id)
		{
		  status = CMS_WAIT_FOR_WRITE_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		}
	      break;

	    default:
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
    }
  else
    {
      if (neutral)
	{
	  switch (internal_access_type)
	    {
	    case CMS_CHECK_IF_READ_ACCESS:
	      queue_check_if_read_encoded ();
	      break;
	    case CMS_READ_ACCESS:
	      queue_read_encoded ();
	      break;
	    case CMS_PEEK_ACCESS:
	      queue_peek_encoded ();
	      break;
	    case CMS_WRITE_ACCESS:
	      queue_write_encoded ();
	      break;
	    case CMS_WRITE_IF_READ_ACCESS:
	      queue_write_if_read_encoded ();
	      break;
	    case CMS_GET_QUEUE_LENGTH_ACCESS:
	      queue_get_queue_length_encoded ();
	      break;
	    case CMS_GET_SPACE_AVAILABLE_ACCESS:
	      queue_get_space_available_encoded ();
	      break;
	    case CMS_GET_MSG_COUNT_ACCESS:
	      queue_get_msg_count_encoded ();
	      break;
	    case CMS_GET_READ_COUNT_ACCESS:
	      queue_get_read_count_encoded ();
	      break;
	    case CMS_GET_IS_CLEAR_ACCESS:
	      queue_get_is_clear_encoded ();
	      break;


	    case CMS_WAIT_FOR_READ_ACCESS:
	      queue_check_if_read_encoded ();
	      queue_get_queue_length_encoded ();
	      queue_get_msg_count_encoded ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  starting_wait_for_queue_length = queuing_header.queue_length;
		  starting_wait_for_queue_head = queuing_header.head;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(
		      (starting_wait_for_queue_length  > queuing_header.queue_length) ||
		      (starting_wait_for_queue_length + (header.write_id - starting_wait_for_write_id)  > queuing_header.queue_length) ||
		      (starting_wait_for_queue_head != queuing_header.head &&
		       starting_wait_for_queue_length > 0))
		{
		  status = CMS_WAIT_FOR_READ_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      break;

	    case CMS_WAIT_FOR_WRITE_ACCESS:
	      queue_get_msg_count_encoded ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_write_id != header.write_id)
		{
		  status = CMS_WAIT_FOR_WRITE_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		}
	      break;

	    case CMS_WAIT_FOR_CLEAR_ACCESS:
	      queue_get_msg_count_encoded ();
	      queue_get_queue_length_encoded ();
	      if(!wait_for_initialized)
		{
		  if(queuing_header.head != 0 ||
		     queuing_header.tail != 0 ||
		     queuing_header.queue_length != 0 ||
		     header.write_id != 0)
		    {
		      wait_for_initialized=true;
		    }
		  status = CMS_WAIT_FOR_CLEAR_INCOMPLETE;
		}
	      else
		{
		  if(queuing_header.head == 0 &&
		     queuing_header.tail == 0 &&
		     queuing_header.queue_length == 0 &&
		     header.write_id == 0)
		    {
		      status = CMS_WAIT_FOR_CLEAR_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_CLEAR_INCOMPLETE;
		    }
		}
	      break;

	    case CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS:
	      queue_get_queue_length_encoded ();
	      if(waiting_for_queue_length_over)
		{
		  if(queue_length_to_wait_for < queuing_header.queue_length)
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE;
		    }
		}
	      else
		{
		  if(queue_length_to_wait_for > queuing_header.queue_length)
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE;
		    }
		}
	      break;

	    default:
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
      else
	{
	  switch (internal_access_type)
	    {
	    case CMS_CHECK_IF_READ_ACCESS:
	      queue_check_if_read_raw ();
	      break;
	    case CMS_READ_ACCESS:
	      queue_read_raw ();
	      break;
	    case CMS_PEEK_ACCESS:
	      queue_peek_raw ();
	      break;
	    case CMS_WRITE_ACCESS:
	      queue_write_raw (_local);
	      break;
	    case CMS_WRITE_IF_READ_ACCESS:
	      queue_write_if_read_raw (_local);
	      break;
	    case CMS_GET_QUEUE_LENGTH_ACCESS:
	      queue_get_queue_length_raw ();
	      break;
	    case CMS_GET_SPACE_AVAILABLE_ACCESS:
	      queue_get_space_available_raw ();
	      break;
	    case CMS_GET_MSG_COUNT_ACCESS:
	      queue_get_msg_count_raw ();
	      break;
	    case CMS_GET_READ_COUNT_ACCESS:
	      queue_get_read_count_raw ();
	      break;
	    case CMS_GET_IS_CLEAR_ACCESS:
	      queue_get_is_clear_raw ();
	      break;

	    case CMS_WAIT_FOR_CLEAR_ACCESS:
	      queue_get_msg_count_raw ();
	      queue_get_queue_length_raw ();
	      if(!wait_for_initialized)
		{
		  if(queuing_header.head != 0 ||
		     queuing_header.tail != 0 ||
		     queuing_header.queue_length != 0 ||
		     header.write_id != 0)
		    {
		      wait_for_initialized=true;
		    }
		  status = CMS_WAIT_FOR_CLEAR_INCOMPLETE;
		}
	      else
		{
		  if(queuing_header.head == 0 &&
		     queuing_header.tail == 0 &&
		     queuing_header.queue_length == 0 &&
		     header.write_id == 0)
		    {
		      status = CMS_WAIT_FOR_CLEAR_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_CLEAR_INCOMPLETE;
		    }
		}
	      break;

	    case CMS_WAIT_FOR_READ_ACCESS:
	      queue_check_if_read_raw ();
	      queue_get_queue_length_raw ();
	      queue_get_msg_count_raw ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  starting_wait_for_queue_length = queuing_header.queue_length;
		  starting_wait_for_queue_head = queuing_header.head;
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(
		      (starting_wait_for_queue_length  > queuing_header.queue_length) ||
		      (starting_wait_for_queue_length + (header.write_id - starting_wait_for_write_id)  > queuing_header.queue_length) ||
		      (starting_wait_for_queue_head != queuing_header.head  &&
		       starting_wait_for_queue_length > 0))
		{
		  status = CMS_WAIT_FOR_READ_OK;
		}
	      else
		{
		  status = CMS_WAIT_FOR_READ_INCOMPLETE;
		}
	      break;

	    case CMS_WAIT_FOR_WRITE_ACCESS:
	      queue_get_msg_count_raw ();
	      if(!wait_for_initialized)
		{
		  starting_wait_for_write_id = header.write_id;
		  starting_wait_for_was_read = header.was_read;
		  status = CMS_WAIT_FOR_WRITE_INCOMPLETE;
		  wait_for_initialized=true;
		}
	      else if(starting_wait_for_write_id != header.write_id)
		{
		  status = CMS_WAIT_FOR_WRITE_OK;
		}
	      break;

	    case CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS:
	      queue_get_queue_length_raw ();
	      if(waiting_for_queue_length_over)
		{
		  if(queue_length_to_wait_for < queuing_header.queue_length)
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE;
		    }
		}
	      else
		{
		  if(queue_length_to_wait_for > queuing_header.queue_length)
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_OK;
		    }
		  else
		    {
		      status = CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE;
		    }
		}
	      break;

	    default:
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
    }

  if (split_buffer)
    {
      handle_to_global_data->set_offset(offset_before_split + 1);
      if (internal_access_type == CMS_READ_ACCESS)
	{
	  was_read_byte = 1;
	}
      else if (!read_only)
	{
	  was_read_byte = 0;
	}
      if (-1 == handle_to_global_data->write (&was_read_byte, 1))
	{
	  rcs_print_error ("CMS: can not set was read flag.\n");
	}
    }


  if (enable_diagnostics)
    {
      handle_to_global_data->set_offset(orig_offset);
      calculate_and_store_diag_info (handle_to_global_data, _local);
    }
  if(status > 0 &&
     start_error_count !=  handle_to_global_data->get_error_count()) {
    status = CMS_MISC_ERROR;
  }
  return (status);
}

/* Clear the shared or global memory. */
CMS_STATUS CMS::internal_clear ()
{
  long temp_offset;

  temp_offset=-1;
  in_buffer_id = 0;

  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  if (-1 == handle_to_global_data->clear_memory ())
    {
      rcs_print_error ("CMS: Can't clear global_memory.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  
  temp_offset = handle_to_global_data->get_offset();

  handle_to_global_data->set_offset(0);
  if (-1 == handle_to_global_data->write (BufferName, 32))
    {
      rcs_print_error ("CMS: Can't clear reset name in global memory.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  handle_to_global_data->set_offset(temp_offset);

  return (status = CMS_CLEAR_OK);
}

/* Determine whether the message in the buffer has been read at least once. */
int
CMS::check_if_read_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the message. */
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d", BufferName,
	 __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }
  return ((int) header.was_read);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_check_if_read_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  return ((int) (queuing_header.queue_length == 0));
}

/* Determine whether the message in the buffer has been read at least once. */
int
CMS::check_if_read_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in header structure. */
  decode_header ();

  return ((int) header.was_read);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_check_if_read_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();

  return ((int) (queuing_header.queue_length == 0));
}



/* Get the number of messages that have been written to this buffer. */
int
CMS::get_msg_count_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the message. */
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d", BufferName,
	 __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }
  return (header.write_id);
}

/* Get the number of messages that have been written to this buffer. */
int
CMS::get_read_count_raw ()
{
  if(!keep_read_count)
    {
      rcs_print_error("get_read_count called buf keep_read_count not set.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return -1;
    }

  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the message. */
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d", BufferName,
	 __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }
  is_clear = (header.was_read == 0 && header.write_id == 0 && header.in_buffer_size == 0);
  read_count = (header.was_read & ~1 & ~(1<<(sizeof(read_count)*8-1)))>>1;
  header.was_read = header.was_read & 1;
  return (read_count);
}

int
CMS::get_is_clear_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the message. */
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d", BufferName,
	 __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }
  is_clear = ((header.was_read & 1)== 0 && header.write_id == 0 && header.in_buffer_size == 0);
  if(keep_read_count)
    {
      read_count = (header.was_read & ~1 & ~(1<<(sizeof(read_count)*8-1)))>>1;
      header.was_read = header.was_read & 1;
    }
  return (is_clear);
}

int
CMS::queue_get_msg_count_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  return (header.write_id = queuing_header.write_id);
}

int
CMS::queue_get_read_count_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  header.write_id = queuing_header.write_id;
  read_count = header.write_id - queuing_header.queue_length;
  return(read_count);
}


int
CMS::queue_get_is_clear_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }
  is_clear = ( queuing_header.write_id == 0 && 
	       queuing_header.queue_length == 0 &&
	       queuing_header.head == 0 && 
	       queuing_header.tail == 0 && 
	       queuing_header.end_queue_space == 0);
  return (is_clear);
}

int
CMS::get_msg_count_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in header structure. */
  decode_header ();

  return ((int) header.write_id);
}

int
CMS::get_read_count_encoded ()
{
  if(!keep_read_count)
    {
      rcs_print_error("get_read_count called buf keep_read_count not set.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return -1;
    }
  
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in header structure. */
  decode_header ();
  read_count = (header.was_read & ~1 & ~(1<<(sizeof(read_count)*8-1)))>>1;
  header.was_read = header.was_read & 1;
  return (read_count);
}

int
CMS::get_is_clear_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in header structure. */
  decode_header ();
  is_clear = ((header.was_read & 1)== 0 && header.write_id == 0 && header.in_buffer_size == 0);
  if(keep_read_count)
    {
      read_count = (header.was_read & ~1 & ~(1<<(sizeof(read_count)*8-1)))>>1;
      header.was_read = header.was_read & 1;
    }
  return (is_clear);
}

int
CMS::queue_get_msg_count_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();

  return (header.write_id = queuing_header.write_id);
}

int
CMS::queue_get_read_count_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();

  header.write_id = queuing_header.write_id;
  read_count = (queuing_header.write_id - queuing_header.queue_length);
  return (read_count);
}

int
CMS::queue_get_is_clear_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();
  is_clear = ( queuing_header.write_id == 0 && 
	       queuing_header.queue_length == 0 &&
	       queuing_header.head == 0 && 
	       queuing_header.tail == 0 && 
	       queuing_header.end_queue_space == 0);
  return (is_clear);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_get_queue_length_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  return (queuing_header.queue_length);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_get_queue_length_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();

  return (queuing_header.queue_length);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_get_space_available_raw ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Store the original offset so that we can update the header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  compute_raw_queue_free_space();
  return (free_space);
}

/* Determine if all of the messages in the buffer have been read. */
/* This means the queue is empty. */
int
CMS::queue_get_space_available_encoded ()
{
  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Store the original offset so that we can update the header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error
	("CMS: Error reading from global memory for %s at %s:%d\n",
	 BufferName, __FILE__, __LINE__);
      status = CMS_INTERNAL_ACCESS_ERROR;
      return 0;
    }

  /* Decode the header and store in queuing_header structure. */
  decode_queuing_header ();
  compute_encoded_queue_free_space();

  return (free_space);
}


/* It takes several steps to perform a read operation. */
/* 1. Read the header. */
/* 2. Check the id and size. */
/* 3. If id and size are ok, then read the message and update the header. */
CMS_STATUS CMS::read_raw ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS:(%s) handle_to_global_data is NULL.\n",
		       BufferName);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the header for the message. */
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Set status to CMS_READ_OLD or CMS_READ_OK */
  if (check_id (header.write_id) == CMS_READ_OK)
    {
      /* Check the size of the message. */
      if (header.in_buffer_size > max_message_size)
	{
	  rcs_print_error
	    ("CMS:(%s) Message size of %ld exceeds maximum of %ld\n",
	     BufferName, header.in_buffer_size, max_message_size);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}    


      /* Read the message. */
      handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
      if (-1 ==
	  handle_to_global_data->read (subdiv_data,
				       (long) header.in_buffer_size))
	{
	  rcs_print_error
	    ("CMS:(%s) Error reading from global memory at %s:%d\n",
	     BufferName, __FILE__, __LINE__);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}
      handle_to_global_data->decrement_offset( sizeof (CMS_HEADER));
    }

  /* Update the header. */
  if(keep_read_count)
    {
      read_count = (header.was_read & ~1)>>1;
      read_count++;
      header.was_read = (read_count&(~(1<<(sizeof(read_count)*8-1))))<<1 | 1;
    }
  else
    {
      header.was_read = 1;
    }

  if (-1 == handle_to_global_data->write (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  if(keep_read_count)
    {
      header.was_read=1;
    }
  return (status);
}

/* It takes several steps to perform a read operation when queuing is enabled. */
/* 1. Read the queuing_header at the beginning of the buffer. */
/* 2. Get the head of the queue from the queuing_header. */
/* 3. Read the message header at the head of the queue. */
/* 4. Check the id and size. */
/* 5. If id and size are ok, */
 /* then read the message and */
 /* update both the queuing header and message header. */
CMS_STATUS CMS::queue_read_raw ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so that we can update the header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check to see if there are any unread messages. */
  if (queuing_header.queue_length == 0)
    {
      return (status = CMS_READ_OLD);
    }

  /* Read the header for the message. */
  handle_to_global_data->increment_offset( queuing_header.head);
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check the size of the message. */
  if (header.in_buffer_size > max_message_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, max_message_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Update the message header. */
  header.was_read = 1;
  if (-1 == handle_to_global_data->write (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 ==
      handle_to_global_data->read (subdiv_data, (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Update the queuing header for the buffer. */
  queuing_header.head += header.in_buffer_size + sizeof (CMS_HEADER);
  if (queuing_header.head >= queuing_header.end_queue_space)
    {
      queuing_header.head = sizeof (CMS_QUEUING_HEADER);
    }
  queuing_header.queue_length--;
  if (queuing_header.queue_length == 0)
    {
      queuing_header.head = queuing_header.tail = sizeof (CMS_QUEUING_HEADER);
      queuing_header.end_queue_space = queuing_header.tail;
    }
  handle_to_global_data->set_offset(queuing_header_offset);
  if (-1 == handle_to_global_data->write (&queuing_header,
					  sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check_id so that debug variables for messages missed can be set. */
  check_id (header.write_id);

  return (status);
}

/* It takes several steps to perform a read operation on an neutral buffer.*/
/* 1. Read the encoded  header. */
/* 2. Decode the header. */
/* 3. Check the id and size. */
/* 4. If id and size are ok, then read the message and update the header. */
CMS_STATUS CMS::read_encoded ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  if (NULL == encoded_data)
    {
      rcs_print_error ("CMS:read_encoded() encoded_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the encoded header for the message. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the header and  store in header structure. */
  decode_header ();

  /* Determine if the message in the buffer is new to this process. */
  if (check_id (header.write_id) == CMS_READ_OK)
    {
      /* Check the size of the message. */
      if (header.in_buffer_size > encoded_data_size)
	{
	  rcs_print_error
	    ("CMS:(%s) Message size of %ld exceeds maximum of %ld\n",
	     BufferName, header.in_buffer_size, encoded_data_size);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}

      /* Read the message. */
      handle_to_global_data->increment_offset( encoded_header_size);
      if (-1 == handle_to_global_data->read (encoded_data,
					     (long) header.in_buffer_size))
	{
	  rcs_print_error
	    ("CMS:(%s) Error reading from global memory.\n", BufferName);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}
      handle_to_global_data->decrement_offset( encoded_header_size);
    }

  /* Update the header. */
  header.was_read = 1;

  encode_header ();
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status);
}

/* It takes several steps to perform a read operation */
 /* when queuing is enabled on a neutral buffer. */
/* 1. Read the encoded queuing_header at the beginning of the buffer. */
/* 2. Decode the queuing_header for the buffer. */
/* 3. Get the head of the queue from the queuing_header. */
/* 4. Read the message header at the head of the queue. */
/* 5. Decode the message header. */
/* 6. Check the id and size. */
/* 7. If id and size are ok, */
 /* then read the message */
CMS_STATUS CMS::queue_read_encoded ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the encoded header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the queuing header and store in the queuing_header structrure. */
  decode_queuing_header ();

  /* Determine if there are any unread messages. */
  if (queuing_header.queue_length == 0)
    {
      return (status = CMS_READ_OLD);
    }

  /* Read the header for the message. */
  handle_to_global_data->increment_offset( queuing_header.head);
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, queuing_header.write_id);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the message header and store in the header structure. */
  decode_header ();

  /* Check the size of the message. */
  if (header.in_buffer_size > encoded_data_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, encoded_data_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Update the message header. */
  header.was_read = 1;
  encode_header ();
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, queuing_header.write_id);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 ==
      handle_to_global_data->read (encoded_data,
				   (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, queuing_header.write_id);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }


  /* Update the buffer header. */
  queuing_header.head += header.in_buffer_size + encoded_header_size;
  if (queuing_header.head >= queuing_header.end_queue_space)
    {
      queuing_header.head = encoded_queuing_header_size;
    }
  queuing_header.queue_length--;
  if (queuing_header.queue_length == 0)
    {
      queuing_header.head = queuing_header.tail = encoded_queuing_header_size;
      queuing_header.end_queue_space = queuing_header.tail;
    }
  encode_queuing_header ();
  handle_to_global_data->set_offset(queuing_header_offset);
  if (-1 == handle_to_global_data->write (encoded_queuing_header,
					  encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, queuing_header.write_id);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check_id so that debug variables for messages missed can be set. */
  check_id (header.write_id);

  return (status);
}

/****************************************************************
* Peek operations are the same as reads,
 except that the header(s) are not updated.
****************************************************************/

/* It takes several steps to perform a peek operation. */
/* 1. Read the header. */
/* 2. Check the id and size. */
/* 3. If id and size are ok, then read the message. */
CMS_STATUS CMS::peek_raw ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS:(%s) handle_to_global_data is NULL.\n",
		       BufferName);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  if(!get_msg_start_only)
    {
      /* Read the header for the message. */
      if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
	{
	  rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
			   BufferName, __FILE__, __LINE__);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}
      
      /* Set status to CMS_READ_OLD or CMS_READ_OK */
      if (check_id (header.write_id) == CMS_READ_OLD)
	{

	  return (status);		/* Don't bother copying out an old message. */
	}
    }
  else
    {
      status = CMS_READ_OK;
    }
  long orig_header_in_buffer_size = header.in_buffer_size;
  if(get_msg_start_only)
    {
      header.in_buffer_size = (long) temp_data_size;
    }

  /* Check the size of the message. */
  if (header.in_buffer_size > max_message_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, max_message_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 ==
      handle_to_global_data->read (subdiv_data, (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  if(get_msg_start_only)
    {
      header.in_buffer_size = orig_header_in_buffer_size;
    }
  return (status);
}

/* It takes several steps to perform a peek  operation when queuing is enabled. */
/* 1. Read the queuing_header at the beginning of the buffer. */
/* 2. Get the head of the queue from the queuing_header. */
/* 3. Read the message header at the head of the queue. */
/* 4. Check the id and size. */
/* 5. If id and size are ok, */
 /* then read the message */
CMS_STATUS
CMS::queue_peek_raw ()
{

  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so that we can update the header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header for the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check to see if there are any unread messages. */
  if (queuing_header.queue_length == 0)
    {
      return (status = CMS_READ_OLD);
    }

  /* Read the header for the message. */
  handle_to_global_data->increment_offset( queuing_header.head);
  if (-1 == handle_to_global_data->read (&header, sizeof (CMS_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  if(get_msg_start_only && ((size_t) header.in_buffer_size) > temp_data_size)
    {
      header.in_buffer_size = (long) temp_data_size;
    }

  /* Check the size of the message. */
  if (header.in_buffer_size > max_message_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, max_message_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 ==
      handle_to_global_data->read (subdiv_data, (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check_id so that debug variables for messages missed can be set. */
  check_id (header.write_id);

  return (status);
}


/* It takes several steps to perform a peek operation on an neutral buffer.*/
/* 1. Read the encoded  header. */
/* 2. Decode the header. */
/* 3. Check the id and size. */
/* 4. If id and size are ok, then read the message. */
CMS_STATUS CMS::peek_encoded ()
{
  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the encoded header for the message. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the header and  store in header structure. */
  decode_header ();

  /* Determine if the message in the buffer is new to this process. */
  if (CMS_READ_OLD == check_id (header.write_id))
    {
      if(!get_msg_start_only || header.write_id == 0)
	{
	  return (CMS_READ_OLD);	/* Don't bother reading an old message. */
	}
      if(header.in_buffer_size == 0)
	{
	  return (CMS_READ_OLD);	/* Buffer was never written or was just cleared. */
	}
    }

  /* Check the size of the message. */
  if(get_msg_start_only && ((size_t) header.in_buffer_size) > temp_data_size)
    {
      header.in_buffer_size = (long) temp_data_size;
    }
      
  if (header.in_buffer_size > encoded_data_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, encoded_data_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 == handle_to_global_data->read (encoded_data,
					 (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status);
}

/* It takes several steps to perform a peek operation */
 /* when queuing is enabled on a neutral buffer. */
/* 1. Read the encoded queuing_header at the beginning of the buffer. */
/* 2. Decode the queuing_header for the buffer. */
/* 3. Get the head of the queue from the queuing_header. */
/* 4. Read the message header at the head of the queue. */
/* 5. Decode the message header. */
/* 6. Check the id and size. */
/* 7. If id and size are ok, */
 /* then read the message */
CMS_STATUS CMS::queue_peek_encoded ()
{

  /* Produce error message if process does not have permission to read. */
  if (!read_permission_flag)
    {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Check that the handle to global memory exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the encoded header for the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the queuing header and store in the queuing_header structrure. */
  decode_queuing_header ();

  /* Determine if there are any unread messages. */
  if (queuing_header.queue_length == 0)
    {
      return (status = CMS_READ_OLD);
    }

  /* Read the header for the message. */
  handle_to_global_data->increment_offset( queuing_header.head);
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Decode the message header and store in the header structure. */
  decode_header ();

  if(get_msg_start_only && ((size_t)header.in_buffer_size) > temp_data_size)
    {
      header.in_buffer_size = (long) temp_data_size;
    }


  /* Check the size of the message. */
  if (header.in_buffer_size > encoded_data_size)
    {
      rcs_print_error
	("CMS:(%s) Message size of %ld exceeds maximum of %ld\n", BufferName,
	 header.in_buffer_size, encoded_data_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 ==
      handle_to_global_data->read (encoded_data,
				   (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check_id so that debug variables for messages missed can be set. */
  check_id (header.write_id);

  return (status);
}

/* It takes several steps to perform a write operation. */
/* 1. Read the header. */
/* 2. Update the header. */
/* 3. Write the message. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::write_raw (void *user_data)
{
  long
    current_header_in_buffer_size;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Store the header information to use after reading the header in the buffer. */
  current_header_in_buffer_size = header.in_buffer_size;

  /* Check that handle to global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the header. */
  if (-1 == handle_to_global_data->read (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Update the header. */
  header.was_read = 0;
  header.write_id++;
  if (split_buffer)
    {
      if ((header.write_id & 1) != toggle_bit)
	{
	  header.write_id++;
	}
    }
  header.in_buffer_size = current_header_in_buffer_size;
  if (-1 == handle_to_global_data->write (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  if (!disable_final_write_raw_for_dma)
    {
      handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
      if(bitwise_op == CMS_BITWISE_AND_OP)
	{
	  if (-1 == handle_to_global_data->write_with_bitwise_and (user_data,
								   (long)
								   current_header_in_buffer_size))
	    {
	      rcs_print_error
		("CMS:(%s) Error writing %ld bytes to global memory at offset %p\n",
		 BufferName, header.in_buffer_size, user_data);
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
      else if(bitwise_op == CMS_BITWISE_OR_OP)
	{
	  if (-1 == handle_to_global_data->write_with_bitwise_or (user_data,
								   (long)
								   current_header_in_buffer_size))
	    {
	      rcs_print_error
		("CMS:(%s) Error writing %ld bytes to global memory at offset %p\n",
		 BufferName, header.in_buffer_size, user_data);
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
      else
	{
	  if (-1 == handle_to_global_data->write(user_data,
						 (long)current_header_in_buffer_size))
	    {
	      rcs_print_error
		("CMS:(%s) Error writing %ld bytes to global memory at offset %p\n",
		 BufferName, header.in_buffer_size, user_data);
	      return (status = CMS_INTERNAL_ACCESS_ERROR);
	    }
	}
    }
  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation when queuing is enabled. */
/* 1. Read the qeuing header at the begining of the buffer. */
/* 2. Determine the amount of free space and where the next node can be placed.*/
/* 3. Set up message header from info in the queuing header. */
/* 4. Write the message header and message  at the tail of the queue. */
/* 5. Update the queuing header. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::queue_write_raw (void *user_data)
{
  CMS_HEADER
    current_header;
  long
    original_tail;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header at the beginning of the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  compute_raw_queue_free_space();
  
  if(max_queue_length > 0 && queuing_header.queue_length >= max_queue_length)
    {
      if(cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	}
      return (status = CMS_QUEUE_FULL);
    } 

  /* Check to see if there is enough free space. */
  if (free_space < ((long) (header.in_buffer_size)))
    {
      if (cms_print_queue_free_space || cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	  rcs_print_error
	    ("(continued) CMS: Message requires %ld bytes but only %ld bytes are left.\n",
	     header.in_buffer_size, free_space);
	}
      return (status = CMS_QUEUE_FULL);
    }

  if(queuing_header.tail > queuing_header.head 
     && (handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset) <
     ((long) (header.in_buffer_size + sizeof(CMS_HEADER))))
    {
      if(queuing_header.head - sizeof(CMS_QUEUING_HEADER) - queuing_header_offset > 
	 header.in_buffer_size + sizeof(CMS_HEADER))
	{
	  queuing_header.end_queue_space = queuing_header.tail;
	  queuing_header.tail = sizeof(CMS_QUEUING_HEADER);
	}
      else
	{
	  if (cms_print_queue_free_space || cms_print_queue_full_messages)
	    {
	      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset;
	      long free_space_bottom = queuing_header.head - sizeof(CMS_QUEUING_HEADER) - sizeof(CMS_HEADER);
	      free_space = (free_space_top >  free_space_bottom)?free_space_top:free_space_bottom;
	      rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	      rcs_print_error
		("(continued) CMS: Message requires %ld bytes but only %ld bytes are free and contiguous.\n",
		 header.in_buffer_size, free_space);
	      rcs_print_error ("queue free_space_top = %ld\n", free_space_top);
	      rcs_print_error ("queue free_space_bottom = %ld\n", free_space_bottom);
	    }
	  return (status = CMS_QUEUE_FULL);
	}
    }
	  
  /* Store original tail so we'll know where to store the message. */
  original_tail = queuing_header.tail;

  /* Update the queuing header. */
  queuing_header.tail += header.in_buffer_size + sizeof (CMS_HEADER);
  queuing_header.queue_length++;
  queuing_header.write_id++;
  if (queuing_header.end_queue_space < queuing_header.tail)
    {
      queuing_header.end_queue_space = queuing_header.tail;
    }

  if (-1 == handle_to_global_data->write (&queuing_header,
					  sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Setup message header. */
  header.write_id = queuing_header.write_id;
  header.was_read = 0;
  header.in_buffer_size = current_header.in_buffer_size;

  /* Write the message header. */
  handle_to_global_data->increment_offset( original_tail);
  if (-1 == handle_to_global_data->write (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 == handle_to_global_data->write (user_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation on a neutral buffer. */
/* 1. Read the header. */
/* 2. Update the header. */
/* 3. Write the message. */
CMS_STATUS CMS::write_encoded ()
{
  CMS_HEADER
    current_header;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that handle to global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check that buffer is large enough for this message. */
  if (header.in_buffer_size > encoded_data_size)
    {
      rcs_print_error
	("CMS:(%s) Message size %ld exceeds maximum for this buffer of %ld.\n",
	 BufferName, header.in_buffer_size, encoded_data_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the header. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  /* Decode the header and store in the header structure. */
  decode_header ();

  /* Update the header. */
  header.was_read = 0;
  header.write_id++;
  if (split_buffer && (header.write_id % 2) != toggle_bit)
    {
      header.write_id++;
    }
  header.in_buffer_size = current_header.in_buffer_size;
  encode_header ();
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 == handle_to_global_data->write (encoded_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

void
CMS::compute_raw_queue_free_space()
{
  /* Determine amount of free space and location of next node. */
  free_space = 0;


  if (queuing_header.tail > queuing_header.head)
    {
      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset - sizeof(CMS_HEADER);
      long free_space_bottom = queuing_header.head - sizeof(CMS_QUEUING_HEADER) - sizeof(CMS_HEADER);
      if (cms_print_queue_free_space)
	{
	  rcs_print ("queue free_space_top = %ld\n", free_space_top);
	  rcs_print ("queue free_space_bottom = %ld\n", free_space_bottom);
	}
      if (free_space_top > 0)
	{
	  free_space = free_space_top;
	}
      if (free_space_bottom > 0)
	{
	  free_space += free_space_bottom;
	}
    }
  else if (queuing_header.tail < queuing_header.head)
    {
      free_space = queuing_header.head - queuing_header.tail - sizeof(CMS_HEADER);
    }

  if (queuing_header.queue_length == 0)
    {
      queuing_header.head = queuing_header.tail = sizeof (CMS_QUEUING_HEADER);
      queuing_header.queue_length = 0;
      queuing_header.end_queue_space = queuing_header.tail;
      free_space = handle_to_global_data->get_size()
	- sizeof (CMS_QUEUING_HEADER) - queuing_header_offset - sizeof(CMS_HEADER);
    }


  if (cms_print_queue_free_space)
    {
      rcs_print ("queue free space = %ld\n", free_space);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, queuing_header.write_id);
    }
  if(free_space < 0)
    {
      free_space = 0;
    }
}

/* 
   CMS::compute_encoded_queue_free_space()
   This should be called after getting and decoding the queuing_header to determine the 
   amount of free space if needed.
*/
void
CMS::compute_encoded_queue_free_space()
{
  /* Determine amount of free space. */
  free_space = 0;
  if (queuing_header.tail > queuing_header.head)
    {
      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset - encoded_header_size;
      long free_space_bottom = queuing_header.head - encoded_queuing_header_size - encoded_header_size;
      if (cms_print_queue_free_space)
	{
	  rcs_print ("queue free_space_top = %ld\n", free_space_top);
	  rcs_print ("queue free_space_bottom = %ld\n", free_space_bottom);
	}
      if (free_space_top > 0)
	{
	  free_space = free_space_top;
	}
      if (free_space_bottom > 0)
	{
	  free_space += free_space_bottom;
	}
    }
  else if (queuing_header.tail < queuing_header.head)
    {
      free_space = queuing_header.head - queuing_header.tail - encoded_header_size;
    }

  if (queuing_header.queue_length == 0)
    {
      queuing_header.head = queuing_header.tail = encoded_queuing_header_size;
      queuing_header.queue_length = 0;
      queuing_header.end_queue_space = queuing_header.tail;
      free_space = handle_to_global_data->get_size()
	- encoded_queuing_header_size - queuing_header_offset - encoded_header_size;
    }

  if (cms_print_queue_free_space)
    {
      rcs_print ("queue free space = %ld\n", free_space);
      rcs_print (" { head=%ld,tail=%ld,end=%ld,length=%ld,id=%ld }\n",
		 queuing_header.head,
		 queuing_header.tail,
		 queuing_header.end_queue_space,
		 queuing_header.queue_length, 
		 queuing_header.write_id);
    }
  if(free_space < 0)
    {
      free_space = 0;
    }
}

/* It takes several steps to perform a write operation when queuing is enabled. */
/* 1. Read the qeuing header at the begining of the buffer. */
/* 2. Determine the amount of free space and where the next node can be placed.*/
/* 3. Set up message header from info in the queuing header. */
/* 4. Write the message header and message  at the tail of the queue. */
/* 5. Update the queuing header. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::queue_write_encoded ()
{
  CMS_HEADER
    current_header;
  long
    original_tail;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }


  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header at the beginning of the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  /* Decode queuing header and store in queuing_header structure. */
  decode_queuing_header ();
  compute_encoded_queue_free_space();
  
  if(max_queue_length > 0 && 
     queuing_header.queue_length >= max_queue_length)
    {
      if(cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	}
      return (status = CMS_QUEUE_FULL);
    } 
  
  /* Check to see if there is enough free space. */
  if (free_space < header.in_buffer_size + encoded_header_size)
    {
      if (cms_print_queue_free_space || cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	  rcs_print_error
	    ("(continued) CMS: Message requires %ld bytes but only %ld bytes are left.\n",
	     header.in_buffer_size, free_space);
	}
      return (status = CMS_QUEUE_FULL);
    }

  if(queuing_header.tail > queuing_header.head 
     && handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset <
     header.in_buffer_size + encoded_header_size)
    {
      if(queuing_header.head - encoded_queuing_header_size > 
	 header.in_buffer_size + encoded_header_size)
	{
	  queuing_header.end_queue_space = queuing_header.tail;
	  queuing_header.tail = encoded_queuing_header_size;
	}
      else
	{
	  if (cms_print_queue_free_space || cms_print_queue_full_messages)
	    {
	      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail - encoded_header_size - queuing_header_offset;
	      long free_space_bottom = queuing_header.head - encoded_queuing_header_size - encoded_header_size;
	      free_space = (free_space_top > free_space_bottom)?free_space_top:free_space_bottom;
	      rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	      rcs_print_error
		("(continued) CMS: Message requires %ld bytes but only %ld bytes are free and contiguous.\n",
		 header.in_buffer_size, free_space);
	      rcs_print_error ("queue free_space_top = %ld\n", free_space_top);
	      rcs_print_error ("queue free_space_bottom = %ld\n", free_space_bottom);
	    }
	  return (status = CMS_QUEUE_FULL);
	}
    }

  /* Store original tail so we'll know where to store the message. */
  original_tail = queuing_header.tail;

  /* Update the queuing header. */
  queuing_header.tail += header.in_buffer_size + encoded_header_size;
  queuing_header.queue_length++;
  queuing_header.write_id++;
  if (queuing_header.end_queue_space < queuing_header.tail)
    {
      queuing_header.end_queue_space = queuing_header.tail;
    }
  encode_queuing_header ();
  if (-1 == handle_to_global_data->write (encoded_queuing_header,
					  encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Setup message header. */
  header.write_id = queuing_header.write_id;
  header.was_read = 0;
  header.in_buffer_size = current_header.in_buffer_size;

  /* Re-encode the  header. */
  encode_header ();

  /* Write the message header. */
  handle_to_global_data->increment_offset( original_tail);
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 == handle_to_global_data->write (encoded_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation. */
/* 1. Read the header. */
/* 2. Update the header. */
/* 3. Write the message. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::write_if_read_raw (void *user_data)
{
  CMS_HEADER
    current_header;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that handle to global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the header. */
  if (-1 == handle_to_global_data->read (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check if the message in the buffer has been read. */
  if (!header.was_read)
    {
      return (status = CMS_WRITE_WAS_BLOCKED);
    }

  /* Update the header. */
  header.was_read = 0;
  header.write_id++;
  if (split_buffer && (header.write_id % 2) != toggle_bit)
    {
      header.write_id++;
    }
  header.in_buffer_size = current_header.in_buffer_size;
  if (-1 == handle_to_global_data->write (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 == handle_to_global_data->write (user_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation when queuing is enabled. */
/* 1. Read the qeuing header at the begining of the buffer. */
/* 2. Determine the amount of free space and where the next node can be placed.*/
/* 3. Set up message header from info in the queuing header. */
/* 4. Write the message header and message  at the tail of the queue. */
/* 5. Update the queuing header. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::queue_write_if_read_raw (void *user_data)
{
  CMS_HEADER
    current_header;
  long
    original_tail;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }



  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header at the beginning of the buffer. */
  if (-1 == handle_to_global_data->read (&queuing_header,
					 sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check if all the messages in the buffer have been read. */
  if (0 != queuing_header.queue_length)
    {
      return (status = CMS_WRITE_WAS_BLOCKED);
    }

  compute_raw_queue_free_space();

  if(max_queue_length > 0 && queuing_header.queue_length >= max_queue_length)
    {
      if(cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	}
      return (status = CMS_QUEUE_FULL);
    } 

  /* Check to see if there is enough free space. */
  if (free_space < ((long) (header.in_buffer_size)))
    {
      if (cms_print_queue_free_space || cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	  rcs_print_error
	    ("(continued) CMS: Message requires %ld bytes but only %ld bytes are left.\n",
	     header.in_buffer_size, free_space);
	}
      return (status = CMS_QUEUE_FULL);
    }

  if(queuing_header.tail > queuing_header.head 
     && handle_to_global_data->get_size() - queuing_header.tail <
     ((long) (header.in_buffer_size + sizeof(CMS_HEADER))))
    {
      if(queuing_header.head - sizeof(CMS_QUEUING_HEADER) - queuing_header_offset > 
	 header.in_buffer_size + sizeof(CMS_HEADER))
	{
	  queuing_header.end_queue_space = queuing_header.tail;
	  queuing_header.tail = sizeof(CMS_QUEUING_HEADER);
	}
      else
	{
	  if (cms_print_queue_free_space || cms_print_queue_full_messages)
	    {
	      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail;
	      long free_space_bottom = queuing_header.head - sizeof(CMS_QUEUING_HEADER) - sizeof(CMS_HEADER);
	      free_space = (free_space_top > free_space_bottom)?free_space_top:free_space_bottom;
	      rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	      rcs_print_error
		("(continued) CMS: Message requires %ld bytes but only %ld bytes are free and contiguous.\n",
		 header.in_buffer_size, free_space);
	      rcs_print_error ("queue free_space_top = %ld\n", free_space_top);
	      rcs_print_error ("queue free_space_bottom = %ld\n", free_space_bottom);
	    }
	  return (status = CMS_QUEUE_FULL);
	}
    }

  /* Store original tail so we'll know where to store the message. */
  original_tail = queuing_header.tail;

  /* Update the queuing header. */
  queuing_header.tail += header.in_buffer_size + sizeof (CMS_HEADER);
  queuing_header.queue_length++;
  queuing_header.write_id++;
  if (queuing_header.end_queue_space < queuing_header.tail)
    {
      queuing_header.end_queue_space = queuing_header.tail;
    }
  if (-1 == handle_to_global_data->write (&queuing_header,
					  sizeof (CMS_QUEUING_HEADER)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Setup message header. */
  header.write_id = queuing_header.write_id;
  header.was_read = 0;
  header.in_buffer_size = current_header.in_buffer_size;

  /* Write the message header. */
  handle_to_global_data->increment_offset( original_tail);
  if (-1 == handle_to_global_data->write (&header, sizeof (header)))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( sizeof (CMS_HEADER));
  if (-1 == handle_to_global_data->write (user_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation on a neutral buffer. */
/* 1. Read the header. */
/* 2. Update the header. */
/* 3. Write the message. */
CMS_STATUS CMS::write_if_read_encoded ()
{
  CMS_HEADER
    current_header;

  /* Produce error message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that handle to global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Check that buffer is large enough for this message. */
  if (header.in_buffer_size > encoded_data_size)
    {
      rcs_print_error
	("CMS:(%s) Message size %ld exceeds maximum for this buffer of %ld.\n",
	 BufferName, header.in_buffer_size, encoded_data_size);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Read the header. */
  if (-1 == handle_to_global_data->read (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  /* Decode the header and store in the header structure. */
  decode_header ();

  /* Check if the message in the buffer has been read. */
  if (!header.was_read)
    {
      return (status = CMS_WRITE_WAS_BLOCKED);
    }

  /* Update the header. */
  header.was_read = 0;
  header.write_id++;
  if (split_buffer && (header.write_id % 2) != toggle_bit)
    {
      header.write_id++;
    }
  header.in_buffer_size = current_header.in_buffer_size;
  encode_header ();
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 == handle_to_global_data->write (encoded_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}

/* It takes several steps to perform a write operation when queuing is enabled. */
/* 1. Read the qeuing header at the begining of the buffer. */
/* 2. Determine the amount of free space and where the next node can be placed.*/
/* 3. Set up message header from info in the queuing header. */
/* 4. Write the message header and message  at the tail of the queue. */
/* 5. Update the queuing header. */
/* Parameters: */
 /* user_data - pointer to where the user stored the message to be written. */
CMS_STATUS CMS::queue_write_if_read_encoded ()
{
  CMS_HEADER
    current_header;
  long
    original_tail;

  /* Produce warning message if process does not have permission to read. */
  if (!write_permission_flag)

    {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
    }

  /* Store the header information to use after reading the header in the buffer. */
  current_header = header;

  /* Check that the handle to the global memory object exists. */
  if (NULL == handle_to_global_data)
    {
      rcs_print_error ("CMS: handle_to_global_data is NULL.\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Store the original offset so we can update the queuing header later. */
  queuing_header_offset = handle_to_global_data->get_offset();

  /* Read the queuing header at the beginning of the buffer. */
  if (-1 == handle_to_global_data->read (encoded_queuing_header,
					 encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error reading from global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }
  /* Decode queuing header and store in queuing_header structure. */
  decode_queuing_header ();

  /* Check if all the messages in the buffer have been read. */
  if (0 != queuing_header.queue_length)
    {
      return (status = CMS_WRITE_WAS_BLOCKED);
    }

  compute_encoded_queue_free_space();

  if(max_queue_length > 0 && queuing_header.queue_length >= max_queue_length)
    {
      if(cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	}
      return (status = CMS_QUEUE_FULL);
    } 

  /* Check to see if there is enough free space. */
  if (free_space < header.in_buffer_size)
    {
      if (cms_print_queue_free_space || cms_print_queue_full_messages)
	{
	  rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	  rcs_print_error
	    ("(continued) CMS: Message requires %ld bytes but only %ld bytes are left.\n",
	     header.in_buffer_size, free_space);
	}
      return (status = CMS_QUEUE_FULL);
    }

  if(queuing_header.tail > queuing_header.head 
     && handle_to_global_data->get_size() - queuing_header.tail - queuing_header_offset <
     header.in_buffer_size + encoded_header_size)
    {
      if(queuing_header.head - encoded_queuing_header_size > 
	 header.in_buffer_size + encoded_header_size)
	{
	  queuing_header.end_queue_space = queuing_header.tail;
	  queuing_header.tail = encoded_queuing_header_size;
	}
      else
	{
	  if (cms_print_queue_free_space || cms_print_queue_full_messages)
	    {
	      long free_space_top = handle_to_global_data->get_size() - queuing_header.tail - encoded_header_size - queuing_header_offset;
	      long free_space_bottom = queuing_header.head - encoded_queuing_header_size - encoded_header_size;
	      free_space = (free_space_top > free_space_bottom)?free_space_top:free_space_bottom;
	      rcs_print_error ("CMS: %s message queue is full.\n", BufferName);
	      rcs_print_error
		("(continued) CMS: Message requires %ld bytes but only %ld bytes are free and contiguous.\n",
		 header.in_buffer_size, free_space);
	      rcs_print_error ("queue free_space_top = %ld\n", free_space_top);
	      rcs_print_error ("queue free_space_bottom = %ld\n", free_space_bottom);
	    }
	  return (status = CMS_QUEUE_FULL);
	}
    }

  /* Store original tail so we'll know where to store the message. */
  original_tail = queuing_header.tail;

  /* Update the queuing header. */
  queuing_header.tail += header.in_buffer_size + encoded_header_size;
  queuing_header.queue_length++;
  queuing_header.write_id++;
  if (queuing_header.end_queue_space < queuing_header.tail)
    {
      queuing_header.end_queue_space = queuing_header.tail;
    }
  encode_queuing_header ();
  if (-1 == handle_to_global_data->write (encoded_queuing_header,
					  encoded_queuing_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Setup message header. */
  header.write_id = queuing_header.write_id;
  header.was_read = 0;
  header.in_buffer_size = current_header.in_buffer_size;

  /* Re-encode the  header. */
  encode_header ();

  /* Write the message header. */
  handle_to_global_data->increment_offset( original_tail);
  if (-1 ==
      handle_to_global_data->write (encoded_header, encoded_header_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  /* Write the message. */
  handle_to_global_data->increment_offset( encoded_header_size);
  if (-1 == handle_to_global_data->write (encoded_data,
					  (long) header.in_buffer_size))
    {
      rcs_print_error ("CMS:(%s) Error writing to global memory at %s:%d\n",
		       BufferName, __FILE__, __LINE__);
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  return (status = CMS_WRITE_OK);
}
