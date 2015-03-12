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
File: oemque.cc
**********************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_OE_INTF)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"		       // EXTERN_C_STD_HEADERS
#endif

#include "oemque.hh"		// class OEMQUE
#include "rcs_prnt.hh"		// rcs_print_error()
#include "oe.hh"                // OE general types and dist object base type
#include "oe_message_queue.hh"  // class OE::Message_Queue 

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy(), strcmp()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

OEMQUE::OEMQUE (const char *bufline, 
		const char *procline, 
		int set_to_server, int set_to_master):
   OEMEM (bufline, procline, set_to_server, set_to_master) 
{
   // No errors encountered while constructing parent, so continue
   if (status == CMS_STATUS_NOT_SET)
   {
      // For Message Queue "Max_Message" attribute, just choose a
      // sufficiently large number.  Eventually get this attribute
      // from a helper config file
      const OE::Count_Type max_messages = 1000;
   
      // Could do some NML configuration file analysis to determine the 
      // requirements for distribution type (e.g. see if all LOCAL access,
      // and then use Distribution of None), but assume Client-Server for 
      // demo
      OE::Status_Type oe_status;
      const OE::Message_Queue_Attributes attr 
         (BufferName, 
          "Client_Server",
          "",
          max_messages,
          (OE::Count_Type)size,
          &oe_status);
          
      if (oe_status.Status_Value != 0)
      {
         rcs_print_error 
           ("Can't create MQ attributes for %s, OE error code %d/%d.", 
            BufferName, oe_status.Status_Value, 
            oe_status.Error_Diagnosis_Value);
      }
      else
      {
         oe_msg_obj = new OE::Message_Queue (attr, &oe_status);
         if (oe_status.Status_Value != 0)
         {
            rcs_print_error ("Can't create OE DS object for %s.", BufferName);
         }
      }
   }
}

CMS_STATUS OEMQUE::clear ()
{
   OE::Status_Type oe_status;

   // Flush the MQ.  
   if (oe_msg_obj)
   {
      ((OE::Message_Queue *) oe_msg_obj)->Flush(OE::Wait_Forever, &oe_status);

      if (oe_status.Status_Value != 0) 
         return CMS_SERVER_SIDE_ERROR;
      else
         return CMS_CLEAR_OK;
   }
   else
      return CMS_CREATE_ERROR;
}

int 
OEMQUE::get_queue_length () 
{
   if(oe_msg_obj)
      return (int)((OE::Message_Queue*)oe_msg_obj)->Get_Current_Message_Count();
   else
      return 0;

}

int
OEMQUE::check_if_read ()
{
   // Check for empty queue, if so return 1
   if (get_queue_length() == 0)
      return 1;
   else
      // Returns zero if it still has data that has not been read
      return 0;
}

CMS_STATUS OEMQUE::read ()
{
   return blocking_read ((double)OE::Do_Not_Wait);
}

CMS_STATUS OEMQUE::blocking_read (double _blocking_timeout)
{
   // Negative value denotes indefinite block
   if (_blocking_timeout < 0)
   {
      blocking_timeout = (double)OE::Wait_Forever;
   }
   else
   {
      blocking_timeout = _blocking_timeout;
   }

/* Produce error message if process does not have permission to read. */
   if (!read_permission_flag)
   {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
   }

   OE::Status_Type oe_status;

   if (NULL != data)
   {
      if (oe_msg_obj)
      {
         ((OE::Message_Queue*)oe_msg_obj)->Get
            (encoded_oe_message, 
             OE::Message_Queue::By_Value, 
             (OE::Time_Interval_Type)blocking_timeout,
             &oe_status);
   
         // If succeeded, return status accordingly (the OE implementation does
         // not alter the data address contents unless it is successful)
         if (oe_status.Status_Value == 0)
         {
            status = CMS_READ_OK;
         }
         // If a message is not available, then return CMS_READ_OLD
         else if ((oe_status.Status_Value == 2) &&
                  (oe_status.Error_Diagnosis_Value == 1))
         {
            status = CMS_READ_OLD;
         }
         else if (oe_status.Status_Value == OE::Failed_Timeout)
         {
            rcs_print_error ("CMS: %s Timeout Reading OE MQ for %s\n",
   		          ProcessName, BufferName);
            fatal_error_occurred = 1;
            status = CMS_TIMED_OUT;
         }
         else
         {
            rcs_print_error ("CMS: %s Unknown Problem Reading OE MQ for %s\n",
   		          ProcessName, BufferName);
            fatal_error_occurred = 1;
            status = CMS_MISC_ERROR;
         }
         return (status);
      }
   }
   else
   {
      rcs_print_error ("CMS: %s CMS object is Null %s\n",
                       ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_MISC_ERROR);
   }
   return(status=CMS_READ_OK);
}

CMS_STATUS OEMQUE::write (void *user_data)
{

   if (!write_permission_flag)
   {
      rcs_print_error ("CMS: %s was not configured to write to %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
   }

   if (!force_raw)
   {
      user_data = encoded_data;
   }

   if (NULL != data)
   {
      OE::Status_Type oe_status;

      // Create a message to send "by value".  
      OE::Message oe_message
         ((OE::Count_Type)header.in_buffer_size, (OE::Data_Address)user_data, &oe_status);
   
      ((OE::Message_Queue*)oe_msg_obj)->Insert(oe_message,
                                               OE::Message_Queue::By_Value,
                                               OE::Wait_Forever,
                                               &oe_status);

      // If succeeded, return status accordingly 
      if (oe_status.Status_Value == 0)
      {
         status = CMS_WRITE_OK;
      }
      else
      {
         rcs_print_error ("CMS: %s Unknown Problem Writing OE MQ for %s\n",
   		       ProcessName, BufferName);
         fatal_error_occurred = 1;
         status = CMS_MISC_ERROR;
      }
      // Eliminate the container (maintains the buffer since it is external)
      oe_message.Delete (&oe_status);
      return (status);
   }
   else
   {
      rcs_print_error ("CMS: %s CMS object is Null %s\n",
                       ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_MISC_ERROR);
   }
   return(status=CMS_WRITE_OK);
}

CMS_STATUS OEMQUE::write_if_read (void *user_data)
{
   if (check_if_read() == 0)
   {
      rcs_print_error 
         ("CMS: %s Attempt to Write OE MQ for %s when hasn't been read\n",
          ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_MISC_ERROR);
   }
   else
   {
      return write(user_data);
   }
   return(status);
}

CMS_STATUS OEMQUE::peek ()
{
   // Produce error message if process does not have permission to read. 
   if (!read_permission_flag)
   {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
   }
   else
   {
      rcs_print_error ("CMS: %s Peek not available for OE queued data for %s\n",
                       ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_NO_IMPLEMENTATION_ERROR);
   }
}

int 
OEMQUE::get_space_available ()
{
   // Since OE MQ's are setup for number of messages in queue, this would
   // only be the product of remaining message slots multiplied by
   // max message size.  Since this really doesn't tell the user what is
   // needed, then it's not worth implementing that way.
   rcs_print_error ("CMS: get_space_available not available for OE data ");
   return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

OEMQUE::~OEMQUE () 
{
   OE::Status_Type oe_status;

   ((OE::Message_Queue*)oe_msg_obj)->Destroy (&oe_status);
}

//  defined(ENABLE_RCS_OE_INTF)
#else
#include "rcs_empty_source"
#endif
