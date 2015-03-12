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
File: oedsto.cc
**********************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_OE_INTF)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"		// EXTERN_C_STD_HEADERS
#endif

#include "oedsto.hh"		// class OEDSTO
#include "rcs_prnt.hh"		// rcs_print_error()
#include "oe.hh"                // OE general types and dist object base type
#include "oe_data_store.hh"     // class OE::Data_Store 
#include "oe_event_flag_attributes.hh" // OE::Event_Flag_Attributes
#include "oe_event_flag.hh"     // class OE::Event_Flag
#include "stdio.h"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy(), strcmp()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

OEDSTO::OEDSTO (const char *bufline, 
		const char *procline, 
		int set_to_server, 
		int set_to_master):
   OEMEM (bufline, procline, set_to_server, set_to_master) 
{
   OE::Status_Type oe_status;

   // No errors encountered while constructing parent, so continue
   if (status == CMS_STATUS_NOT_SET)
   {
      // Could do some NML configuration file analysis to determine the 
      // requirements for distribution type (e.g. see if all LOCAL access,
      // and then use Distribution of None), but assume Client-Server for 
      // demo
      const OE::Data_Store_Attributes attr 
         (BufferName,
          "Client_Server",
          "",
          (OE::Count_Type)size,
          &oe_status);
          
      if (oe_status.Status_Value != 0)
      {
         rcs_print_error 
              ("Can't create OE DS attributes for %s due to OE error %d.\n", 
               BufferName, oe_status.Status_Value);
      }
      else
      {
         oe_msg_obj = new OE::Data_Store (attr, &oe_status);
         if (oe_status.Status_Value != 0)
         {
            rcs_print_error 
              ("Can't create OE DS object for %s due to OE error %d.\n", 
               BufferName, oe_status.Status_Value);
         }
         else
         {
            char EventFlagName [50];
            strcpy (EventFlagName, BufferName);
            strcat (EventFlagName, "-EventFlag");
            const OE::Event_Flag_Attributes attr 
               ((char *)EventFlagName,
                "Client_Server",
                "",
                (OE::Event_Mask_Type)0,
                &oe_status);
                
            if (oe_status.Status_Value != 0)
            {
               rcs_print_error 
                  ("Can't create EF attributes for %s because status = %d.\n", 
                   BufferName, oe_status.Status_Value);
            }
            else
            {
               oe_event_flag = new OE::Event_Flag(attr, &oe_status);
               if (oe_status.Status_Value != 0)
               {
                  rcs_print_error 
                    ("Can't create EF object for %s because status = %d.\n", 
                     BufferName, oe_status.Status_Value);
               }
            }
         }
      }
   }
   else
   {
      rcs_print_error ("Can't create DS attributes for %s due to CMS error %d.", BufferName, status);
   }
}

CMS_STATUS OEDSTO::clear ()
{
   OE::Status_Type oe_status;
   const OE::Message Zero_Length_Message(0, &oe_status);

   // Write a zero length msg to DS
   ((OE::Data_Store *)oe_msg_obj)->Write
      (Zero_Length_Message, OE::Wait_Forever, &oe_status);

   if (oe_status.Status_Value != 0) 
      return CMS_SERVER_SIDE_ERROR;
   else
      return CMS_CLEAR_OK;
}

int
OEDSTO::get_queue_length ()
{
   return 1;
}

int
OEDSTO::check_if_read ()
{
   OE::Status_Type oe_status;
   OE::Event_Mask_Type mask;

   oe_event_flag->Peek_Mask(OE::Wait_Forever, &mask, &oe_status);
   
   // Returns 1 if it has not been written to since previously read
   if (mask == (OE::Event_Mask_Type)0)
      return 1;
   else
      // Returns zero if it has fresh data that has not been read
      return 0;
}

CMS_STATUS OEDSTO::read ()
{
   OE::Status_Type     oe_status;

   if (check_if_read ())
   {
      return (status = CMS_READ_OLD);
   }
   else
   {
      ((OE::Data_Store *)oe_msg_obj)->Read 
          (OE::Do_Not_Wait, encoded_oe_message, &oe_status);
   
      // If succeeded, return status accordingly (the OE implementation does 
      // not alter the data address contents unless it is successful)
      if (oe_status.Status_Value == 0)
      {
         oe_event_flag->Clear
            (OEDSTO::DATA_WRITTEN_MASK, OE::Wait_Forever, &oe_status);
         return (status = CMS_READ_OK);
      }
      else
      {
         rcs_print_error ("CMS: %s Unknown Problem Reading OE DS %s\n",
                          ProcessName, BufferName);
         fatal_error_occurred = 1;
         return (status = CMS_MISC_ERROR);
      }
   }
}

CMS_STATUS OEDSTO::blocking_read (double _blocking_timeout)
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

   // Produce error message if process does not have permission to read. 
   if (!read_permission_flag)
   {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
		       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
   }

   OE::Status_Type     oe_status;
   OE::Event_Mask_Type current_value;

   // Block until new data has been written
   oe_event_flag->Wait
      (OEDSTO::DATA_WRITTEN_MASK,       /* Wait Mask  */
       (OE::Event_Mask_Type)0,          /* Reset Mask -- Don't reset any*/
       OE::Event_Flag::Wait_For_All, 
       (OE::Time_Interval_Type)blocking_timeout, 
       &current_value,                 /* Unused Current Value of Flag */
       &oe_status);
   
   if (oe_status.Status_Value == OE::Failed_Timeout) 
   {
      rcs_print_error 
         ("CMS: Process %s Timeout Waiting for new data OE DS %s, time was %f\n",
          ProcessName, BufferName, blocking_timeout);
      fatal_error_occurred = 1;
      return (status = CMS_TIMED_OUT);
   }
   else
   {
      ((OE::Data_Store *)oe_msg_obj)->Read 
          (OE::Do_Not_Wait, encoded_oe_message, &oe_status);
   
      // If succeeded, return status accordingly (the OE implementation does 
      // not alter the data address contents unless it is successful)
      if (oe_status.Status_Value == 0)
      {
         oe_event_flag->Clear
            (OEDSTO::DATA_WRITTEN_MASK, OE::Wait_Forever, &oe_status);
         return (status = CMS_READ_OK);
      }
      else
      {
         rcs_print_error ("CMS: %s Unknown Problem Reading OE DS %s\n",
                          ProcessName, BufferName);
         fatal_error_occurred = 1;
         return (status = CMS_MISC_ERROR);
      }
   }
}

CMS_STATUS OEDSTO::write (void *user_data)
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

   if (NULL != user_data)
   {
      OE::Status_Type oe_status;
      // Create a message using the provided user data
      OE::Message oe_message
         ((OE::Count_Type)header.in_buffer_size, (OE::Data_Address)user_data, &oe_status);
      oe_message.Set_Data_Size ((OE::Count_Type)header.in_buffer_size);
   
      ((OE::Data_Store *)oe_msg_obj)->Write
         (oe_message, 
          OE::Wait_Forever,
          &oe_status);
   
      // If succeeded, then set event flag and return status accordingly 
      if (oe_status.Status_Value == 0)
      {
         oe_event_flag->Set 
            (OEDSTO::DATA_WRITTEN_MASK, OE::Wait_Forever, &oe_status);
         return (status = CMS_WRITE_OK);
      }
      else
      {
         rcs_print_error ("CMS: %s Unknown Problem Writing OE MQ for %s\n",
                          ProcessName, BufferName);
         fatal_error_occurred = 1;
         return (status = CMS_MISC_ERROR);
      }
   }
   else
   {
      rcs_print_error ("CMS: %s user data not allocated for OE DS %s\n",
                       ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_MISC_ERROR);
   }
}

CMS_STATUS OEDSTO::write_if_read (void *user_data)
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
      write(user_data);
   }
   return(status=CMS_WRITE_OK);
}

CMS_STATUS OEDSTO::peek ()
{
   /* Produce error message if process does not have permission to read. */
   if (!read_permission_flag)
   {
      rcs_print_error ("CMS: %s was not configured to read %s\n",
                       ProcessName, BufferName);
      return (status = CMS_PERMISSIONS_ERROR);
   }
   else if (NULL == data)
   {
      rcs_print_error ("CMS: %s user data not allocated for OE DS %s\n",
                       ProcessName, BufferName);
      fatal_error_occurred = 1;
      return (status = CMS_MISC_ERROR);
   }
   else
   {
      // Allocate the local message with the encoded data address 
      OE::Status_Type oe_status;
      OE::Message oe_message
         ((OE::Count_Type)max_encoded_message_size, (OE::Data_Address)encoded_data, &oe_status);

      ((OE::Data_Store *)oe_msg_obj)->Read 
         (OE::Do_Not_Wait, &oe_message, &oe_status);

      // If succeeded, return status accordingly (the OE implementation does 
      // not alter the data address contents unless it is successful)
      if (oe_status.Status_Value == 0)
      {
         return (status = CMS_READ_OK);
      }
      else
      {
         rcs_print_error ("CMS: %s Unknown Problem Reading OE DS for %s\n",
                          ProcessName, BufferName);
         fatal_error_occurred = 1;
         return (status = CMS_MISC_ERROR);
      }
   }

}

int
OEDSTO::get_space_available ()
{
   // Since it's non-queued, the entire message store is available.  Couldn't
   // tell from NML users guide if that is expected behavior or not.
   return (int)size;
}

OEDSTO::~OEDSTO () 
{
   OE::Status_Type oe_status;

   ((OE::Data_Store*)oe_msg_obj)->Destroy (&oe_status);

   oe_event_flag->Destroy (&oe_status);
   delete oe_event_flag;
}


//  defined(ENABLE_RCS_OE_INTF)

#else
#include "rcs_empty_source"
#endif
