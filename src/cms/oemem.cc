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
File: oemem.cc
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

#include "oemem.hh"		       // class OEMEM
#include "cms.hh"		       // class CMS
#include "rcs_prnt.hh"		       // rcs_print_error()
#include "oe.hh"                       // OE types and dist object base type
#include "oe_message.hh"               // OE::Message
#include "oe_naming_service.hh"        // OE::Naming_Service
#include "oe_services.hh"              // OE::Services

#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */

#if HAVE_TOE_SERVICES_HH
#include "toe_services.hh"             // TOE_Services
#endif

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy(), strcmp()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

extern "C" {
  extern void adainit(void);
  extern void adafinal(void);
}

const OE::Event_Mask_Type OEMEM::DATA_WRITTEN_MASK = 1; // 2#0000 0000 0000 0001#
int OEMEM::initialized = 0;
int OEMEM::object_count = 0;

OEMEM::OEMEM (const char *bufline, const char *procline, int set_to_server, int set_to_master):
   CMS (bufline, procline, set_to_server) 
{
   OE::Status_Type oe_status;

   object_count++;

   // If OE has not yet been initialized, then do so now
   if (initialized == 0)
   {

      // NML buffers can be specified in many files for a single process, 
      // however, the OE for a single process can only read a single file.
      // So there isn't a way to make an assumption about the config file
      // path from known data in the application (i.e. NML file location)
      char *config_file_path = "./config.dat";

#if HAVE_ADAINIT
      adainit();
#endif
#if HAVE_TOE_SERVICES_HH
      rcs_print_debug(PRINT_MISC,"TOE_Services::Set_Local_Host (%s)\n",ProcessName);
      TOE_Services::Set_Local_Host ((char *)&ProcessName);
#endif
      OE::Services::Initialize (config_file_path, &oe_status);
      initialized = 1;
   }

   // Determine if this is the master process from passed in paramaters.  If set
   // to 1 or -1, then it overrides the value that was set from the config file.
   if (set_to_master == 1)
      is_local_master = 1;
   else if (set_to_master == -1)
      is_local_master = 0;

   // Search for a matching object name if not designated as master
   if ((!is_local_master) &&
      (OE::Naming_Service::Object_Exists (BufferName) == OE::False))
   {
      rcs_print_error 
         ("OEMEM - Object can't find master for %s.\n", BufferName);
      status = CMS_NO_MASTER_ERROR;
   }
   else
   {
      status = CMS_STATUS_NOT_SET;
   }
   // If the encoded data hasn't been created, then allocated a
   // a message along with a buffer
   if (NULL == encoded_data)
   {
      encoded_data = DEBUG_MALLOC (size);
   }
   encoded_oe_message = new OE::Message
                            ((OE::Count_Type)size,
                            (OE::Data_Address)encoded_data,
                            &oe_status);
   encoded_oe_message->Set_Data_Size ((OE::Count_Type)size);

   return;
}

int 
OEMEM::get_msg_count ()
{
   // Omit since this is just for debug, and application could set up
   // an event to capture this information.  But it should be such that
   // it is disabled when not needed, as there will be an impact on 
   // performance
   rcs_print_error ("CMS: get_msg_count not support for OE Demo\n");
   fatal_error_occurred = 1;
   status = CMS_NO_IMPLEMENTATION_ERROR;
   return 0;
}

OEMEM::~OEMEM () 
{
   OE::Status_Type oe_status;

   object_count--;

   if(encoded_oe_message)
     {
       encoded_oe_message->Delete (&oe_status);
       delete encoded_oe_message;
       encoded_oe_message=0;
     }
   
   if(oe_msg_obj)
     {
       delete oe_msg_obj;
       oe_msg_obj=0;
     }

   // if last OE object, finalize the OE
   if (object_count == 0)
   {
      initialized = 0;
#if HAVE_ADAFINAL
      //adafinal();
#endif
   }
}


//  defined(ENABLE_RCS_OE_INTF)

#else
#include "rcs_empty_source"
#endif
