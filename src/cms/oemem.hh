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
File: oemem.hh
Purpose: The class OEMEM should be used by procedures for the OE / NML   *
*  compatibility demo.                                                   *
**********************************************************************/

#ifndef OEMEM_HH
#define OEMEM_HH



#include "cms.hh"		// class CMS
#include "oe.hh"                // OE class

class OEMEM:public CMS
{
public:
  OEMEM (const char *bufline, const char *procline, int set_to_server, int set_to_master);
  virtual ~ OEMEM ();

  // Overloaded CMS function
  int get_msg_count ();

protected:
  // The OE Message-based object to be used
  OE::Base_Object *oe_msg_obj;

  // The local message used to contain the (potentially) encoded data
  OE::Message *encoded_oe_message;

  // One initialization performed for all OE access within this process
  static int initialized;

  // Constants used to manage event flag
  static const OE::Event_Mask_Type DATA_WRITTEN_MASK;

  // Used to determine when last OE object destructed, so OE::Finalize
  // can be called
  static int object_count;
};

#endif
