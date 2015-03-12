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

/**********************************************************************
File: oedsto.hh
Purpose: The class OEDSTO should be used for the OE / NML demo.         
**********************************************************************/

#ifndef OEDSTO_HH
#define OEDSTO_HH

#include "oemem.hh"             // class OEMEM
#include "oe_event_flag.hh"     // class OE::Event_Flag

class OEDSTO : public OEMEM
{
public:
  OEDSTO  (const char *bufline, const char *procline, int set_to_server, int set_to_master);
  virtual ~ OEDSTO ();
 
  // Override CMS defaults
  CMS_STATUS clear ();
  int get_queue_length ();
  int check_if_read ();
  CMS_STATUS read ();
  CMS_STATUS blocking_read (double _blocking_timeout);
  CMS_STATUS write (void *user_data);
  CMS_STATUS write_if_read (void *user_data);
  CMS_STATUS peek ();
  int get_space_available ();

private:
  // The event flag to hold state information about the message data
  OE::Event_Flag *oe_event_flag;
};

#endif
