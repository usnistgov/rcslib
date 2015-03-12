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


#ifndef TTYMEM_HH
#define TTYMEM_HH

#include "ttyintf.h"
#include "cms.hh"


class TTYMEM:public CMS
{
public:

public:
  TTYMEM (const char *bufline, 
	  const char *procline);
    virtual ~ TTYMEM ();

  /* Overloaded CMS functions. */
  void verify_bufname ();
  CMS_STATUS clear ();
  int check_if_read ();
  CMS_STATUS read ();
  CMS_STATUS blocking_read (double);
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
  CMS_STATUS write_if_read (void *data);
  void reconnect ();
  void disconnect ();

protected:
  RCS_SERIAL_PORT_HANDLE handle;
  char ttyDevName[80];
  rcs_serial_port_setting settings;
  char temp_buffer[0x2000];
  int serial_number;
  int returned_serial_number;
  int message_size;
  int id;
};

#endif
