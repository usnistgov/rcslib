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

/**************************************************************************
 * File: bbdmem.hh
 *
 * Purpose: Header file for the BBDMEM class which provides the link
 * between legacy bbd applications and CMS.
 ************************************************************************/


#ifndef BBDMEM_HH
#define BBDMEM_HH

#include "cms.hh"		// class CMS


#ifndef BBD_TYPEDEFED
#define BBD_TYPEDEFED
typedef void *BBD;
#endif


class BBDMEM:public CMS
{
public:
  BBDMEM (const char *bufline, const char *procline, int set_to_server =
	  0, int set_to_master = 0);
   ~BBDMEM ();

  /* Overloaded CMS functions. */
  CMS_STATUS read ();
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
  CMS_STATUS write_if_read (void *data);
  BBD reader_bbd_id;
  BBD writer_bbd_id;
  void *bbd_data;
  int bbd_timeout;
  long bbd_size;
  int id;
};


#endif
  // BBDMEM_HH
