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

#if ( !defined(NO_RTL) || defined(rtai) || defined(linux_rtai) ) && defined(linux)


#ifndef RTLMEM_HH
#define RTLMEM_HH

#include "cms.hh"
#include "rtlnml.h"

class RTLMEM:public CMS
{
public:
  RTLMEM (const char *bufline,const  char *procline, int set_to_server = 0,
	  int set_to_master = 0);
    virtual ~ RTLMEM ();

  /* Overloaded CMS functions. */
  CMS_STATUS read ();
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
protected:
    rtlnml_t r;
  int key;
};

#endif

#endif
