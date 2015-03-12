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


#ifndef RCS_WM_HH
#define RCS_WM_HH

class CMS;
#include "nml.hh"		/* class NML, typedef NML_FORMAT_PTR,NMLTYPE */
#include "nmlmsg.hh"		/* class NMLmsg */

class RCS_WM_MSG:public NMLmsg
{
public:
  RCS_WM_MSG (NMLTYPE t, long sz);
};

extern int RCS_WM_MSG_format (NMLTYPE, void *, CMS *);

class RCS_WM_CHANNEL:public NML
{
public:
  RCS_WM_CHANNEL (NML_FORMAT_PTR, char *, char *, char *,
		  int set_to_server = 0);
   ~RCS_WM_CHANNEL ();
  RCS_WM_MSG *get_address ()
  {
    return ((RCS_WM_MSG *) NML::get_address ());
  };
};

#define RCS_GENERIC_WM_TYPE     ((NMLTYPE) 3000000)

class RCS_GENERIC_WM:public RCS_WM_MSG
{
public:
  RCS_GENERIC_WM ();
  void update (CMS *);
};

#endif
