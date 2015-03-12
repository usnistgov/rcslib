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

/*********************************************************************
*  File: nml_oi.hh
*
*  Defines Generic NML Message structures used to log errors and interact
*  with an Operator Interface from within an NML_MODULE.
*
**********************************************************************/

/*
   MODIFICATIONS:

   29-May-1997 WPS took definitions from nml_emc.hh
*/

#ifndef NML_OI_HH
#define NML_OI_HH

class CMS;

#include "nmlmsg.hh"		// class NMLmsg

// NML operator interface stuff for errors, text, and graphics display

#define NML_ERROR_TYPE    ((NMLTYPE) 1)
#define NML_TEXT_TYPE     ((NMLTYPE) 2)
#define NML_DISPLAY_TYPE  ((NMLTYPE) 3)
#define NML_OPERATOR_REQUEST_TYPE ((NMLTYPE) 4)
#define NML_OPERATOR_REPLY_TYPE ((NMLTYPE) 5)

// Sizes for strings for the above messages

#define NML_ERROR_LEN 256
#define NML_TEXT_LEN 256
#define NML_DISPLAY_LEN 256
#define NML_OPERATOR_REQUEST_LEN 256
#define NML_OPERATOR_REPLY_LEN 256

class  NML_ERROR:public NMLmsg
{
public:
  NML_ERROR ():NMLmsg (NML_ERROR_TYPE, sizeof (NML_ERROR))
  {
  };
  ~NML_ERROR ()
  {
  };

  void update (CMS * cms);
  char error[NML_ERROR_LEN];
};

class  NML_TEXT:public NMLmsg
{
public:
  NML_TEXT ():NMLmsg (NML_TEXT_TYPE, sizeof (NML_TEXT))
  {
  };
  ~NML_TEXT ()
  {
  };

  void update (CMS * cms);
  char text[NML_TEXT_LEN];
};

class  NML_DISPLAY:public NMLmsg
{
public:
  NML_DISPLAY ():NMLmsg (NML_DISPLAY_TYPE, sizeof (NML_DISPLAY))
  {
  };
  ~NML_DISPLAY ()
  {
  };

  void update (CMS * cms);
  char display[NML_DISPLAY_LEN];
};

class  NML_OPERATOR_REQUEST:public NMLmsg
{
public:
  NML_OPERATOR_REQUEST ():
    NMLmsg (NML_OPERATOR_REQUEST_TYPE,
	    sizeof (NML_OPERATOR_REQUEST)),
    key(0)
  {
  };
  ~NML_OPERATOR_REQUEST ()
  {
  };

  void update (CMS * cms);
  int key;
  char request[NML_OPERATOR_REQUEST_LEN];
};

class  NML_OPERATOR_REPLY:public NMLmsg
{
public:
  NML_OPERATOR_REPLY ():
    NMLmsg (NML_OPERATOR_REPLY_TYPE,
	    sizeof (NML_OPERATOR_REPLY)),
    key(0)
  {
  };
  ~NML_OPERATOR_REPLY ()
  {
  };

  void update (CMS * cms);
  int key;
  char reply[NML_OPERATOR_REPLY_LEN];
};

// NML format function
extern int  nmlErrorFormat (NMLTYPE type, void *buffer, CMS * cms);

// NML_OI_HH
#endif
