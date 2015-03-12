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

/***************************************************************
* File: nml_oi.cc
***************************************************************/

/*
   MODIFICATIONS:

   29-May-1997 WPS took functions from nml_emc.cc
*/

#include "nml_oi.hh"
#include "cms.hh"		// class CMS

// NML operator interface messages for errors, text, and display

void
NML_ERROR::update (CMS * cms)
{
  cms->update (error, NML_ERROR_LEN);
}

void
NML_TEXT::update (CMS * cms)
{
  cms->update (text, NML_TEXT_LEN);
}

void
NML_DISPLAY::update (CMS * cms)
{
  cms->update (display, NML_DISPLAY_LEN);
}

void
NML_OPERATOR_REQUEST::update (CMS * cms)
{
  cms->update (key);
  cms->update (request, NML_OPERATOR_REQUEST_LEN);
}

void
NML_OPERATOR_REPLY::update (CMS * cms)
{
  cms->update (key);
  cms->update (reply, NML_OPERATOR_REPLY_LEN);
}

int
nmlErrorFormat (NMLTYPE type, void *buffer, CMS * cms)
{
  switch (type)
    {
    case NML_ERROR_TYPE:
      ((NML_ERROR *) buffer)->update (cms);
      break;

    case NML_TEXT_TYPE:
      ((NML_TEXT *) buffer)->update (cms);
      break;

    case NML_DISPLAY_TYPE:
      ((NML_DISPLAY *) buffer)->update (cms);
      break;

    case NML_OPERATOR_REQUEST_TYPE:
      ((NML_OPERATOR_REQUEST *) buffer)->update (cms);
      break;

    case NML_OPERATOR_REPLY_TYPE:
      ((NML_OPERATOR_REPLY *) buffer)->update (cms);
      break;

      // unknown type
    default:
      return 0;			// 0 signifies didn't find
    }

  return 1;			// 1 signifies found it
}
