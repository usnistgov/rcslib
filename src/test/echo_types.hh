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

#ifndef ECHO_TYPES_HH
#define ECHO_TYPES_HH

#include "rcs.hh"

#define ECHO_QUERY_TYPE 101
#define ECHO_REPLY_TYPE 102

class ECHO_QUERY: public NML_QUERY_MSG
{
public:
  ECHO_QUERY();
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,line,80);
};

class ECHO_REPLY: public NMLmsg
{
public:
  ECHO_REPLY();
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,line,80);
  int reply_num;
  int client_num;
};

extern int ECHO_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
