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


#ifndef CMS_CLNT_INFO_HH
#define CMS_CLNT_INFO_HH
 
#include "rem_msg.hh" 		// REMOTE_CLIENT_ID

class CMS_CLIENT_INFO;
class CMS;

class CMS_MSG_INFO
{
  CMS_MSG_INFO ();
  ~CMS_MSG_INFO ();

public:
  friend class CMS_CLIENT_INFO;

  void *addr;
  size_t size;
  int deleteaddr;

private:
  // Prevent copying
  CMS_MSG_INFO(const CMS_MSG_INFO &);
  CMS_MSG_INFO &operator=(const CMS_MSG_INFO &);
};

class CMS_CLIENT_INFO
{
public:
  CMS_CLIENT_INFO();
  ~CMS_CLIENT_INFO();
  void release(void);

  REMOTE_CLIENT_ID id;
  int buffer_num;
  int mrpq_reader_id;
  CMS_MSG_INFO last_message_info;
  REMOTE_READ_REPLY *temp_read_reply;
  size_t read_reply_data_size;
  class CMS *cms;
  RCS_LINKED_LIST *nmlcopies;

private:
  // Prevent copying
  CMS_CLIENT_INFO(const CMS_CLIENT_INFO &);
  CMS_CLIENT_INFO &operator=(const CMS_CLIENT_INFO &);
};

#endif
// CMS_CLNT_INFO_HH

