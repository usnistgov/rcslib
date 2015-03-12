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



#ifndef NMLQR_HH
#define NMLQR_HH

#include "nml.hh"
#include "nmlmsg.hh"

class NML_QUERY_MSG:public NMLmsg
{
public:
  NML_QUERY_MSG (NMLTYPE t, long s):
    NMLmsg (t, s),
    subdiv_for_reply(0)
  {
  };

  void update (CMS *);
  int subdiv_for_reply;
};

class NML_QUERY_CHANNEL;
class NML_ID_CHANNEL;
class NML;

#define NML_NO_TIMEOUT (-1.0)

class NML_QR_SERVER
{
public:
  NML_QR_SERVER (NML_FORMAT_PTR f_ptr, char *qr_name, char *process_name,
		 char *config_file);
  ~NML_QR_SERVER ();

  NMLTYPE readQuery ();
  NMLTYPE waitForQuery (double timeout);
  class NML_QUERY_MSG *getQueryAddress ();
  int replyToLastQuery (NMLmsg * message_to_send);
  int valid ();
  int reset ();
  void interrupt_operation();
  void clear_interrupt_operation();

protected:
  class NML * replyChannel;
  class NML *queryChannel;
  class NML_ID_CHANNEL *idChannel;
  int reply_subdiv;
  char *subdiv_allocation_table;

private:
  NML_QR_SERVER(const NML_QR_SERVER &);
  NML_QR_SERVER &operator=(const NML_QR_SERVER &);
};

class NML_QR_CLIENT
{
public:
  NML_QR_CLIENT (NML_FORMAT_PTR f_ptr, char *qr_name, char *process_name,
		 char *config_file);
   ~NML_QR_CLIENT ();

  int sendQuery (NML_QUERY_MSG *);
  NMLTYPE readReply ();
  NMLTYPE waitForReply (double timeout);
  class NMLmsg *getReplyAddress ();
  int valid ();
  int reset ();
  void interrupt_operation();
  void clear_interrupt_operation();

protected:
  class NML * replyChannel;
  class NML_QUERY_CHANNEL *queryChannel;
  class NML_ID_CHANNEL *idChannel;
  int reply_subdiv;

private:
  NML_QR_CLIENT(const NML_QR_CLIENT &);
  NML_QR_CLIENT &operator=(const NML_QR_CLIENT &);
};

//NMLQR_HH
#endif



