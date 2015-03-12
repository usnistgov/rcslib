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



#ifndef UDPMEM_HH
#define UDPMEM_HH

#include "cms.hh"		/* class CMS */

class UDPMEM_NONPORT_INTERNALS;

class UDPMEM:public CMS
{
public:
  UDPMEM (const char *bufline, 
	  const char *procline);
 
  virtual ~ UDPMEM ();
  
  /* Overloaded CMS functions. */
  CMS_STATUS clear ();
  int check_if_read ();
  int get_msg_count ();
  int get_queue_length ();
  int get_space_available ();
  CMS_STATUS read ();
  CMS_STATUS blocking_read (double);
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
  CMS_STATUS write_if_read (void *data);
  CMS_STATUS setup_subscription(double _subscription_period);
  CMS_STATUS cancel_subscription ();
  long get_msg_type();

protected:
  int call_on_server ();
  int verify_bufname ();
  int init_variables ();
  int polling;
  int get_reply;
  int last_reply_timed_out;
  long reply_size;
  long request_size;
  double retry_timeout;
  unsigned long serial_number;
  unsigned long returned_serial_number;
  int socket_fd;
  char request_buffer[32];
  char reply_buffer[32];
  int send_broadcast;
  long send_broadcast_addr;
  int recieve_broadcast;
  long recieve_broadcast_port;
  int subscription_type;
  int poll_interval_millis;
  int subscription_id;
  int send_request;
  int broadcast_subscriptions;
  int broadcast_clnt_port;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];

  class UDPMEM_NONPORT_INTERNALS *internals;
  void throwaway_extra_data_on_socket (int throwaway_socket_fd);

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  UDPMEM(const UDPMEM &);
  UDPMEM &operator=(const UDPMEM &);

};




#endif
