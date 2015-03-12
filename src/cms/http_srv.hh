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


#ifndef HTTP_SRV_HH
#define HTTP_SRV_HH

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */
#include "sokintrf.h"		// fd_set, dl_sa, timeval
#include "rem_msg.hh"

#define MAX_HTTP_BUFFER_SIZE 16
class CLIENT_HTTP_PORT;
class RCS_LINKED_LIST;
struct dl_sa;

class CMS_SERVER_REMOTE_HTTP_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_HTTP_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_HTTP_PORT ();
  int accept_local_port_cms (CMS *);
  void run ();
  void register_port ();
  void unregister_port ();
  double dtimeout;
protected:
    fd_set read_fd_set, write_fd_set;
  void handle_request (CLIENT_HTTP_PORT *);
  int maxfdpl;
  RCS_LINKED_LIST *client_ports;
  RCS_LINKED_LIST *subscription_buffers;
  int connection_socket;
  long connection_port;
  struct dl_sa *ptr_to_server_socket_address;
  REMOTE_CMS_REQUEST *request;
  char temp_buffer[0x2000];
  int current_poll_interval_millis;
  int polling_enabled;
  void * ptr_to_select_timeout;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  int use_ipv6;
private:
  CMS_SERVER_REMOTE_HTTP_PORT(const CMS_SERVER_REMOTE_HTTP_PORT &);
  CMS_SERVER_REMOTE_HTTP_PORT &operator=(const CMS_SERVER_REMOTE_HTTP_PORT &);
};

#endif /* HTTP_SRV_HH */



