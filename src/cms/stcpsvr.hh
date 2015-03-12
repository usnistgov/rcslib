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


#ifndef STCP_SRV_HH
#define STCP_SRV_HH

#ifndef HAVE_CONFIG_H
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */
#endif

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */

#define MAX_STCP_BUFFER_SIZE 16
class CLIENT_STCP_PORT;
#define TEMP_BUFFER_MAX 512

struct dl_sa;

class RCS_LINKED_LIST;

class CMS_SERVER_REMOTE_STCP_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_STCP_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_STCP_PORT ();
  int accept_local_port_cms (CMS *);
  void run ();
  void register_port ();
  void unregister_port ();

protected:
    fd_set read_fd_set, write_fd_set;
  void handle_request (CLIENT_STCP_PORT *);
  int maxfdpl;
  RCS_LINKED_LIST *client_ports;
  int connection_socket;
  long buffer_number;
  long connection_port;
  struct dl_sa *server_socket_address_ptr;
  REMOTE_CMS_REQUEST *request;
  char *temp_buffer;
  long temp_buffer_size;
  int handle_request_error_occured;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];

private:
  CMS_SERVER_REMOTE_STCP_PORT(const CMS_SERVER_REMOTE_STCP_PORT &);
  CMS_SERVER_REMOTE_STCP_PORT &operator=(const CMS_SERVER_REMOTE_STCP_PORT &);

};

#endif /* STCP_SRV_HH */
