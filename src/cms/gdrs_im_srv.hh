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


#ifndef GDRS_IM_SRV_HH
#define GDRS_IM_SRV_HH

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */

#define MAX_GDRS_IM_BUFFER_SIZE 16

class GDRS_IM_BROADCAST_DATA;

class CLIENT_GDRS_IM_PORT;

struct sockaddr_in;
struct dl_sa;
struct hostent;
struct iovec;
struct msghdr;
struct timeval;
class RCS_LINKED_LIST;

class CMS_SERVER_REMOTE_GDRS_IM_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_GDRS_IM_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_GDRS_IM_PORT ();
  int accept_local_port_cms (CMS *);
  void run ();
  void register_port ();
  void unregister_port ();
  double dtimeout;

protected:
    fd_set read_fd_set, write_fd_set;
  void handle_request ();
  int maxfdpl;
  int request_length;
  RCS_LINKED_LIST *client_ports;
  int connection_socket;
  long connection_port;
  struct dl_sa *ptr_to_server_socket_address;
  REMOTE_CMS_REQUEST *request;
  char temp_buffer[32];
  CMS_SERVER *server;
  struct dl_sa *ptr_to_client_address;
  struct msghdr *ptr_to_message_header;
  int client_addresslen;
  double polling_period;
  struct iovec *ptr_to_iov2;
  RCS_LINKED_LIST *subscription_buffers;
  int current_poll_interval_millis;
  int polling_enabled;
  struct timeval *ptr_to_select_timeout;
  void update_subscriptions ();
  void add_subscription_client (int buffer_number, int subdiv,
				int subscription_type,
				int poll_interval_millis,
				CLIENT_GDRS_IM_PORT * clnt);
  void remove_subscription (int subscription_id, int buffer_number,
			    int subdiv);
  void remove_subscription_client (CLIENT_GDRS_IM_PORT * clnt, int buffer_number,
				   int subdiv);
  CLIENT_GDRS_IM_PORT *get_client_port (dl_sa * client_address);
  void recalculate_polling_interval ();
  int last_subscription_id;
  RCS_LINKED_LIST *broadcast_ports;
  int request_header_size;
  int reply_header_size;
  struct sockaddr_in *ptr_to_broadcast_address;
  int broadcast_address_set;
  void set_broadcast_address (CMS *);
#ifndef VXWORKS
  struct hostent *broadcast_server_host_entry;
#endif
  int broadcast_subscriptions;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
private:
  CMS_SERVER_REMOTE_GDRS_IM_PORT(const CMS_SERVER_REMOTE_GDRS_IM_PORT &);
  CMS_SERVER_REMOTE_GDRS_IM_PORT &operator=(const CMS_SERVER_REMOTE_GDRS_IM_PORT &);
};

/* GDRS_IM_SRV_HH */
#endif

