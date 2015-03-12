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


#ifndef TCP_SRV_HH
#define TCP_SRV_HH

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */
#include "rem_msg.hh"

#define MAX_TCP_BUFFER_SIZE 16
class CLIENT_TCP_PORT;

struct dl_sa;
class RCS_LINKED_LIST;

class TCP_BUFFER_MONITOR;
class TCP_SINGLE_VAR_LOG_MONITOR;

class CMS_SERVER_REMOTE_TCP_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_TCP_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_TCP_PORT ();
  int accept_local_port_cms (CMS *);
  void run ();
  void register_port ();
  void unregister_port ();
  double dtimeout;
  RCS_LINKED_LIST *single_var_log_monitors;
protected:
  fd_set read_fd_set, write_fd_set;
  void handle_request (CLIENT_TCP_PORT *);
  int maxfdpl;
  RCS_LINKED_LIST *client_ports;
  RCS_LINKED_LIST *subscription_buffers;
  int connection_socket;
  long connection_port;
  struct dl_sa *ptr_to_server_socket_address;
  REMOTE_CMS_REQUEST *request;
  int current_poll_interval_millis;
  int polling_enabled;
  struct timeval *ptr_to_select_timeout;
  void update_subscriptions ();
  void update_single_var_logs ();
  void add_subscription_client (long buffer_number, int subscription_type,
				int poll_interval_millis,
				CLIENT_TCP_PORT * clnt);
  void remove_subscription_client (CLIENT_TCP_PORT * clnt, long buffer_number);
  void recalculate_polling_interval ();
  int add_buffer_monitor(long buffer_number, bool private_server_object);
  void remove_client(CLIENT_TCP_PORT *_client_tcp_port);
  void get_request_header(CLIENT_TCP_PORT *ctp);
  void get_request_tail(CLIENT_TCP_PORT *ctp);
  void get_request_data(CLIENT_TCP_PORT *ctp);
  void send_reply_header(CLIENT_TCP_PORT *ctp);
  void send_reply_data(CLIENT_TCP_PORT *ctp);
  void process_received_request(CLIENT_TCP_PORT *ctp);
  void set_tbm_recent_write_occured(long buffer_number);

  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  RCS_LINKED_LIST *buffer_monitors;
  int handle_request_count;
  int *single_var_log_buffers_read;
  int sizeof_single_var_log_buffers_read;

  void handle_buffer_updates(class TCP_BUFFER_MONITOR *);
  int add_ctp_to_buffer_monitor(CLIENT_TCP_PORT *ctp);

  friend class CLIENT_TCP_PORT;

private:
  CMS_SERVER_REMOTE_TCP_PORT(const CMS_SERVER_REMOTE_TCP_PORT &);
  CMS_SERVER_REMOTE_TCP_PORT &operator=(const CMS_SERVER_REMOTE_TCP_PORT &);
};


#endif /* TCP_SRV_HH */
