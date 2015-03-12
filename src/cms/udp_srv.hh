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


#ifndef UDP_SRV_HH
#define UDP_SRV_HH

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */

#define MAX_UDP_BUFFER_SIZE 16

class UDP_BROADCAST_DATA;

class CLIENT_UDP_PORT;

struct sockaddr_in;
struct dl_sa;
struct hostent;
struct iovec;
struct msghdr;
struct timeval;
class RCS_LINKED_LIST;

class CMS_SERVER_REMOTE_UDP_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_UDP_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_UDP_PORT ();
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

  // Added May 9,2008.
  // Tracking each udp client is necessary to eliminate the possibility that
  // a single write from the client could result in multiple packets hitting the 
  // server and multiple identical redundant messages being written to the buffer.
  // This would occur very rarerly and rarely matters for non-queued buffers even when 
  // it does but to be safe it is enabled by default.
  // It is also used for managing subscriptions.
  // It is code that was responsible for a now fixed memory leak affecting only VxWorks,
  // reported by Mark del Giorno of GDRS.
  // Removing the #define TRACK_CLIENT_UDP_PORTS will allow these redundant messages to occur and disable 
  // subscriptions 
  // but save memory and processing speed in tracking the clients, and eliminate the possibility for a
  // memory leak of the sort discussed above or of slowly fragmenting memory if there are a lot of clients.
  // The same effect can be achieved at run-time on a per server bases by adding "DO_NOT_TRACK_CLIENT_UDP_PORTS"
  // to the config file buffer or process lines.
#define TRACK_CLIENT_UDP_PORTS
#ifdef TRACK_CLIENT_UDP_PORTS
  CLIENT_UDP_PORT *get_client_port (dl_sa * client_address);
  void remove_client_port (dl_sa * client_address);

  void add_subscription_client (int buffer_number, int subdiv,
				int subscription_type,
				int poll_interval_millis,
				CLIENT_UDP_PORT * clnt);
  void remove_subscription (int subscription_id, int buffer_number,
			    int subdiv);
  void remove_subscription_client (CLIENT_UDP_PORT * clnt, int buffer_number,
				   int subdiv);


#endif

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
  unsigned char sendmsgt_collection_buffer[0x4000];
  unsigned char recvmsgt_collection_buffer[0x4000];
  bool track_client_udp_ports;
private:
  CMS_SERVER_REMOTE_UDP_PORT(const CMS_SERVER_REMOTE_UDP_PORT &);
  CMS_SERVER_REMOTE_UDP_PORT &operator=(const CMS_SERVER_REMOTE_UDP_PORT &);
};

/* UDP_SRV_HH */
#endif

