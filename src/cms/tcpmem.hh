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



#ifndef TCPMEM_HH
#define TCPMEM_HH

#include "rem_msg.hh" /* enum REMOTE_CMS_REQUEST_TYPE */
#include "cms.hh"		/* class CMS */

// struct dl_sa -- Used by dl_socket functions as an address type; see sokintf.h,sockintf.c
struct dl_sa;

class TCPMEM:public CMS
{
public:
  TCPMEM (const char *bufline, 
	  const char *procline);

  virtual ~ TCPMEM ();

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
  long get_msg_type();

  int do_wait_for(REMOTE_CMS_REQUEST_TYPE t, long lparam1,long lparam2, double timeout);

  int wait_for_anything(double timeout);
  int wait_for_read(double timeout);
  int wait_for_clear(double timeout);
  int wait_for_write(double timeout);
  int wait_for_queue_length_over(int , double timeout);
  int wait_for_queue_length_under(int , double timeout);

  int login (const char *, const char *);
  void reconnect ();
  void disconnect ();
  CMS_DIAGNOSTICS_INFO *get_diagnostics_info ();
  void interrupt_operation(void);
  void clear_interrupt_operation(void);

  CMS_STATUS setup_subscription(double _subscription_period);
  CMS_STATUS cancel_subscription();

protected:
  void setup_polling();
  CMS_STATUS handle_old_replies ();
  void send_diag_info ();
  char diag_info_buf[0x400];
  int recvd_bytes;
  long serial_number;
  long returned_serial_number;
  int subscription_type;
  int poll_interval_millis;
  struct dl_sa *server_socket_address_ptr;
  int socket_fd;
  char temp_buffer[0x2000];
  enum REMOTE_CMS_REQUEST_TYPE timedout_request;
  enum REMOTE_CMS_REQUEST_TYPE last_request_type;
  long bytes_to_throw_away;
  int polling;
  int write_socket_fd;
  int read_socket_fd;
  long write_serial_number;
  long read_serial_number;
  void set_socket_fds (int new_fd);
  enum CMS_STATUS timedout_request_status;
  unsigned long timedout_request_writeid;
  int max_consecutive_timeouts;
  int waiting_for_message;
  unsigned long waiting_message_size;
  unsigned long waiting_message_id;
  int autoreconnect;
  int reconnect_needed;
  int sigpipe_count;
  void (*old_handler) (int);
  void disable_sigpipe ();
  void reenable_sigpipe ();
  void verify_bufname ();
  int subscription_count;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  int interrupting_operation_int;
  int reconnect_count;
  long orig_pid;
  long connect_pid;
  struct dl_sa *cli_addr;

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  TCPMEM(const TCPMEM &);
  TCPMEM &operator=(const TCPMEM &);

};

// TCPMEM_HH
#endif



