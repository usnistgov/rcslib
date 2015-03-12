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

/*************************************************************************
* File:cms_srv.hh
* Authors: Fred Proctor, Will Shackleford
* Purpose: C++ Header file for server that reads and writes
*          to a local CMS buffer for remote processes.
* Includes:
*          1. class CMS_SERVER
*          2. class CMS_SERVER_LOCAL_PORT
*************************************************************************/

#ifndef CMS_SERVER_HH
#define CMS_SERVER_HH

#include "cms_user.hh"		/* class CMS, CMS_STATUS */

struct REMOTE_READ_REQUEST;
struct REMOTE_READ_REPLY;
struct REMOTE_WRITE_REQUEST;
struct REMOTE_WRITE_REPLY;
struct REMOTE_SET_DIAG_INFO_REPLY;
struct REMOTE_SET_DIAG_INFO_REQUEST;
struct REMOTE_GET_DIAG_INFO_REPLY;
struct REMOTE_GET_DIAG_INFO_REQUEST;
struct REMOTE_GET_BUF_NAME_REPLY;
struct REMOTE_GET_BUF_NAME_REQUEST;
struct REMOTE_GET_MSG_COUNT_REPLY;
struct REMOTE_GET_MSG_COUNT_REQUEST;
struct REMOTE_GET_MSG_TYPE_REPLY;
struct REMOTE_GET_MSG_TYPE_REQUEST;
struct REMOTE_GET_READ_COUNT_REPLY;
struct REMOTE_GET_READ_COUNT_REQUEST;
struct REMOTE_GET_IS_CLEAR_REPLY;
struct REMOTE_GET_IS_CLEAR_REQUEST;
struct REMOTE_CHECK_IF_READ_REPLY;
struct REMOTE_CHECK_IF_READ_REQUEST;
struct REMOTE_SET_SUBSCRIPTION_REPLY;
struct REMOTE_SET_SUBSCRIPTION_REQUEST;
struct REMOTE_GET_KEYS_REPLY;
struct REMOTE_GET_KEYS_REQUEST;
struct REMOTE_GET_QUEUE_LENGTH_REPLY;
struct REMOTE_GET_QUEUE_LENGTH_REQUEST;
struct REMOTE_GET_SPACE_AVAILABLE_REPLY;
struct REMOTE_GET_SPACE_AVAILABLE_REQUEST;
struct REMOTE_LOGIN_REPLY;
struct REMOTE_LOGIN_REQUEST;
struct REMOTE_CLEAR_REPLY;
struct REMOTE_CLEAR_REQUEST;
struct REMOTE_CLIENT_ID;
struct REMOTE_CMS_REQUEST;
struct REMOTE_CMS_REPLY;

extern int cms_server_count;
extern void wait_for_servers (int);

class RCS_LINKED_LIST;
class CMS_SERVER;
class CMS_DIAG_PROC_INFO;
class CMS_USER_INFO;
class CMS_CLIENT_INFO;
class CMS_SERVER_PROCESS_THREAD_INFO;

extern RCS_LINKED_LIST *cms_server_list;

typedef void (*svr_start_func)(void);
extern svr_start_func cms_svr_sfunc;

class CMS_SERVER_LOCAL_WAITING_OBJECT
{
public:
  CMS_SERVER_LOCAL_WAITING_OBJECT();
  virtual ~CMS_SERVER_LOCAL_WAITING_OBJECT();
  virtual int wait_for_anything(void);
  virtual int valid(void);
};

class CMS_SERVER_LOCAL_PORT:public virtual CMS_USER
{
public:
  long buffer_number;
protected:
  int list_id;
  int security_enabled;
  friend class CMS_SERVER;


  /* virtual functions for accessing local buffer. */
  virtual REMOTE_READ_REPLY *reader (  CMS_CLIENT_INFO *current_client_info,
				       REMOTE_READ_REQUEST * req);
  virtual REMOTE_READ_REPLY *blocking_read (  CMS_CLIENT_INFO *current_client_info,
					      REMOTE_READ_REQUEST * req);
  virtual REMOTE_WRITE_REPLY *writer (  CMS_CLIENT_INFO *current_client_info,
					REMOTE_WRITE_REQUEST * buf);
  virtual REMOTE_SET_DIAG_INFO_REPLY
    * set_diag_info (  CMS_CLIENT_INFO *current_client_info,
		       REMOTE_SET_DIAG_INFO_REQUEST * buf);
  virtual REMOTE_GET_DIAG_INFO_REPLY
    * get_diag_info (  CMS_CLIENT_INFO *current_client_info,
		       REMOTE_GET_DIAG_INFO_REQUEST * buf);
  virtual REMOTE_GET_MSG_COUNT_REPLY
    * get_msg_count (  CMS_CLIENT_INFO *current_client_info,
		       REMOTE_GET_MSG_COUNT_REQUEST * buf);
  virtual REMOTE_GET_MSG_TYPE_REPLY
    * get_msg_type (  CMS_CLIENT_INFO *current_client_info,
		       REMOTE_GET_MSG_TYPE_REQUEST * buf);

  virtual void reset_diag_info (  CMS_CLIENT_INFO *current_client_info);
  virtual CMS_SERVER_LOCAL_WAITING_OBJECT *get_new_local_waiting_object(void);
  virtual double get_dvar(const char *dvar_name, int &id, long type, bool &got_dvar, bool read_new);

  REMOTE_READ_REPLY *read_reply_ptr;
  REMOTE_WRITE_REPLY *write_reply_ptr;
  REMOTE_GET_DIAG_INFO_REPLY *get_diag_info_reply_ptr;
  REMOTE_GET_BUF_NAME_REPLY *get_buf_name_reply_ptr;
  REMOTE_GET_MSG_TYPE_REPLY *get_msg_type_reply_ptr;

public:
  CMS_SERVER_LOCAL_PORT (CMS * _cms);
  virtual ~ CMS_SERVER_LOCAL_PORT ();

  REMOTE_GET_BUF_NAME_REPLY *get_buf_name_reply(void);

  int local_channel_reused;
  bool enable_xml_differencing;
  bool multireader_priority_queue_enabled;
protected:
  CMS_DIAG_PROC_INFO *orig_info;
private:
  // prevent copying
  CMS_SERVER_LOCAL_PORT(const CMS_SERVER_LOCAL_PORT &);
  CMS_SERVER_LOCAL_PORT &operator=(const CMS_SERVER_LOCAL_PORT &);
};


class CMS_USER_CONNECT_CLASS;

class CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_PORT (CMS_SERVER *);
  virtual ~ CMS_SERVER_REMOTE_PORT ();
  virtual void run () = 0;
  virtual void killme();
  virtual void register_port ();
  virtual void unregister_port ();
  virtual int accept_local_port_cms (CMS * cms);
  int port_registered;
  CMS_USER_INFO *current_user_info;
  CMS_USER_INFO *get_connected_user (int);
  void add_connected_user (int);

protected:
  RCS_LINKED_LIST * connected_users;
  class CMS_USER_CONNECT_CLASS *current_connected_user_struct;
  int security_enabled;
  CMS_SERVER *cms_server_parent;
  static CMS_SERVER *find_server (long _pid, long _tid = 0);
  static void print_servers ();
  friend class CMS_SERVER;
  double min_compatible_version;
  int confirm_write;
public:
  static void release_client_info(REMOTE_CLIENT_ID *rcid,long pid, long tid);
  int running;
  int max_total_subdivisions;
  int port_num;
  int max_clients;
  int current_clients;
  bool killed;
  int use_ipv6;
private:
  CMS_SERVER_REMOTE_PORT(const CMS_SERVER_REMOTE_PORT &);
  CMS_SERVER_REMOTE_PORT &operator=(const CMS_SERVER_REMOTE_PORT &);
};

class CMS_SERVER_REMOTE_NULL_PORT : public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_NULL_PORT(CMS_SERVER *);
  virtual ~CMS_SERVER_REMOTE_NULL_PORT();
  void run ();
  void register_port ();
  void unregister_port ();
  int accept_local_port_cms (CMS * cms);
};

class CMS_SERVER
{
public:
  int security_enabled;
  int server_spawned;
  int server_registered;
  int list_id;
  RCS_LINKED_LIST *cms_local_ports;

  CMS_SERVER_LOCAL_PORT *find_local_port (long _buffer_num);
  REMOTE_CHECK_IF_READ_REPLY *cir_reply_ptr;
  REMOTE_GET_MSG_COUNT_REPLY *gmc_reply_ptr;
  REMOTE_GET_MSG_TYPE_REPLY *gmt_reply_ptr;
  REMOTE_GET_READ_COUNT_REPLY *grc_reply_ptr;
  REMOTE_GET_IS_CLEAR_REPLY *gic_reply_ptr;
  REMOTE_GET_QUEUE_LENGTH_REPLY *gql_reply_ptr;
  REMOTE_GET_SPACE_AVAILABLE_REPLY *gsa_reply_ptr;
  REMOTE_CLEAR_REPLY *clear_reply_struct_ptr;
  int using_passwd_file;
  long get_message_type ();
  const char *get_buffer_name (long _buf_num);
  int requests_processed;
  void read_passwd_file ();
  virtual void list_cleanup(void);
  virtual CMS_SERVER_LOCAL_WAITING_OBJECT *get_new_local_waiting_object(long _buffer_number);

public:
  virtual double get_dvar(long buffer_num, const char *dvar_name, int &id, long type, bool &got_dvar, bool read_new);

  int get_total_subdivisions (long _buffer_num);
  CMS_SERVER_REMOTE_PORT *remote_port;
  void gen_random_key (char key[], int len);
  int security_check (REMOTE_CMS_REQUEST *, CMS_USER_INFO * user_info, long _buf_num);
  int is_using_passwd_file ();
  CMS_USER_INFO *get_user_info (const char *name, const char *passwd);
  int get_user_keys (const char *name, char *key1, char *key2);

  static void clean (int);

  long get_server_pid_long_int(void);
  void set_spawner_pid_to_server_pid(void);
  bool current_pid_equals_spawner_pid(void);


  class CMS_SERVER_PROCESS_THREAD_INFO *ptinfo;

  long maximum_cms_size;
  REMOTE_CMS_REPLY *process_request (REMOTE_CMS_REQUEST *_req, CMS_CLIENT_INFO *_info = 0);
  CMS_CLIENT_INFO *get_client_info(REMOTE_CLIENT_ID *_rcid, long _buf_num);
  void release_client_info(REMOTE_CLIENT_ID *rcid);
  void register_server (int setup_CC_signal_handler = 1);
  void unregister_server ();
  void run (int setup_CC_signal_handler = 1);
  int spawn ();
  void kill_server ();
    CMS_SERVER ();
  void add_local_port (CMS_SERVER_LOCAL_PORT *);
  void remove_local_port_cms (CMS *);
  void delete_all_local_ports (void);
  virtual void delete_all_local_ports_preserving_resources (void);
  virtual void delete_from_list (void);
  virtual ~ CMS_SERVER ();
  virtual void initialize_write_request_space (void);
  virtual int accept_local_port_cms (CMS *);

  REMOTE_READ_REQUEST *read_req_ptr;
  REMOTE_WRITE_REQUEST *write_req_ptr;
  REMOTE_GET_KEYS_REQUEST *get_keys_req_ptr;
  REMOTE_LOGIN_REQUEST *login_req_ptr;
  REMOTE_SET_SUBSCRIPTION_REQUEST *set_subscription_req_ptr;
  REMOTE_CHECK_IF_READ_REQUEST *check_if_read_req_ptr;
  REMOTE_GET_MSG_TYPE_REQUEST *get_msg_type_req_ptr;
  REMOTE_GET_MSG_COUNT_REQUEST *get_msg_count_req_ptr;
  REMOTE_GET_READ_COUNT_REQUEST *get_read_count_req_ptr;
  REMOTE_GET_IS_CLEAR_REQUEST *get_is_clear_req_ptr;
  REMOTE_GET_QUEUE_LENGTH_REQUEST *get_queue_length_req_ptr;
  REMOTE_GET_SPACE_AVAILABLE_REQUEST *get_space_available_req_ptr;
  REMOTE_CLEAR_REQUEST *clear_req_ptr;
  REMOTE_SET_DIAG_INFO_REQUEST *set_diag_info_req_ptr;
  REMOTE_GET_DIAG_INFO_REQUEST *get_diag_info_req_ptr;
  REMOTE_WRITE_REPLY *write_reply_ptr;
  REMOTE_GET_KEYS_REPLY *get_keys_reply_ptr;
  REMOTE_LOGIN_REPLY *login_reply_ptr;
  REMOTE_SET_SUBSCRIPTION_REPLY *set_subscription_reply_ptr;
  REMOTE_CHECK_IF_READ_REPLY *check_if_read_reply_ptr;
  REMOTE_GET_MSG_COUNT_REPLY *get_msg_count_reply_ptr;
  REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply_ptr;
  REMOTE_GET_SPACE_AVAILABLE_REPLY *get_space_available_reply_ptr;
  REMOTE_CLEAR_REPLY *clear_reply_ptr;
  REMOTE_SET_DIAG_INFO_REPLY *set_diag_info_reply_ptr;
  REMOTE_GET_DIAG_INFO_REPLY *get_diag_info_reply_ptr;

  CMS_SERVER_LOCAL_PORT *last_local_port_used;
  int diag_enabled;
  char set_diag_info_buf[0x400];
  int max_total_subdivisions;
  virtual void set_diag_info (  CMS_CLIENT_INFO *_current_client_info,
			       REMOTE_SET_DIAG_INFO_REQUEST * _diag_info);
  virtual void reset_diag_info ( CMS_CLIENT_INFO *current_client_info,
				 long buffer_number);

  virtual void set_diag_info_from_client_id (  REMOTE_CLIENT_ID *_client_id,
					       REMOTE_SET_DIAG_INFO_REQUEST * _diag_info);
  virtual void reset_diag_info_from_client_id ( REMOTE_CLIENT_ID *_client_id,
						long buffer_number);

private:
  double time_of_last_key_request;
  RCS_LINKED_LIST *known_users;
  char passwd_file[256];
  CMS_USER_INFO *find_user (const char *);
  int guest_can_read;
  int guest_can_write;
  RCS_LINKED_LIST *client_info_list;

private:
  // Prevent copying
  CMS_SERVER(const CMS_SERVER &);
  CMS_SERVER &operator=(const CMS_SERVER &);
};

extern int (*detailed_security_check) (const char *user_name,
				       const char *buffer_name,
				       long msg_type, int access_type);

extern "C" {
  extern  void cms_print_servers (void);
  extern void cms_server_list_cleanup(void);
  extern int get_cms_server_task_priority(void);
  extern int get_cms_server_task_stack_size(void);
}


#endif /* !CMS_SERVER_HH */


