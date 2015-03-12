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

/***************************************************************
* File: rem_msg.hh
* Purpose: Defines the structs passed between REMOTE clients and servers
*  as part of CMS.
* Includes:
*         1. struct REMOTE_READ_REQUEST,  REMOTE_READ_REPLY,
*       REMOTE_WRITE_REPLY, REMOTE_WRITE_REQUEST.
*       2. Function prototypes for functions that XDR encode and decode these
*       structs.
* NOTES:
*  In windows, these XDR functions need to use the Pascal calling convention
* so that the PC-NFS Toolkit DLL can call them.
*****************************************************************/


#ifndef REM_MSG_HH
#define REM_MSG_HH


class  CMS_DIAGNOSTICS_INFO;
struct  CMS_HEADER;


struct  REMOTE_CMS_MESSAGE
{
};

enum REMOTE_CMS_REQUEST_TYPE
{
  NO_REMOTE_CMS_REQUEST=0,
  REMOTE_CMS_READ_REQUEST_TYPE=1,
  REMOTE_CMS_WRITE_REQUEST_TYPE=2,
  REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE=3,
  REMOTE_CMS_CLEAN_REQUEST_TYPE=  4,
  REMOTE_CMS_CLEAR_REQUEST_TYPE=5,
  REMOTE_CMS_CLOSE_CHANNEL_REQUEST_TYPE=6,
  REMOTE_CMS_GET_KEYS_REQUEST_TYPE=7,
  REMOTE_CMS_LOGIN_REQUEST_TYPE=8,
  REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE=9,
  REMOTE_CMS_READ_COMBINED_REQUEST_TYPE=10,
  REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE=11,
  REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE=12,
  REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE=13,
  REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE=14,
  REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE=15,
  REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE=16,
  REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE=17,
  REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE=18,
  REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE=19,
  REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE=20,
  REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE=21,
  REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE=22,
  REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE=23,
  REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE=24,
  REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE=25,
  REMOTE_CMS_GET_READ_COUNT_REQUEST_TYPE=26,
  REMOTE_CMS_GET_IS_CLEAR_REQUEST_TYPE=27,
  REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE=28,
  REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE=29,
  REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE=30,
  REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE=31,
  REMOTE_CMS_GET_BUFFER_LIST_REQUEST_TYPE=32,
  REMOTE_CMS_GET_BUFFER_INFO_REQUEST_TYPE=33,
};

struct  REMOTE_CLIENT_ID
{
public:
  REMOTE_CLIENT_ID ():use_me(false)
  {
    use_me = false;
    long_id[0] = 0;
    long_id[1] = 0;
  };
  long long_id[2];
  bool use_me;
};


struct  REMOTE_CMS_REQUEST:public REMOTE_CMS_MESSAGE
{
  REMOTE_CMS_REQUEST (REMOTE_CMS_REQUEST_TYPE _type):
    buffer_number(0),type(_type),subdiv(0),clientid()
  {
    type = (int) _type;
    buffer_number = 0;
    subdiv = 0;
  };
  long buffer_number;
  int type;
  int subdiv;
  struct REMOTE_CLIENT_ID clientid;
};

struct  REMOTE_CMS_REPLY:public REMOTE_CMS_MESSAGE
{
  REMOTE_CMS_REPLY ():
    status(0)
  {
    status = 0;
  };
  int status;
};

/* Structure sent by client to server to initiate a read. */
struct  REMOTE_BLOCKING_READ_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_BLOCKING_READ_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE),
    access_type(0),last_id_read(0),timeout_millis(0),_nml(0),_data(0),_reply(0)
  {
  };
  int access_type;		/* read or just peek */
  long last_id_read;		/* The server can compare with id from buffer */
  /* to determine if the buffer is new */
  /* to this client */
  long timeout_millis;		/* Milliseconds for blocking_timeout or -1
				 * to wait forever */
  void *_nml;
  void *_data;
  void *_reply;

private:
  // Prevent copying
  REMOTE_BLOCKING_READ_REQUEST(const REMOTE_BLOCKING_READ_REQUEST &_rbrr);
  REMOTE_BLOCKING_READ_REQUEST &operator=(const REMOTE_BLOCKING_READ_REQUEST &_rbrr);
};

/* Structure sent by client to server to initiate a read. */
struct  REMOTE_GET_BUF_NAME_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_BUF_NAME_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE)
  {
  };
};

struct  REMOTE_READ_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_READ_REQUEST ():
    REMOTE_CMS_REQUEST (REMOTE_CMS_READ_REQUEST_TYPE),
    access_type(0),last_id_read(0)
  {
  };
  int access_type;		/* read or just peek */
  long last_id_read;		/* The server can compare with id from buffer */
  /* to determine if the buffer is new */
  /* to this client */
};



/* Structure returned by server to client after a read. */
struct REMOTE_READ_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_READ_REPLY():
    REMOTE_CMS_REPLY(),
    size(0),write_id(0),was_read(0),data(0)
  {
  };

  int size;			/* size of message stored in data. */
  long write_id;		/* Id from the buffer. */
  long was_read;		/* Was this message already read? */
  void *data;		/* Location of stored message. */

private:
  REMOTE_READ_REPLY(const REMOTE_READ_REPLY &_rrr);
  REMOTE_READ_REPLY &operator=(const REMOTE_READ_REPLY &_rrr);
};

/* Structure returned by server to client after a read. */
struct  REMOTE_GET_BUF_NAME_REPLY:public REMOTE_CMS_REPLY
{
  char name[32];		/* Location of stored buffer name (truncated to 31 characters). */
};


/* Structure returned by server to client after a read. */
struct  REMOTE_BLOCKING_READ_REPLY:public REMOTE_READ_REPLY
{
};

/* Structure sent by client to server to initiate a write. */
struct  REMOTE_WRITE_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WRITE_REQUEST ():
    REMOTE_CMS_REQUEST (REMOTE_CMS_WRITE_REQUEST_TYPE),
    access_type(0),size(0),data(0),_nml(0),priority(0),bitwise_op(0)
  {
    data = 0;
    size = 0;
    priority =0;
    bitwise_op=0;
  };
  int access_type;		/* write or write_if_read */
  int size;			/* size of message in data */
  void *data;		/* location of message to write into buffer */
  void *_nml;
  int priority;
  int bitwise_op;

private:
  REMOTE_WRITE_REQUEST(const REMOTE_WRITE_REQUEST &_rwr);
  REMOTE_WRITE_REQUEST &operator=(const REMOTE_WRITE_REQUEST &_rwr);
};


/* Structure returned by server to client after a write. */
struct  REMOTE_WRITE_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_WRITE_REPLY():
    REMOTE_CMS_REPLY(),
    was_read(0),confirm_write(0)
  {
  };

  long was_read;		/* Was the message to be overwriten ever read? */
  int confirm_write;
};

struct  REMOTE_CHECK_IF_READ_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_CHECK_IF_READ_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE)
  {
  };
};

struct  REMOTE_CHECK_IF_READ_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_CHECK_IF_READ_REPLY():
    REMOTE_CMS_REPLY(),
    was_read(0)
  {
  };

  int was_read;
};

struct  REMOTE_CLEAR_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_CLEAR_REQUEST ():REMOTE_CMS_REQUEST (REMOTE_CMS_CLEAR_REQUEST_TYPE)
  {
  };
};

struct  REMOTE_CLEAR_REPLY:public REMOTE_CMS_REPLY
{
};

struct  REMOTE_CLOSE_CHANNEL_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_CLOSE_CHANNEL_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_CLOSE_CHANNEL_REQUEST_TYPE)
  {
  };
};

struct  REMOTE_CLOSE_CHANNEL_REPLY:public REMOTE_CMS_REPLY
{
};


struct  REMOTE_GET_KEYS_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_KEYS_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_KEYS_REQUEST_TYPE)
  {
  };
  char name[16];
};

struct  REMOTE_GET_KEYS_REPLY:public REMOTE_CMS_REPLY
{
  char key1[8];
  char key2[8];
};

struct  REMOTE_LOGIN_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_LOGIN_REQUEST ():REMOTE_CMS_REQUEST (REMOTE_CMS_LOGIN_REQUEST_TYPE)
  {
  };
  char name[16];
  char passwd[16];
};

struct  REMOTE_LOGIN_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_LOGIN_REPLY():
    REMOTE_CMS_REPLY(),
    success(0)
  {
  };
  
  int success;			// 1 = logged in, 0 = not
};

enum CMS_REMOTE_SUBSCRIPTION_REQUEST_TYPE
{
  CMS_POLLED_SUBSCRIPTION = 1,
  CMS_NO_SUBSCRIPTION,
  CMS_VARIABLE_SUBSCRIPTION
};

struct  REMOTE_SET_SUBSCRIPTION_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_SET_SUBSCRIPTION_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE),
    subscription_type(0),poll_interval_millis(0),last_id_read(0)
  {
  };
  int subscription_type;
  int poll_interval_millis;
  int last_id_read;
};

struct  REMOTE_SET_SUBSCRIPTION_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_SET_SUBSCRIPTION_REPLY() :
    REMOTE_CMS_REPLY(),
    success(0),subscription_id(0)
  {
  };

  int success;			// 1 = logged in, 0 = not
  int subscription_id;		// used by UDP clients to cancel a subscription.
};


struct  REMOTE_CANCEL_SUBSCRIPTION_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_CANCEL_SUBSCRIPTION_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE),
    subscription_id(0)
  {
  };
  int subscription_id;
};

struct  REMOTE_CANCEL_SUBSCRIPTION_REPLY:public REMOTE_CMS_REPLY
{
  int success;			// 1 = logged in, 0 = not
  int subscription_id;		// used by UDP clients to cancel a subscription.
};


struct  REMOTE_SET_DIAG_INFO_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_SET_DIAG_INFO_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE),
    pid(0),c_num(0),rcslib_ver(0),reverse_flag(0),bytes_moved(0),bytes_moved_across_socket(0)
  {
  };
  char process_name[16];
  char host_sysinfo[256];
  int pid;
  int c_num;
  double rcslib_ver;
  int reverse_flag;
  double bytes_moved;
  double bytes_moved_across_socket;
};


/* Structure returned by server to client after a read. */
struct  REMOTE_SET_DIAG_INFO_REPLY:public REMOTE_CMS_REPLY
{
};


struct REMOTE_GET_DIAG_INFO_REQUEST:public REMOTE_CMS_REQUEST
{
public:
  REMOTE_GET_DIAG_INFO_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE)
  {
  };
};

/* Structure returned by server to client after a read. */
struct  REMOTE_GET_DIAG_INFO_REPLY:public REMOTE_CMS_REPLY
{
public:
  class CMS_DIAGNOSTICS_INFO *cdi;
  REMOTE_GET_DIAG_INFO_REPLY():
    REMOTE_CMS_REPLY(),
    cdi(0)
  {
  };

private:
  REMOTE_GET_DIAG_INFO_REPLY(const REMOTE_GET_DIAG_INFO_REPLY &_rgdir_ref);
  REMOTE_GET_DIAG_INFO_REPLY &operator=(const REMOTE_GET_DIAG_INFO_REPLY &_rgdir_ref);
};


struct  REMOTE_GET_MSG_COUNT_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_MSG_COUNT_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a read. */
struct  REMOTE_GET_MSG_COUNT_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_MSG_COUNT_REPLY():
    REMOTE_CMS_REPLY(),
    count(0)
  {
  };

  long count;
};

struct  REMOTE_GET_READ_COUNT_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_READ_COUNT_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_READ_COUNT_REQUEST_TYPE)
  {
  };
};

/* Structure returned by server to client after a read. */
struct  REMOTE_GET_READ_COUNT_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_READ_COUNT_REPLY():
    REMOTE_CMS_REPLY(),
    count(0)
  {
  };

  long count;
};

struct  REMOTE_GET_IS_CLEAR_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_IS_CLEAR_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_IS_CLEAR_REQUEST_TYPE)
  {
  };
};

/* Structure returned by server to client after a read. */
struct  REMOTE_GET_IS_CLEAR_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_IS_CLEAR_REPLY():
    REMOTE_CMS_REPLY(),
    is_clear(0)
  {
  };

  long is_clear;
};


struct  REMOTE_GET_QUEUE_LENGTH_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_QUEUE_LENGTH_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a read. */
struct  REMOTE_GET_QUEUE_LENGTH_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_QUEUE_LENGTH_REPLY ():
    REMOTE_CMS_REPLY(),
    queue_length(0)
  {
  };

  long queue_length;
};


struct  REMOTE_GET_SPACE_AVAILABLE_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_SPACE_AVAILABLE_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a read. */
struct  REMOTE_GET_SPACE_AVAILABLE_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_SPACE_AVAILABLE_REPLY() :
    REMOTE_CMS_REPLY(),
    space_available(0)
  {
  };

  long space_available;
};


struct  REMOTE_WAIT_FOR_WRITE_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WAIT_FOR_WRITE_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a wait_for_write. */
struct  REMOTE_WAIT_FOR_WRITE_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_WAIT_FOR_WRITE_REPLY():
    REMOTE_CMS_REPLY()
  {
  };
  
};

struct  REMOTE_WAIT_FOR_READ_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WAIT_FOR_READ_REQUEST ():REMOTE_CMS_REQUEST
    (REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a wait_for_read. */
struct  REMOTE_WAIT_FOR_READ_REPLY:public REMOTE_CMS_REPLY
{
};

struct  REMOTE_WAIT_FOR_QUEUE_LENGTH_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WAIT_FOR_QUEUE_LENGTH_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE),
    waiting_for_queue_length_over(0),queue_length(0)
  {
  };
  int waiting_for_queue_length_over;
  int queue_length;
};


/* Structure returned by server to client after a wait_for_queue_length. */
struct  REMOTE_WAIT_FOR_QUEUE_LENGTH_REPLY:public REMOTE_CMS_REPLY
{
};

struct  REMOTE_WAIT_FOR_CLEAR_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WAIT_FOR_CLEAR_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a wait_for_clear. */
struct  REMOTE_WAIT_FOR_CLEAR_REPLY:public REMOTE_CMS_REPLY
{
};

struct  REMOTE_WAIT_FOR_ANYTHING_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_WAIT_FOR_ANYTHING_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a wait_for_anything. */
struct  REMOTE_WAIT_FOR_ANYTHING_REPLY:public REMOTE_CMS_REPLY
{
};


struct  REMOTE_SETUP_SINGLE_VAR_LOG_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_SETUP_SINGLE_VAR_LOG_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE),
    message_type(0),max_log_count(0),min_time_millis(0)
  {
  };
  char varname[256];
  long message_type;
  long max_log_count;
  long min_time_millis;
};


/* Structure returned by server to client after a setup_single_var_log. */
struct  REMOTE_SETUP_SINGLE_VAR_LOG_REPLY:public REMOTE_CMS_REPLY
{
  int var_log_number;
};


struct  REMOTE_GET_SINGLE_VAR_LOG_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_SINGLE_VAR_LOG_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE),
    var_log_number(0)
  {
  };
  int var_log_number;
};

struct single_var_log_item
{
  double value;
  double timestamp;
};

/* Structure returned by server to client after a get_single_var_log. */
struct  REMOTE_GET_SINGLE_VAR_LOG_REPLY:public REMOTE_CMS_REPLY
{
  int num_log_items;
  struct singl_var_log_items *log;
};


struct  REMOTE_CLOSE_SINGLE_VAR_LOG_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_CLOSE_SINGLE_VAR_LOG_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE),
    var_log_number(0)
  {
  };
  int var_log_number;
};

/* Structure returned by server to client after a close_single_var_log. */
struct  REMOTE_CLOSE_SINGLE_VAR_LOG_REPLY:public REMOTE_CMS_REPLY
{
};

struct  REMOTE_GET_MSG_TYPE_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_MSG_TYPE_REQUEST ():
    REMOTE_CMS_REQUEST(REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE)  {
  };
};

struct  REMOTE_GET_MSG_TYPE_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_MSG_TYPE_REPLY():
    REMOTE_CMS_REPLY(),
    msg_type(0)
  {
  };

  long msg_type;
};


struct  REMOTE_GET_BUFFER_INFO_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_BUFFER_INFO_REQUEST ():
    REMOTE_CMS_REQUEST (REMOTE_CMS_GET_BUFFER_INFO_REQUEST_TYPE)
  {
  };

  char name[64];
};


/* Structure returned by server to client after a read. */
struct REMOTE_GET_BUFFER_INFO_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_BUFFER_INFO_REPLY():
    REMOTE_CMS_REPLY(),
    size(0),data(0)
  {
  };
  int size;
  void *data;		/* Location of stored message. */

private:
  REMOTE_GET_BUFFER_INFO_REPLY(const REMOTE_GET_BUFFER_INFO_REPLY &_rrr);
  REMOTE_GET_BUFFER_INFO_REPLY &operator=(const REMOTE_GET_BUFFER_INFO_REPLY &_rrr);
};



struct  REMOTE_GET_BUFFER_LIST_REQUEST:public REMOTE_CMS_REQUEST
{
  REMOTE_GET_BUFFER_LIST_REQUEST ():
    REMOTE_CMS_REQUEST (REMOTE_CMS_GET_BUFFER_LIST_REQUEST_TYPE)
  {
  };
};


/* Structure returned by server to client after a read. */
struct REMOTE_GET_BUFFER_LIST_REPLY:public REMOTE_CMS_REPLY
{
  REMOTE_GET_BUFFER_LIST_REPLY():
    REMOTE_CMS_REPLY(),
    size(0),data(0)
  {
  };
  int size;
  void *data;		/* Location of stored message. */

private:
  REMOTE_GET_BUFFER_LIST_REPLY(const REMOTE_GET_BUFFER_LIST_REPLY &_rrr);
  REMOTE_GET_BUFFER_LIST_REPLY &operator=(const REMOTE_GET_BUFFER_LIST_REPLY &_rrr);
};

// end of ifndef REM_MSG_HH
#endif










