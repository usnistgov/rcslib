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

 
/****************************************************************************
* File: tcp_srv.cc
* Purpose: Provides the functions for the class CMS_SERVER_REMOTE_T_PORT
*  which provides TCP specific overrides of the CMS_SERVER_REMOTE_PORT class.
****************************************************************************/

#define CMS_NETWORK_SOURCE

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "tcp_srv_no_config.h"
#endif

#include "sokintrf.h"		/* dl_ioctl() */
#include "cms.hh"		/* class CMS */
#include "nml.hh"		// class NML
#include "cms_srv.hh"		// class CMS_SERVER
#include "tcp_srv.hh"		/* class CMS_SERVER_REMOTE_TCP_PORT */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "linklist.hh"		/* class RCS_LINKED_LIST */
#include "tcp_opts.hh"		/* SET_TCP_NODELAY */
#include "timer.hh"		// esleep()
#include "_timer.h"
#include "cmsdiag.hh"		// class CMS_DIAGNOSTICS_INFO
#include "recvn.h"		/* recvn() */
#include "sendn.h"		/* sendn() */
#include "physmem.hh"		// class PHYSMEM_HANDLE
#include "cms_clnt_info.hh"	// CMS_CLIENT_INFO
#include "ntohhton.hh"

#ifdef __GLIBC__
#if __GLIBC_PREREQ(2,3)
// undef BAD_PTHREADS
#else
#define BAD_PTHREADS 1
#endif
#endif

#ifdef BAD_PTHREADS
// I have found pthreads on a RedHat 7.0 which uses libpthread 
// box to be buggy enough
// that although you could successfully compile with POSIX_THREADS and it
// will even work for simple applications, it is necessary to avoid.
// The problem was that pthread_create would not return until after a the 
// user pressed control C to send SIGINT to the process and begin shutdown. 
#define NO_THREADS
#undef POSIX_THREADS
#endif

#ifndef INT_MAX
#define INT_MAX 2147483647
#endif

static inline bool 
int_lt_uint(int i, unsigned int ui)
{
  if(i < 0)
    {
      return true;
    }
  if(ui > INT_MAX)
    {
      return true;
    }
  return(((int) i) < ((int)ui));
}
  
  
class TCPSVR_BLOCKING_READ_REQUEST;

class TCP_BUFFER_SUBSCRIPTION_INFO
{
public:
  TCP_BUFFER_SUBSCRIPTION_INFO ();
  ~TCP_BUFFER_SUBSCRIPTION_INFO ();
  long buffer_number;
  int min_last_id;
  int list_id;
  class RCS_LINKED_LIST *sub_clnt_info;

private:
  TCP_BUFFER_SUBSCRIPTION_INFO(const TCP_BUFFER_SUBSCRIPTION_INFO &);
  TCP_BUFFER_SUBSCRIPTION_INFO &operator=(const TCP_BUFFER_SUBSCRIPTION_INFO &);

};

class TCP_CLIENT_SUBSCRIPTION_INFO
{
public:
  TCP_CLIENT_SUBSCRIPTION_INFO ();
  ~TCP_CLIENT_SUBSCRIPTION_INFO ();
  int subscription_type;
  int poll_interval_millis;
  double last_sub_sent_time;
  int subscription_list_id;
  long buffer_number;
  int subscription_paused;
  int last_id_read;
  class TCP_BUFFER_SUBSCRIPTION_INFO *sub_buf_info;
  class CLIENT_TCP_PORT *clnt_port;

private:
  TCP_CLIENT_SUBSCRIPTION_INFO(const TCP_CLIENT_SUBSCRIPTION_INFO &);
  TCP_CLIENT_SUBSCRIPTION_INFO &operator=(const TCP_CLIENT_SUBSCRIPTION_INFO &);

};

class TCP_BUFFER_MONITOR
{
public:
  TCP_BUFFER_MONITOR();
  ~TCP_BUFFER_MONITOR();
  
  class CMS_SERVER_LOCAL_WAITING_OBJECT *lwo;
  long buffer_number;
  int socket_pair[2];
  int connected_clients_count;
  int released_clients_count;
  class RCS_LINKED_LIST *clients_list;
  int send_count;
  int wakeup_count;
  bool awake;
  bool quit;
  bool started;
  bool new_request;
  bool private_server_object;
  bool recent_write_occured;
  char stupid_buf[512];
#ifdef POSIX_THREADS
  pthread_t threadId;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  pthread_cond_t started_cond;
#else
  int threadId;
#endif
  class TCP_BUFFER_MONITOR *loopback;

private:
  TCP_BUFFER_MONITOR(const TCP_BUFFER_MONITOR &);
  TCP_BUFFER_MONITOR &operator=(const TCP_BUFFER_MONITOR &);

};


TCP_BUFFER_MONITOR::TCP_BUFFER_MONITOR():
    lwo(0),buffer_number(0),connected_clients_count(0),released_clients_count(0),
    clients_list(0),send_count(0),wakeup_count(0),awake(false),quit(false),
    started(false),new_request(false),private_server_object(false),
    recent_write_occured(false),
#ifdef POSIX_THREADS
    threadId(0),mutex(),cond(),started_cond(),
#else
    threadId(0),
#endif
    loopback(0)
{
  new_request=false;
  loopback=this;
  started=false;
  lwo=0;
  socket_pair[0] = -1;
  socket_pair[1] = -1;
  connected_clients_count=0;
  released_clients_count=0;
  send_count=0;
  wakeup_count=0;
  clients_list=0;
  quit=false;
  awake=false;
#ifdef POSIX_THREADS
  threadId=0;
  pthread_mutex_init(&mutex,0);
  pthread_cond_init(&cond,0);
  pthread_cond_init(&started_cond,0);
#endif
}

TCP_BUFFER_MONITOR::~TCP_BUFFER_MONITOR()
{
  rcs_print_debug(PRINT_MISC,
		  "TCP_BUFFER_MONITOR::~TCP_BUFFER_MONITOR() called. this=%p,loopback=%p,lwo=%p\n",
		  (void*)this,(void*)loopback,(void*)lwo);

  if(loopback == this)
    {
      loopback=0;
      quit=true;
      awake=false;
#ifdef POSIX_THREADS
      if(threadId > 0)
	{
	  pthread_t thread_to_cancel = threadId;
	  threadId=0;
	  if(!is_cancelled_thread_id((void *) (&thread_to_cancel)))
	    {
	      mark_cancelled_thread_id((void *) (&thread_to_cancel));
	      // printf("%s:%d pthread_cancel(%d (0x%X))\n",
	      // 		     __FILE__,__LINE__,
	      // (int) thread_to_cancel, (unsigned) thread_to_cancel);
	      pthread_cancel(thread_to_cancel);
	    }
	}
#elif defined(VXWORKS)
      if(threadId > 0 && OK == taskIdVerify (threadId))
	{
	  taskDelete(threadId);
	  threadId =0;
	}
#endif
#ifdef NO_THREADS
      if(threadId > 0)
	{
	  kill(threadId,SIGTERM);
#if HAVE_WAITPID
	  int tries = 0;
	  int waitpid_ret = 0;
	  int status=0;
	  while(tries < 20 && waitpid_ret == 0)
	    {
	      waitpid_ret = waitpid(threadId,&status,WNOHANG);
	      if(waitpid == 0)
		{
		  esleep(0.02);
		}
	      tries++;
	    }
#endif
	  threadId=0;
	}
#endif 
      if(socket_pair[0] > 0)
	{
	  dl_closesocket(socket_pair[0]);
	  socket_pair[0] = -1;
	}
      if(socket_pair[1] > 0)
	{
	  dl_closesocket(socket_pair[1]);
	  socket_pair[1] = -1;
	}
      if(clients_list)
	{
	  delete clients_list;
	  clients_list=0;
	}
      if(lwo)
	{
	  delete lwo;
	  lwo=0;
	}
    }
}

class CMS_CLIENT_INFO;

enum TCP_CLIENT_STATE
  {
    WAITING_FOR_REQUEST_HEADER=11,
    WAITING_FOR_REQUEST_HEADER_COMPLETE, // 12
    WAITING_FOR_REQUEST_TAIL,	// 13
    WAITING_FOR_REQUEST_TAIL_COMPLETE, // 14
    WAITING_FOR_REQUEST_DATA,	// 15
    WAITING_FOR_REQUEST_DATA_COMPLETE, // 16
    WAITING_FOR_PROCESS_REQUEST, // 17
    WAITING_FOR_PROCESS_REQUEST_COMPLETE, // 18
    WAITING_FOR_REPLY_HEADER,	// 19
    WAITING_FOR_REPLY_HEADER_COMPLETE, // 20
    WAITING_FOR_REPLY_DATA,	// 21
    WAITING_FOR_REPLY_DATA_COMPLETE // 22
  };
    

enum over_under_enum
  {
    OVER_UNDER_NOT_SET=0,
    OVER,
    UNDER
  };

class CLIENT_TCP_PORT
{
public:
  CLIENT_TCP_PORT (class CMS_SERVER_REMOTE_TCP_PORT *_parent);
  ~CLIENT_TCP_PORT ();
  long serial_number;
  long received_serial_number;
  long request_type_long;
  enum REMOTE_CMS_REQUEST_TYPE request_type;
  enum REMOTE_CMS_REQUEST_TYPE last_request_type;

  int errors;
  int max_errors;
  struct dl_sa *address_ptr;
  int socket_fd;
  RCS_LINKED_LIST *subscriptions;
  struct REMOTE_CLIENT_ID rcid;
  int blocking;
  int total_subdivisions;
  int request_subdiv;
  int requests_complete;
  long start_request_msg_count;
  long start_request_read_count;
  long start_request_is_clear;
  long start_request_queue_length;
  long last_request_queue_length;
  long max_request_queue_length;
  long min_request_queue_length;
  long request_queue_length_to_wait_for;
  enum over_under_enum request_queue_length_over_or_under;

  struct REMOTE_SET_DIAG_INFO_REQUEST *diag_info;
  class RCS_LINKED_LIST  *buffer_monitors;
  class CMS_CLIENT_INFO *cms_clnt_info;
  long buffer_number;
  long last_buffer_number;
  void *extra_data_buf_address;
  size_t extra_data_buf_size;
  void *extra_reply_data_buf;
  size_t extra_reply_data_size;
  const void *reply_data_address;
  size_t reply_data_size;
  void *reply_header_address;
  size_t reply_header_size;
  void *request_header_address;
  size_t request_header_size;
  void *request_tail_address;
  size_t request_tail_size;
  void *request_data_address;
  size_t request_data_size;
  
  void *output_waiting_buffer;
  size_t output_waiting_buffer_size;
  size_t size_of_unsent_data;
  char *ptr_to_begin_of_unsent_data;
  void *input_waiting_buffer;
  size_t input_waiting_buffer_size;
  char *ptr_to_begin_of_unreceived_data;
  size_t size_of_unreceived_data;
  int last_read_id;
  const void *last_send_addr;
  size_t last_send_size;
  int last_send_retval;
  int last_send_errno;
  void *last_recv_addr;
  size_t last_recv_size;
  int last_recv_retval;
  int last_recv_errno;
  char sockerrbuf[256];
  const char *sockerrstr;
  bool something_blocked;
  bool using_input_waiting_buffer;
  bool blocked_on_write;
  bool remove_me;

#if MS_WINDOWS_API
  DWORD tid;
  DWORD pid;
#else
#ifdef VXWORKS
  int tid;
  int pid;
#else
  pid_t tid;
  pid_t pid;
#endif
#endif

#if defined(sunos5) && !defined(NO_THREADS)
  thread_t threadId;
#else
#ifdef POSIX_THREADS
  pthread_t threadId;
#else
  int threadId;
#endif
#endif
#if defined(sunos5) && !defined(NO_THREADS)
  thread_t last_threadId;
#else
#ifdef POSIX_THREADS
  pthread_t last_threadId;
#else
  int last_threadId;
#endif
#endif
  char temp_buffer[0x2000];
  class CMS_SERVER_REMOTE_TCP_PORT *parent;
  class CLIENT_TCP_PORT *loopback;

  int Send(const void *addr, size_t s);
  int retry_last_send(void);
  int Recv(void *addr, size_t s);
  int retry_last_recv(void);
  enum TCP_CLIENT_STATE get_state(void);
  void set_state(enum TCP_CLIENT_STATE);

  //  bool send_thread_forked;
  //  bool send_thread_started;
  RCS_LINKED_LIST *single_var_log_monitors;
private:
  enum TCP_CLIENT_STATE state;

private:
  // Prevent copying.
  CLIENT_TCP_PORT(const CLIENT_TCP_PORT &_ctp);  
  CLIENT_TCP_PORT &operator=(const CLIENT_TCP_PORT &_ctp);

};

class TCP_SINGLE_VAR_LOG_MONITOR
{
public:
  long message_type;
  long buffer_number;
  long log_count;
  int in_buffer_id;
  int list_id;
  int ctp_list_id;
  int min_period_millis;
  double min_period;
  double last_log_add_time;
  long log_item_list_length;
  long max_log_item_list_length;
  int head_index;
  struct single_var_log_item *log_item_list;
  char *reply_buffer;
  class CLIENT_TCP_PORT *ctp;
  CMS_SERVER_REMOTE_TCP_PORT *rport;
  char varname[256];

  TCP_SINGLE_VAR_LOG_MONITOR():
  message_type(0),
  buffer_number(0),
  log_count(0),
  in_buffer_id(0),
  list_id(0),
  ctp_list_id(0),
  min_period_millis(0),
  min_period(0),
  last_log_add_time(0),
  log_item_list_length(0),
  max_log_item_list_length(0),
  head_index(0),
  log_item_list(0),
  reply_buffer(0),
  ctp(0),
  rport(0)
  {};    

  ~TCP_SINGLE_VAR_LOG_MONITOR()
  {
    if(rport && rport->single_var_log_monitors && list_id > 0)
      {
	rport->single_var_log_monitors->delete_node(list_id);
      }
    if(ctp && ctp->single_var_log_monitors && ctp_list_id > 0)
      {
	ctp->single_var_log_monitors->delete_node(ctp_list_id);
      }

    if(reply_buffer)
      {
	free(reply_buffer);
	reply_buffer = 0;
      }
    if(log_item_list)
      {
	free(log_item_list);
	log_item_list = 0;
      }
    max_log_item_list_length=0;
    ctp=0;
    list_id = -1;
    ctp_list_id = -1;
  }

private:
  // Prevent copying.
  TCP_SINGLE_VAR_LOG_MONITOR(const TCP_SINGLE_VAR_LOG_MONITOR &);  
  TCP_SINGLE_VAR_LOG_MONITOR &operator=(const TCP_SINGLE_VAR_LOG_MONITOR &);
  
};

  

enum TCP_CLIENT_STATE 
CLIENT_TCP_PORT::get_state(void)
{
  return state;
}

void 
CLIENT_TCP_PORT::set_state(enum TCP_CLIENT_STATE new_state)
{
  rcs_print_debug(PRINT_MISC,"CLIENT_TCP_PORT::set_state() state changed from %d to %d -- this=%p,socket_fd=%d,buffer_number=%ld,serial_number=%ld\n",
		  state,new_state,(void*)this,socket_fd,buffer_number,serial_number);
#ifdef DEBUG_521
  printf("parent=%p new_state=%d, state=%d, request_type=%d\n",
	 (void*)parent,(int)new_state,(int)state,(int)request_type);
#endif

  if(parent &&
     new_state == WAITING_FOR_REQUEST_HEADER && 
     state != WAITING_FOR_REQUEST_HEADER)
    {
      if(request_type == REMOTE_CMS_WRITE_REQUEST_TYPE ||
	 request_type == REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE ||
	 request_type == REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE)
	{
	  parent->set_tbm_recent_write_occured(buffer_number);
	}
    }
  state = new_state;
}



int
CLIENT_TCP_PORT::Send(const void *addr, size_t s)
{
  if(!addr || s < 1)
    {
      rcs_print_error("CLIENT_TCP_PORT::Send called with bad arguments\n");
    }
  last_send_addr = addr;
  last_send_size = s;
  last_send_retval = dl_send(socket_fd, (char *) addr,(int) s,0);
  if(last_send_retval < 0)
    {
      something_blocked=true;
      blocked_on_write=true;
      last_send_errno = dl_get_last_socket_error_int( socket_fd );
      if(!dl_socket_error_was_would_block ( socket_fd, last_send_errno))
	{
	  ptr_to_begin_of_unsent_data = 0;
	  sockerrstr = dl_get_last_socket_error_string(socket_fd,last_send_errno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("socket send error: %d -- %s\n", last_send_errno, sockerrstr);
	  return -1;
	}
    }
  if(int_lt_uint(last_send_retval,(unsigned int) s))
    {
      size_of_unsent_data = s;
      if(last_send_retval > 0)
	{
	  size_of_unsent_data -= last_send_retval;
	}
      if(addr >= temp_buffer && addr < temp_buffer+sizeof(temp_buffer))
	{
	  ptr_to_begin_of_unsent_data = (char *) addr;
	  if(last_send_retval > 0)
	    {
	      ptr_to_begin_of_unsent_data += last_send_retval;
	    }
	}
      else if(extra_data_buf_address && addr >= extra_data_buf_address && 
	      addr < (((char*)extra_data_buf_address)+extra_data_buf_size))
	{
	  ptr_to_begin_of_unsent_data = (char *) addr;
	  if(last_send_retval > 0)
	    {
	      ptr_to_begin_of_unsent_data += last_send_retval;
	    }
	}
      else
	{
	  if (!output_waiting_buffer)
	    {
	      output_waiting_buffer_size = s + (0x2000 - (s%0x2000));
	      output_waiting_buffer = malloc(output_waiting_buffer_size);
	    }
	  else if(output_waiting_buffer_size < size_of_unsent_data)
	    {
	      output_waiting_buffer_size = s + (0x2000 - (s%0x2000));
	      output_waiting_buffer = realloc(output_waiting_buffer,output_waiting_buffer_size);
	    }
	  if(last_send_retval > 0)
	    {
	      memcpy(output_waiting_buffer, (((char *)addr)+last_send_retval),size_of_unsent_data);
	    }
	  else
	    {
	      memcpy(output_waiting_buffer,addr,size_of_unsent_data);
	    }
	  ptr_to_begin_of_unsent_data = (char *) output_waiting_buffer;
	}
      something_blocked=true;
      blocked_on_write=true;
    }
  else 
    {
      blocked_on_write=false;
      ptr_to_begin_of_unsent_data=0;
      size_of_unsent_data = 0;
    }
  return last_send_retval;
}

int
CLIENT_TCP_PORT::retry_last_send(void)
{
  if ( size_of_unsent_data > 0 && ptr_to_begin_of_unsent_data)
    {
      last_send_retval = dl_send(socket_fd, ptr_to_begin_of_unsent_data, (int) size_of_unsent_data,0);
      if(last_send_retval < 0)
	{
	  something_blocked=true;
	  blocked_on_write=true;
	  last_send_errno = dl_get_last_socket_error_int( socket_fd );
	  if(!dl_socket_error_was_would_block ( socket_fd, last_send_errno))
	  {
	      ptr_to_begin_of_unsent_data = 0;
	      sockerrstr = dl_get_last_socket_error_string(socket_fd,last_send_errno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("socket send error: %d -- %s\n", last_send_errno, sockerrstr);
	      return -1;
	    }
	}
      else
	{
	  if (int_lt_uint(last_send_retval, (unsigned int) size_of_unsent_data) )
	    {
	      something_blocked=true;
	      blocked_on_write=true;
	      ptr_to_begin_of_unsent_data += last_send_retval;
	      size_of_unsent_data -= last_send_retval;
	    }
	  else 
	    {
	      blocked_on_write=false;
	      ptr_to_begin_of_unsent_data = 0;
	      size_of_unsent_data = 0;
	    }
	}
      return last_send_retval;
    }
  return 0;
}

int
CLIENT_TCP_PORT::Recv(void *addr, size_t s)
{
  if(!addr || s < 1)
    {
      rcs_print_error("CLIENT_TCP_PORT::Recv called with bad arguments\n");
    }
  blocked_on_write=false;
  last_recv_addr = addr;
  last_recv_size = s;
  using_input_waiting_buffer=false;
  last_recv_retval = dl_recv(socket_fd, (char *) addr,(int) s,0);
  if(last_recv_retval < 0)
    {    
      something_blocked=true;
      last_recv_errno = dl_get_last_socket_error_int( socket_fd );
      if(!dl_socket_error_was_would_block ( socket_fd, last_recv_errno))
	{
	  errors++;
	  ptr_to_begin_of_unreceived_data = 0;
	  sockerrstr = dl_get_last_socket_error_string(socket_fd,last_recv_errno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("socket recv error {this=%p,socket_fd=%d,addr=%s:%d}: %d -- %s\n", 
			   (void*)this,socket_fd,
			   dl_sa_get_host(address_ptr),
			   dl_sa_get_port(address_ptr),
			   last_recv_errno, sockerrstr);
#ifdef ECONNRESET
	  if(last_recv_errno == ECONNRESET)
	    {
	      remove_me=true;
	    }
#endif
	  return -1;
	}
    }
  if(int_lt_uint(last_recv_retval,(unsigned int) s))
    {
      something_blocked=true;
      size_of_unreceived_data = s;
      if(last_recv_retval > 0)
	{
	  size_of_unreceived_data -= last_recv_retval;
	}
      if(addr >= temp_buffer && addr < temp_buffer+sizeof(temp_buffer))
	{
	  ptr_to_begin_of_unreceived_data = (char *) addr;
	  if(last_recv_retval > 0)
	    {
	      ptr_to_begin_of_unreceived_data += last_recv_retval;
	    }
	}
      else
	{
	  if (!input_waiting_buffer)
	    {
	      input_waiting_buffer_size = s + (0x2000 - (s%0x2000));
	      input_waiting_buffer = malloc(input_waiting_buffer_size);
	    }
	  else if(input_waiting_buffer_size < s )
	    {
	      input_waiting_buffer_size = s + (0x2000 - (s%0x2000));
	      input_waiting_buffer = realloc(input_waiting_buffer,input_waiting_buffer_size);
	    }
	  ptr_to_begin_of_unreceived_data = (char *) input_waiting_buffer;
	  using_input_waiting_buffer=true;
	  if(last_recv_retval > 0)
	    {
	      memcpy(input_waiting_buffer,addr,last_recv_retval);
	      ptr_to_begin_of_unreceived_data += last_recv_retval;
	    }
	}
    }
  else
    {
      size_of_unreceived_data=0;
      ptr_to_begin_of_unreceived_data =0;
    }
  return last_recv_retval;
}

int 
CLIENT_TCP_PORT::retry_last_recv(void)
{
  blocked_on_write=false;
  if(ptr_to_begin_of_unreceived_data && size_of_unreceived_data > 0)
    {
      last_recv_retval = recv(socket_fd,ptr_to_begin_of_unreceived_data, (int) size_of_unreceived_data,0);
      if(last_recv_retval < 0)
	{ 
	  something_blocked=true;
	  last_recv_errno = dl_get_last_socket_error_int( socket_fd );
	  if(!dl_socket_error_was_would_block ( socket_fd, last_recv_errno))
	    {
	      ptr_to_begin_of_unreceived_data = 0;
	      sockerrstr = dl_get_last_socket_error_string(socket_fd,last_recv_errno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("socket recv error: %d -- %s\n", last_recv_errno, sockerrstr);
	      errors++;
	      return -1;
	    }
	}
      else 
	{
	  if(int_lt_uint(last_recv_retval, (unsigned int) size_of_unreceived_data))
	    {
	      if(last_recv_retval > 0)
		{
		  size_of_unreceived_data -= last_recv_retval;
		  ptr_to_begin_of_unreceived_data += last_recv_retval;
		}
	      something_blocked=true;
	    }
	  else
	    {
	      if(using_input_waiting_buffer)
		{
		  memcpy(last_recv_addr,input_waiting_buffer,last_recv_size);
		  using_input_waiting_buffer=false;
		}
	      ptr_to_begin_of_unreceived_data =0;
	      size_of_unreceived_data=0;
	    }
	}
      return last_recv_retval;
    }
  return 0;
}

static inline double
tcp_svr_reverse_double (double in)
{
  double out;
  char *c1, *c2;

  c1 = ((char *) &in) + 7;
  c2 = (char *) &out;
  for (int i = 0; i < 8; i++)
    {
      *c2 = *c1;
      c1--;
      c2++;
    }
  return out;
}

CMS_SERVER_REMOTE_TCP_PORT::CMS_SERVER_REMOTE_TCP_PORT (CMS_SERVER * _cms_server):
  CMS_SERVER_REMOTE_PORT (_cms_server),
  dtimeout(0.0),  single_var_log_monitors(0),
#ifndef VXWORKS
  read_fd_set(),write_fd_set(),
#endif
  maxfdpl(0),client_ports(0),
  subscription_buffers(0),connection_socket(0),connection_port(0),
  ptr_to_server_socket_address(0),
  request(0),current_poll_interval_millis(0),polling_enabled(0),
  ptr_to_select_timeout(0),sockerrno(0),sockerrstr(0),
  buffer_monitors(0),handle_request_count(0),
  single_var_log_buffers_read(0),
  sizeof_single_var_log_buffers_read(0)
{
  client_ports = (RCS_LINKED_LIST *) NULL;
  connection_socket = 0;
  connection_port = 0;
  maxfdpl = 0;
  buffer_monitors=0;
  handle_request_count=0;
  dtimeout = 20.0;
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      return;
    }
  if(getenv("IPV6") || getenv("NML_IPV6") || getenv("CMS_IPV6"))
    {
      use_ipv6=1;
    }

  char *bind_to_host = 
    getenv("NML_BINDTO_HOST");
  ptr_to_server_socket_address = dl_create_sa(bind_to_host,
					      0,use_ipv6);

  client_ports = new RCS_LINKED_LIST;
  if (NULL == client_ports)
    {
      rcs_print_error ("Can not create linked list for client ports.\n");
      return;
    }
  polling_enabled = 0;
  ptr_to_select_timeout = new struct timeval;
  memset (ptr_to_select_timeout, 0, sizeof (struct timeval));
  ((struct timeval*)ptr_to_select_timeout)->tv_sec = 30;
  ((struct timeval*)ptr_to_select_timeout)->tv_usec = 30;
  subscription_buffers = NULL;
  current_poll_interval_millis = 30000;
  memset (&read_fd_set, 0, sizeof (read_fd_set));
  memset (&write_fd_set, 0, sizeof (write_fd_set));
}

CMS_SERVER_REMOTE_TCP_PORT::~CMS_SERVER_REMOTE_TCP_PORT ()
{
  unregister_port ();
  if( buffer_monitors)
    {
            
      rcs_print_debug(PRINT_MISC,"buffer_monitors->list_size=%d\n",
		      buffer_monitors->list_size);
      TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *)
	buffer_monitors->get_head();
      while(tbm)
	{
	  rcs_print_debug(PRINT_MISC,"tbm=%p\n",(void*)tbm);
	  delete tbm;
	  buffer_monitors->delete_current_node();
	  tbm = (TCP_BUFFER_MONITOR *)
	    buffer_monitors->get_next();
	}
      delete buffer_monitors;
      buffer_monitors=0;
    }
  if (NULL != client_ports)
    {
      delete client_ports;
      client_ports = (RCS_LINKED_LIST *) NULL;
    }
  if(ptr_to_server_socket_address)
    {
      dl_free_sa(ptr_to_server_socket_address);
      ptr_to_server_socket_address=0;
    }
  if(ptr_to_select_timeout)
    {
      delete ptr_to_select_timeout;
      ptr_to_select_timeout=0;
    }
  if(single_var_log_monitors)
    {
      delete single_var_log_monitors;
      single_var_log_monitors = 0;
    }
  if(single_var_log_buffers_read)
    {
      free(single_var_log_buffers_read);
      single_var_log_buffers_read=0;
    }
  unload_socket_interface();
}

void
CMS_SERVER_REMOTE_TCP_PORT::unregister_port ()
{
  CLIENT_TCP_PORT *client;
  int number_of_connected_clients = 0;
#if 0
  client = (CLIENT_TCP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      if (client->threadId > 0 && client->blocking)
	{
	  tcp_srv_blocking_thread_kill (client->threadId, client);
	  client->threadId = 0;
	}
      client = (CLIENT_TCP_PORT *) client_ports->get_next ();
    }
#endif
  client = (CLIENT_TCP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      rcs_print ("Exiting even though client on %s is still connected.\n",
		 dl_sa_get_host(client->address_ptr)
		 );
      client = (CLIENT_TCP_PORT *) client_ports->get_next ();
      number_of_connected_clients++;
    }
  client = (CLIENT_TCP_PORT *) client_ports->get_head ();
  while (NULL != client)
    {
      delete client;
      client_ports->delete_current_node ();
      client = (CLIENT_TCP_PORT *) client_ports->get_next ();
    }
  if (NULL != subscription_buffers)
    {
      TCP_BUFFER_SUBSCRIPTION_INFO *sub_info =
	(TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
      while (NULL != sub_info)
	{
	  delete sub_info;
	  sub_info =
	    (TCP_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	}
      delete subscription_buffers;
      subscription_buffers = NULL;
    }
  if (number_of_connected_clients > 0)
    {
      esleep (2.0);
    }
  if (connection_socket > 0)
    {
      dl_closesocket (connection_socket);
      connection_socket = 0;
    }
}


int
CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(long _buffer_number,
					       bool _private_server_object)
{
  CMS_SERVER *svr = 0;
  TCP_BUFFER_MONITOR *tbm_private =0;
  TCP_BUFFER_MONITOR *tbm=0;
  CMS_SERVER_LOCAL_WAITING_OBJECT *lwo=0;

  svr = cms_server_parent;
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(%ld) : svr=%p\n",
		  _buffer_number,(void*)svr);

  if(_private_server_object)
    {
      tbm_private = new TCP_BUFFER_MONITOR();
      
      rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(%ld) : tbm=%p\n",
		      _buffer_number,(void*)tbm_private);
      tbm_private->lwo = 0;
      tbm_private->private_server_object=true;
      tbm_private->buffer_number = _buffer_number;
      if(0 == buffer_monitors)
	{
	  buffer_monitors = new RCS_LINKED_LIST();
	}
      buffer_monitors->store_at_tail(tbm_private,sizeof(TCP_BUFFER_MONITOR),0);
      return 0;
    }

  if(!svr)
    {
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      DWORD pid = GetCurrentProcessId ();
      DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
      int pid = taskIdSelf ();
      
      int tid = 0;
#else
      pid_t pid = getpid ();
      pid_t tid = 0;
#endif
#endif
      svr = find_server(pid,tid);
    }
  if(!svr)
    {
      rcs_print_error("svr is NULL\n");
      return -1;
    }

  lwo = svr->get_new_local_waiting_object(_buffer_number);
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(%ld) : lwo=%p\n",
		  _buffer_number,(void*)lwo);
  if(!lwo)
    {
      rcs_print_error("lwo is NULL (buffer_number=%ld)\n",_buffer_number);
      return -1;
    }
  if(0 == buffer_monitors)
    {
      buffer_monitors = new RCS_LINKED_LIST();
    }
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(%ld) : buffer_monitors=%p\n",
		  _buffer_number, (void*)buffer_monitors);

  tbm = new TCP_BUFFER_MONITOR();
  
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::add_buffer_monitor(%ld) : tbm=%p\n",
		  _buffer_number,(void*)tbm);
  tbm->lwo = lwo;
  tbm->buffer_number = _buffer_number;
  buffer_monitors->store_at_tail(tbm,sizeof(TCP_BUFFER_MONITOR),0);
  return 0;
}

int
CMS_SERVER_REMOTE_TCP_PORT::accept_local_port_cms (CMS * _cms)
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_TCP_PORT::accept_local_port_cms(_cms=%p) called. this=%p\n",
		  (void*)_cms,(void*)this);

  if (NULL == _cms)
    {
      return 0;
    }
  if (_cms->remote_port_type != CMS_TCP_REMOTE_PORT_TYPE)
    {
      return 0;
    }
  if (NULL != _cms)
    {
      if (min_compatible_version < 1e-6 ||
	  (min_compatible_version > _cms->min_compatible_version &&
	   _cms->min_compatible_version > 1e-6))
	{
	  min_compatible_version = _cms->min_compatible_version;
	}
      if (_cms->confirm_write)
	{
	  confirm_write = _cms->confirm_write;
	}
    }
  if (_cms->total_subdivisions > max_total_subdivisions)
    {
      max_total_subdivisions = _cms->total_subdivisions;
    }
  if (!ptr_to_server_socket_address ||
      dl_sa_get_port(ptr_to_server_socket_address) == 0)
    {
      if(use_ipv6 != _cms->use_ipv6 && ptr_to_server_socket_address)
	{
	  dl_free_sa(ptr_to_server_socket_address);
	  ptr_to_server_socket_address=0;
	}
      use_ipv6 = _cms->use_ipv6;
      if(!ptr_to_server_socket_address)
	{
	  ptr_to_server_socket_address = dl_create_sa(
						      (_cms->bind_proc_host?_cms->ProcessHost:0),
						      _cms->tcp_port_number,
						      _cms->use_ipv6);
	}
      dl_sa_set_port(ptr_to_server_socket_address,_cms->tcp_port_number);
      port_num = _cms->tcp_port_number;
      return 1;
    }
  if (ptr_to_server_socket_address && 
      dl_sa_get_port(ptr_to_server_socket_address) == _cms->tcp_port_number)
    {
      port_num = _cms->tcp_port_number;
      return 1;
    }
  return 0;
}

void
CMS_SERVER_REMOTE_TCP_PORT::register_port ()
{
  if(port_registered)
    {
      rcs_print_warning("This CMS_SERVER_REMOTE_TCP_PORT:register_port() (%p) called when port_registered already equals %d\n",
			(void*)this,port_registered);
    }
  port_registered = 0;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "Registering server on TCP port %d.\n",
		   dl_sa_get_port(ptr_to_server_socket_address));
  if (!ptr_to_server_socket_address ||
      dl_sa_get_port(ptr_to_server_socket_address) == 0)
    {
      rcs_print_error ("server can not register on port number 0.\n");
      return;
    }
  if ((connection_socket = (int) dl_tcp_socket(use_ipv6)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("socket error: %d -- %s\n", sockerrno, sockerrstr);
      rcs_print_error ("Server can not open stream socket.\n");
      return;
    }

  if (set_tcp_socket_options (connection_socket) < 0)
    {
      return;
    }
  if (dl_bind (connection_socket, dl_sa_addr(ptr_to_server_socket_address),
	       dl_sa_len(ptr_to_server_socket_address)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("bind error: %d -- %s\n", 
		       sockerrno,sockerrstr );
      rcs_print_error
	("Server can not bind the connection socket on port %d with address %s.\n",
	 dl_sa_get_port(ptr_to_server_socket_address),
	 dl_sa_get_host(ptr_to_server_socket_address));
      return;
    }
  port_registered = 1;
}

#if !defined(DOS_WINDOWS) && !defined(MS_WINDOWS_API)
static int last_pipe_signum = 0;

static void
handle_pipe_error (int signum)
{
  last_pipe_signum = signum;
  rcs_print_error ("SIGPIPE intercepted.\n");
}
// #if !defined(DOS_WINDOWS) && !defined(MS_WINDOWS_API)
#endif

void 
CMS_SERVER_REMOTE_TCP_PORT::remove_client(CLIENT_TCP_PORT *_client_tcp_port)
{  
  if(!_client_tcp_port)
    {
      rcs_print_error("_client_tcp_port is NULL\n");
      return;
    }
  rcs_print_debug (PRINT_SOCKET_CONNECT,
		   "Socket closed by host with IP address %s.\n",
		   dl_sa_get_host(_client_tcp_port->address_ptr));
  CLIENT_TCP_PORT *current_ctp = (CLIENT_TCP_PORT *) 
    client_ports->get_current();
  if(current_ctp && _client_tcp_port->socket_fd == current_ctp->socket_fd)
    {
      client_ports->delete_current_node ();
    }
  else
    {
      current_ctp = (CLIENT_TCP_PORT *) 
	client_ports->get_head();
      while(current_ctp)
	{
	  if(_client_tcp_port->socket_fd == current_ctp->socket_fd)
	    {
	      client_ports->delete_current_node();
	      break;
	    }
	  current_ctp = (CLIENT_TCP_PORT *) 
	    client_ports->get_next();
	}
    }
  if (NULL != _client_tcp_port->subscriptions)
    {
      TCP_CLIENT_SUBSCRIPTION_INFO *clnt_sub_info =
	(TCP_CLIENT_SUBSCRIPTION_INFO *)
	_client_tcp_port->subscriptions->get_head ();
      while (NULL != clnt_sub_info)
	{
	  if (NULL != clnt_sub_info->sub_buf_info &&
	      clnt_sub_info->subscription_list_id >= 0)
	    {
	      if (NULL !=
		  clnt_sub_info->sub_buf_info->sub_clnt_info)
		{
		  clnt_sub_info->sub_buf_info->sub_clnt_info->
		    delete_node (clnt_sub_info->
				 subscription_list_id);
		  if (clnt_sub_info->sub_buf_info->
		      sub_clnt_info->list_size < 1)
		    {
		      delete clnt_sub_info->sub_buf_info->
			sub_clnt_info;
		      clnt_sub_info->sub_buf_info->
			sub_clnt_info = NULL;
		      if (NULL != subscription_buffers
			  && clnt_sub_info->sub_buf_info->
			  list_id >= 0)
			{
			  subscription_buffers->delete_node
			    (clnt_sub_info->sub_buf_info->
			     list_id);
			  delete clnt_sub_info->sub_buf_info;
			  clnt_sub_info->sub_buf_info = NULL;
			}
		    }
		  clnt_sub_info->sub_buf_info = NULL;
		}
	      delete clnt_sub_info;
	      clnt_sub_info =
		(TCP_CLIENT_SUBSCRIPTION_INFO *)
		_client_tcp_port->subscriptions->
		get_next ();
	    }
	  delete _client_tcp_port->subscriptions;
	  _client_tcp_port->subscriptions = NULL;
	  recalculate_polling_interval ();
	}
    }
  dl_closesocket (_client_tcp_port->socket_fd);
  RCS_FD_CLR (_client_tcp_port->socket_fd, &read_fd_set);
  RCS_FD_CLR (_client_tcp_port->socket_fd, &write_fd_set);
  _client_tcp_port->socket_fd = -1;
  delete _client_tcp_port;
}

void
CMS_SERVER_REMOTE_TCP_PORT::handle_buffer_updates(class TCP_BUFFER_MONITOR *tbm)
{
  bool read_needed = false; 
  bool get_queue_length_needed=false;
  bool get_is_clear_needed=false;
  bool get_msg_count_needed=false;
  bool get_read_count_needed=false;
  int min_read_id=0;
  bool wakeup_tbm=false;
  bool nonzero_subdiv_read_encountered=false;

  if(!tbm)
    {
      rcs_print_error("tbm is NULL\n");
      return;
    }


#ifdef DEBUG_521
  if(tbm->recent_write_occured)
    {
            printf("handle_buffer_updates : buffer_number=%ld, time=%f\n",tbm->buffer_number,etime());
    }
#endif

  tbm->new_request=false;
  tbm->recent_write_occured=false;
  
  if(tbm->socket_pair[0] <= 0)
    {
      rcs_print_error("tbm->socket_pair[0] <= 0 \n");
      return;
    }
  int bytes_ready=0;
  dl_ioctlsocket_fionread(tbm->socket_pair[0],&bytes_ready);
  while(bytes_ready > 0 && !killed)
    {
      if(bytes_ready < 512)
	{
	  dl_recv(tbm->socket_pair[0],tbm->stupid_buf,bytes_ready,0);
	  break;
	}
      else
	{
	  dl_recv(tbm->socket_pair[0],tbm->stupid_buf,512,0);
	}
      dl_ioctlsocket_fionread(tbm->socket_pair[0],&bytes_ready);
    }

  CMS_SERVER *svr = 0;
  svr = cms_server_parent;
  if(!svr)
    {
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      DWORD pid = GetCurrentProcessId ();
      DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
      int pid = taskIdSelf ();
      
      int tid = 0;
#else
      pid_t pid = getpid ();
      pid_t tid = 0;
#endif
#endif
      svr = find_server(pid,tid);
    }
  if(!svr)
    {
      rcs_print_error("svr is NULL\n");
      return;
    }

  if(!tbm->clients_list)
    {
      rcs_print_debug(PRINT_MISC,
		      "clients_list is NULL(tbm->buffer_number=%ld)\n",
		      tbm->buffer_number);
      return;
    }
  
  CLIENT_TCP_PORT *ctp = (CLIENT_TCP_PORT *)
    tbm->clients_list->get_head();
  while(ctp && !killed)
    {
      if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	{
	  switch(ctp->request_type)
	    {
	    case REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE:
	      read_needed = true;
	      if(min_read_id == 0)
		{
		  min_read_id = ctp->last_read_id;
		}
	      else if(min_read_id > ctp->last_read_id)
		{
		  min_read_id = ctp->last_read_id;
		}
	      if(ctp->request_subdiv != 0)
		{
		  nonzero_subdiv_read_encountered=true;
		}
	      break;
	      
	    case REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE:
	      get_msg_count_needed=true;
	      break;

	    case REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE:
	      get_read_count_needed=true;
	      break;

	    case REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE:
	      get_queue_length_needed=true;
	      break;

	    case REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE:
	      get_is_clear_needed=true;
	      break;	      
	     
	    case REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE:
	      break;

	    default:
	      rcs_print_error("Should not be blocking on request_type %d\n",
			      ctp->request_type);
	      ctp->errors++;
	      break;
	    }
	}
      if(ctp->request_type == REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE
	 && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	{
	  read_needed = true;
	}
      ctp = (CLIENT_TCP_PORT *)
	tbm->clients_list->get_next();
    }
  int highest_subdiv_checked=-1;
  bool all_subdivs_checked=false;

  while(read_needed && !killed)
    {
      if(0 == svr->read_req_ptr)
	{
	  svr->read_req_ptr = new REMOTE_READ_REQUEST();
	}
      svr->read_req_ptr->buffer_number = tbm->buffer_number;
      svr->read_req_ptr->access_type = CMS_READ_ACCESS;
      svr->read_req_ptr->last_id_read = min_read_id;
      svr->read_req_ptr->clientid.use_me = 0;
      if(!nonzero_subdiv_read_encountered)
	{
	  svr->read_req_ptr->subdiv = 0;
	  read_needed=false;
	}
      else
	{
	  read_needed=false;
	  all_subdivs_checked = true;
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_head();
	  while(ctp && !killed)
	    {
	      if(ctp->request_type == REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE
		 && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE
		 && ctp->request_subdiv > highest_subdiv_checked)
		{
		  svr->read_req_ptr->subdiv = ctp->request_subdiv;
		  highest_subdiv_checked = ctp->request_subdiv;
		  read_needed=true;
		  all_subdivs_checked=false;
		  break;
		}
	      ctp = (CLIENT_TCP_PORT *)
		tbm->clients_list->get_next();
	    }
	}
      if(all_subdivs_checked)
	{
	  break;
	}
      REMOTE_READ_REPLY *read_reply=
	(REMOTE_READ_REPLY *) svr->process_request (svr->read_req_ptr);

      rcs_print_debug(PRINT_MISC,
		     "read_reply=%p, read_reply->size=%d, read_reply->status=%d\n",
		     (void*)read_reply,(int)read_reply->size, (int)read_reply->status);

      if(!read_reply || (read_reply->size > 0 && read_reply->status != CMS_READ_OLD))
	{	
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_head();
	  while(ctp && !killed)
	    {
	      if(ctp->request_type == REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE
		 && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE
		 && ( ctp->request_subdiv == highest_subdiv_checked ||
		 !nonzero_subdiv_read_encountered ))
		{
		  if (NULL == read_reply)
		    {
		      rcs_print_error ("Server could not process request from %s port %d.(NULL == read_reply) \n",
				       dl_sa_get_host(ctp->address_ptr),
				       dl_sa_get_port(ctp->address_ptr)
				       );
		      hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		      hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		      hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0); /* size */
		      hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) 0); /* write_id */
		      hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) 0); /* was_read */
		      ctp->reply_header_address = ctp->temp_buffer;
		      ctp->reply_header_size = 20;
		      ctp->reply_data_address = 0;
		      ctp->reply_data_size = 0;
		      ctp->set_state(WAITING_FOR_REPLY_HEADER);
		      handle_request(ctp);
		    }
		  else if(read_reply->write_id != ctp->last_read_id)
		    {
		      hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		      hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) read_reply->status);
		      hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) read_reply->size);
		      hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) read_reply->write_id);
		      hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) read_reply->was_read);
		      ctp->last_read_id = read_reply->write_id;
		      if (read_reply->size < (0x2000 - 20)
			  && read_reply->size > 0)
			{
			  memcpy (ctp->temp_buffer + 20, read_reply->data,read_reply->size);
			  ctp->reply_header_address = ctp->temp_buffer;
			  ctp->reply_header_size = 20 +read_reply->size;
			  ctp->reply_data_address = 0;
			  ctp->reply_data_size = 0;
			  ctp->set_state(WAITING_FOR_REPLY_HEADER);
			  handle_request(ctp);
			}
		      else
			{
			  ctp->set_state(WAITING_FOR_REPLY_HEADER);
			  ctp->reply_header_address = ctp->temp_buffer;
			  ctp->reply_header_size = 20;
			  if(read_reply->size > 0)
			    {
			      ctp->reply_data_address = read_reply->data;
			      ctp->reply_data_size = read_reply->size;
			    }
			  else
			    {
			      ctp->reply_data_address = 0;
			      ctp->reply_data_size = 0;
			    }
			  handle_request(ctp);
			}
		    }
		}
	      ctp = (CLIENT_TCP_PORT *)
		tbm->clients_list->get_next();
	    }
	}
    }
  if(get_msg_count_needed)
    {
      if(svr->get_msg_count_req_ptr ==0)
	{
	  svr->get_msg_count_req_ptr = new REMOTE_GET_MSG_COUNT_REQUEST();
	}
      svr->get_msg_count_req_ptr->buffer_number = tbm->buffer_number;
      svr->get_msg_count_req_ptr->clientid.use_me = 0;
      svr->get_msg_count_req_ptr->subdiv = 0;
      REMOTE_GET_MSG_COUNT_REPLY *get_msg_count_reply =
	(REMOTE_GET_MSG_COUNT_REPLY *) svr->process_request (svr->get_msg_count_req_ptr);
      ctp = (CLIENT_TCP_PORT *)
	tbm->clients_list->get_head();
      while(ctp && !killed)
	{
	  if(ctp->request_type == REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE
	     && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	    {
	      if(get_msg_count_reply == NULL)
		{
		  rcs_print_error ("Server could not process request from %s port %d.(get_msg_count_reply == NULL) \n",
				   dl_sa_get_host(ctp->address_ptr),
				   dl_sa_get_port(ctp->address_ptr)
				   );
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0); /* msg_count */
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	      else if (get_msg_count_reply->count != ctp->start_request_msg_count)
		{
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_msg_count_reply->status);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_msg_count_reply->count);
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	    }
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_next();
	}
    }
  if(get_read_count_needed)
    {
      if(svr->get_read_count_req_ptr ==0)
	{
	  svr->get_read_count_req_ptr = new REMOTE_GET_READ_COUNT_REQUEST();
	}
      svr->get_read_count_req_ptr->buffer_number = tbm->buffer_number;
      svr->get_read_count_req_ptr->clientid.use_me = 0;
      svr->get_read_count_req_ptr->subdiv = 0;
      REMOTE_GET_READ_COUNT_REPLY *get_read_count_reply =
	(REMOTE_GET_READ_COUNT_REPLY *) svr->process_request (svr->get_read_count_req_ptr);
      ctp = (CLIENT_TCP_PORT *)
	tbm->clients_list->get_head();
      while(ctp && !killed)
	{
	  if(ctp->request_type == REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE
	     && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	    {
	      if(get_read_count_reply == NULL)
		{
		  rcs_print_error ("Server could not process request from %s port %d.(get_read_count_reply == NULL) \n",
				   dl_sa_get_host(ctp->address_ptr),
				   dl_sa_get_port(ctp->address_ptr)
				   );
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0); /* msg_count */
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	      else if (get_read_count_reply->count != ctp->start_request_read_count)
		{
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_read_count_reply->status);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_read_count_reply->count);
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	    }
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_next();
	}
    }
  if(get_queue_length_needed)
    {
      if(svr->get_queue_length_req_ptr ==0)
	{
	  svr->get_queue_length_req_ptr = new REMOTE_GET_QUEUE_LENGTH_REQUEST();
	}
      svr->get_queue_length_req_ptr->buffer_number = tbm->buffer_number;
      svr->get_queue_length_req_ptr->clientid.use_me = 0;
      svr->get_queue_length_req_ptr->subdiv = 0;
      REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply =
	(REMOTE_GET_QUEUE_LENGTH_REPLY *) svr->process_request (svr->get_queue_length_req_ptr);
      ctp = (CLIENT_TCP_PORT *)
	tbm->clients_list->get_head();
      while(ctp && !killed)
	{
	  if(ctp->request_type == REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE
	     && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	    {
	      if(get_queue_length_reply == NULL)
		{
		  rcs_print_error ("Server could not process request from %s port %d.(get_queue_length_reply == NULL)\n",
				   dl_sa_get_host(ctp->address_ptr),
				   dl_sa_get_port(ctp->address_ptr)
				   );
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	      else if (get_queue_length_reply->queue_length != ctp->last_request_queue_length)
		{
		  ctp->last_request_queue_length = get_queue_length_reply->queue_length;
		  if(get_queue_length_reply->queue_length >
		     ctp->max_request_queue_length)
		    {
		      ctp->max_request_queue_length = get_queue_length_reply->queue_length;
		    }
		  if(get_queue_length_reply->queue_length <
		     ctp->min_request_queue_length)
		    {
		      ctp->min_request_queue_length = get_queue_length_reply->queue_length;
		    }
		  if( (get_queue_length_reply->queue_length < 
		       ctp->request_queue_length_to_wait_for &&
		       ctp->request_queue_length_over_or_under == UNDER) ||
		      (get_queue_length_reply->queue_length >
		       ctp->request_queue_length_to_wait_for &&
		       ctp->request_queue_length_over_or_under == OVER))
		    {		      
		      hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		      hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_queue_length_reply->status);
		      hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_queue_length_reply->queue_length);
		      ctp->reply_header_address = ctp->temp_buffer;
		      ctp->reply_header_size = 12;
		      ctp->reply_data_address = 0;
		      ctp->reply_data_size = 0;
		      ctp->set_state(WAITING_FOR_REPLY_HEADER);
		      handle_request(ctp);
		    }
		}
	    }
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_next();
	}
    }
  if(get_is_clear_needed)
    {
      if(svr->get_is_clear_req_ptr ==0)
	{
	  svr->get_is_clear_req_ptr = new REMOTE_GET_IS_CLEAR_REQUEST();
	}
      svr->get_is_clear_req_ptr->buffer_number = tbm->buffer_number;
      svr->get_is_clear_req_ptr->clientid.use_me = 0;
      svr->get_is_clear_req_ptr->subdiv = 0;
      REMOTE_GET_IS_CLEAR_REPLY *get_is_clear_reply =
	(REMOTE_GET_IS_CLEAR_REPLY *) svr->process_request (svr->get_is_clear_req_ptr);
      ctp = (CLIENT_TCP_PORT *)
	tbm->clients_list->get_head();
      while(ctp && !killed)
	{
	  if(ctp->request_type == REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE
	     && ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	    {
	      if(get_is_clear_reply == NULL)
		{
		  rcs_print_error ("Server could not process request from %s port %d.(get_is_clear_reply == NULL)\n",
				   dl_sa_get_host(ctp->address_ptr),
				   dl_sa_get_port(ctp->address_ptr)
				   );
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	      else if (get_is_clear_reply->is_clear &&
		       ! ctp->start_request_is_clear)
		{
		  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_is_clear_reply->status);
		  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_is_clear_reply->is_clear);
		  ctp->reply_header_address = ctp->temp_buffer;
		  ctp->reply_header_size = 12;
		  ctp->reply_data_address = 0;
		  ctp->reply_data_size = 0;
		  ctp->set_state(WAITING_FOR_REPLY_HEADER);
		  handle_request(ctp);
		}
	      if(!get_is_clear_reply->is_clear)
		{
		  ctp->start_request_is_clear=0;
		}
	    }
	  ctp = (CLIENT_TCP_PORT *)
	    tbm->clients_list->get_next();
	}
    }
  ctp = (CLIENT_TCP_PORT *) tbm->clients_list->get_head();
  wakeup_tbm=false;
  while(ctp && !killed)
    {
      if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST_COMPLETE)
	{
	  wakeup_tbm = true;
	  break;
	}
      ctp = (CLIENT_TCP_PORT *) tbm->clients_list->get_next();
    }
  if(wakeup_tbm)
    {
      tbm->wakeup_count++;
#ifdef POSIX_THREADS
      pthread_cond_signal(&(tbm->cond));
#endif
    }
}

void *
buffer_monitor_thread_func(void *arg)
{
  int last_wakeup_count=0;
  long lx;
  char x;
#ifdef POSIX_THREADS
  int orig_cancelstate;
  int orig_canceltype;
  sigset_t new_sigmask;
  sigemptyset(&new_sigmask);
  sigaddset(&new_sigmask,SIGINT);
  sigaddset(&new_sigmask,SIGPIPE);
  pthread_sigmask(SIG_BLOCK,&new_sigmask,0);
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE,&orig_cancelstate);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,&orig_canceltype);
#endif
  TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *) arg;

  if(!tbm || !tbm->lwo)
    {
#ifdef POSIX_THREADS
      pthread_exit(0);
#endif
      return(0);
    } 

  last_wakeup_count = tbm->wakeup_count;
  x=0;
  lx=0;
  tbm->started=true;
#ifdef POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"buffer_monitor_thread_func : calling pthread_cond_broadcast(&(tbm->started_cond)) . . .");
  pthread_cond_broadcast(&(tbm->started_cond));
  rcs_print_debug(PRINT_MISC,"buffer_monitor_thread_func : finished pthread_cond_broadcast(&(tbm->started_cond));");
#endif

  while(!tbm->quit)
    {
      lx++;
      x= (char) (lx&0xFF);

      rcs_print_debug(PRINT_MISC,"buffer_monitor_thread_func : tbm->buffer_number=%ld,tbm->threadId=%ld,x=%d,tbm->wakeup_count=%d,last_wakeup_count=%d \n",tbm->buffer_number,(long)tbm->threadId,x, tbm->wakeup_count, last_wakeup_count);

#ifdef POSIX_THREADS
      pthread_testcancel();
      pthread_mutex_lock(&(tbm->mutex));
      if(tbm->wakeup_count == last_wakeup_count)
	{
	  pthread_cond_wait(&(tbm->cond),&(tbm->mutex));
	  last_wakeup_count = tbm->wakeup_count;
	}
      pthread_mutex_unlock(&(tbm->mutex));
#endif
      tbm->lwo->wait_for_anything();
      dl_send(tbm->socket_pair[1],&x,sizeof(x),0);
    }
#ifdef POSIX_THREADS
  pthread_exit(0);
#endif
  return(0);
}


void
CMS_SERVER_REMOTE_TCP_PORT::run ()
{
  unsigned long bytes_ready;
  int ready_descriptors;
  int select_ret;
  if (NULL == client_ports)
    {
      rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
      return;
    }
  CMS_SERVER *svr = 0;
  svr = cms_server_parent;
  if(!svr)
    {
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      DWORD pid = GetCurrentProcessId ();
      DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
      int pid = taskIdSelf ();
      
      int tid = 0;
#else
      pid_t pid = getpid ();
      pid_t tid = 0;
#endif
#endif
      cms_server_parent = svr = find_server(pid,tid);
    }
  if(!svr)
    {
      rcs_print_error("svr is NULL\n");
      return;
    }
  void (*old_sigint_handler)(int);
  old_sigint_handler = 0;
  old_sigint_handler = signal(SIGINT,SIG_DFL);
  
  CLIENT_TCP_PORT *new_client_port, *client_port_to_check;
  FD_ZERO (&read_fd_set);
  FD_ZERO (&write_fd_set);
  RCS_FD_SET (connection_socket, &read_fd_set);
  maxfdpl = connection_socket + 1;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "running server for TCP port %d (connection_socket = %d).\n",
		   dl_sa_get_port(ptr_to_server_socket_address),
		   connection_socket);


  CMS_SERVER_LOCAL_PORT *cslp=0;
  rcs_print_debug(PRINT_MISC,"svr->cms_local_ports->list_size=%d\n",
		  svr->cms_local_ports->list_size);
  cslp = (CMS_SERVER_LOCAL_PORT *) svr->cms_local_ports->get_head();
  while(cslp && !killed)
    {
      rcs_print_debug(PRINT_MISC,"cslp=%p,cslp->cms=%p\n",
		      (void*)cslp,(void*)cslp->cms);
      if(cslp->cms)
	{
	  rcs_print_debug(PRINT_MISC,
			  "cslp->cms->buffer_number=%ld,cslp->cms->blocking_support_enabled=%d\n",
			  cslp->cms->buffer_number,cslp->cms->blocking_support_enabled);
	}
      if(cslp->cms && 
	 ( cslp->cms->blocking_support_enabled ||
	   cslp->cms->private_server_object ))
	{
	  add_buffer_monitor(cslp->cms->buffer_number,
			     cslp->cms->private_server_object);
	}
      cslp = (CMS_SERVER_LOCAL_PORT *) svr->cms_local_ports->get_next();
    }
  if(buffer_monitors)
    {
      rcs_print_debug(PRINT_MISC,"buffer_monitors->list_size=%d\n",
		      buffer_monitors->list_size);
      TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *)
	buffer_monitors->get_head();
      while(tbm)
	{
	  rcs_print_debug(PRINT_MISC,"tbm=%p\n",(void*)tbm);
	  rcs_print_debug(PRINT_MISC,"tbm=%p,tbm->buffer_number=%ld\n",
			  (void*)tbm,tbm->buffer_number);
	  rcs_print_debug(PRINT_MISC,"tbm=%p,tbm->private_server_object=%d\n",
			  (void*)tbm,(int)tbm->private_server_object);
	  dl_socketpair(PF_UNIX,SOCK_STREAM,0,tbm->socket_pair);
	  rcs_print_debug(PRINT_MISC,"tbm=%p,tbm->socket_pair[0]=%d,tbm->socket_pair[1]=%d\n",
			  (void*)tbm,tbm->socket_pair[0],tbm->socket_pair[1]);
	  RCS_FD_SET(tbm->socket_pair[0],&read_fd_set);
	  if(tbm->socket_pair[0] + 1 > maxfdpl)
	    {
	      maxfdpl = tbm->socket_pair[0] + 1;
	    }
	  if(tbm->private_server_object)
	    {
	      tbm = (TCP_BUFFER_MONITOR *)
		buffer_monitors->get_next();
	      continue;
	    }
#ifdef POSIX_THREADS
	  pthread_t temp_thread_id=0L;
	  int pcret = pthread_create(&(temp_thread_id), // pthread_t *,
				     0, // pthread_attr_t * attr
				     buffer_monitor_thread_func, // void *(*start_routine)(void *)
				     tbm // void * arg
				     );
	  rcs_print_debug(PRINT_MISC,
			  "%s:%d pthread_create created %ld(0x%lX)\n",
			  __FILE__,__LINE__, 
			  (long) temp_thread_id,(unsigned long) temp_thread_id);
	  rcs_print_debug(PRINT_MISC,
			  "pcret=%d,thread=%ld\n",
			  pcret,(long)temp_thread_id);
	  tbm->threadId=temp_thread_id;
	  if(pcret != 0)
	    {
	      rcs_print_error("pthread_create failed returning %d (%s)\n",
			      pcret,strerror(pcret));
	    }
#elif defined(MS_WINDOWS_API)
	  tbm->threadId = (int) _beginthread ((void (__cdecl *) (void *)) buffer_monitor_thread_func,	// start_address
						   0,	// stack_size
						   tbm // arglist
					);
	  if (tbm->threadId < 1)
	    {
	      rcs_print_error("beginthread error\n");
	    }
#elif !defined(VXWORKS)
	  int fork_ret = fork();
	  rcs_print_debug(PRINT_MISC,"fork_ret=%d\n",fork_ret);
	  if(fork_ret)
	    {
	      tbm->threadId = fork_ret;
	    }
	  else
	    {
	      signal(SIGINT,SIG_DFL);
	      signal(SIGPIPE,SIG_DFL);
	      signal(SIGTERM,SIG_DFL);
	      buffer_monitor_thread_func(tbm);
	      exit(0);
	    }
#else
	  // VXWORKS
	  tbm->threadId = 
	    taskSpawn (NULL, 
		       get_cms_server_task_priority(), VX_FP_TASK,
		       get_cms_server_task_stack_size(),
		       (FUNCPTR) buffer_monitor_thread_func, (int) tbm,
		       0, 0, 0, 0,
		       0, 0, 0, 0, 0);
#endif
	  tbm = (TCP_BUFFER_MONITOR *)
	    buffer_monitors->get_next();
	}
    }

#ifdef POSIX_THREADS
  if(buffer_monitors)
    {
      rcs_print_debug(PRINT_MISC,"buffer_monitors->list_size=%d\n",
		      buffer_monitors->list_size);
      TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *)
	buffer_monitors->get_head();
      while(tbm)
	{
	  rcs_print_debug(PRINT_MISC,"tbm->started=%d,tbm->threadId=%ld,tbm->buffer_number=%ld\n",
			  tbm->started,(long)tbm->threadId, tbm->buffer_number);
	  if(tbm->private_server_object)
	    {
	      tbm = (TCP_BUFFER_MONITOR *)
		buffer_monitors->get_next();
	      continue;
	    }
#if 0
	  if(!tbm->started)
	    {
	      pthread_mutex_lock(&(tbm->mutex));
	      if(!tbm->started)
		{
		  pthread_cond_wait(&(tbm->started_cond),&(tbm->mutex));
		}
	      pthread_mutex_unlock(&(tbm->mutex));
	    }
#endif
	  while(!tbm->started) {
	    rcs_print_debug(PRINT_MISC,"tbm->started=%d,tbm->threadId=%ld,tbm->buffer_number=%ld\n",
			    tbm->started,(long)tbm->threadId, tbm->buffer_number);
	    esleep(0.05);
	  }
	  tbm = (TCP_BUFFER_MONITOR *)
	    buffer_monitors->get_next();
	}
    }
#endif

  if (dl_listen (connection_socket, 500) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connection_socket );
      sockerrstr = dl_get_last_socket_error_string(connection_socket,sockerrno,sockerrbuf,sizeof(sockerrbuf));
      rcs_print_error ("listen error: %d -- %s\n", sockerrno,sockerrstr);
      rcs_print_error ("TCP Server: error on call to listen for port %d.\n",
		       dl_sa_get_port(ptr_to_server_socket_address));
      return;
    }

  cms_server_count++;
  fd_set read_fd_set_copy, write_fd_set_copy;
  FD_ZERO (&read_fd_set_copy);
  FD_ZERO (&write_fd_set_copy);
  RCS_FD_SET (connection_socket, &read_fd_set_copy);
  if(old_sigint_handler != SIG_DFL &&
     old_sigint_handler != 0 &&
     old_sigint_handler != SIG_ERR)
    {
      signal(SIGINT,old_sigint_handler);
    }

#if !defined(DOS_WINDOWS) && !defined(MS_WINDOWS_API)
  signal (SIGPIPE, handle_pipe_error);
#endif
  rcs_print_debug(PRINT_MISC,"cms_server_count=%d,maxfdpl=%d, connection_socket=%d\n",cms_server_count,maxfdpl,connection_socket);

  int cycle_count=0;
  int last_num_deleted=0;

  if(cms_svr_sfunc) {
    svr_start_func stmp = cms_svr_sfunc;
    cms_svr_sfunc=0;
    rcs_print("calling cms_svr_sfunc()\n");
    (*stmp)();
    rcs_print("\nfinished cms_svr_sfunc()\n");
  }

  while (!killed)
    {
      cycle_count++;
#ifdef DEBUG_521
      printf("cycle_count=%d, time=%f\n",cycle_count,etime());
#endif
      if (polling_enabled)
	{
	  memcpy (&read_fd_set_copy, &read_fd_set, sizeof (fd_set));
	  memcpy (&write_fd_set_copy, &write_fd_set, sizeof (fd_set));
	  ((struct timeval*)ptr_to_select_timeout)->tv_sec = current_poll_interval_millis / 1000;
	  ((struct timeval*)ptr_to_select_timeout)->tv_usec =
	    (current_poll_interval_millis % 1000) * 1000;

	  select_ret =   
	    dl_select (maxfdpl, &read_fd_set, &write_fd_set, (fd_set *) NULL,
		       (timeval *) ptr_to_select_timeout);
	  ready_descriptors = select_ret;
	  if(killed)
	    {
	      rcs_print_debug(PRINT_MISC,"killed=%d\n",killed);
	      return;
	    }
	  update_subscriptions ();
	  update_single_var_logs ();
	  if (ready_descriptors == 0)
	    {
	      memcpy (&read_fd_set, &read_fd_set_copy, sizeof (fd_set));
	      memcpy (&write_fd_set, &write_fd_set_copy, sizeof (fd_set));
	      continue;
	    }
	}
      else
	{
	  select_ret =
	    dl_select (maxfdpl, &read_fd_set, &write_fd_set, (fd_set *) NULL,
		       (timeval *) NULL);
	  ready_descriptors = select_ret;
	}
      ready_descriptors = select_ret;
      if(killed)
	{
	  rcs_print_debug(PRINT_MISC,"killed=%d\n",killed);
	  return;
	}
      if (select_ret < 0)
	{
	  sockerrno = dl_get_last_socket_error_int( -1 );
	  sockerrstr = dl_get_last_socket_error_string(-1,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	  rcs_print_error ("server: select error.(errno = %d | %s)\n",
			   sockerrno,sockerrstr);
	  ready_descriptors=0;
	}
      if (NULL == client_ports)
	{
	  rcs_print_error ("CMS_SERVER: List of client ports is NULL.\n");
	  return;
	}
      client_port_to_check = (CLIENT_TCP_PORT *) client_ports->get_head ();
      while (NULL != client_port_to_check && ready_descriptors > 0)
	{
	  if (dl_fd_isset (client_port_to_check->socket_fd, &read_fd_set))
	    {
	      dl_ioctlsocket_fionread_ulp(client_port_to_check->socket_fd,&bytes_ready);
	      if (bytes_ready <= 0)
		{
		  client_port_to_check->remove_me=true;
		  if (dl_fd_isset (client_port_to_check->socket_fd, 
				   &write_fd_set))
		    {
		      ready_descriptors--;
		    }
		  client_port_to_check =
		    (CLIENT_TCP_PORT *) client_ports->get_next ();
		  ready_descriptors--;
		  continue;
		}
	      else
		{
		  handle_request (client_port_to_check);
		}
	      ready_descriptors--;
	    }
	  else
	    {
 	      RCS_FD_SET (client_port_to_check->socket_fd, &read_fd_set);
	    }
	  if (dl_fd_isset (client_port_to_check->socket_fd, &write_fd_set))
	    {
	      if(client_port_to_check->blocked_on_write) {
		handle_request (client_port_to_check);
	      }
	      if(!client_port_to_check->blocked_on_write)
		{
		  RCS_FD_CLR (client_port_to_check->socket_fd, &write_fd_set);
		}
	      ready_descriptors--;
	    }
	  else if(client_port_to_check->blocked_on_write)
	    {
 	      RCS_FD_SET (client_port_to_check->socket_fd, &write_fd_set);
	    }
	  client_port_to_check =
	    (CLIENT_TCP_PORT *) client_ports->get_next ();
	}
      if(buffer_monitors)
	{
	  RCS_LINKED_LIST_NODE *temp_tbm_cur_node=0;
	  temp_tbm_cur_node=0;
	  TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *)
	    buffer_monitors->get_head_with_external_current (&temp_tbm_cur_node);

	  while(tbm && ready_descriptors > 0)
	    {
	      if(tbm->clients_list)
		{
		  if(dl_fd_isset(tbm->socket_pair[0],&read_fd_set))
		    {
		      ready_descriptors--;
		      handle_buffer_updates(tbm);
		    }
		  else
		    {
		      if(tbm->clients_list &&
			 (tbm->new_request || tbm->recent_write_occured))
			{
			  handle_buffer_updates(tbm);
			}
		      RCS_FD_SET(tbm->socket_pair[0],&read_fd_set);
		    }
		}
	      else if(dl_fd_isset(tbm->socket_pair[0],&read_fd_set))
		{
		  RCS_FD_CLR(tbm->socket_pair[0],&read_fd_set);
		  ready_descriptors--;
		}
	      tbm = (TCP_BUFFER_MONITOR *)
		buffer_monitors->get_next_with_external_current (&temp_tbm_cur_node);
	    }
	}
      if (ready_descriptors > 0 &&
	  dl_fd_isset (connection_socket, &read_fd_set))
	{
	  ready_descriptors--;
	  int client_address_length;
	  new_client_port = new CLIENT_TCP_PORT (this);
	  client_address_length = dl_sa_len(new_client_port->address_ptr);
	  new_client_port->socket_fd = dl_accept (connection_socket,
						  dl_sa_addr(new_client_port->address_ptr),
						  &client_address_length);
	  make_tcp_socket_nonblocking(new_client_port->socket_fd);
	  new_client_port->rcid.long_id[0] = new_client_port->socket_fd;
	  new_client_port->rcid.long_id[1] = dl_sa_get_port(new_client_port->address_ptr);
	  new_client_port->rcid.use_me=true;
	  current_clients++;
	  if (current_clients > max_clients)
	    {
	      max_clients = current_clients;
	    }
	  if (new_client_port->socket_fd < 0)
	    {
	      sockerrno = dl_get_last_socket_error_int( -1 );
	      sockerrstr = dl_get_last_socket_error_string(-1,sockerrno,sockerrbuf,sizeof(sockerrbuf));
	      rcs_print_error ("server: accept error -- %d %s \n", 
			       sockerrno, sockerrstr);
	      continue;
	    }
	  rcs_print_debug (PRINT_SOCKET_CONNECT,
			   "Socket opened by host with IP address %s.\n",
			   dl_sa_get_host(new_client_port->address_ptr));
	  new_client_port->serial_number = 0;
	  new_client_port->blocking = 0;
	  if (NULL != client_ports)
	    {
	      client_ports->store_at_tail (new_client_port,
					   sizeof (new_client_port), 0);
	    }
	  if (maxfdpl < new_client_port->socket_fd + 1)
	    {
	      maxfdpl = new_client_port->socket_fd + 1;
	    }
	  RCS_FD_SET (new_client_port->socket_fd, &read_fd_set);
	}
      else
	{
	  RCS_FD_SET (connection_socket, &read_fd_set);
	}
      update_subscriptions ();
      update_single_var_logs();
      client_port_to_check = (CLIENT_TCP_PORT *) client_ports->get_head();
      int num_deleted=0;
      while (NULL != client_port_to_check)
	{
	  if(client_port_to_check->remove_me)
	    {
	      num_deleted++;
	      remove_client(client_port_to_check);
	      client_port_to_check = (CLIENT_TCP_PORT *) client_ports->get_head();
	      continue;
	    }
	  client_port_to_check =
	    (CLIENT_TCP_PORT *) client_ports->get_next();
	}
      if (0 != ready_descriptors 
	  && select_ret != -1)
	{
	  rcs_print_error ("%d descriptors ready but not serviced. (num_deleted=%d,%d : client_ports->size=%d, buffer_monitors->size=%d)\n",
			   ready_descriptors,
			   num_deleted,
			   last_num_deleted,
			   client_ports?client_ports->list_size:0,
			   buffer_monitors?buffer_monitors->list_size:0);
	}
      last_num_deleted = num_deleted;
      FD_ZERO (&read_fd_set);
      FD_ZERO (&write_fd_set);
      RCS_FD_SET (connection_socket, &read_fd_set);
      client_port_to_check = 
	(CLIENT_TCP_PORT *) client_ports->get_head();
      maxfdpl=connection_socket+1;
      while (NULL != client_port_to_check)
	{
	  RCS_FD_SET (client_port_to_check->socket_fd, &read_fd_set);
	  if(client_port_to_check->blocked_on_write) {
	    RCS_FD_SET (client_port_to_check->socket_fd, &write_fd_set);
	  }
	  if(client_port_to_check->socket_fd >= maxfdpl)
	    {
	      maxfdpl = client_port_to_check->socket_fd + 1;
	    }
	  client_port_to_check =
		(CLIENT_TCP_PORT *) client_ports->get_next();
	}
      if(buffer_monitors)
	{
	  RCS_LINKED_LIST_NODE *temp_tbm_cur_node=0;
	  temp_tbm_cur_node=0;
	  TCP_BUFFER_MONITOR *tbm = (TCP_BUFFER_MONITOR *)
	    buffer_monitors->get_head_with_external_current (&temp_tbm_cur_node);
	  while(tbm)
	    {
	      if(tbm->clients_list != 0)
		{
		  RCS_FD_SET(tbm->socket_pair[0],&read_fd_set);
		  if(tbm->socket_pair[0] >= maxfdpl)
		    {
		      maxfdpl = tbm->socket_pair[0] + 1;
		    }
		}
	      tbm = (TCP_BUFFER_MONITOR *)
		buffer_monitors->get_next_with_external_current (&temp_tbm_cur_node);
	    }
	}
    }
  rcs_print_debug(PRINT_MISC,"killed=%d\n",killed);
}


void
CMS_SERVER_REMOTE_TCP_PORT::get_request_header(CLIENT_TCP_PORT *ctp)
{
  if(ctp->get_state() == WAITING_FOR_REQUEST_HEADER)
    {
      memset(ctp->temp_buffer,0,20);
      if(ctp->Recv(ctp->temp_buffer,20) < 0)
	{
	  return;
	}
      ctp->set_state(WAITING_FOR_REQUEST_HEADER_COMPLETE);
    }
  else
    {
      ctp->retry_last_recv();
    }
  if(!ctp->something_blocked)
    {
      ctp->last_request_type = ctp->request_type;
      ctp->last_buffer_number = ctp->buffer_number;
      ctp->received_serial_number = ntoh_uint32_array_get(ctp->temp_buffer,0);
      ctp->request_type_long =  ntoh_uint32_array_get(ctp->temp_buffer,1);
      ctp->request_type = (REMOTE_CMS_REQUEST_TYPE) ctp->request_type_long;
      ctp->buffer_number = ntoh_uint32_array_get(ctp->temp_buffer,2);
      ctp->set_state(WAITING_FOR_REQUEST_TAIL);

      rcs_print_debug(PRINT_MISC,"CMS_SERVER_REMOTE_TCP_PORT::get_request_header() called -- ctp=%p,ctp->buffer_number=%ld,ctp->serial_number=%ld,ctp->socket_fd=%d,ctp->sockaddr_in=%s:%d,ctp->request_type=%d\n", 
		      (void*)ctp,ctp->buffer_number,ctp->serial_number,
		      ctp->socket_fd,
		      dl_sa_get_host(ctp->address_ptr),
		      dl_sa_get_port(ctp->address_ptr),
		      ctp->request_type);
      if (ctp->received_serial_number != ctp->serial_number)
	{
	  rcs_print_error
	    ("received_serial_number (%ld) does not equal expected serial number.(%ld)  -- ctp=%p,ctp->buffer_number=%ld,ctp->last_buffer_number=%ld, ctp->serial_number=%ld,ctp->socket_fd=%d,ctp->sockaddr_in=%s:%d,ctp->request_type=%d, ctp->last_request_type=%d, ctp->last_request_tail_size=%u, ctp->errors=%d, connection_port=%ld\n",
	     ctp->received_serial_number, 
	     ctp->serial_number,
	     (void*)ctp,
	     ctp->buffer_number,
	     ctp->last_buffer_number,
	     ctp->serial_number,
	     ctp->socket_fd,
	     dl_sa_get_host(ctp->address_ptr),
	     dl_sa_get_port(ctp->address_ptr),
	     ctp->last_request_type,
	     ctp->request_type,
	     ((unsigned int) ctp->request_tail_size),
	     ctp->errors,
	     connection_port
	     );
	  ctp->remove_me=true;
	  ctp->errors++;
	  return;
	}
      ctp->serial_number++;

#ifdef ENABLE_RCS_DIAG
      if (NULL != ctp->diag_info)
	{
	  ctp->diag_info->buffer_number = ctp->buffer_number;
	  svr->set_diag_info_from_client_id (&(ctp->rcid),ctp->diag_info);
	}
      else if (svr->diag_enabled)
	{
	  svr->reset_diag_info_from_client_id (&(ctp->rcid),ctp->buffer_number);
	}
#endif
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::get_request_tail(CLIENT_TCP_PORT *ctp)
{
  CMS_SERVER *svr;
  svr = cms_server_parent;
  if(ctp->get_state() == WAITING_FOR_REQUEST_TAIL)
    {
      ctp->request_tail_size = 0;
      ctp->set_state(WAITING_FOR_REQUEST_TAIL_COMPLETE);
      switch(ctp->request_type)
	{
	case REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE:
	case REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE:
	case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
	case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE:
	  ctp->request_tail_size=4;
	case REMOTE_CMS_READ_REQUEST_TYPE:
	case REMOTE_CMS_WRITE_REQUEST_TYPE:
	  ctp->total_subdivisions = 1;
	  if (max_total_subdivisions > 1)
	    {
	      ctp->total_subdivisions =
		svr->get_total_subdivisions (ctp->buffer_number);
	    }
	  if (ctp->total_subdivisions > 1)
	    {
	      ctp->request_tail_size += 4;
	    }
	  break;

	case REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE:
	  {
	    ctp->request_tail_size= 268;
	  }
	  break;

	case REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE:
	  {
	    ctp->request_tail_size= 0;
	  }
	  break;
	  
	case REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE:
	  {
	    ctp->request_tail_size= 0;
	  }
	  break;


	case REMOTE_CMS_GET_BUFFER_INFO_REQUEST_TYPE:
	  {
	    ctp->request_tail_size= 64;
	  }
	  break;

	default:
	  ctp->request_tail_size = 0;
	  break;
	}
      if(ctp->request_tail_size > sizeof(ctp->temp_buffer) - 20)
	{
	  rcs_print_error("ctp->request_tail_size =%u but sizeof(ctp->temp_buffer) only %u.\n",
			  ((unsigned int)ctp->request_tail_size),
			  ((unsigned int)sizeof(ctp->temp_buffer)));
	  ctp->request_tail_size = sizeof(ctp->temp_buffer) - 20;
	}
      if(ctp->request_tail_size > 0)
	{
	  ctp->Recv(ctp->temp_buffer+20, ctp->request_tail_size);
	}
      else
	{
	  ctp->request_tail_size = 0;
	  ctp->set_state(WAITING_FOR_REQUEST_DATA);
	}
    }
  else
    {
      if(ctp->request_tail_size > 0)
	{
	  ctp->retry_last_recv();
	}
      else
	{
	  ctp->request_tail_size = 0;
	  ctp->set_state(WAITING_FOR_REQUEST_DATA);
	}
    }
  if(!ctp->something_blocked)
    {
      ctp->request_tail_size = 0;
      ctp->set_state(WAITING_FOR_REQUEST_DATA);
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::get_request_data(CLIENT_TCP_PORT *ctp)
{
  CMS_SERVER *svr;
  svr = cms_server_parent;
  if(ctp->get_state() == WAITING_FOR_REQUEST_DATA)
    {
      ctp->set_state(WAITING_FOR_REQUEST_DATA_COMPLETE);
      ctp->request_data_size = 0;
      ctp->request_data_address = 0;
      ctp->request_subdiv=0;
      switch(ctp->request_type)
	{
	case REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE:
	  if (NULL == ctp->diag_info)
	    {
	      ctp->diag_info = new REMOTE_SET_DIAG_INFO_REQUEST ();
	    }
	  ctp->request_data_size = 68;
	  ctp->request_data_address = svr->set_diag_info_buf;
	  break;
	    
	case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
	case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE:
	case REMOTE_CMS_WRITE_REQUEST_TYPE:
	  if(0 == svr->write_req_ptr)
	    {
	      svr->write_req_ptr = new REMOTE_WRITE_REQUEST();
	    }
	  ctp->request_data_size = ntoh_uint32_array_get(ctp->temp_buffer,4);
	  if(((int)ctp->request_data_size) >  svr->maximum_cms_size)
	    {
	      rcs_print_error("ctp->request_data_size=%u but svr->maximum_cms_size is only %ld\n",
			      ((unsigned int)ctp->request_data_size),
			      svr->maximum_cms_size);
	      ctp->request_data_size=0;
	      ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	      ctp->request_type= NO_REMOTE_CMS_REQUEST;
	      ctp->requests_complete++;
	      ctp->remove_me=true;
	      return;
	    }
	  svr->write_req_ptr->size = (int) ctp->request_data_size;
	  ctp->request_data_address = svr->write_req_ptr->data;
	  break;

	case REMOTE_CMS_GET_KEYS_REQUEST_TYPE:
	  if(svr->get_keys_req_ptr == 0)
	    {
	      svr->get_keys_req_ptr = new REMOTE_GET_KEYS_REQUEST();
	    }
	  ctp->request_data_address = svr->get_keys_req_ptr->name;
	  ctp->request_data_size = 16;
	  break;
	  
	case REMOTE_CMS_LOGIN_REQUEST_TYPE:
	  if(svr->login_req_ptr ==0)
	    {
	      svr->login_req_ptr = new REMOTE_LOGIN_REQUEST();
	    }
	  ctp->request_data_address = svr->login_req_ptr->name;
	  ctp->request_data_size = 32;
	  break;

	default:
	  ctp->request_data_size = 0;
	  break;
	}
      if(ctp->request_data_size > 0 && ctp->request_data_address)
	{
	  ctp->Recv(ctp->request_data_address, ctp->request_data_size);
	}
      else
	{
	  ctp->set_state(WAITING_FOR_PROCESS_REQUEST);
	}
    }
  else
    {
      if(ctp->request_data_size > 0)
	{
	  ctp->retry_last_recv();
	}
      else
	{
	  ctp->set_state(WAITING_FOR_PROCESS_REQUEST);
	}
    }
  if(!ctp->something_blocked)
    {
      ctp->set_state(WAITING_FOR_PROCESS_REQUEST);
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::send_reply_header(CLIENT_TCP_PORT *ctp)
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER_REMOTE_TCP_PORT::send_reply_header()   called -- ctp=%p,ctp->buffer_number=%ld,ctp->serial_number=%ld,ctp->socket_fd=%d,ctp->sockaddr_in=%s:%d,ctp->request_type=%d,ctp->reply_header_address=%p,ctp->reply_header_size=%lu\n", 
		  (void*)ctp,ctp->buffer_number,ctp->serial_number,
		  ctp->socket_fd,
		  dl_sa_get_host(ctp->address_ptr),
		  dl_sa_get_port(ctp->address_ptr),
		  ctp->request_type,
		  (void*)ctp->reply_header_address,
		  (unsigned long) ctp->reply_header_size);
  if(ctp->get_state() == WAITING_FOR_REPLY_HEADER)
    {
      if(ctp->reply_header_size > 0 && ctp->reply_header_address)
	{
	  if(((unsigned long)ctp->serial_number) != 
	     ntoh_uint32_array_get(ctp->reply_header_address,0))
	    {
	      rcs_print_error("ctp->serial_number(%ld) = NOT EQUAL TO ntoh_uint32_array_get(ctp->reply_header_address,0)( or %lu) : ctp->reply_header_address=%p, ctp->temp_buffer=%p\n",
			      ctp->serial_number,
			      ntoh_uint32_array_get(ctp->reply_header_address,0),
			      (void*)ctp->reply_header_address,
			      (void*)ctp->temp_buffer);

	      hton_uint32_array_set(ctp->reply_header_address,0,(unsigned long) ctp->serial_number);
	    }
	  ctp->set_state(WAITING_FOR_REPLY_HEADER_COMPLETE);
	  ctp->Send(ctp->reply_header_address,ctp->reply_header_size);
	}
      else
	{
	  ctp->set_state(WAITING_FOR_REPLY_DATA);
	}
    }
  else
    {
      if(ctp->reply_header_size > 0 && ctp->reply_header_address)
	{
	  ctp->retry_last_send();
	}
      else
	{
	  ctp->set_state(WAITING_FOR_REPLY_DATA);
	}
    }
  if(!ctp->something_blocked)
    {
      ctp->set_state(WAITING_FOR_REPLY_DATA);
      ctp->reply_header_size = 0;
      ctp->reply_header_address = 0;
    }
  else
    {
      if(ctp->reply_data_size > 0 && ctp->reply_data_address 
	 && ctp->reply_data_address != ctp->extra_reply_data_buf)
	{
	  if(!ctp->extra_reply_data_buf)
	    {
	      ctp->extra_reply_data_size = ctp->reply_data_size;
	      ctp->extra_reply_data_size += (0x2000 - (ctp->extra_reply_data_size%0x2000));
	      ctp->extra_reply_data_buf = malloc(ctp->extra_reply_data_size);
	    }
	  else if(ctp->extra_reply_data_size < ctp->reply_data_size)
	    {
	      ctp->extra_reply_data_size = ctp->reply_data_size;
	      ctp->extra_reply_data_size += (0x2000 - (ctp->extra_reply_data_size%0x2000));
	      ctp->extra_reply_data_buf = realloc(ctp->extra_reply_data_buf, ctp->extra_reply_data_size);
	    }
	  memcpy(ctp->extra_reply_data_buf,ctp->reply_data_address, ctp->reply_data_size);
	  ctp->reply_data_address = ctp->extra_reply_data_buf;
	}
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::send_reply_data(CLIENT_TCP_PORT *ctp)
{
  if(ctp->get_state() == WAITING_FOR_REPLY_DATA)
    {
      if(ctp->reply_data_size > 0 && ctp->reply_data_address)
	{
	  ctp->set_state(WAITING_FOR_REPLY_DATA_COMPLETE);
	  ctp->Send(ctp->reply_data_address,ctp->reply_data_size);
	}
      else
	{
	  ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	  ctp->request_type= NO_REMOTE_CMS_REQUEST;
	  ctp->reply_data_size = 0;
	  ctp->reply_data_address = 0;
	  ctp->requests_complete++;
	  return;
	}
    }
  else
    {
      if(ctp->reply_data_size > 0 && ctp->reply_data_address)
	{
	  ctp->retry_last_send();
	}
    }
  if(!ctp->something_blocked)
    {
      ctp->set_state(WAITING_FOR_REQUEST_HEADER);
      ctp->request_type= NO_REMOTE_CMS_REQUEST;
      ctp->reply_data_size = 0;
      ctp->reply_data_address = 0;
      ctp->requests_complete++;
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::handle_request (CLIENT_TCP_PORT *ctp)
{
  CMS_SERVER *svr;
  svr = cms_server_parent;
  if (NULL == svr)
    {
      rcs_print_error
	("CMS_SERVER_REMOTE_TCP_PORT::handle_request() Cannot find server object for\n");
      return;
    }

  if (svr->using_passwd_file)
    {
      current_user_info = get_connected_user (ctp->socket_fd);
    }

  if (ctp->errors >= ctp->max_errors)
    {
      rcs_print_error ("Too many errors - closing connection(%d)\n",
		       ctp->socket_fd);
      ctp->remove_me=true;
      return;
    }

  ctp->something_blocked=false;
  int starting_requests_complete = ctp->requests_complete;
  int starting_errors = ctp->errors;
  int this_handle_request_loop_count=0;
  handle_request_count++;
  
  rcs_print_debug(PRINT_MISC,"CMS_SERVER_REMOTE_TCP_PORT::handle_request(), handle_request_count=%d,ctp=%p,ctp->socket_fd=%d,ctp->sockaddr_in=%s:%d\n", 
		  handle_request_count,(void*)ctp,
		  ctp->socket_fd,
		  dl_sa_get_host(ctp->address_ptr),
		  dl_sa_get_port(ctp->address_ptr));
  while(!ctp->something_blocked 
	&& ctp->requests_complete == starting_requests_complete
	&& ctp->errors == starting_errors 
	&& !killed)
    {
      this_handle_request_loop_count++;
      rcs_print_debug(PRINT_MISC,"this_handle_request_loop_count=%d\n",
		      this_handle_request_loop_count);
      rcs_print_debug(PRINT_MISC,"ctp->requests_complete=%d,ctp->errors=%d,ctp->state=%d,ctp->socket_fd=%d,ctp->size_of_unreceived_data=%lu,ctp->size_of_unsent_data=%lu,ctp->serial_number=%ld,ctp->buffer_number=%ld,ctp->request_type=%d\n",
		      ctp->requests_complete,ctp->errors,ctp->get_state(),ctp->socket_fd,
		      (unsigned long)ctp->size_of_unreceived_data,
		      (unsigned long)ctp->size_of_unsent_data,
		      ctp->serial_number,ctp->buffer_number,ctp->request_type);
      switch(ctp->get_state())
	{
	case WAITING_FOR_REQUEST_HEADER:
	case WAITING_FOR_REQUEST_HEADER_COMPLETE:
	  get_request_header(ctp);
	  break;
	  
	case WAITING_FOR_REQUEST_TAIL:
	case WAITING_FOR_REQUEST_TAIL_COMPLETE:
	  get_request_tail(ctp);
	  break;

	case WAITING_FOR_REQUEST_DATA:
	case WAITING_FOR_REQUEST_DATA_COMPLETE:
	  get_request_data(ctp);
	  break;

	case WAITING_FOR_PROCESS_REQUEST:
	case WAITING_FOR_PROCESS_REQUEST_COMPLETE:
	  process_received_request(ctp);
	  break;

	case WAITING_FOR_REPLY_HEADER:
	case WAITING_FOR_REPLY_HEADER_COMPLETE:
	  send_reply_header(ctp);
	  break;

	case WAITING_FOR_REPLY_DATA:
	case WAITING_FOR_REPLY_DATA_COMPLETE:
	  send_reply_data(ctp);
	  break;
	default:
	  rcs_print_error("invalid state %d\n",ctp->get_state());
	  ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	  ctp->request_type= NO_REMOTE_CMS_REQUEST;
	  ctp->errors++;
	  break;
	}
    }
}

void 
CMS_SERVER_REMOTE_TCP_PORT::set_tbm_recent_write_occured(long _buffer_number)
{
  TCP_BUFFER_MONITOR *tbm = 0;
  RCS_LINKED_LIST_NODE *tmp_cur_node=0;
  char x = 1;

#ifdef DEBUG_521
  printf("set_tbm_recent_write_occured buffer_number=%ld, time=%f\n",
 	 _buffer_number,etime());
#endif

  if(buffer_monitors)
    {
      tbm = (TCP_BUFFER_MONITOR *) buffer_monitors->get_head_with_external_current (&tmp_cur_node);
      while(tbm && !killed)
	{
	  if(tbm->buffer_number == _buffer_number)
	    {
	      tbm->recent_write_occured=true;
	      if(tbm->socket_pair[1] > 0 && 0 != tbm->clients_list)
		{
		  dl_send(tbm->socket_pair[1],&x,sizeof(x),0);
		}
	      break;
	    }
	  tbm = (TCP_BUFFER_MONITOR *) buffer_monitors->get_next_with_external_current (&tmp_cur_node);
	}
    }  
}


int
CMS_SERVER_REMOTE_TCP_PORT::add_ctp_to_buffer_monitor(CLIENT_TCP_PORT *ctp)
{
  TCP_BUFFER_MONITOR *tbm = 0;
  RCS_LINKED_LIST_NODE *tmp_cur_node=0;

  if(!ctp)
    {
      return -1;
    }
#if DEBUG_521
  printf("add_ctp_to_buffer_monitor buffer_number=%ld, time=%f\n",
 	 ctp->buffer_number,etime());
#endif

  if(buffer_monitors)
    {
      tbm = (TCP_BUFFER_MONITOR *) buffer_monitors->get_head_with_external_current (&tmp_cur_node);
      while(tbm && !killed)
	{
	  if(tbm->buffer_number == ctp->buffer_number)
	    {
	      break;
	    }
	  tbm = (TCP_BUFFER_MONITOR *) buffer_monitors->get_next_with_external_current (&tmp_cur_node);;
	}
    }
  if(!tbm)
    {
      return -1;
    }
  CLIENT_TCP_PORT *ctp_for_compare=0;
#ifdef POSIX_THREADS
  if(!tbm->private_server_object)
    {
      pthread_mutex_lock(&(tbm->mutex));
    }
#endif
  tbm->wakeup_count++;
  if(tbm->clients_list)
    {
      ctp_for_compare = (CLIENT_TCP_PORT *) tbm->clients_list->get_head();
      while(ctp_for_compare && !killed)
	{
	  if(ctp_for_compare == ctp)
	    {
	      break;
	    }
	  ctp_for_compare = (CLIENT_TCP_PORT *) tbm->clients_list->get_next();
	}
    }
  if(!ctp_for_compare)
    {
      if(!tbm->clients_list)
	{
	  tbm->clients_list = new RCS_LINKED_LIST();
	}
      tbm->clients_list->store_at_tail(ctp,sizeof(CLIENT_TCP_PORT), 0);
    }
  tbm->new_request=true;
#ifdef POSIX_THREADS
  if(!tbm->private_server_object)
    {
      pthread_cond_signal(&(tbm->cond));
      pthread_mutex_unlock(&(tbm->mutex));
    }
#endif
  return 0;
}


void
CMS_SERVER_REMOTE_TCP_PORT::process_received_request(CLIENT_TCP_PORT *ctp)
{
  CMS_SERVER *svr;
  svr = cms_server_parent;
  switch (ctp->request_type)
    {
    case REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE:
      {
	if (NULL == ctp->diag_info)
	  {
	    ctp->diag_info = new REMOTE_SET_DIAG_INFO_REQUEST ();
	  }
	ctp->diag_info->bytes_moved = 0.0;
	ctp->diag_info->buffer_number = ctp->buffer_number;
	memcpy (ctp->diag_info->process_name,
		svr->set_diag_info_buf, 16);
	memcpy (ctp->diag_info->host_sysinfo,
		svr->set_diag_info_buf + 16, 32);
	ctp->diag_info->pid =
	  ntoh_uint32_array_get(svr->set_diag_info_buf + 48,0);
	ctp->diag_info->c_num =
	  ntoh_uint32_array_get(svr->set_diag_info_buf + 52,0);
	memcpy (&(ctp->diag_info->rcslib_ver),
		svr->set_diag_info_buf + 56, 8);
	ctp->diag_info->reverse_flag =
	  *((int *) ((char *) svr->set_diag_info_buf + 64));
	if (ctp->diag_info->reverse_flag == 0x44332211)
	  {
	    ctp->diag_info->rcslib_ver =
	      (double) tcp_svr_reverse_double ((double)
					       ctp->
					       diag_info->rcslib_ver);
	  }
	ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	ctp->request_type= NO_REMOTE_CMS_REQUEST;
	ctp->requests_complete++;
      }
      break;

    case REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE:
      {
	REMOTE_GET_DIAG_INFO_REQUEST diagreq;
	diagreq.buffer_number = ctp->buffer_number;
	diagreq.clientid.long_id[0] = ctp->socket_fd;
	diagreq.clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	diagreq.clientid.use_me = 1;
	REMOTE_GET_DIAG_INFO_REPLY *diagreply = NULL;
	diagreply =
	  (REMOTE_GET_DIAG_INFO_REPLY *) svr->process_request (&diagreq);
	if (NULL == diagreply || NULL == diagreply->cdi)
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 24;
	    return;
	  }
	else
	  {
	    memset (ctp->temp_buffer, 0, 0x2000);
	    unsigned long dpi_offset = 32;
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) diagreply->status);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) diagreply->cdi->last_writer);
	    hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) diagreply->cdi->last_reader);
	    double curtime = etime ();
	    double reversed_temp = 0.0;
	    if (ctp->diag_info->reverse_flag == 0x44332211)
	      {
		reversed_temp =
		  (double) tcp_svr_reverse_double ((double) curtime);
		memcpy (ctp->temp_buffer + 16, &reversed_temp, 8);
	      }
	    else
	      {
		memcpy (ctp->temp_buffer + 16, &(curtime), 8);
	      }
	    int dpi_count = 0;
	    if (NULL != diagreply->cdi->dpis)
	      {
		CMS_DIAG_PROC_INFO *dpi =
		  (CMS_DIAG_PROC_INFO *) diagreply->cdi->dpis->get_head ();
		while ((dpi_offset < ((int) 0x2000 - sizeof (CMS_DIAG_PROC_INFO)))
		       && dpi != NULL && !killed)
		  {
		    dpi_count++;
		    memcpy (ctp->temp_buffer + dpi_offset, dpi->name, 16);
		    dpi_offset += 16;
		    memcpy (ctp->temp_buffer + dpi_offset, dpi->host_sysinfo, 32);
		    dpi_offset += 32;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->pid);
		    dpi_offset += 4;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->rcslib_ver);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset, &(dpi->rcslib_ver), 8);
		      }
		    dpi_offset += 8;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->access_type);
		    dpi_offset += 4;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->msg_id);
		    dpi_offset += 4;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->msg_size);
		    dpi_offset += 4;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->msg_type);
		    dpi_offset += 4;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->number_of_accesses);
		    dpi_offset += 4;
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) dpi->number_of_new_messages);
		    dpi_offset += 4;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->bytes_moved);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset, &(dpi->bytes_moved), 8);
		      }
		    dpi_offset += 8;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->
							   bytes_moved_across_socket);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset,
				&(dpi->bytes_moved_across_socket), 8);
		      }
		    dpi_offset += 8;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->last_access_time);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset,
				&(dpi->last_access_time), 8);
		      }
		    dpi_offset += 8;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->
							   first_access_time);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset,
				&(dpi->first_access_time), 8);
		      }
		    dpi_offset += 8;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->min_difference);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset, &(dpi->min_difference),
				8);
		      }
		    dpi_offset += 8;
		    if (ctp->diag_info->reverse_flag == 0x44332211)
		      {
			reversed_temp =
			  (double) tcp_svr_reverse_double ((double)
							   dpi->max_difference);
			memcpy (ctp->temp_buffer + dpi_offset, &reversed_temp, 8);
		      }
		    else
		      {
			memcpy (ctp->temp_buffer + dpi_offset, &(dpi->max_difference),
				8);
		      }
		    dpi_offset += 8;
		    int is_last_writer = (dpi == diagreply->cdi->last_writer_dpi);
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) is_last_writer);
		    dpi_offset += 4;
		    int is_last_reader = (dpi == diagreply->cdi->last_reader_dpi);
		    hton_uint32_array_set(ctp->temp_buffer+dpi_offset,0,(unsigned long) is_last_reader);
		    dpi_offset += 4;
		    dpi =
		      (CMS_DIAG_PROC_INFO *) diagreply->cdi->dpis->get_next ();
		  }
	      }
	    hton_uint32_array_set(ctp->temp_buffer,6,(unsigned long) dpi_count);
	    hton_uint32_array_set(ctp->temp_buffer,7,(unsigned long) dpi_offset);
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = dpi_offset;
	    return;
	  }
      }
      break;

    case REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE:
      {
	REMOTE_GET_BUF_NAME_REQUEST namereq;
	namereq.buffer_number = ctp->buffer_number;
	REMOTE_GET_BUF_NAME_REPLY *namereply = NULL;
	namereq.clientid.long_id[0] = ctp->socket_fd;
	namereq.clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	namereq.clientid.use_me = 1;
	namereply =
	  (REMOTE_GET_BUF_NAME_REPLY *) svr->process_request (&namereq);
	memset (ctp->temp_buffer, 0, 40);
	if (NULL != namereply)
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) namereply->status);
	    strncpy (ctp->temp_buffer + 8, namereply->name, 31);
	  }
	else
	  {
	    rcs_print_error("Server could not process request from %s port %d. (NULL == namereply)\n",
			    dl_sa_get_host(ctp->address_ptr),
			    dl_sa_get_port(ctp->address_ptr)
			    );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    memset(ctp->temp_buffer+8,0,31);
	  }
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 40;
      }
      break;

#ifdef NO_BLOCKING
    case REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE:
      {
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0); /* size */
	    hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) 0); /* write_id */
	    hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) 0); /* was_read */
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 20;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    ctp->something_blocked=false;
	    rcs_print_error("Blocking operation  %d attempted for buffer %d however this server was compiled without blocking support.\n", ctp->request_type, ctp->buffer_number);
	    ctp->errors++;
	    return;
	  }
      }
      break;

    case REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE:
    case REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE:
    case REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE:
    case REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE:
    case REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE:
      if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	{
	  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0); /* filler */
	  ctp->reply_header_address = ctp->temp_buffer;
	  ctp->reply_header_size = 12;
	  ctp->set_state(WAITING_FOR_REPLY_HEADER);
	  rcs_print_error("Blocking operation  %d attempted for buffer %d however this server was compiled without blocking support.\n", ctp->request_type, ctp->buffer_number);
	  ctp->something_blocked=false;
	  return;
	}
      break;

      // #ifdef NO_THREADS
#else

    case REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    if(ctp->total_subdivisions > 1)
	      {
		ctp->request_subdiv = ntoh_uint32_array_get(ctp->temp_buffer,6);
	      }
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) 0);
		hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 20;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		ctp->something_blocked=false;
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		return;
	      }
	  }
      }
      break;

    case REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    if(svr->get_msg_count_req_ptr ==0)
	      {
		svr->get_msg_count_req_ptr = new REMOTE_GET_MSG_COUNT_REQUEST();
	      }
	    svr->get_msg_count_req_ptr->buffer_number = ctp->buffer_number;
	    svr->get_msg_count_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	    svr->get_msg_count_req_ptr->clientid.long_id[1] = (long)
	      dl_sa_get_port(ctp->address_ptr);
	    svr->get_msg_count_req_ptr->clientid.use_me = 1;
	    svr->get_msg_count_req_ptr->subdiv =
	      ntoh_uint32_array_get(ctp->temp_buffer,3);
	    REMOTE_GET_MSG_COUNT_REPLY *get_msg_count_reply =
	      (REMOTE_GET_MSG_COUNT_REPLY *) svr->process_request (svr->get_msg_count_req_ptr);
	    if(get_msg_count_reply == NULL)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Get message count failed for buffer %ld.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    ctp->start_request_msg_count = get_msg_count_reply->count;
	  }
      }
      break;

    case REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    if(svr->get_read_count_req_ptr ==0)
	      {
		svr->get_read_count_req_ptr = new REMOTE_GET_READ_COUNT_REQUEST();
	      }
	    svr->get_read_count_req_ptr->buffer_number = ctp->buffer_number;
	    svr->get_read_count_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	    svr->get_read_count_req_ptr->clientid.long_id[1] = (long)
	      dl_sa_get_port(ctp->address_ptr);
	    svr->get_read_count_req_ptr->clientid.use_me = 1;
	    svr->get_read_count_req_ptr->subdiv =
	      ntoh_uint32_array_get(ctp->temp_buffer,3);
	    REMOTE_GET_READ_COUNT_REPLY *get_read_count_reply =
	      (REMOTE_GET_READ_COUNT_REPLY *) svr->process_request (svr->get_read_count_req_ptr);
	    if(get_read_count_reply == NULL)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Get message count failed for buffer %ld.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    ctp->start_request_read_count = get_read_count_reply->count;
	  }
      }
      break;
      
    case REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    long l1 = ntoh_uint32_array_get(ctp->temp_buffer,3);
	    switch(l1)
	      {
	      case 0:
		ctp->request_queue_length_over_or_under = UNDER;
		break;

	      case 1:
		ctp->request_queue_length_over_or_under = OVER;
		break;
		
	      default:
		rcs_print_error("bad value for request_queue_length_over_or_under = %ld\n",l1);
		break;
	      }
	    ctp->request_queue_length_to_wait_for = 
	       ntoh_uint32_array_get(ctp->temp_buffer,4);
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    if(svr->get_queue_length_req_ptr ==0)
	      {
		svr->get_queue_length_req_ptr = new REMOTE_GET_QUEUE_LENGTH_REQUEST();
	      }
	    svr->get_queue_length_req_ptr->buffer_number = ctp->buffer_number;
	    svr->get_queue_length_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	    svr->get_queue_length_req_ptr->clientid.long_id[1] = (long)
	      dl_sa_get_port(ctp->address_ptr);
	    svr->get_queue_length_req_ptr->clientid.use_me = 1;
	    svr->get_queue_length_req_ptr->subdiv =
	       ntoh_uint32_array_get(ctp->temp_buffer,3);
	    REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply =
	      (REMOTE_GET_QUEUE_LENGTH_REPLY *) svr->process_request (svr->get_queue_length_req_ptr);
	    if(get_queue_length_reply == NULL)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Get message count failed for buffer %ld.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    ctp->start_request_queue_length = get_queue_length_reply->queue_length;
	    ctp->max_request_queue_length = get_queue_length_reply->queue_length;
	    ctp->min_request_queue_length = get_queue_length_reply->queue_length;
	    ctp->last_request_queue_length = get_queue_length_reply->queue_length;
	  }
      }
      break;

    case REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    if(svr->get_is_clear_req_ptr ==0)
	      {
		svr->get_is_clear_req_ptr = new REMOTE_GET_IS_CLEAR_REQUEST();
	      }
	    svr->get_is_clear_req_ptr->buffer_number = ctp->buffer_number;
	    svr->get_is_clear_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	    svr->get_is_clear_req_ptr->clientid.long_id[1] = (long)
	      dl_sa_get_port(ctp->address_ptr);
	    svr->get_is_clear_req_ptr->clientid.use_me = 1;
	    svr->get_is_clear_req_ptr->subdiv =
	      ntoh_uint32_array_get(ctp->temp_buffer,3);
	    REMOTE_GET_IS_CLEAR_REPLY *get_is_clear_reply =
	      (REMOTE_GET_IS_CLEAR_REPLY *) svr->process_request (svr->get_is_clear_req_ptr);
	    if(get_is_clear_reply == NULL)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Get message count failed for buffer %ld.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	    ctp->start_request_is_clear = get_is_clear_reply->is_clear;
	  }
      }
      break;

    case REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE:
      {
	ctp->something_blocked=true;
	if(ctp->get_state() == WAITING_FOR_PROCESS_REQUEST)
	  {
	    ctp->set_state(WAITING_FOR_PROCESS_REQUEST_COMPLETE);
	    if(add_ctp_to_buffer_monitor(ctp) != 0)
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 12;
		ctp->set_state(WAITING_FOR_REPLY_HEADER);
		rcs_print_error("Blocking operation attempted for buffer %ld which does not appear to support blocking.\n", ctp->buffer_number);
		ctp->something_blocked=false;
		return;
	      }
	  }
      }
      break;

// #ifndef NO_THREADS
#endif

    case REMOTE_CMS_READ_REQUEST_TYPE:
      {
	if(0 == svr->read_req_ptr)
	  {
	    svr->read_req_ptr = new REMOTE_READ_REQUEST();
	  }
	svr->read_req_ptr->buffer_number = ctp->buffer_number;
	svr->read_req_ptr->access_type = 
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	svr->read_req_ptr->last_id_read =
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	svr->read_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->read_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->read_req_ptr->clientid.use_me = 1;
	if(ctp->total_subdivisions > 1)
	  {
	    svr->read_req_ptr->subdiv = ntoh_uint32_array_get(ctp->temp_buffer,5);
	    ctp->request_subdiv =svr->read_req_ptr->subdiv;
	  }
	else
	  {
	    svr->read_req_ptr->subdiv = 0;
	    ctp->request_subdiv =0;
	  }
	REMOTE_READ_REPLY *read_reply=
	  (REMOTE_READ_REPLY *) svr->process_request (svr->read_req_ptr);
	if (NULL == read_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == read_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) 0);
	    hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 20;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	else
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) read_reply->status);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) read_reply->size);
	    hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) read_reply->write_id);
	    hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) read_reply->was_read);
	    if (read_reply->size < (0x2000 - 20)
		&& read_reply->size > 0)
	      {
		memcpy (ctp->temp_buffer + 20, read_reply->data,
			read_reply->size);
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 20 +read_reply->size;
		ctp->reply_data_address = 0;
		ctp->reply_data_size = 0;
	      }
	    else
	      {
		ctp->reply_header_address = ctp->temp_buffer;
		ctp->reply_header_size = 20;
		if(read_reply->size > 0)
		  {
		    ctp->reply_data_address = read_reply->data;
		    ctp->reply_data_size = read_reply->size;
		  }
		else
		  {
		    ctp->reply_data_address = 0;
		    ctp->reply_data_size = 0;
		  }
	      }
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
      }
      break;

    case REMOTE_CMS_WRITE_REQUEST_TYPE:
      {
	if(0 == svr->write_req_ptr)
	  {
	    svr->write_req_ptr = new REMOTE_WRITE_REQUEST();
	  }
	svr->write_req_ptr->type = REMOTE_CMS_WRITE_REQUEST_TYPE;
	svr->write_req_ptr->buffer_number = ctp->buffer_number;
	svr->write_req_ptr->access_type =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	svr->write_req_ptr->size = 
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	svr->write_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->write_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->write_req_ptr->clientid.use_me = 1;
	if (max_total_subdivisions > 1)
	  {
	    ctp->total_subdivisions = svr->get_total_subdivisions (ctp->buffer_number);
	  }
	if (ctp->total_subdivisions > 1)
	  {
	    svr->write_req_ptr->subdiv =
	      ntoh_uint32_array_get(ctp->temp_buffer,5);
	  }
	else
	  {
	    svr->write_req_ptr->subdiv = 0;
	  }
	REMOTE_WRITE_REPLY *write_reply=
	  (REMOTE_WRITE_REPLY *) svr->process_request (svr->write_req_ptr);
	svr->write_req_ptr->clientid.use_me = 0;
	ctp->request_data_size = 0;
	ctp->request_data_address = 0;
	if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
	    || (write_reply && write_reply->confirm_write))
	  {
	    if (NULL == write_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d.(NULL == write_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	      }
	    else
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) write_reply->status);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) write_reply->was_read);
	      }
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	  }
	else
	  {
	    if (NULL == write_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d. (null == write_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
	      }	
	    ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	    ctp->request_type= NO_REMOTE_CMS_REQUEST;
	    ctp->requests_complete++;
	  }
      }
      break;

    case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
      {
	if(0 == svr->write_req_ptr)
	  {
	    svr->write_req_ptr = new REMOTE_WRITE_REQUEST();
	  }
	svr->write_req_ptr->type = REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE;
	svr->write_req_ptr->buffer_number = ctp->buffer_number;
	svr->write_req_ptr->access_type =
  	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	svr->write_req_ptr->size = 
  	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	svr->write_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->write_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->write_req_ptr->clientid.use_me = 1;
	svr->write_req_ptr->priority = 
  	  ntoh_uint32_array_get(ctp->temp_buffer,5);
	ctp->total_subdivisions = 1;
	if (max_total_subdivisions > 1)
	  {
	    ctp->total_subdivisions = svr->get_total_subdivisions (ctp->buffer_number);
	  }
	if (ctp->total_subdivisions > 1)
	  {
	    svr->write_req_ptr->subdiv = 
	      ntoh_uint32_array_get(ctp->temp_buffer,6);
	  }
	else
	  {
	    svr->write_req_ptr->subdiv = 0;
	  }
	REMOTE_WRITE_REPLY *write_with_priority_reply =
	  (REMOTE_WRITE_REPLY *) svr->process_request (svr->write_req_ptr);
	svr->write_req_ptr->clientid.use_me = 0;
	if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
	    || write_with_priority_reply->confirm_write)
	  {
	    if (NULL == write_with_priority_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d.(NULL == write_with_priority_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	      }
	    else
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) write_with_priority_reply->status);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) write_with_priority_reply->was_read);
	      }
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	  }
	else
	  {
	    if (NULL == write_with_priority_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d.(NULL == write_with_priority_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
	      }
	    ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	    ctp->request_type= NO_REMOTE_CMS_REQUEST;
	    ctp->requests_complete++;
	  }
      }
      break;


    case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE: 
      {
	if(0 == svr->write_req_ptr)
	  {
	    svr->write_req_ptr = new REMOTE_WRITE_REQUEST();
	  }
	svr->write_req_ptr->type = REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE;
	svr->write_req_ptr->buffer_number = ctp->buffer_number;
	svr->write_req_ptr->access_type =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	svr->write_req_ptr->size = 
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	svr->write_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->write_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->write_req_ptr->clientid.use_me = 1;
	
	svr->write_req_ptr->bitwise_op = 
	  ntoh_uint32_array_get(ctp->temp_buffer,5);
	ctp->total_subdivisions = 1;
	if (max_total_subdivisions > 1)
	  {
	    ctp->total_subdivisions = svr->get_total_subdivisions (ctp->buffer_number);
	  }
	if (ctp->total_subdivisions > 1)
	  {
	    svr->write_req_ptr->subdiv = 
	      ntoh_uint32_array_get(ctp->temp_buffer,6);
	  }
	else
	  {
	    svr->write_req_ptr->subdiv = 0;
	  }
	REMOTE_WRITE_REPLY *write_with_bitwise_op_reply=
	  (REMOTE_WRITE_REPLY *) svr->process_request (svr->write_req_ptr);
	svr->write_req_ptr->clientid.use_me = 0;
	if ((min_compatible_version < 2.58 && min_compatible_version > 1e-6)
	    || write_with_bitwise_op_reply->confirm_write)
	  {
	    if (NULL == write_with_bitwise_op_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d.(NULL == write_with_bitwise_op_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	      }
	    else
	      {
		hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
		hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) write_with_bitwise_op_reply->status);
		hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) write_with_bitwise_op_reply->was_read);
	      }
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	  }
	else
	  {
	    if (NULL == write_with_bitwise_op_reply)
	      {
		rcs_print_error ("Server could not process request from %s port %d.(NULL == write_with_bitwise_op_reply)\n",
				 dl_sa_get_host(ctp->address_ptr),
				 dl_sa_get_port(ctp->address_ptr)
				 );
	      }
	    ctp->set_state(WAITING_FOR_REQUEST_HEADER);
	    ctp->request_type= NO_REMOTE_CMS_REQUEST;
	    ctp->requests_complete++;
	  }
      }
      break;

    case REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE:
      {
	if( svr->check_if_read_req_ptr == 0)
	  {
	    svr->check_if_read_req_ptr = new REMOTE_CHECK_IF_READ_REQUEST();
	  }
	svr->check_if_read_req_ptr->buffer_number = ctp->buffer_number;
	  svr->check_if_read_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->check_if_read_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->check_if_read_req_ptr->clientid.use_me = 1;
	svr->check_if_read_req_ptr->subdiv =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_CHECK_IF_READ_REPLY *check_if_read_reply =
	  (REMOTE_CHECK_IF_READ_REPLY *) svr->process_request (svr->
								  check_if_read_req_ptr);
	if (NULL == check_if_read_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == check_if_read_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) check_if_read_reply->status);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) check_if_read_reply->was_read);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
    break;


    case REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE:
      {
	if(svr->get_msg_count_req_ptr ==0)
	  {
	    svr->get_msg_count_req_ptr = new REMOTE_GET_MSG_COUNT_REQUEST();
	  }
	svr->get_msg_count_req_ptr->buffer_number = ctp->buffer_number;
	svr->get_msg_count_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->get_msg_count_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->get_msg_count_req_ptr->clientid.use_me = 1;
	svr->get_msg_count_req_ptr->subdiv =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_GET_MSG_COUNT_REPLY *get_msg_count_reply =
	  (REMOTE_GET_MSG_COUNT_REPLY *) svr->process_request (svr->
								  get_msg_count_req_ptr);
	if (NULL == get_msg_count_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == get_msg_count_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_msg_count_reply->status);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_msg_count_reply->count);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;


    case REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE:
      {
	if(svr->get_msg_type_req_ptr ==0)
	  {
	    svr->get_msg_type_req_ptr = new REMOTE_GET_MSG_TYPE_REQUEST();
	  }
	svr->get_msg_type_req_ptr->buffer_number = ctp->buffer_number;
	svr->get_msg_type_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->get_msg_type_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->get_msg_type_req_ptr->clientid.use_me = 1;
	svr->get_msg_type_req_ptr->subdiv =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_GET_MSG_TYPE_REPLY *get_msg_type_reply =
	  (REMOTE_GET_MSG_TYPE_REPLY *) svr->process_request (svr->
								  get_msg_type_req_ptr);
	if (NULL == get_msg_type_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == get_msg_type_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_msg_type_reply->status);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_msg_type_reply->msg_type);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;


    case REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE:
      {
	if(svr->get_queue_length_req_ptr ==0)
	  {
	    svr->get_queue_length_req_ptr = new REMOTE_GET_QUEUE_LENGTH_REQUEST();
	  }
	svr->get_queue_length_req_ptr->buffer_number = ctp->buffer_number;
	svr->get_queue_length_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->get_queue_length_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->get_queue_length_req_ptr->clientid.use_me = 1;
	svr->get_queue_length_req_ptr->subdiv =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_GET_QUEUE_LENGTH_REPLY *get_queue_length_reply =
	  (REMOTE_GET_QUEUE_LENGTH_REPLY *) svr->process_request (svr->
								     get_queue_length_req_ptr);
	if (NULL == get_queue_length_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == get_queue_length_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_queue_length_reply->status);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_queue_length_reply->queue_length);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;


    case REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE:
      {
	if(svr->get_space_available_req_ptr ==0)
	  {
	    svr->get_space_available_req_ptr = new REMOTE_GET_SPACE_AVAILABLE_REQUEST();
	  }
	svr->get_space_available_req_ptr->buffer_number = ctp->buffer_number;
	svr->get_space_available_req_ptr->clientid.long_id[0] = 
	  ctp->socket_fd;
	svr->get_space_available_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->get_space_available_req_ptr->clientid.use_me = 1;
	svr->get_space_available_req_ptr->subdiv =
	      ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_GET_SPACE_AVAILABLE_REPLY *get_space_available_reply =
	  (REMOTE_GET_SPACE_AVAILABLE_REPLY *) svr->
	  process_request (svr->get_space_available_req_ptr);
	if (NULL == get_space_available_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == get_space_available_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 12;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) get_space_available_reply->status);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) get_space_available_reply->space_available);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;


    case REMOTE_CMS_CLEAR_REQUEST_TYPE:
      {
	if(svr->clear_req_ptr ==0)
	  {
	    svr->clear_req_ptr = new REMOTE_CLEAR_REQUEST();
	  }
	svr->clear_req_ptr->buffer_number = ctp->buffer_number;
	svr->clear_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->clear_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->clear_req_ptr->clientid.use_me = 1;
	svr->clear_req_ptr->subdiv = 
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	REMOTE_CLEAR_REPLY *clear_reply =
	  (REMOTE_CLEAR_REPLY *) svr->process_request (svr->clear_req_ptr);
	if (NULL == clear_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == clear_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) CMS_SERVER_SIDE_ERROR);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 8;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) clear_reply->status);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 8;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;

    case REMOTE_CMS_CLEAN_REQUEST_TYPE:
      if(ctp)
	{
	  ctp->requests_complete++;
	}
      killed=true;
      svr->set_spawner_pid_to_server_pid();
      svr->kill_server ();
      break;

    case REMOTE_CMS_CLOSE_CHANNEL_REQUEST_TYPE:
      ctp->remove_me=true;
      break;

    case REMOTE_CMS_GET_KEYS_REQUEST_TYPE:
      {
	if(svr->get_keys_req_ptr == 0)
	  {
	    svr->get_keys_req_ptr = new REMOTE_GET_KEYS_REQUEST();
	  }
	svr->get_keys_req_ptr->buffer_number = ctp->buffer_number;
	svr->get_keys_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->get_keys_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->get_keys_req_ptr->clientid.use_me = 1;
	REMOTE_GET_KEYS_REPLY *get_keys_reply =
	  (REMOTE_GET_KEYS_REPLY *) svr->process_request (svr->get_keys_req_ptr);
	if (NULL == get_keys_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == get_keys_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    memset (ctp->temp_buffer, 0, 20);
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    svr->gen_random_key (((char *) ctp->temp_buffer) + 4, 2);
	    svr->gen_random_key (((char *) ctp->temp_buffer) + 12, 2);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 20;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	else
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    memcpy (((char *) ctp->temp_buffer) + 4, get_keys_reply->key1,
		    8);
	    memcpy (((char *) ctp->temp_buffer) + 12,get_keys_reply->key2,
		    8);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 20;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
      }
      break;

    case REMOTE_CMS_LOGIN_REQUEST_TYPE:
      {
	if(svr->login_req_ptr ==0)
	  {
	    svr->login_req_ptr = new REMOTE_LOGIN_REQUEST();
	  }
	svr->login_req_ptr->buffer_number = ctp->buffer_number;
	svr->login_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->login_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->login_req_ptr->clientid.use_me = 1;
	REMOTE_LOGIN_REPLY *login_reply =
	  (REMOTE_LOGIN_REPLY *) svr->process_request (svr->login_req_ptr);
	if (NULL == login_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == login_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 8;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	else
	  {
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) login_reply->success);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 8;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
      }
      break;

    case REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE:
      {
	if(svr->set_subscription_req_ptr ==0)
	  {
	    svr->set_subscription_req_ptr = new REMOTE_SET_SUBSCRIPTION_REQUEST();
	  }
	svr->set_subscription_req_ptr->buffer_number = ctp->buffer_number;
	svr->set_subscription_req_ptr->clientid.long_id[0] = ctp->socket_fd;
	svr->set_subscription_req_ptr->clientid.long_id[1] = (long)
	  dl_sa_get_port(ctp->address_ptr);
	svr->set_subscription_req_ptr->clientid.use_me = 1;
	svr->set_subscription_req_ptr->subscription_type =
	  ntoh_uint32_array_get(ctp->temp_buffer,3);
	svr->set_subscription_req_ptr->poll_interval_millis =
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	REMOTE_SET_SUBSCRIPTION_REPLY *set_subscription_reply =
	  (REMOTE_SET_SUBSCRIPTION_REPLY *) svr->process_request (svr->
								     set_subscription_req_ptr);
	if (NULL == set_subscription_reply)
	  {
	    rcs_print_error ("Server could not process request from %s port %d.(NULL == set_subscription_reply)\n",
			     dl_sa_get_host(ctp->address_ptr),
			     dl_sa_get_port(ctp->address_ptr)
			     );
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 0); /* not successful */
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 8;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
	else
	  {
	    if (set_subscription_reply->success)
	      {
		if (svr->set_subscription_req_ptr->subscription_type ==
		    CMS_POLLED_SUBSCRIPTION
		    || svr->set_subscription_req_ptr->subscription_type ==
		    CMS_VARIABLE_SUBSCRIPTION)
		  {
		    add_subscription_client (ctp->buffer_number,
					     svr->set_subscription_req_ptr->
					     subscription_type,
					     svr->set_subscription_req_ptr->
					     poll_interval_millis,
					     ctp);
		  }
		if (svr->set_subscription_req_ptr->subscription_type ==
		    CMS_NO_SUBSCRIPTION)
		  {
		    remove_subscription_client (ctp,
						ctp->buffer_number);
		  }
	      }
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) set_subscription_reply->success);
	    /*  successful ? */
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 8;
	    ctp->reply_data_address = 0;
	    ctp->reply_data_size = 0;
	    ctp->set_state(WAITING_FOR_REPLY_HEADER);
	    return;
	  }
      }
      break;


    case REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE:
      {
	remove_subscription_client (ctp,
				    ctp->buffer_number);
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 1);
	/*  successful ? */
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 8;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
	return;
      }
      break;

    case REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE:
      {
	TCP_SINGLE_VAR_LOG_MONITOR *vlm = new TCP_SINGLE_VAR_LOG_MONITOR();
	vlm->ctp = ctp;
	vlm->rport = this;
	vlm->buffer_number = ctp->buffer_number;
	strncpy(vlm->varname,ctp->temp_buffer+20,256);
	vlm->min_period_millis =
	  ntoh_uint32_array_get((ctp->temp_buffer+276),0);
	vlm->max_log_item_list_length =
	  ntoh_uint32_array_get((ctp->temp_buffer+276),1);
	vlm->message_type =
	  ntoh_uint32_array_get((ctp->temp_buffer+276),2);
	vlm->reply_buffer = (char *)
	  malloc(32 + 16*vlm->max_log_item_list_length);
	vlm->log_item_list = (struct single_var_log_item *) 
	  malloc(sizeof(struct single_var_log_item)*vlm->max_log_item_list_length);
	if(!single_var_log_monitors)
	  {
	    single_var_log_monitors = new RCS_LINKED_LIST();
	  }
	vlm->list_id = 
	  single_var_log_monitors->store_at_tail(vlm,sizeof(TCP_SINGLE_VAR_LOG_MONITOR),0);	
	if(!ctp->single_var_log_monitors)
	  {
	    ctp->single_var_log_monitors = new RCS_LINKED_LIST();
	  }
	vlm->ctp_list_id = 
	  ctp->single_var_log_monitors->store_at_tail(vlm,sizeof(TCP_SINGLE_VAR_LOG_MONITOR),0);	
	recalculate_polling_interval();
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 0);
	hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) vlm->list_id);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 12;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;

    case REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE:
      {
	TCP_SINGLE_VAR_LOG_MONITOR *vlm = 0;
	int vlm_list_id = 
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	if(single_var_log_monitors && vlm_list_id > 0)
	  {
	      vlm = 
		(TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_by_id(vlm_list_id);
	  }
	if(vlm)
	  {
	    int items_to_send = vlm->log_count;
	    int reply_buf_index = 2;
	    hton_uint32_array_set(vlm->reply_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(vlm->reply_buffer,1,(unsigned long) 0);
	    if(vlm->log_count > vlm->max_log_item_list_length)
	      {
		items_to_send = vlm->max_log_item_list_length;
		hton_uint32_array_set(vlm->reply_buffer,2,(unsigned long) items_to_send);
		hton_uint32_array_set(vlm->reply_buffer,3,(unsigned long) vlm->log_count);
		for(int i = vlm->head_index; i <  vlm->max_log_item_list_length; i++)
		  {

		    hton_float64_array_set(vlm->reply_buffer,reply_buf_index,vlm->log_item_list[i].value);
		    reply_buf_index++;
		    hton_float64_array_set(vlm->reply_buffer,reply_buf_index,vlm->log_item_list[i].timestamp);
		    reply_buf_index++;
		  }
	      }
	    else
	      {
		hton_uint32_array_set(vlm->reply_buffer,2,(unsigned long) items_to_send);
		hton_uint32_array_set(vlm->reply_buffer,3,(unsigned long) vlm->log_count);
	      }	    
	    for(int i = 0; i <  vlm->head_index; i++)
	      {
		hton_float64_array_set(vlm->reply_buffer,reply_buf_index,vlm->log_item_list[i].value);
		reply_buf_index++;
		hton_float64_array_set(vlm->reply_buffer,reply_buf_index,vlm->log_item_list[i].timestamp);
		reply_buf_index++;
	      }
	    ctp->reply_header_address = vlm->reply_buffer;
	    ctp->reply_header_size = 16*(1+items_to_send);
	    vlm->head_index = 0;
	    vlm->log_count = 0;
	  }
	else
	  {
	    rcs_print_error("Can't get single var log for log id %d\n",
			    vlm_list_id);
	    hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	    hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 0);
	    hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) 0);
	    hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) 0);
	    ctp->reply_header_address = ctp->temp_buffer;
	    ctp->reply_header_size = 16;
	  }
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;

    case REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE:
      {
	TCP_SINGLE_VAR_LOG_MONITOR *vlm = 0;
	int vlm_list_id = 
	  ntoh_uint32_array_get(ctp->temp_buffer,4);
	if(single_var_log_monitors && vlm_list_id > 0)
	  {
	    vlm = 
	      (TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_by_id(vlm_list_id);
	    single_var_log_monitors->delete_node(vlm_list_id);
	    if(vlm)
	      {
		if(vlm->ctp_list_id > 0 && vlm->ctp &&
		   vlm->ctp->single_var_log_monitors)
		  {
		    vlm->ctp->single_var_log_monitors->delete_node(vlm->ctp_list_id);
		  }
		vlm->ctp=0;
		vlm->list_id=-1;
		vlm->ctp_list_id=-1;
		delete vlm;
	      }
	  }
	else
	  {
	    rcs_print_error("Can't close single var log for log id %d\n",
			    vlm_list_id);
	  }
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) 0);
	recalculate_polling_interval();
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->reply_header_size = 8;
	ctp->reply_data_address = 0;
	ctp->reply_data_size = 0;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;

    case REMOTE_CMS_GET_BUFFER_LIST_REQUEST_TYPE:
      {
	ctp->reply_header_size = 8;
	ctp->reply_data_address = (void *) nmlGetStaticBufferList(',');
	rcs_print("BufferList Requested = %s\n",
		  ((const char *) ctp->reply_data_address));
	ctp->reply_data_size = strlen(((const char *)ctp->reply_data_address))+1;
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) ctp->reply_data_size);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;


    case REMOTE_CMS_GET_BUFFER_INFO_REQUEST_TYPE:
      {
	ctp->reply_header_size = 8;
	ctp->reply_data_address = (void *) nmlGetStaticBufferInfo(ctp->temp_buffer+20);
	rcs_print("BufferList Requested = %s\n",
		  ((const char *) ctp->reply_data_address));
	ctp->reply_data_size = strlen(((const char *)ctp->reply_data_address))+1;
	hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) ctp->serial_number);
	hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) ctp->reply_data_size);
	ctp->reply_header_address = ctp->temp_buffer;
	ctp->set_state(WAITING_FOR_REPLY_HEADER);
      }
      break;

    default:
      ctp->errors++;
      rcs_print_error ("Unrecognized request type received.(%d)\n",
		       ctp->request_type);
      break;
    }

#ifdef ENABLE_RCS_DIAG
  if (NULL != ctp->diag_info &&
      NULL != svr->last_local_port_used && svr->diag_enabled)
    {
      if (NULL != svr->last_local_port_used->cms)
	{
	  if (NULL !=
	      svr->last_local_port_used->cms->handle_to_global_data)
	    {
	      ctp->diag_info->bytes_moved =
		svr->last_local_port_used->cms->handle_to_global_data->
		get_total_bytes_moved();
	    }
	}
    }
#endif
}


void
CMS_SERVER_REMOTE_TCP_PORT::add_subscription_client (long buffer_number,
						     int subscription_type,
						     int poll_interval_millis,
						     CLIENT_TCP_PORT * clnt)
{
  if (NULL == subscription_buffers)
    {
      subscription_buffers = new RCS_LINKED_LIST ();
    }
  if (NULL == subscription_buffers)
    {
      rcs_print_error ("Can`t create subscription_buffers list.\n");
    }

  TCP_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info && !killed)
    {
      if (buf_info->buffer_number == buffer_number)
	{
	  break;
	}
      buf_info =
	(TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }
  if (NULL == buf_info)
    {
      buf_info = new TCP_BUFFER_SUBSCRIPTION_INFO ();
      buf_info->buffer_number = buffer_number;
      buf_info->sub_clnt_info = new RCS_LINKED_LIST ();
      buf_info->list_id =
	subscription_buffers->store_at_tail (buf_info, sizeof (*buf_info), 0);
    }
  buf_info->min_last_id = 0;
  if (NULL == clnt->subscriptions)
    {
      clnt->subscriptions = new RCS_LINKED_LIST ();
    }
  TCP_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
    (TCP_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_head ();
  while (temp_clnt_info != NULL && !killed)
    {
      if (temp_clnt_info->buffer_number == buffer_number)
	{
	  break;
	}
      temp_clnt_info =
	(TCP_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_next ();
    }
  if (NULL == temp_clnt_info)
    {
      temp_clnt_info = new TCP_CLIENT_SUBSCRIPTION_INFO ();
      temp_clnt_info->last_sub_sent_time = 0.0;
      temp_clnt_info->buffer_number = buffer_number;
      temp_clnt_info->subscription_paused = 0;
      temp_clnt_info->last_id_read = 0;
      temp_clnt_info->sub_buf_info = buf_info;
      temp_clnt_info->clnt_port = clnt;
      temp_clnt_info->last_sub_sent_time = etime ();
      temp_clnt_info->subscription_list_id =
	clnt->subscriptions->store_at_tail (temp_clnt_info,
					    sizeof (*temp_clnt_info), 0);
      buf_info->sub_clnt_info->store_at_tail (temp_clnt_info,
					      sizeof (*temp_clnt_info), 0);
    }
  temp_clnt_info->subscription_type = subscription_type;
  temp_clnt_info->poll_interval_millis = poll_interval_millis;
  recalculate_polling_interval ();
}


void
CMS_SERVER_REMOTE_TCP_PORT::remove_subscription_client (CLIENT_TCP_PORT *
							clnt,
							long buffer_number)
{
  TCP_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
    (TCP_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_head ();
  while (temp_clnt_info != NULL)
    {
      if (temp_clnt_info->buffer_number == buffer_number)
	{
	  if (NULL != temp_clnt_info->sub_buf_info)
	    {
	      if (NULL != temp_clnt_info->sub_buf_info->sub_clnt_info)
		{
		  temp_clnt_info->sub_buf_info->sub_clnt_info->
		    delete_node (temp_clnt_info->subscription_list_id);
		  if (temp_clnt_info->sub_buf_info->sub_clnt_info->
		      list_size == 0)
		    {
		      subscription_buffers->delete_node (temp_clnt_info->
							 sub_buf_info->
							 list_id);
		      delete temp_clnt_info->sub_buf_info->sub_clnt_info;
		      temp_clnt_info->sub_buf_info->sub_clnt_info = NULL;
		      delete temp_clnt_info->sub_buf_info;
		      temp_clnt_info->sub_buf_info = NULL;
		    }
		}
	    }
	  delete temp_clnt_info;
	  temp_clnt_info = NULL;
	  break;
	}
      temp_clnt_info =
	(TCP_CLIENT_SUBSCRIPTION_INFO *) clnt->subscriptions->get_next ();
    }
  recalculate_polling_interval ();
}

void
CMS_SERVER_REMOTE_TCP_PORT::recalculate_polling_interval ()
{
  int min_poll_interval_millis = 30000;
  polling_enabled = 0;
  if(subscription_buffers)
    {
      TCP_BUFFER_SUBSCRIPTION_INFO *buf_info =
	(TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
      while (NULL != buf_info && !killed)
	{
	  TCP_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
	    (TCP_CLIENT_SUBSCRIPTION_INFO *) buf_info->sub_clnt_info->get_head ();
	  while (temp_clnt_info != NULL && !killed)
	    {
	      if (temp_clnt_info->poll_interval_millis < min_poll_interval_millis
		  && temp_clnt_info->subscription_type == CMS_POLLED_SUBSCRIPTION)
		{
		  min_poll_interval_millis = temp_clnt_info->poll_interval_millis;
		  polling_enabled = 1;
		}
	      temp_clnt_info =
		(TCP_CLIENT_SUBSCRIPTION_INFO *)
		buf_info->sub_clnt_info->get_next ();
	    }
	  buf_info =
	    (TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
	}
    }
  if(single_var_log_monitors && !killed)
    {
      TCP_SINGLE_VAR_LOG_MONITOR *vlm = 
	(TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_head();
      while(vlm && !killed)
	{
	  if(vlm->min_period_millis < min_poll_interval_millis)
	    {
	      min_poll_interval_millis = vlm->min_period_millis;
	      polling_enabled = 1;
	    }
	  vlm = 
	    (TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_next();
	}
    }
  
  if (min_poll_interval_millis >= ((int) (clk_tck () * 1000.0)))
    {
      current_poll_interval_millis = min_poll_interval_millis;
    }
  else
    {
      current_poll_interval_millis = ((int) (clk_tck () * 1000.0));
    }
  ((struct timeval*)ptr_to_select_timeout)->tv_sec = current_poll_interval_millis / 1000;
  ((struct timeval*)ptr_to_select_timeout)->tv_usec = (current_poll_interval_millis % 1000) * 1000;
  // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
 dtimeout = (current_poll_interval_millis + 10) / 1000.0;
  if (dtimeout < 0.5)
    {
      dtimeout = 0.5;
    }
}


void
CMS_SERVER_REMOTE_TCP_PORT::update_single_var_logs ()
{
  CMS_SERVER *svr;
  if(!single_var_log_monitors || 
     single_var_log_monitors->list_size < 1)
    {
      return;
    }

  svr = cms_server_parent;
  if(!svr)
    {
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      DWORD pid = GetCurrentProcessId ();
      DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
      int pid = taskIdSelf ();
      
      int tid = 0;
#else
      pid_t pid = getpid ();
      pid_t tid = 0;
#endif
#endif
      cms_server_parent = svr = find_server(pid,tid);
    }

  if(!single_var_log_buffers_read  || 
     sizeof_single_var_log_buffers_read < single_var_log_monitors->list_size)
    {
      if(single_var_log_buffers_read)
	{
	  free(single_var_log_buffers_read);
	}
      single_var_log_buffers_read = (int *) 
        malloc(sizeof(int)*single_var_log_monitors->list_size);
      sizeof_single_var_log_buffers_read = single_var_log_monitors->list_size;
    }
  memset(single_var_log_buffers_read, 0, (sizeof(int)*single_var_log_monitors->list_size));
  int bufs_read=0;
  
  if(single_var_log_monitors && !killed)
    {
      TCP_SINGLE_VAR_LOG_MONITOR *vlm = 
	(TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_head();
      while(vlm && !killed)
	{
	  double cur_time =etime();
	  if(cur_time - vlm->last_log_add_time > vlm->min_period)
	    {
	      bool got_var=false;
	      bool already_read_this_buf=false;
	      for(int i = 0; i < bufs_read; i++)
		{
		  if(single_var_log_buffers_read[i] == vlm->buffer_number)
		    {
		      already_read_this_buf=true;
		      break;
		    }
		}
	      if(!already_read_this_buf)
		{
		  single_var_log_buffers_read[bufs_read] = vlm->buffer_number;
		  bufs_read++;
		}

	      int start_id = vlm->in_buffer_id;
	      double d = svr->get_dvar(vlm->buffer_number,vlm->varname,
				       vlm->in_buffer_id,vlm->message_type,got_var, !already_read_this_buf);
	      if(got_var && start_id != vlm->in_buffer_id)
		{
		  
		  vlm->last_log_add_time = cur_time;
		  vlm->log_count++;
		  vlm->log_item_list[vlm->head_index].timestamp = vlm->last_log_add_time;
		  vlm->log_item_list[vlm->head_index].value = d;
		  vlm->head_index++;
		  if(vlm->head_index >= vlm->max_log_item_list_length)
		    {
		      vlm->head_index = 0;
		    }
		}
	    }
	  vlm = 
	    (TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_next();
	}
    }
}

void
CMS_SERVER_REMOTE_TCP_PORT::update_subscriptions ()
{
  if (NULL == subscription_buffers)
    {
      return;
    }

  CMS_SERVER *svr;
  svr = cms_server_parent;
  if(!svr)
    {
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      DWORD pid = GetCurrentProcessId ();
      DWORD tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
      int pid = taskIdSelf ();
      
      int tid = 0;
#else
      pid_t pid = getpid ();
      pid_t tid = 0;
#endif
#endif
      cms_server_parent = svr = find_server(pid,tid);
    }

  if(!svr)
    {
      rcs_print_error("svr is NULL\n");
      return;
    }

  if(0 == svr->read_req_ptr)
    {
      svr->read_req_ptr = new REMOTE_READ_REQUEST();
    }

  double cur_time = etime ();
  TCP_BUFFER_SUBSCRIPTION_INFO *buf_info =
    (TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_head ();
  while (NULL != buf_info && !killed)
    {
      svr->read_req_ptr->buffer_number = buf_info->buffer_number;
      svr->read_req_ptr->access_type = CMS_READ_ACCESS;
      svr->read_req_ptr->last_id_read = buf_info->min_last_id;
      REMOTE_READ_REPLY *read_reply=
	(REMOTE_READ_REPLY *) svr->process_request (svr->read_req_ptr);
      if (NULL == read_reply)
	{
	  rcs_print_error ("Server could not process request to read needed to update subscriptions. (NULL == read_reply)\n");
	  buf_info =
	    (TCP_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      if (read_reply->write_id == buf_info->min_last_id ||
	  read_reply->size < 1)
	{
	  buf_info =
	    (TCP_BUFFER_SUBSCRIPTION_INFO *)
	    subscription_buffers->get_next ();
	  continue;
	}
      
      TCP_CLIENT_SUBSCRIPTION_INFO *temp_clnt_info =
	(TCP_CLIENT_SUBSCRIPTION_INFO *) buf_info->sub_clnt_info->get_head ();
      buf_info->min_last_id = read_reply->write_id;
      while (temp_clnt_info != NULL && !killed)
	{
	  CLIENT_TCP_PORT *ctp = temp_clnt_info->clnt_port;
	  hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) 0);
	  hton_uint32_array_set(ctp->temp_buffer,1,(unsigned long) read_reply->status);
	  hton_uint32_array_set(ctp->temp_buffer,2,(unsigned long) read_reply->size);
	  hton_uint32_array_set(ctp->temp_buffer,3,(unsigned long) read_reply->write_id);
	  hton_uint32_array_set(ctp->temp_buffer,4,(unsigned long) read_reply->was_read);
	  double time_diff = cur_time - temp_clnt_info->last_sub_sent_time;
	  int time_diff_millis = (int) ((double) time_diff * 1000.0);
	  rcs_print_debug (PRINT_SERVER_SUBSCRIPTION_ACTIVITY,
			   "Subscription time_diff_millis=%d\n",
			   time_diff_millis);
	  if (((temp_clnt_info->subscription_type == CMS_POLLED_SUBSCRIPTION
		&& time_diff_millis + 10 >=
		temp_clnt_info->poll_interval_millis)
	       || temp_clnt_info->subscription_type ==
	       CMS_VARIABLE_SUBSCRIPTION)
	      && temp_clnt_info->last_id_read != read_reply->write_id)
	    {
	      temp_clnt_info->last_id_read = read_reply->write_id;
	      temp_clnt_info->last_sub_sent_time = cur_time;
	      temp_clnt_info->clnt_port->serial_number++;
	      hton_uint32_array_set(ctp->temp_buffer,0,(unsigned long) temp_clnt_info->clnt_port->serial_number);
	      if (read_reply->size < 0x2000 - 20
		  && read_reply->size > 0)
		{
		  memcpy (temp_clnt_info->clnt_port->temp_buffer + 20, read_reply->data,
			  read_reply->size);
		  if (sendn
		      (temp_clnt_info->clnt_port->socket_fd, temp_clnt_info->clnt_port->temp_buffer,
		       20 + read_reply->size, 0, dtimeout) < 0)
		    {
		      temp_clnt_info->clnt_port->errors++;
		      return;
		    }
		}
	      else
		{
		  if (sendn (temp_clnt_info->clnt_port->socket_fd,
			     temp_clnt_info->clnt_port->temp_buffer, 20, 0, dtimeout) < 0)
		    {
		      temp_clnt_info->clnt_port->errors++;
		      return;
		    }
		  if (read_reply->size > 0)
		    {
		      if (sendn (temp_clnt_info->clnt_port->socket_fd,
				 read_reply->data,
				 read_reply->size, 0, dtimeout) < 0)
			{
			  temp_clnt_info->clnt_port->errors++;
			  return;
			}
		    }
		}
	    }
	  if (temp_clnt_info->last_id_read < buf_info->min_last_id)
	    {
	      buf_info->min_last_id = temp_clnt_info->last_id_read;
	    }
	  temp_clnt_info =
	    (TCP_CLIENT_SUBSCRIPTION_INFO *)
	    buf_info->sub_clnt_info->get_next ();
	}
      buf_info =
	(TCP_BUFFER_SUBSCRIPTION_INFO *) subscription_buffers->get_next ();
    }
}


TCP_BUFFER_SUBSCRIPTION_INFO::TCP_BUFFER_SUBSCRIPTION_INFO ():
    buffer_number(0),min_last_id(0),list_id(0),sub_clnt_info(0)
{
  buffer_number = -1;
  min_last_id = 0;
  list_id = -1;
  sub_clnt_info = NULL;
}

TCP_BUFFER_SUBSCRIPTION_INFO::~TCP_BUFFER_SUBSCRIPTION_INFO ()
{
  buffer_number = -1;
  min_last_id = 0;
  list_id = -1;
  if (NULL != sub_clnt_info)
    {
      delete sub_clnt_info;
      sub_clnt_info = NULL;
    }
}

TCP_CLIENT_SUBSCRIPTION_INFO::TCP_CLIENT_SUBSCRIPTION_INFO ():
    subscription_type(0),poll_interval_millis(0),last_sub_sent_time(0.0),
    subscription_list_id(0),buffer_number(0),subscription_paused(0),last_id_read(0),
    sub_buf_info(0),clnt_port(0)
{
  subscription_type = CMS_NO_SUBSCRIPTION;
  poll_interval_millis = 30000;
  last_sub_sent_time = 0.0;
  subscription_list_id = -1;
  buffer_number = -1;
  subscription_paused = 0;
  last_id_read = 0;
  sub_buf_info = NULL;
  clnt_port = NULL;
}

TCP_CLIENT_SUBSCRIPTION_INFO::~TCP_CLIENT_SUBSCRIPTION_INFO ()
{
  subscription_type = CMS_NO_SUBSCRIPTION;
  poll_interval_millis = 30000;
  last_sub_sent_time = 0.0;
  subscription_list_id = -1;
  buffer_number = -1;
  subscription_paused = 0;
  last_id_read = 0;
  sub_buf_info = NULL;
  clnt_port = NULL;
}

CLIENT_TCP_PORT::CLIENT_TCP_PORT (class CMS_SERVER_REMOTE_TCP_PORT *_parent):
    serial_number(0),received_serial_number(0),request_type_long(0),
    request_type(NO_REMOTE_CMS_REQUEST),
    errors(0),max_errors(0),
    address_ptr(0),
    socket_fd(0),subscriptions(0),
    rcid(),
    blocking(0),total_subdivisions(0),request_subdiv(0),requests_complete(0),
    start_request_msg_count(0),start_request_read_count(0),
    start_request_is_clear(0),start_request_queue_length(0),
    last_request_queue_length(0),max_request_queue_length(0),min_request_queue_length(0),
    request_queue_length_to_wait_for(0),
    request_queue_length_over_or_under(OVER_UNDER_NOT_SET),
    diag_info(0),
    buffer_monitors(0),
    cms_clnt_info(0),
    buffer_number(0),
    extra_data_buf_address(0),
    extra_data_buf_size(0),
    extra_reply_data_buf(0),
    extra_reply_data_size(0),
    reply_data_address(0),
    reply_data_size(0),
    reply_header_address(0),
    reply_header_size(0),
    request_header_address(0),
    request_header_size(0),
    request_tail_address(0),
    request_tail_size(0),
    request_data_address(0),
    request_data_size(0),
    output_waiting_buffer(0),
    output_waiting_buffer_size(0),
    size_of_unsent_data(0),
    ptr_to_begin_of_unsent_data(0),
    input_waiting_buffer(0),
    input_waiting_buffer_size(0),
    ptr_to_begin_of_unreceived_data(0),
    size_of_unreceived_data(0),
    last_read_id(0),
    last_send_addr(0),
    last_send_size(0),
    last_send_retval(0),
    last_send_errno(0),
    last_recv_addr(0),
    last_recv_size(0),
    last_recv_retval(0),
    last_recv_errno(0),
    sockerrstr(0),
    something_blocked(false),
    using_input_waiting_buffer(false),
    blocked_on_write(false),remove_me(false),
    tid(0),pid(0),threadId(0),last_threadId(0),
    parent(_parent),loopback(0),
    single_var_log_monitors(0),
    state(WAITING_FOR_REQUEST_HEADER)
{
  loopback=this;
  serial_number=0;
  received_serial_number=0;
  request_type=NO_REMOTE_CMS_REQUEST;
  request_type_long= (long) request_type;
  errors=0;
  request_subdiv=0;
  max_errors=50;
  address_ptr = dl_create_sa(0,0,_parent->use_ipv6);
  socket_fd=-1;
  subscriptions=0;
  rcid.use_me=false;
  rcid.long_id[0] = socket_fd;
  rcid.long_id[1] = (long) dl_sa_get_port(address_ptr);
  blocking=0;
  total_subdivisions=1;
  requests_complete=0;
  start_request_msg_count=0;
  start_request_read_count=0;
  start_request_is_clear=0;
  start_request_queue_length=-1;
  last_request_queue_length=-1;
  max_request_queue_length=-1;
  min_request_queue_length=-1;
  request_queue_length_to_wait_for=-1;
  request_queue_length_over_or_under=OVER_UNDER_NOT_SET;

#if 0
  blocking_read_req=0;
  blocking_read_req1=0;
  blocking_read_req2=0;
#endif
  diag_info=0;

#if MS_WINDOWS_API
  tid=0;
  pid=0;
#else
#ifdef VXWORKS
  tid=0;
  pid=0;
#else
  tid=0;
  pid=0;
#endif
#endif

#if defined(sunos5) && !defined(NO_THREADS)
  threadId=0;
#else
#ifdef POSIX_THREADS
  threadId=0;
#else
  threadId=0;
#endif
#endif
#if defined(sunos5) && !defined(NO_THREADS)
  last_threadId=0;
#else
#ifdef POSIX_THREADS
  last_threadId=0;
#else
  last_threadId=0;
#endif
#endif
#ifdef POSIX_THREADS
  buffer_monitors=0;
#endif
  cms_clnt_info=0;
  memset(temp_buffer,0,sizeof(temp_buffer));
  buffer_number=0;
  extra_data_buf_address=0;
  extra_data_buf_size=0;
  extra_reply_data_buf=0;
  extra_reply_data_size=0;
  reply_data_address=0;
  reply_data_size=0;
  reply_header_address=0;
  reply_header_size=0;
  request_header_address=0;
  request_header_size=0;
  request_tail_address=0;
  request_tail_size=0;
  request_data_address=0;
  request_data_size=0;
  
  output_waiting_buffer=0;
  output_waiting_buffer_size=0;
  size_of_unsent_data=0;
  ptr_to_begin_of_unsent_data=0;
  input_waiting_buffer=0;
  input_waiting_buffer_size=0;
  ptr_to_begin_of_unreceived_data=0;
  size_of_unreceived_data=0;
  last_read_id=0;
  request_type= NO_REMOTE_CMS_REQUEST;
  state =WAITING_FOR_REQUEST_HEADER;
  last_send_addr=0;
  last_send_size=0;
  last_send_retval=0;
  last_send_errno=0;
  last_recv_addr=0;
  last_recv_size=0;
  last_recv_retval=0;
  last_recv_errno=0;
  memset(sockerrbuf,0,sizeof(sockerrbuf));
  sockerrstr=sockerrbuf;
  something_blocked=false;
  using_input_waiting_buffer=false;
  blocked_on_write=false;
  remove_me=false;

  cms_clnt_info=0;
  serial_number = 0;
  errors = 0;
  max_errors = 50;
  socket_fd = -1;
  subscriptions = NULL;
  tid = 0;
  pid = 0;
  threadId = 0;
  last_threadId = 0;
  diag_info = NULL;
}

CLIENT_TCP_PORT::~CLIENT_TCP_PORT ()
{
  rcs_print_debug(PRINT_MISC,"~CLIENT_TCP_PORT() started :socket_fd=%d,address=%s:%d,pid=%ld,tid=%ld,threadId=%ld,last_threadId=%ld",
		  socket_fd,
		  dl_sa_get_host(address_ptr),
		  dl_sa_get_port(address_ptr),
		  (long)pid,(long)tid,
		  (long)threadId,(long)last_threadId);

#if 0
  if(threadId > 0)
    {
      tcp_srv_blocking_thread_kill(threadId,this);
    }
  if(last_threadId > 0)
    {
      tcp_srv_blocking_thread_kill(last_threadId,this);
    }
#endif

  if (socket_fd > 0)
    {
      dl_closesocket (socket_fd);
      rcid.use_me=true;
      rcid.long_id[0] = socket_fd;
      rcid.long_id[1] = (long) dl_sa_get_port(address_ptr);
      if(!pid && !tid)
	{
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
	  pid = GetCurrentProcessId ();
	  tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
	  pid = taskIdSelf ();
      
	  tid = 0;
#else
	  pid = getpid ();
	  tid = 0;
#endif
#endif
	}
      CMS_SERVER_REMOTE_PORT::release_client_info(&rcid,pid,tid);
      socket_fd = -1;
    }
  if (NULL != subscriptions)
    {
      TCP_CLIENT_SUBSCRIPTION_INFO *sub_info =
	(TCP_CLIENT_SUBSCRIPTION_INFO *) subscriptions->get_head ();
      while (NULL != sub_info)
	{
	  delete sub_info;
	  sub_info =
	    (TCP_CLIENT_SUBSCRIPTION_INFO *) subscriptions->get_next ();
	}
      delete subscriptions;
      subscriptions = NULL;
    }

  if(single_var_log_monitors)
    {
      TCP_SINGLE_VAR_LOG_MONITOR *vlm = 
	(TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_head();
      while(vlm)
	{
	  if(vlm->ctp != this)
	    {
	      continue;
	    }
	  delete vlm;
	  vlm = 
	    (TCP_SINGLE_VAR_LOG_MONITOR *) single_var_log_monitors->get_next();
	}
      delete single_var_log_monitors;
      single_var_log_monitors=0;
    }


#if 0
  if (NULL != blocking_read_req1) 
    {
      delete blocking_read_req1;
      blocking_read_req1 = NULL;
    }
  if (NULL != blocking_read_req2)
    {
      delete blocking_read_req2;
      blocking_read_req2 = NULL;
    }
  blocking_read_req=NULL;
#endif
  if (NULL != diag_info)
    {
      delete diag_info;
      diag_info = NULL;
    }
  if(address_ptr)
    {
      dl_free_sa(address_ptr);
      address_ptr=0;
    }
  if(NULL != output_waiting_buffer)
    {
      free(output_waiting_buffer);
      output_waiting_buffer=0;
      output_waiting_buffer_size=0;
    }
  if(NULL != input_waiting_buffer)
    {
      free(input_waiting_buffer);
      input_waiting_buffer=0;
      input_waiting_buffer_size=0;
    }
  parent=0;
}

