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
* File:cms_srv.cc                                                        *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for server that reads and writes to a local          *
*          CMS buffer for remote processes.                              *
* Includes:                                                              *
*          1. member functions for class CMS_SERVER                      *
*          2. CMS_SERVER friend functions cms_server_dispatch and        *
*             cms_server_clean.                                          *
*************************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_SERVER)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "cms_srv_no_config.h"
#endif

#include "cms.hh"		/* class CMS */
#include "rem_msg.hh"		/* struct REMOTE_READ_REQUEST, */
				/* struct REMOTE_WRITE_REQUEST, */
#include "cms_srv.hh"		/* class CMS_SERVER  */
#include "linklist.hh"		// class RCS_LINKED_LIST
#include "rcs_prnt.hh"		/* rcs_print_error() */


#ifdef ENABLE_RCS_TCP
#include "tcp_srv.hh"		/* CMS_SERVER_TCP_PORT */
#endif
#ifdef ENABLE_RCS_HTTP
#include "http_srv.hh"		/* CMS_SERVER_TCP_PORT */
#endif
#ifdef ENABLE_RCS_STCP
#include "stcpsvr.hh"		/* CMS_SERVER_STCP_PORT */
#endif

#ifdef ENABLE_RCS_TTY
#include "tty_srv.hh"
#endif

#ifdef ENABLE_RCS_UDP
#include "udp_srv.hh"		/* CMS_SERVER_UDP_PORT */
#endif

#ifdef ENABLE_RCS_GDRS_IM
#include "gdrs_im_srv.hh"      /* CMS_SERVER_GDRS_IM_PORT */
#endif

#ifdef ENABLE_RCS_INET_FILES
#include "inetfile.hh"		// INET_FILE, inet_file_open(), . . .
#endif

#ifdef ENABLE_RCS_CRYPT2
#include "crypt2.hh"		// crypt()
#endif

#include "timer.hh"		// etime()
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */

#ifdef ENABLE_RCS_DIAG
#include "cmsdiag.hh"
#endif

#include "nml.hh"

#include "cms_clnt_info.hh"

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

class CMS_USER_CONNECT_CLASS
{
public:
  CMS_USER_CONNECT_CLASS(): 
    user_info(0),fd(-1) 
  {
    user_info = 0;
    fd = -1;
  }

  CMS_USER_INFO *user_info;
  int fd;
private:
  //prevent copying
  CMS_USER_CONNECT_CLASS (const CMS_USER_CONNECT_CLASS &);
  CMS_USER_CONNECT_CLASS &operator=(const CMS_USER_CONNECT_CLASS &);
};

svr_start_func cms_svr_sfunc =0;

class CMS_SERVER_PROCESS_THREAD_INFO
{
public:
#if MS_WINDOWS_API
  DWORD current_pid;
  DWORD current_tid;
  DWORD creator_pid;
  DWORD creator_tid;
  DWORD spawner_pid;
  DWORD spawner_tid;
  DWORD server_pid;
  DWORD server_tid;
#else

  long current_pid;
  long current_tid;
  long creator_pid;
  long creator_tid;
  long spawner_pid;
  long spawner_tid;
  long server_pid;
  long server_tid;
#endif

#ifdef POSIX_THREADS
  pthread_t server_pthread_id;
  pthread_t spawner_pthread_id;
#endif

#if defined(WIN32)
  HANDLE server_thread_handle;
#endif

};



CMS_MSG_INFO::CMS_MSG_INFO():
  addr(0),size(0),deleteaddr(0)
{
  addr = 0;
  size = 0;
  deleteaddr = 0;
}

CMS_MSG_INFO::~CMS_MSG_INFO()
{
  if (addr && size && deleteaddr)
    {
      DEBUG_FREE (addr);
      addr = 0;
    }
}

CMS_CLIENT_INFO::CMS_CLIENT_INFO():
  id(),
  buffer_num(0),
  mrpq_reader_id(-1),last_message_info(),
  temp_read_reply(0),
  read_reply_data_size(0),
  cms(0),
  nmlcopies(0)
{
  cms=0;
  mrpq_reader_id=-1;
  temp_read_reply=0;
  read_reply_data_size=0;
}

CMS_CLIENT_INFO::~CMS_CLIENT_INFO()
{
  release();
}

void CMS_CLIENT_INFO::release(void)
{
  if(cms && mrpq_reader_id >= 0)
    {
      cms->set_current_mrpq_reader_id(mrpq_reader_id);
      cms->remove_current_mrpq_reader_id();
      cms = 0;
      mrpq_reader_id = -1;
    }
  if(last_message_info.addr != 0 && last_message_info.size > 0 && last_message_info.deleteaddr)
    {
      DEBUG_FREE(last_message_info.addr);
      last_message_info.addr = 0;
      last_message_info.size =0;
      last_message_info.deleteaddr=0;
    }
  if(temp_read_reply)
    {
      if(temp_read_reply->data)
	{
	  DEBUG_FREE(temp_read_reply->data);
	  temp_read_reply->data=0;
	}
      delete temp_read_reply;
      temp_read_reply=0;
    }
}

int cms_server_count = 0;
int cms_server_task_priority = 100;
int cms_server_task_stack_size = 32768;

int get_cms_server_task_priority(void)
{
  return cms_server_task_priority;
}

int get_cms_server_task_stack_size(void)
{
  return cms_server_task_stack_size;
}


void
wait_for_servers (int count_to_waitfor)
{
  do
    {
      esleep (0.1);
    }
  while (cms_server_count < count_to_waitfor);
}



class CMS_USER_INFO
{
public:
  CMS_USER_INFO ();
private:
  char passwd[16];
  char epasswd[16];
  char name[16];
  char passwd_file_line[256];
  char key1[8];
  char key2[8];
  int has_passwd;
  int user_number;
  int allow_read;
  int allow_write;
  friend class CMS_SERVER;
};


CMS_USER_INFO::CMS_USER_INFO ():
  has_passwd(0),
  user_number(0),
  allow_read(0),
  allow_write(0)
{
  memset (passwd, 0, 16);
  memset (epasswd, 0, 16);
  memset (name, 0, 16);
  memset (passwd_file_line, 0, 256);
  memset (key1, 0, 8);
  memset (key2, 0, 8);
  has_passwd = 0;
  user_number = 0;
  allow_read = 0;
  allow_write = 0;
}


/* CMS_SERVER Global  Variables */
RCS_LINKED_LIST *cms_server_list = NULL;

#ifdef VXWORKS
SEM_ID cms_server_list_mutex = 0;
#elif POSIX_THREADS
static pthread_mutex_t cms_local_ports_pmutex= PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t cms_server_list_pmutex= PTHREAD_MUTEX_INITIALIZER;
#endif

void cms_server_list_cleanup(void)
{
  RCS_LINKED_LIST *list_to_delete = cms_server_list;
  cms_server_list = 0;
  if(list_to_delete)
    {
      delete list_to_delete;
      list_to_delete=0;
    }
}

static bool cms_svr_already_clean = false;
static void (*old_sigint_handler)(int) = SIG_IGN;

CMS_SERVER_LOCAL_PORT::CMS_SERVER_LOCAL_PORT (CMS * _cms):
  buffer_number(0),
  list_id(0),
  security_enabled(0),
  read_reply_ptr(0),
  write_reply_ptr(0),
  get_diag_info_reply_ptr(0),
  get_buf_name_reply_ptr(0),
  get_msg_type_reply_ptr(0),
  local_channel_reused(0),
  enable_xml_differencing(false),
  multireader_priority_queue_enabled(false),
  orig_info(0)
{
  local_channel_reused = 1;
  security_enabled = 0;
  cms = _cms;
  if (cms)
    {
      enable_xml_differencing = cms->enable_xml_differencing;
      multireader_priority_queue_enabled = cms->multireader_priority_queue_enabled;
    }
  else
    {
      multireader_priority_queue_enabled = false;
      enable_xml_differencing = 0;
    }

  read_reply_ptr=new REMOTE_READ_REPLY();
  write_reply_ptr=new REMOTE_WRITE_REPLY();
  get_msg_type_reply_ptr = new REMOTE_GET_MSG_TYPE_REPLY();
#if ENABLE_RCS_DIAG
  get_diag_info_reply_ptr = new REMOTE_GET_DIAG_INFO_REPLY();
#else
  get_diag_info_reply_ptr = 0;
#endif
  get_buf_name_reply_ptr = new REMOTE_GET_BUF_NAME_REPLY();
  get_buf_name_reply_ptr->name[0]=0;

  orig_info = NULL;
  if (NULL != cms)
    {
      buffer_number = cms->buffer_number;
    }
  else
    {
      buffer_number = 0;
    }
  list_id = 0;
}

CMS_SERVER_LOCAL_PORT::~CMS_SERVER_LOCAL_PORT ()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_LOCAL_PORT::~CMS_SERVER_LOCAL_PORT() called. (this=%p)\n",
		  (void*)this);

#ifdef ENABLE_RCS_DIAG
  if (NULL != orig_info)
    {
      delete orig_info;
      orig_info = NULL;
    }
  if(get_diag_info_reply_ptr)
    {
      delete get_diag_info_reply_ptr;
      get_diag_info_reply_ptr=0;
    }
#endif
  if(read_reply_ptr)
    {
      delete read_reply_ptr;
      read_reply_ptr=0;
    }
  if(get_msg_type_reply_ptr)
    {
      delete get_msg_type_reply_ptr;
      get_msg_type_reply_ptr=0;
    }
  if(write_reply_ptr)
    {
      delete write_reply_ptr;
      write_reply_ptr=0;
    }
  if(get_buf_name_reply_ptr)
    {
      delete get_buf_name_reply_ptr;
      get_buf_name_reply_ptr=0;
    }
}

REMOTE_GET_BUF_NAME_REPLY *
CMS_SERVER_LOCAL_PORT::get_buf_name_reply(void)
{
  if(0 == get_buf_name_reply_ptr)
    {
      get_buf_name_reply_ptr = new REMOTE_GET_BUF_NAME_REPLY();
      get_buf_name_reply_ptr->name[0]=0;
    }
  if(0 == get_buf_name_reply_ptr->name[0] && cms)
    {
      strcpy(get_buf_name_reply_ptr->name,cms->BufferName);
    }
  return get_buf_name_reply_ptr;
}
 
/* local_port function for reads */
REMOTE_READ_REPLY *
CMS_SERVER_LOCAL_PORT::reader (  
			       __unused_parameter__ CMS_CLIENT_INFO *,
			       __unused_parameter__ REMOTE_READ_REQUEST *)
{
  return (NULL);
}

REMOTE_READ_REPLY *
CMS_SERVER_LOCAL_PORT::blocking_read (  
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_READ_REQUEST *
				      )
{
  return (NULL);
}

/* local_port function for writes */
REMOTE_WRITE_REPLY *
CMS_SERVER_LOCAL_PORT::writer (  
			       __unused_parameter__ CMS_CLIENT_INFO *,
			       __unused_parameter__ REMOTE_WRITE_REQUEST *
			       )
{
  return (NULL);
}


REMOTE_SET_DIAG_INFO_REPLY *
CMS_SERVER_LOCAL_PORT::set_diag_info (  
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_SET_DIAG_INFO_REQUEST *
				      )
{
  return (NULL);
}

REMOTE_GET_DIAG_INFO_REPLY *
CMS_SERVER_LOCAL_PORT::get_diag_info (  
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_DIAG_INFO_REQUEST *
				      )
{
#ifdef ENABLE_RCS_DIAG
  get_diag_info_reply_ptr->cdi = cms->get_diagnostics_info ();
  get_diag_info_reply_ptr->status = cms->status;
  return (get_diag_info_reply_ptr);
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}

REMOTE_GET_MSG_COUNT_REPLY *
CMS_SERVER_LOCAL_PORT::get_msg_count (  
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_MSG_COUNT_REQUEST *
				      )
{
  return (NULL);
}


REMOTE_GET_MSG_TYPE_REPLY *
CMS_SERVER_LOCAL_PORT::get_msg_type (  
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_MSG_TYPE_REQUEST *
				      )
{
  return (NULL);
}

void
CMS_SERVER_LOCAL_PORT::reset_diag_info ( 
					__unused_parameter__ CMS_CLIENT_INFO *
					)
{
}


CMS_SERVER_REMOTE_PORT::CMS_SERVER_REMOTE_PORT (CMS_SERVER *
						_cms_server_parent):
  port_registered(0),
  current_user_info(0),
  connected_users(0),
  current_connected_user_struct(0),
  security_enabled(0),
  cms_server_parent(0),
  min_compatible_version(0),
  confirm_write(0),
  running(0),
  max_total_subdivisions(0),
  port_num(0),
  max_clients(0),
  current_clients(0),
  killed(false),
  use_ipv6(0)
{ 
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_PORT::CMS_SERVER_REMOTE_PORT(_cms_server_parent=%p) called. this=%p\n",
		  (void*)_cms_server_parent,(void*)this);
  killed=false;
  current_clients = 0;
  max_clients = 0;
  port_registered = 0;
  cms_server_parent = _cms_server_parent;
  connected_users = NULL;
  security_enabled = 0;
  confirm_write = 0;
  min_compatible_version = 0.0;
  current_user_info = NULL;
  running = 0;
  max_total_subdivisions = _cms_server_parent->max_total_subdivisions;
}

CMS_SERVER_REMOTE_PORT::~CMS_SERVER_REMOTE_PORT ()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_PORT::~CMS_SERVER_REMOTE_PORT() called. this=%p\n",
		  (void*)this);
  killed=true;
  if (NULL != connected_users)
    {
      class CMS_USER_CONNECT_CLASS *connected_user_struct =
	(class CMS_USER_CONNECT_CLASS *) connected_users->get_head ();
      while (NULL != connected_user_struct)
	{
	  delete connected_user_struct;
	  connected_user_struct = NULL;
	  connected_users->delete_current_node ();
	  connected_user_struct =
	    (class CMS_USER_CONNECT_CLASS *) connected_users->get_next ();
	}
      delete connected_users;
    }
  current_connected_user_struct = NULL;
}

void
CMS_SERVER_REMOTE_PORT::killme()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_PORT::killme() called. this=%p,killed=%d\n",
		  (void*)this,killed);
  killed=true;
}

void
CMS_SERVER_REMOTE_PORT::add_connected_user (int _fd)
{
  current_connected_user_struct = NULL;
  rcs_print_debug (PRINT_SOCKET_CONNECT, "Adding connected user %d\n", _fd);
  class CMS_USER_CONNECT_CLASS *connected_user_struct =
    new class CMS_USER_CONNECT_CLASS ();
  if (NULL == connected_user_struct)
    {
      return;
    }
  connected_user_struct->fd = _fd;
  if (NULL == connected_users)
    {
      connected_users = new RCS_LINKED_LIST ();
    }
  if (NULL == connected_users)
    {
      return;
    }
  connected_users->store_at_tail (connected_user_struct,
				  sizeof (connected_user_struct), 0);
  current_connected_user_struct = connected_user_struct;
  //delete connected_user_struct;
}

CMS_USER_INFO *
CMS_SERVER_REMOTE_PORT::get_connected_user (int _fd)
{
  current_connected_user_struct = NULL;
  if (NULL == connected_users)
    {
      connected_users = new RCS_LINKED_LIST ();
    }
  if (NULL == connected_users)
    {
      return NULL;
    }
  class CMS_USER_CONNECT_CLASS *connected_user_struct =
    (class CMS_USER_CONNECT_CLASS *) connected_users->get_head ();
  while (NULL != connected_user_struct)
    {
      if (connected_user_struct->fd == _fd)
	{
	  current_connected_user_struct = connected_user_struct;
	  return connected_user_struct->user_info;
	}
      connected_user_struct =
	(class CMS_USER_CONNECT_CLASS  *) connected_users->get_next ();
    }
  add_connected_user (_fd);
  return NULL;
}


void
CMS_SERVER_REMOTE_PORT::register_port ()
{
  return;
}

void
CMS_SERVER_REMOTE_PORT::unregister_port ()
{
  return;
}

void
CMS_SERVER::remove_local_port_cms (CMS * _cms)
{
  CMS_SERVER_LOCAL_PORT *cms_local_port;
  cms_local_port=0;
  if(cms_local_ports)
    {      
      cms_local_port = (CMS_SERVER_LOCAL_PORT *) 
	cms_local_ports->get_head();
      while (NULL != cms_local_port)
	{
	  if (cms_local_port->cms == _cms)
	    {
	      delete cms_local_port;
	      cms_local_ports->delete_current_node();
	      cms_local_ports->get_head();
	      return;
	    }
	  cms_local_port = (CMS_SERVER_LOCAL_PORT *) 
	    cms_local_ports->get_next();
	}
    }
}


int
CMS_SERVER_REMOTE_PORT::accept_local_port_cms (CMS * _cms)
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_PORT::accept_local_port_cms(_cms=%p) called. this=%p\n",
		  (void*)_cms,(void*)this);
  if (NULL != _cms)
    {
      if (min_compatible_version < 1e-6 ||
	  (min_compatible_version > _cms->min_compatible_version &&
	   _cms->min_compatible_version > 1e-6))
	{
	  min_compatible_version = _cms->min_compatible_version;
	}
    }
  if (_cms->total_subdivisions > max_total_subdivisions)
    {
      max_total_subdivisions = _cms->total_subdivisions;
    }
  return 1;
}


CMS_SERVER *
CMS_SERVER_REMOTE_PORT::find_server (long _pid, long _tid /* =0 */ )
{
  CMS_SERVER *cms_server;
  class RCS_LINKED_LIST_NODE *tmp_cur_node;
  tmp_cur_node=0;

#if 0
#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semTake (cms_server_list_mutex, WAIT_FOREVER);
    }
  else
    {
      taskLock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"locking mutex . . .\n");
  pthread_mutex_lock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"locked mutex.\n");
#endif
#endif

  if (NULL == cms_server_list)
    {
    }
  cms_server = (CMS_SERVER *) cms_server_list->get_head_with_external_current (&tmp_cur_node);
  while (NULL != cms_server)
    {
      if(!cms_server_list)
	{
	  break;
	}
      if(!cms_server->ptinfo)
	{
	  cms_server = (CMS_SERVER *) cms_server_list->get_next_with_external_current (&tmp_cur_node);
	  continue;
	}
#if MS_WINDOWS_API
      if (cms_server->ptinfo->server_pid == ((DWORD) _pid)
	  && cms_server->ptinfo->server_tid == ((DWORD) _tid))
#else
      if (cms_server->ptinfo->server_pid == _pid 
	  && cms_server->ptinfo->server_tid == _tid)
#endif
	{
	  break;
	}
      if(!cms_server_list)
	{
	  break;
	}
      cms_server = (CMS_SERVER *) cms_server_list->get_next_with_external_current (&tmp_cur_node);
    }

#if 0
#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semGive (cms_server_list_mutex);
    }
  else
    {
      taskUnlock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"unlocking mutex . . .\n");
  pthread_mutex_unlock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"unlocked mutex.\n");
#endif
#endif

  return (cms_server);
}

void 
CMS_SERVER_REMOTE_PORT::release_client_info(REMOTE_CLIENT_ID *rcid,long pid, long tid)
{
  if(!rcid)
    {
      return;
    }
  CMS_SERVER *svr = find_server(pid,tid);
  if(svr)
    {
      svr->release_client_info(rcid);
    }
}

void
CMS_SERVER_REMOTE_PORT::print_servers ()
{
  CMS_SERVER *cms_server;

  if (NULL == cms_server_list)
    {
      rcs_print ("cms_server_list is NULL.\n");
      return;
    }

  cms_server = (CMS_SERVER *) cms_server_list->get_head ();
  rcs_print ("Server Tasks for this remote port.\n");
  while (NULL != cms_server)
    {
      rcs_print (" \t(%ld (0x%X), %ld (0x%X))\n",
		 cms_server->ptinfo->server_pid, (unsigned)cms_server->ptinfo->server_pid,
		 cms_server->ptinfo->server_tid, (unsigned)cms_server->ptinfo->server_tid);

      cms_server = (CMS_SERVER *) cms_server_list->get_next ();
    }
}


void
CMS_SERVER::read_passwd_file ()
{
#ifdef ENABLE_RCS_CRYPT2
  using_passwd_file = 1;
  int user_name_length;
  int passwd_length;
  if (NULL == known_users)
    {
      known_users = new RCS_LINKED_LIST ();
    }
  srand ((int) ((2 ^ 32) * etime ()));
  char buf[256];
#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *ifp = NULL;
  rcs_print ("Reading passwd file %s.\n", passwd_file);
  ifp = inet_file_open (passwd_file, "r");
  if (NULL == ifp)
    {
      rcs_print_error ("Can not open passwd file %s.\n", passwd_file);
      return;
    }
#else
  FILE *fp = NULL;
  rcs_print ("Reading passwd file %s.\n", passwd_file);
  fp = fopen(passwd_file, "r");
  if (NULL == fp)
    {
      rcs_print_error ("Can not open passwd file %s.\n", passwd_file);
      return;
    }
#endif

  CMS_USER_INFO *user_info = NULL;

  while (
#ifdef ENABLE_RCS_INET_FILES
	 !inet_file_eof (ifp)
#else
	 !feof(fp)
#endif
	 )
    {
      memset (buf, 0, 256);
#ifdef ENABLE_RCS_INET_FILES
      inet_file_gets (buf, 256, ifp);
#else
      fgets(buf,256,fp);
#endif
      user_name_length = strcspn (buf, "\n\r \t:");
      if (user_name_length > 16)
	{
	  rcs_print_error ("CMS_SERVER: user name is too long.\n");
	  continue;
	}
      if (user_name_length < 2)
	{
	  continue;
	}
      user_info = new CMS_USER_INFO ();
      if (NULL == user_info)
	{
	  break;
	}
      strcpy (user_info->passwd_file_line, buf);
      memcpy (user_info->name, buf, user_name_length);
      passwd_length = strcspn (buf + user_name_length + 1, "\n\r \t:");
      if (passwd_length > 16)
	{
	  rcs_print_error ("CMS_SERVER: password is too long.\n");
	  continue;
	}
      if (passwd_length > 16)
	{
	  rcs_print_error ("CMS_SERVER: password is too long.\n");
	}
      if (passwd_length > 2)
	{
	  memcpy (user_info->passwd, buf + user_name_length + 1,
		  passwd_length);
	  memcpy (user_info->key1, buf + user_name_length + 1, 2);
	  user_info->has_passwd = 1;
	}
      else
	{
	  user_info->has_passwd = 0;
	}
      gen_random_key (user_info->key2, 2);
      strcpy (user_info->epasswd,
	      rcs_crypt (user_info->passwd, user_info->key2));
      user_info->allow_read = (NULL != strstr (buf, "read=true"));
      user_info->allow_write = (NULL != strstr (buf, "write=true"));
      user_info->user_number =
	known_users->store_at_tail (user_info, sizeof (user_info), 0);
      rcs_print ("Storing info for user (%s).\n", user_info->name);
      //delete user_info;
      if (!strcmp (user_info->name, "guest"))
	{
	  guest_can_read = user_info->allow_read;
	  guest_can_write = user_info->allow_write;
	}
      user_info = NULL;
    }
#else
  rcs_print_error("CMS_SERVER::read_passwd_file  called but the RCS library was compiled without crypt2 support so passwords can not be used.\n");
#endif
}

void
CMS_SERVER::gen_random_key (char key[], int len)
{
  for (int i = 0; i < len; i++)
    {
      while (!isgraph (key[i]) || !key[i])
	{
	  key[i] = (char) ((rand () % 128));
	}
    }
}

CMS_USER_INFO *
CMS_SERVER::find_user (const char *name)
{
  if (NULL == known_users)
    {
      return NULL;
    }
  CMS_USER_INFO *user_info = (CMS_USER_INFO *) known_users->get_head ();
  while (NULL != user_info)
    {
      rcs_print ("CMS_SERVER::find_user: strcmp(%s,%s)\n", name,
		 user_info->name);
      if (!strcmp (name, user_info->name))
	{
	  return user_info;
	}
      user_info = (CMS_USER_INFO *) known_users->get_next ();
    }
  rcs_print_error ("CMS_SERVER: Can't find entry for user %s.\n", name);
  return NULL;
}

int
CMS_SERVER::get_user_keys (
#ifdef ENABLE_RCS_CRYPT2
			   const char *name, 
			   char *key1, 
			   char *key2
#else
			   __unused_parameter__ const char *, 
			   __unused_parameter__ char *, 
			   __unused_parameter__ char *
#endif

			   )
{
#ifdef ENABLE_RCS_CRYPT2
  if (NULL == known_users)
    {
      gen_random_key (key1, 2);
      gen_random_key (key2, 2);
      return -1;
    }
  CMS_USER_INFO *user_info = find_user (name);
  if (NULL == user_info)
    {
      gen_random_key (key1, 2);
      gen_random_key (key2, 2);
      return -1;
    }
  strcpy (key1, user_info->key1);
  if (fabs (etime () - time_of_last_key_request) > 30.0)
    {
      memset (user_info->key2, 0, 8);
      memset (user_info->epasswd, 0, 16);
      gen_random_key (user_info->key2, 2);
      strcpy (user_info->epasswd,
	      rcs_crypt (user_info->passwd, user_info->key2));
    }
  strcpy (key2, user_info->key2);
  time_of_last_key_request = etime ();
  return 0;
#else
  return -1;
#endif
}

CMS_USER_INFO *
CMS_SERVER::get_user_info (
#ifdef ENABLE_RCS_CRYPT2
			   const char *name, 
			   const char *epasswd
#else
			   __unused_parameter__ const char *, 
			   __unused_parameter__ const char *
#endif
			   )
{
#ifdef ENABLE_RCS_CRYPT2
  if (NULL == known_users)
    {
      return NULL;
    }
  CMS_USER_INFO *user_info = find_user (name);
  if (NULL == user_info)
    {
      return NULL;
    }
  if (!strcmp (user_info->epasswd, epasswd) || !user_info->has_passwd)
    {
      return user_info;
    }
  rcs_print_error ("CMS_SERVER: %s gave the wrong passwd.\n", name);
  rcs_print_error ("CMS_SERVER: user_info->passwd = %s\n", user_info->passwd);
  rcs_print_error ("CMS_SERVER: user_info->epasswd = %s\n",
		   user_info->epasswd);
  rcs_print_error ("CMS_SERVER: epasswd = %s\n", epasswd);

  return NULL;
#else
  rcs_print_error("CMS_SERVER::get_user_info called but the RCS library was compiled without crypt2 support so passwords can not be used.\n");
  return NULL;
#endif
}



void
CMS_SERVER::add_local_port (CMS_SERVER_LOCAL_PORT * _local_port)
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::add_local_port (_local_port=%p) called. (this=%p,remote_port=%p)\n",
		  (void*)_local_port,(void*)this,(void*)remote_port);
  if (NULL == _local_port)
    {
      rcs_print_error ("CMS_SERVER: Attempt to add NULL local port.\n");
      return;
    }
  if (NULL == _local_port->cms)
    {
      rcs_print_error
	("CMS_SERVER: Attempt to add local port with NULL cms object.\n");
      return;
    }
  if (NULL == cms_local_ports)
    {
      rcs_print_error
	("CMS_SERVER: Attempt to add local port when local ports list is NULL.\n");
      return;
    }

  if (NULL == remote_port)
    {
      rcs_print_debug(PRINT_MISC,"CMS_SERVER::add_local_port (_local_port=%p) : remote_port=NULL, _local_port->cms->remote_port_type=%d\n",
		      (void*)_local_port, _local_port->cms->remote_port_type);
      switch (_local_port->cms->remote_port_type)
	{
	case CMS_TCP_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_TCP
	  remote_port = new CMS_SERVER_REMOTE_TCP_PORT (this);
#else
	  rcs_print_error("The RCS library was not compiled with TCP support.\n");
#endif

	  break;
	case CMS_HTTP_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_HTTP
	  remote_port = new CMS_SERVER_REMOTE_HTTP_PORT (this);
#else
	  rcs_print_error("The RCS library was not compiled with HTTP support.\n");
#endif
	  break;
	case CMS_STCP_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_STCP
	  remote_port = new CMS_SERVER_REMOTE_STCP_PORT (this);
#else
	  rcs_print_error("The RCS library was not compiled with STCP support.\n");
#endif
	  break;

#ifdef ENABLE_RCS_TTY
#if !defined(VXWORKS) && (!defined(__MSDOS__) || defined(MS_WINDOWS_API)) && !defined(DARWIN) && !defined(qnx)
	case CMS_TTY_REMOTE_PORT_TYPE:
	  remote_port = new CMS_SERVER_REMOTE_TTY_PORT (this);
	  break;
#endif
#endif

	case CMS_UDP_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_UDP
	  remote_port = new CMS_SERVER_REMOTE_UDP_PORT (this);
#else
	  rcs_print_error("The RCS library was compiled without UDP support.\n");
#endif
	  break;


	case CMS_GDRS_IM_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_GDRS_IM
	  remote_port = new CMS_SERVER_REMOTE_GDRS_IM_PORT (this);
#else
	  rcs_print_error("The RCS library was compiled without GDRS_IM support.\n");
#endif
	  break;

	case CMS_OE_REMOTE_PORT_TYPE:
#ifdef ENABLE_RCS_OE_INTRF
	  remote_port = new CMS_SERVER_REMOTE_NULL_PORT(this);
	  _local_port->list_id =
	    cms_local_ports->store_at_tail (_local_port,
					    sizeof (CMS_SERVER_LOCAL_PORT), 0);
	  return;
#else
	  rcs_print_error("The RCS library was compiled without OE support.\n");
#endif
	  break;
	default:
	  rcs_print_error ("CMS_SERVER: Invalid remote port type. (%d)\n",
			   _local_port->cms->remote_port_type);
	  return;
	}
    }
  if (NULL == remote_port)
    {
      rcs_print_error ("CMS_SERVER: couldn't create remote port object.\n");
      return;
    }
  if (!accept_local_port_cms (_local_port->cms))
    {
      rcs_print_error
	("CMS_SERVER: Attempt to add local port failed because the port was of an incompatible type.\n");
    }
  char *passwd_eq = strstr (_local_port->cms->BufferLine, "passwd=");
  if (NULL != passwd_eq)
    {
      if (!using_passwd_file)
	{
	  memset (passwd_file, 0, 256);
	  for (int i = 0; i < 256 && passwd_eq[i + 7]; i++)
	    {
	      if (passwd_eq[i + 7] == ' ' || passwd_eq[i + 7] == '\t'
		  || passwd_eq[i + 7] == '\n' || passwd_eq[i + 7] == '\r')
		{
		  break;
		}
	      passwd_file[i] = passwd_eq[i + 7];
	    }
	  if (strlen (passwd_file) > 0)
	    {
	      read_passwd_file ();
	    }
	}
      _local_port->security_enabled = 1;
      remote_port->security_enabled = 1;
    }


  _local_port->list_id =
    cms_local_ports->store_at_tail (_local_port,
				    sizeof (CMS_SERVER_LOCAL_PORT), 0);
  if (-1 == _local_port->list_id)
    {
      rcs_print_error
	("CMS_SERVER: Can not store local port on linked list.\n");
    }
}

int
CMS_SERVER::accept_local_port_cms (CMS * _cms)
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::accept_local_port_cms (_cms=%p) called. (this=%p,remote_port=%p)\n",
		  (void*)_cms,(void*)this,(void*)remote_port);
  if (NULL == remote_port || NULL == _cms)
    {
      return (0);
    }

  return (remote_port->accept_local_port_cms (_cms));
}

CMS_SERVER_LOCAL_PORT *
CMS_SERVER::find_local_port (long _buffer_number)
{
  CMS_SERVER_LOCAL_PORT *cms_local_port;
  RCS_LINKED_LIST_NODE *temp_current;
  temp_current=0;
  cms_local_port=0;
  if(cms_local_ports)
    {      
      cms_local_port = (CMS_SERVER_LOCAL_PORT *) 
	cms_local_ports->get_head_with_external_current(&temp_current);
      while (NULL != cms_local_port)
	{
	  if (cms_local_port->buffer_number == _buffer_number)
	    {
	      break;
	    }
	  cms_local_port = (CMS_SERVER_LOCAL_PORT *) 
	    cms_local_ports->get_next_with_external_current(&temp_current);
	}
    }
  return (cms_local_port);
}


int
CMS_SERVER::get_total_subdivisions (long _buffer_number)
{
  CMS_SERVER_LOCAL_PORT *cms_local_port = find_local_port (_buffer_number);
  if (NULL == cms_local_port)
    {
      return 1;
    }
  if (NULL == cms_local_port->cms)
    {
      return 1;
    }
  return cms_local_port->cms->total_subdivisions;
}

void
CMS_SERVER::set_diag_info_from_client_id (  
					  __unused_parameter__ REMOTE_CLIENT_ID *,
					       
					  REMOTE_SET_DIAG_INFO_REQUEST * _diag_info
					  )
{
  set_diag_info(0,_diag_info);
}

void
CMS_SERVER::reset_diag_info_from_client_id ( 
					    __unused_parameter__ REMOTE_CLIENT_ID *,
					    long buffer_number
					    )
{
  reset_diag_info(0,buffer_number);
}


void
CMS_SERVER::set_diag_info ( 
#ifdef ENABLE_RCS_DIAG
			   CMS_CLIENT_INFO *_current_client_info,
			   REMOTE_SET_DIAG_INFO_REQUEST * _diag_info
#else
			   __unused_parameter__ CMS_CLIENT_INFO *,
			   __unused_parameter__ REMOTE_SET_DIAG_INFO_REQUEST *
#endif
			   )
{
#ifdef ENABLE_RCS_DIAG
  diag_enabled = 1;
  CMS_SERVER_LOCAL_PORT *local_port =
    find_local_port (_diag_info->buffer_number);
  if (NULL == local_port)
    {
      rcs_print_error
	("CMS_SERVER: Cannot find local port for buffer number %ld\n",
	 _diag_info->buffer_number);
      return;
    }
  local_port->set_diag_info (_current_client_info,_diag_info);
  last_local_port_used = local_port;
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
#endif
}

void
CMS_SERVER::reset_diag_info ( 
#ifdef ENABLE_RCS_DIAG
			     CMS_CLIENT_INFO *_current_client_info,
			     long buffer_number
#else
			     __unused_parameter__ CMS_CLIENT_INFO *,
			     __unused_parameter__ long
#endif
			     )
{
#ifdef ENABLE_RCS_DIAG
  diag_enabled = 0;
  CMS_SERVER_LOCAL_PORT *local_port = find_local_port (buffer_number);
  if (NULL == local_port)
    {
      rcs_print_error
	("CMS_SERVER: Cannot find local port for buffer number %ld\n",
	 buffer_number);
      return;
    }
  local_port->reset_diag_info (_current_client_info);
  last_local_port_used = NULL;
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
#endif
}

void CMS_SERVER::release_client_info(REMOTE_CLIENT_ID *rcid)
{
  if(!rcid || !client_info_list)
    {
      return;
    }
  CMS_CLIENT_INFO *info =
    (CMS_CLIENT_INFO *) client_info_list->get_head ();
  while (info)
    {
      if (info->id.long_id[0] == rcid->long_id[0] &&
	  info->id.long_id[1] == rcid->long_id[1])
	{
	  info->release();
	  client_info_list->delete_current_node();
	}
      info = (CMS_CLIENT_INFO *) client_info_list->get_next ();
    }
}

CMS_CLIENT_INFO *
CMS_SERVER::get_client_info(REMOTE_CLIENT_ID *_rcid, long _buffer_number)
{
  CMS_CLIENT_INFO *info=0;
  if (0 == client_info_list)
    {
      client_info_list = new RCS_LINKED_LIST ();
    }
  info =(CMS_CLIENT_INFO *) client_info_list->get_head ();
  while (info)
    {
      if (info->id.long_id[0] == _rcid->long_id[0] &&
	  info->id.long_id[1] == _rcid->long_id[1] &&
	  info->buffer_num == _buffer_number)
	{
	  break;
	}
      info = (CMS_CLIENT_INFO *) client_info_list->get_next ();
    }
  if (!info)
    {
      info = new CMS_CLIENT_INFO ();
      info->mrpq_reader_id = -1;
      info->id = *(_rcid);
      info->buffer_num = _buffer_number;
      rcs_print_debug(PRINT_MISC,
		      "new CMS_CLIENT_INFO () = %p, info->long_id[0]=%ld,info->long_id[1]=%ld,info->buffer_num=%d\n",
		      (void*)info,
		      info->id.long_id[0],info->id.long_id[1],info->buffer_num);
      client_info_list->store_at_tail (info, sizeof (CMS_CLIENT_INFO), 0);
    }
  return info;
}


double
CMS_SERVER::get_dvar(long buffer_number, const char *var_name, int &id, long type, bool &got_dvar, bool read_new)
{
  CMS_SERVER_LOCAL_PORT *local_port;
 
  got_dvar=false;
  local_port = find_local_port (buffer_number);
  last_local_port_used = local_port;
  if (NULL == local_port)
    {
      rcs_print_error
	("CMS_SERVER: Cannot find local port for buffer number %ld\n",
	 buffer_number);
      return (-99999.999);
    }
  return local_port->get_dvar(var_name,id,type,got_dvar,read_new);
}

REMOTE_CMS_REPLY *
CMS_SERVER::process_request (REMOTE_CMS_REQUEST * _request, CMS_CLIENT_INFO *_info)
{
  CMS_SERVER_LOCAL_PORT *local_port;

  requests_processed++;

  if (NULL == _request)
    {
      rcs_print_error ("CMS_SERVER: Request is NULL.\n");
      return NULL;
    }

  local_port = find_local_port (_request->buffer_number);
  last_local_port_used = local_port;
  if (NULL == local_port)
    {
      rcs_print_error
	("CMS_SERVER: Cannot find local port for buffer number %ld\n",
	 _request->buffer_number);
      return (NULL);
    }
  rcs_print_debug(PRINT_MISC,"_request->type=%d,_request->clientid.use_me=%d,  local_port->enable_xml_differencing=%d,local_port->multireader_priority_queue_enabled=%d\n",
		  _request->type,
		  _request->clientid.use_me,
		  local_port->enable_xml_differencing, 
		  local_port->multireader_priority_queue_enabled);

  CMS_CLIENT_INFO *info = _info;

  if (_request->clientid.use_me && 0 == info && 
       (local_port->enable_xml_differencing  
	|| local_port->multireader_priority_queue_enabled))
    {
      info = get_client_info(&_request->clientid, _request->buffer_number);
    }


  if (!security_check
      (_request, remote_port->current_user_info, _request->buffer_number))
    {
      return NULL;
    }

  local_port->cms->set_subdivision (_request->subdiv);
  _request->subdiv = 0;

  switch (_request->type)
    {
    case REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE:
      {
	REMOTE_GET_BUF_NAME_REPLY *namereply = local_port->get_buf_name_reply();
	return namereply;
      }

    case REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE:
      {
	REMOTE_GET_MSG_TYPE_REPLY *msg_type_reply  = 
	  local_port->get_msg_type(info,(REMOTE_GET_MSG_TYPE_REQUEST *)_request);
	return msg_type_reply;
      }

    case REMOTE_CMS_READ_REQUEST_TYPE:
      return (local_port->reader (info,(REMOTE_READ_REQUEST *) _request));
    case REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE:
#ifdef ENABLE_RCS_DIAG
      return (local_port->get_diag_info
	      (info, (REMOTE_GET_DIAG_INFO_REQUEST *) _request));
#else
      rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
      return NULL;
#endif
    case REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE:
      return (local_port->blocking_read (info, (REMOTE_READ_REQUEST *) _request));
    case REMOTE_CMS_WRITE_REQUEST_TYPE:
    case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
    case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE:
      return (local_port->writer (info, (REMOTE_WRITE_REQUEST *) _request));
    case REMOTE_CMS_CHECK_IF_READ_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(cir_reply_ptr ==0)
	{
	  cir_reply_ptr = new REMOTE_CHECK_IF_READ_REPLY();
	}
      cir_reply_ptr->was_read = local_port->cms->check_if_read ();
      cir_reply_ptr->status = local_port->cms->status;
      return (cir_reply_ptr);

    case REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(gmc_reply_ptr ==0)
	{
	  gmc_reply_ptr = new REMOTE_GET_MSG_COUNT_REPLY();
	}
      gmc_reply_ptr->count = local_port->cms->get_msg_count ();
      gmc_reply_ptr->status = local_port->cms->status;
      return (gmc_reply_ptr);


    case REMOTE_CMS_GET_READ_COUNT_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(grc_reply_ptr ==0)
	{
	  grc_reply_ptr = new REMOTE_GET_READ_COUNT_REPLY();
	}
      grc_reply_ptr->count = local_port->cms->get_read_count ();
      grc_reply_ptr->status = local_port->cms->status;
      return (grc_reply_ptr);

    case REMOTE_CMS_GET_IS_CLEAR_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(gic_reply_ptr ==0)
	{
	  gic_reply_ptr = new REMOTE_GET_IS_CLEAR_REPLY();
	}
      gic_reply_ptr->is_clear = local_port->cms->get_is_clear ();
      gic_reply_ptr->status = local_port->cms->status;
      return (gic_reply_ptr);

    case REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(gql_reply_ptr ==0)
	{
	  gql_reply_ptr = new REMOTE_GET_QUEUE_LENGTH_REPLY();
	}
      gql_reply_ptr->queue_length = local_port->cms->get_queue_length ();
      gql_reply_ptr->status = local_port->cms->status;
      return (gql_reply_ptr);

    case REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      if(gsa_reply_ptr ==0)
	{
	  gsa_reply_ptr = new REMOTE_GET_SPACE_AVAILABLE_REPLY();
	}
      gsa_reply_ptr->space_available = local_port->cms->get_space_available ();
      gsa_reply_ptr->status = local_port->cms->status;
      return (gsa_reply_ptr);

    case REMOTE_CMS_CLEAR_REQUEST_TYPE:
      if (NULL == local_port->cms)
	{
	  rcs_print_error
	    ("CMS_SERVER: cms object associated with local port is NULL.\n");
	  return (NULL);
	}
      local_port->cms->clear ();
      if(clear_reply_struct_ptr ==0)
	{
	  clear_reply_struct_ptr = new REMOTE_CLEAR_REPLY();
	}
      clear_reply_struct_ptr->status = local_port->cms->status;
      return (clear_reply_struct_ptr);

    case REMOTE_CMS_GET_KEYS_REQUEST_TYPE:
      if(get_keys_reply_ptr ==0)
	{
	  get_keys_reply_ptr = new REMOTE_GET_KEYS_REPLY();
	}
      get_user_keys (((REMOTE_GET_KEYS_REQUEST *) _request)->name,
		     get_keys_reply_ptr->key1, get_keys_reply_ptr->key2);
      return (get_keys_reply_ptr);

    case REMOTE_CMS_LOGIN_REQUEST_TYPE:
      if(login_reply_ptr ==0)
	{
	  login_reply_ptr = new REMOTE_LOGIN_REPLY();
	}
      if (NULL == remote_port->current_connected_user_struct)
	{
	  login_reply_ptr->success = 0;
	  return (login_reply_ptr);
	}
      remote_port->current_connected_user_struct->user_info =
	get_user_info (((REMOTE_LOGIN_REQUEST *) _request)->name,
		       ((REMOTE_LOGIN_REQUEST *) _request)->passwd);
      login_reply_ptr->success =
	(NULL != remote_port->current_connected_user_struct->user_info);
      if (login_reply_ptr->success)
	{
	  rcs_print ("%s logged in.\n",
		     remote_port->current_connected_user_struct->user_info->
		     name);
	}
      return (login_reply_ptr);

    case REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE:
      if(set_subscription_reply_ptr ==0)
	{
	  set_subscription_reply_ptr = new REMOTE_SET_SUBSCRIPTION_REPLY();
	}
      set_subscription_reply_ptr->success = 1;
      return (set_subscription_reply_ptr);

    default:
      rcs_print_error ("CMS_SERVER: Invalid request type (%d)\n",
		       _request->type);
      return (NULL);
    }
}

#if MS_WINDOWS_API
DWORD
CMS_ServerRun (LPVOID svr)
{
  if (NULL == svr)
    {
      return FALSE;
    }
  ((CMS_SERVER *) svr)->run ();
  return FALSE;
}
#endif

void CMS_SERVER::list_cleanup()
{
}

void CMS_SERVER::delete_all_local_ports_preserving_resources()
{
}

#ifdef POSIX_THREADS
void *cms_server_pthread_func(void *arg)
{
  CMS_SERVER *svr=(CMS_SERVER *)arg;
  rcs_print_debug(PRINT_MISC,
		  "cms_server_pthread_func(%p) called.\n",(void*)arg);
  sigset_t new_sigmask;
  sigemptyset(&new_sigmask);
  sigaddset(&new_sigmask,SIGINT);
  sigaddset(&new_sigmask,SIGPIPE);
  pthread_sigmask(SIG_BLOCK,&new_sigmask,0);
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE,0);
  if(svr)
    {
      svr->run(0);
    }
  return 0;
}
#endif

#ifdef VXWORKS
extern "C" int vx_cms_server_run_func(CMS_SERVER *svr,...);

int vx_cms_server_run_func(CMS_SERVER *svr,...)
{
  if(svr)
    {
      svr->run();
    }
  return 0;
}
#endif

/* Spawning Routine. */
int
CMS_SERVER::spawn ()
{
  cms_svr_already_clean=false;
  if (0 == server_spawned)
    {
      if (NULL != remote_port)
	{
	  remote_port->running = 0;
	}
      server_spawned = 1;
      rcs_print_debug(PRINT_MISC,
		      "CMS_SERVER::spawn() this=%p,ptinfo=%p, creator_pid=%ld,spawner_pid=%ld\n",
		      (void*)this,
		      (void*)ptinfo,ptinfo->creator_pid,ptinfo->spawner_pid);

#if defined(MS_WINDOWS_API) && defined(MULTITHREADED)
      ptinfo->server_pid = ptinfo->current_pid = ptinfo->spawner_pid = GetCurrentProcessId ();
      ptinfo->current_tid = ptinfo->spawner_tid = GetCurrentThreadId ();
      ptinfo->server_thread_handle = (HANDLE) _beginthread ((void (__cdecl *)
						     (void *)) CMS_ServerRun,
						    0, (LPVOID) this);
      if (((long) ptinfo->server_thread_handle) < 0)
	{
	  rcs_print_sys_error (ERRNO_ERROR_SOURCE, "CreateThread failed.");
	  return -1;
	}
#else
#ifndef VXWORKS
#ifdef POSIX_THREADS
      ptinfo->spawner_pthread_id = pthread_self();
      rcs_print_debug(PRINT_MISC,"ptinfo->spawner_pthread_id=%ld",(long)ptinfo->spawner_pthread_id);
      int pcret = pthread_create(&(ptinfo->server_pthread_id), // pthread_t *,
				 0, // pthread_attr_t * attr
				 cms_server_pthread_func, // void *(*start_routine)(void *)
				 this // void * arg
				 );
      // printf("%s:%d pthread_create created %d(0x%x)\n",
      // 	     __FILE__,__LINE__, 
      // 	     (int) ptinfo->server_pthread_id, (unsigned) ptinfo->server_pthread_id);
      rcs_print_debug(PRINT_MISC,
		      "pcret=%d,thread=%ld\n",
		      pcret,(long)(ptinfo->server_pthread_id));
#else
      ptinfo->current_pid = ptinfo->spawner_pid = getpid ();
      if (0 == (ptinfo->server_pid = fork ()))
	{
	  /* Child */
	  rcs_print_debug(PRINT_MISC,"CMS_SERVER::spawn() doing list_cleanup()");
	  list_cleanup();
	  rcs_print_debug(PRINT_MISC,"CMS_SERVER::spawn() -> calling run()");
	  run ();		/* This will only return if an error occurs. */
	  rcs_print_debug(PRINT_MISC,"CMS_SERVER::spawn() -> run() returned.");
	  clean (2);
	  rcs_print_debug(PRINT_MISC,"CMS_SERVER::spawn() calling exit(-1).");
	  exit (-1);
	}
      else
	{
	  rcs_print_debug(PRINT_MISC,"CMS_SERVER::spawn() doing delete_all_local_ports_preserving_resources()");
	  delete_all_local_ports_preserving_resources();
	  /* Parent */
	}
#endif
#else
      ptinfo->spawner_pid = taskIdSelf ();
      if (ptinfo->spawner_pid == ptinfo->creator_pid)
	{
	  ptinfo->server_pid =
	    taskSpawn (NULL, cms_server_task_priority, VX_FP_TASK,
		       cms_server_task_stack_size,
		       (FUNCPTR) (&vx_cms_server_run_func), 
		       (int) this, 0, 0, 0, 0,
		       0, 0, 0, 0, 0);
	}
      else
	{
	  ptinfo->spawner_pid = 0;
	  return 0;
	}
#endif
#endif
      int waits = 0;
      while (waits < 20)
	{
	  esleep (0.01);
	  if (NULL == remote_port)
	    {
	      break;
	    }
	  if (remote_port->running)
	    {
	      break;
	    }
	  waits++;
	}
      return 1;
    }
  return 0;
}

void
CMS_SERVER::kill_server ()
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::kill_server() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);

  if(remote_port)
    {
      remote_port->killme();
    }

#ifdef POSIX_THREADS
  pthread_t self_ret = pthread_self();
  rcs_print_debug(PRINT_MISC,"self_ret=%ld,ptinfo->server_pthread_id=%ld,ptinfo->spawner_pthread_id=%ld\n",
		  (long) self_ret,
		  (long) ptinfo->server_pthread_id,
		  (long) ptinfo->spawner_pthread_id);
  if(ptinfo->server_pthread_id > 0 && 
     ptinfo->spawner_pthread_id > 0 &&
     self_ret > 0 &&
     !pthread_equal(ptinfo->server_pthread_id,self_ret) &&
     pthread_equal(ptinfo->spawner_pthread_id,self_ret))
    {
      rcs_print_debug(PRINT_MISC,"calling pthread_cancel(%ld) . . .\n",(long) ptinfo->server_pthread_id);
      pthread_t thread_to_cancel = ptinfo->server_pthread_id;
      ptinfo->server_pthread_id=0;
      // printf("%s:%d pthread_cancel(%d (0x%X))\n",
      // 	     __FILE__,__LINE__,
      // 	     (int) thread_to_cancel, (unsigned) thread_to_cancel);
      if(is_cancelled_thread_id((void *) (&thread_to_cancel)))
	{
	  return;
	}
      mark_cancelled_thread_id((void *) (&thread_to_cancel));
      pthread_cancel(thread_to_cancel);
      rcs_print_debug(PRINT_MISC,"called pthread_cancel(%ld).\n",(long) ptinfo->server_pthread_id);

      rcs_print_debug(PRINT_MISC,"calling pthread_join(%ld) . . .\n",(long) ptinfo->server_pthread_id);
      void *returned_value;
     //  printf("%s:%d pthread_join(%d (0x%X))\n",
      // 	     __FILE__,__LINE__,
      // 	     (int) thread_to_cancel, (unsigned) thread_to_cancel);
      pthread_join(thread_to_cancel,
		   &returned_value);
      rcs_print_debug(PRINT_MISC,"called pthread_join(%ld).\n",(long) ptinfo->server_pthread_id);
      ptinfo->server_pthread_id=0;
      return;
    }
#else
#ifdef VXWORKS
  ptinfo->current_pid = taskIdSelf ();
  if (ptinfo->current_pid != ptinfo->spawner_pid)
    {
      return;
    }
#else
#if HAVE_GETPID
#if MS_WINDOWS_API
  DWORD my_pid;
#else
  long my_pid;
#endif
  my_pid = getpid();
  if(!ptinfo || my_pid == ptinfo->server_pid)
    {
      return;
    }
#endif
#endif
  if (0 != ptinfo->server_pid)
    {
#if MS_WINDOWS_API
      if (ptinfo->server_pid != ptinfo->current_pid)
	{
	  GenerateConsoleCtrlEvent (CTRL_C_EVENT, ptinfo->server_pid);
	}
#else
      signal (SIGINT, SIG_DFL);
      cms_server_count--;
      kill (ptinfo->server_pid, SIGINT);
#ifdef VXWORKS
      int count = 0;
      while (OK == taskIdVerify (ptinfo->server_pid) && count < 100)
	{
	  taskDelay (1);
	  count++;
	}
      if (OK == taskIdVerify (ptinfo->server_pid))
	{
	  taskDelete (ptinfo->server_pid);
	}
#else
      int waitpidtries=0;
      while(-1 == waitpid(ptinfo->server_pid,NULL,WNOHANG) &&
	    waitpidtries < 20)
	{
	  esleep(0.05);
	  waitpidtries++;
	}
      if(waitpidtries >= 20)
	{
	  kill (ptinfo->server_pid, SIGKILL);
	}
#endif
#endif
      ptinfo->server_pid = 0;
    }
#endif
}



/* MAIN ROUTINE */
void
CMS_SERVER::register_server (int setup_CC_signal_local_port)
{
  last_local_port_used = NULL;
  server_registered = 1;
#ifdef VXWORKS
  taskLock ();
  if (NULL == cms_server_list_mutex)
    {
      cms_server_list_mutex =
	semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE | SEM_DELETE_SAFE);
    }

  if (NULL == cms_server_list_mutex)
    {
      taskUnlock ();
      return;
    }
  int sem_take_succeeded = 0;
  if (OK == semTake (cms_server_list_mutex, NO_WAIT))
    {
      sem_take_succeeded = 1;
      taskUnlock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"locking mutex . . .\n");
  pthread_mutex_lock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"locked mutex.\n");
#endif

  if (NULL == cms_server_list)
    {
      cms_server_list = new RCS_LINKED_LIST;
    }
  list_id = cms_server_list->store_at_tail (this, sizeof (CMS_SERVER), 0);

#ifdef VXWORKS
  if (sem_take_succeeded)
    {
      semGive (cms_server_list_mutex);
    }
  else
    {
      taskUnlock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"unlocking mutex . . .\n");
  pthread_mutex_unlock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"unlocked mutex.\n");
#endif


  /* Set up interrupt local_port. */
  if (setup_CC_signal_local_port)
    {
      rcs_print_debug(PRINT_MISC,"old_sigint_handler=%p\n",(void*)old_sigint_handler);

#if MS_WINDOWS_API && HAVE_SET_CONSOLE_CTRL_HANDLER
      SetConsoleCtrlHandler ((PHANDLER_ROUTINE) clean, TRUE);
#else
      old_sigint_handler = signal (SIGINT, clean);	/* Set up interrupt local_port. */
#endif
      rcs_print_debug(PRINT_MISC,"old_sigint_handler=%p\n",(void*)old_sigint_handler);
      cms_svr_already_clean=false;
    }

  if (NULL == remote_port)
    {
      rcs_print_error ("CMS_SERVER: Can't register with NULL remote port.\n");
      return;
    }
  remote_port->register_port ();

}


void
CMS_SERVER::run (int setup_CC_signal_local_port)
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::run() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);
  if(ptinfo)
    {
      rcs_print_debug(PRINT_MISC,"CMS_SERVER::run() creator_pid=%ld,spawner_pid=%ld\n",
		      ptinfo->creator_pid,ptinfo->spawner_pid);
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
      ptinfo->server_pid = ptinfo->current_pid = GetCurrentProcessId ();
      ptinfo->server_tid = ptinfo->current_tid = GetCurrentThreadId ();
#else
      ptinfo->server_tid = ptinfo->current_tid = 0;
#ifdef VXWORKS
      ptinfo->server_pid = taskIdSelf ();
#else
      ptinfo->current_pid = ptinfo->server_pid = getpid ();
#endif
#endif
      rcs_print_debug(PRINT_MISC,"CMS_SERVER::run() called. server_pid=%ld\n",ptinfo->server_pid);
    }
  if (!server_registered)
    {
      register_server (setup_CC_signal_local_port);
    }
  initialize_write_request_space ();
  if (NULL == remote_port)
    {
      rcs_print_error
	("CMS_SERVER: Cannot run with remote port equal to NULL.\n");
      return;
    }
  remote_port->running = 1;
  if (remote_port->port_registered)
    {
      remote_port->run ();
    }
}

void
CMS_SERVER::initialize_write_request_space ()
{
  max_total_subdivisions = 1;
  maximum_cms_size = 0;
  CMS_SERVER_LOCAL_PORT *local_port;
  if (NULL == cms_local_ports)
    {
      rcs_print_error
	("CMS_SERVER: Can not search list of local ports to determine the size of space needed for the write request\n"
	 "because the list is NULL.\n");
      return;
    }
  local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_head ();
  while (NULL != local_port)
    {
      if (NULL != local_port->cms)
	{
	  if (local_port->cms->max_message_size > maximum_cms_size)
	    {
	      maximum_cms_size = local_port->cms->max_message_size;
	    }
	  if (local_port->cms->total_subdivisions > max_total_subdivisions)
	    {
	      max_total_subdivisions = local_port->cms->total_subdivisions;
	    }
	  if (NULL != remote_port)
	    {
	      if (local_port->cms->total_subdivisions >
		  remote_port->max_total_subdivisions)
		{
		  remote_port->max_total_subdivisions =
		    local_port->cms->total_subdivisions;
		}
	    }
	  if (local_port->cms->max_encoded_message_size > maximum_cms_size)
	    {
	      maximum_cms_size = local_port->cms->max_encoded_message_size;
	    }
	}
      local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_next ();
    }
  if(write_req_ptr && write_req_ptr->data)
    {
      DEBUG_FREE (write_req_ptr->data);
      write_req_ptr->data = NULL;
    }
  if(write_req_ptr ==0)
    {
      write_req_ptr = new REMOTE_WRITE_REQUEST();
    }
  write_req_ptr->data = DEBUG_MALLOC (maximum_cms_size);
  if (NULL == write_req_ptr->data)
    {
      rcs_print_error ("malloc(%ld) failed.\n", maximum_cms_size);
    }
  local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_head ();
  while (NULL != local_port)
    {
      if (NULL != local_port->cms)
	{
	  local_port->cms->set_encoded_data (write_req_ptr->data,
					     maximum_cms_size);
	}
      local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_next ();
    }
}

CMS_SERVER::CMS_SERVER ():
  security_enabled(0),
  server_spawned(0),
  server_registered(0),
  list_id(0),
  cms_local_ports(0),
  cir_reply_ptr(0),
  gmc_reply_ptr(0),
  grc_reply_ptr(0),
  gic_reply_ptr(0),
  gql_reply_ptr(0),
  gsa_reply_ptr(0),
  clear_reply_struct_ptr(0),
  using_passwd_file(0),
  requests_processed(0),
  remote_port(0),
  ptinfo(0),
  maximum_cms_size(0),
  read_req_ptr(0),
  write_req_ptr(0),
  get_keys_req_ptr(0),
  login_req_ptr(0),
  set_subscription_req_ptr(0),
  check_if_read_req_ptr(0),
  get_msg_count_req_ptr(0),
  get_read_count_req_ptr(0),
  get_is_clear_req_ptr(0),
  get_queue_length_req_ptr(0),
  get_space_available_req_ptr(0),
  clear_req_ptr(0),
  set_diag_info_req_ptr(0),
  get_diag_info_req_ptr(0),
  write_reply_ptr(0),
  get_keys_reply_ptr(0),
  login_reply_ptr(0),
  set_subscription_reply_ptr(0),
  check_if_read_reply_ptr(0),
  get_msg_count_reply_ptr(0),
  get_queue_length_reply_ptr(0),
  get_space_available_reply_ptr(0),
  clear_reply_ptr(0),
  set_diag_info_reply_ptr(0),
  get_diag_info_reply_ptr(0),
  last_local_port_used(0),
  diag_enabled(0),
  max_total_subdivisions(0),
  time_of_last_key_request(0),
  known_users(0),
  guest_can_read(0),
  guest_can_write(0),
  client_info_list(0)
{
  ptinfo = new CMS_SERVER_PROCESS_THREAD_INFO;
  last_local_port_used = NULL;
  
  read_req_ptr=0;
  write_req_ptr=0;
  get_keys_req_ptr=0;
  login_req_ptr=0;
  set_subscription_req_ptr=0;
  check_if_read_req_ptr=0;
  get_msg_count_req_ptr=0;
  get_queue_length_req_ptr=0;
  get_space_available_req_ptr=0;
  clear_req_ptr=0;
  set_diag_info_req_ptr=0;
  get_diag_info_req_ptr=0;
  write_reply_ptr=0;
  get_keys_reply_ptr=0;
  login_reply_ptr=0;
  set_subscription_reply_ptr=0;
  check_if_read_reply_ptr=0;
  get_msg_count_reply_ptr=0;
  get_queue_length_reply_ptr=0;
  get_space_available_reply_ptr=0;
  clear_reply_ptr=0;
  set_diag_info_reply_ptr=0;
  get_diag_info_reply_ptr=0;
  cir_reply_ptr = 0;
  gmc_reply_ptr = 0;
  grc_reply_ptr = 0;
  gic_reply_ptr = 0;
  gql_reply_ptr = 0;
  gsa_reply_ptr = 0;
  clear_reply_struct_ptr=0;
    
  diag_enabled = 0;
  using_passwd_file = 0;
  ptinfo->current_pid = 0;
  ptinfo->server_pid = 0;
  ptinfo->spawner_pid = 0;
  server_registered = 0;
  guest_can_read = 0;
  guest_can_write = 0;
  server_spawned = 0;
  list_id = 0;
  requests_processed = 0;
  write_reply_ptr = NULL;
  check_if_read_reply_ptr = NULL;
  clear_reply_ptr = NULL;
  remote_port = NULL;
  write_req_ptr = NULL;
  cms_local_ports = new RCS_LINKED_LIST;
  known_users = NULL;
  max_total_subdivisions = 1;
  memset (passwd_file, 0, 256);
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  ptinfo->creator_pid = GetCurrentProcessId ();
  ptinfo->creator_tid = GetCurrentThreadId ();
#else
#ifndef VXWORKS
  ptinfo->creator_pid = getpid ();
#else
  ptinfo->creator_pid = taskIdSelf ();
#endif
#endif
#ifdef POSIX_THREADS
  ptinfo->server_pthread_id=0;
  ptinfo->spawner_pthread_id=0;
#endif
  client_info_list = NULL;
}

CMS_SERVER::~CMS_SERVER ()
{
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::~CMS_SERVER() called. (this=%p)\n",
		  (void*)this);

  last_local_port_used = NULL;
#ifdef VXWORKS
  ptinfo->current_pid = taskIdSelf ();
#endif
  if (server_registered && (!server_spawned || ptinfo->current_pid == ptinfo->server_pid))
    {
      unregister_server ();
    }
  else if (server_spawned && ptinfo->current_pid == ptinfo->spawner_pid)
    {
      kill_server ();
    }
  delete_all_local_ports ();
  if (NULL != remote_port)
    {
      delete remote_port;
      remote_port = NULL;
    }
  if (NULL != cms_local_ports)
    {
      delete cms_local_ports;
      cms_local_ports = NULL;
    }

  // Leave this to NML_SERVER destructor.
  // delete_from_list();

  if (write_req_ptr)
    {
      if(NULL != write_req_ptr->data)
	{
	  DEBUG_FREE (write_req_ptr->data);
	  write_req_ptr->data = NULL;
	}
      delete write_req_ptr;
      write_req_ptr=0;
    }
  if (0 != client_info_list)
    {
      CMS_CLIENT_INFO *info =
	(CMS_CLIENT_INFO *) client_info_list->get_head ();
      while (info)
	{
	  info->release();
	  info =
	    (CMS_CLIENT_INFO *) client_info_list->get_next ();
	}
      client_info_list->delete_members ();
      delete client_info_list;
      client_info_list = 0;
    }
  if(0 != ptinfo)
    {
      delete ptinfo;
      ptinfo=0;
    }
  if(cir_reply_ptr)
    {
      delete cir_reply_ptr;
      cir_reply_ptr=0;
    }
  if(gmc_reply_ptr)
    {
      delete gmc_reply_ptr;
      gmc_reply_ptr=0;
    }
  if(grc_reply_ptr)
    {
      delete grc_reply_ptr;
      grc_reply_ptr=0;
    }
  if(gic_reply_ptr)
    {
      delete gic_reply_ptr;
      gic_reply_ptr=0;
    }
  if(gql_reply_ptr)
    {
      delete gql_reply_ptr;
      gql_reply_ptr=0;
    }
  if(gsa_reply_ptr)
    {
      delete gsa_reply_ptr;
      gsa_reply_ptr=0;
    }
  if(clear_reply_struct_ptr)
    {
      delete clear_reply_struct_ptr;
      clear_reply_struct_ptr=0;
    }

  if(read_req_ptr)
    {
      delete read_req_ptr;
      read_req_ptr=0;
    }

  if(write_req_ptr)
    {
      delete write_req_ptr;
      write_req_ptr=0;
    }

  if(get_keys_req_ptr)
    {
      delete get_keys_req_ptr;
      get_keys_req_ptr=0;
    }

  if(login_req_ptr)
    {
      delete login_req_ptr;
      login_req_ptr=0;
    }

  if(set_subscription_req_ptr)
    {
      delete set_subscription_req_ptr;
      set_subscription_req_ptr=0;
    }

  if(check_if_read_req_ptr)
    {
      delete check_if_read_req_ptr;
      check_if_read_req_ptr=0;
    }

  if(get_msg_count_req_ptr)
    {
      delete get_msg_count_req_ptr;
      get_msg_count_req_ptr=0;
    }

  if(get_queue_length_req_ptr)
    {
      delete get_queue_length_req_ptr;
      get_queue_length_req_ptr=0;
    }

  if(get_space_available_req_ptr)
    {
      delete get_space_available_req_ptr;
      get_space_available_req_ptr=0;
    }

  if(clear_req_ptr)
    {
      delete clear_req_ptr;
      clear_req_ptr=0;
    }

  if(set_diag_info_req_ptr)
    {
      delete set_diag_info_req_ptr;
      set_diag_info_req_ptr=0;
    }

  if(get_diag_info_req_ptr)
    {
      delete get_diag_info_req_ptr;
      get_diag_info_req_ptr=0;
    }

  if(write_reply_ptr)
    {
      delete write_reply_ptr;
      write_reply_ptr=0;
    }

  if(get_keys_reply_ptr)
    {
      delete get_keys_reply_ptr;
      get_keys_reply_ptr=0;
    }

  if(login_reply_ptr)
    {
      delete login_reply_ptr;
      login_reply_ptr=0;
    }

  if(set_subscription_reply_ptr)
    {
      delete set_subscription_reply_ptr;
      set_subscription_reply_ptr=0;
    }

  if(check_if_read_reply_ptr)
    {
      delete check_if_read_reply_ptr;
      check_if_read_reply_ptr=0;
    }

  if(get_msg_count_reply_ptr)
    {
      delete get_msg_count_reply_ptr;
      get_msg_count_reply_ptr=0;
    }

  if(get_queue_length_reply_ptr)
    {
      delete get_queue_length_reply_ptr;
      get_queue_length_reply_ptr=0;
    }

  if(get_space_available_reply_ptr)
    {
      delete get_space_available_reply_ptr;
      get_space_available_reply_ptr=0;
    }

  if(clear_reply_ptr)
    {
      delete clear_reply_ptr;
      clear_reply_ptr=0;
    }

  if(set_diag_info_reply_ptr)
    {
      delete set_diag_info_reply_ptr;
      set_diag_info_reply_ptr=0;
    }

  if(get_diag_info_reply_ptr)
    {
      delete get_diag_info_reply_ptr;
      get_diag_info_reply_ptr=0;
    }
}


void
CMS_SERVER::delete_all_local_ports ()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER::delete_all_local_ports() called. (this=%p,cms_local_ports=%p)\n",
		  (void*)this,(void*)cms_local_ports);
#ifdef POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"locking mutex . . .\n");
  pthread_mutex_lock(&cms_local_ports_pmutex);
  rcs_print_debug(PRINT_MISC,"locked mutex.\n");
#endif
  if (NULL != cms_local_ports)
    {
      CMS_SERVER_LOCAL_PORT *local_port;
      local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_head ();
      while (NULL != local_port)
	{
	  delete local_port;
	  cms_local_ports->delete_current_node ();
	  local_port = (CMS_SERVER_LOCAL_PORT *) cms_local_ports->get_next ();
	}
      delete cms_local_ports;
      cms_local_ports=0;
    }
#ifdef POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"unlocking mutex . . .\n");
  pthread_mutex_unlock(&cms_local_ports_pmutex);
  rcs_print_debug(PRINT_MISC,"unlocked mutex.\n");
#endif
}

static int last_cms_server_signum = 0;

void
CMS_SERVER::clean (int signum)
{
  last_cms_server_signum = signum;
  rcs_print_debug(PRINT_MISC,"CMS_SERVER::clean(%d) called. cms_svr_already_clean=%d\n",signum,cms_svr_already_clean);
  rcs_print_debug(PRINT_MISC,"old_sigint_handler=%p\n",
		  (void*)old_sigint_handler);
#ifdef POSIX_THREADS
  if(cms_svr_already_clean)
    {
      return;
    }
#endif
  if(old_sigint_handler)
    {
      if(old_sigint_handler != SIG_IGN &&
	 old_sigint_handler != SIG_DFL)
	{
	  old_sigint_handler(signum);
	}
    }
  cms_svr_already_clean=true;
#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  DWORD current_pid = GetCurrentProcessId ();
  DWORD current_tid = GetCurrentThreadId ();
#else
#ifdef VXWORKS
  int current_pid;
  int current_tid = 0;
  current_pid = taskIdSelf ();
#else
  pid_t current_pid;
  pid_t current_tid = 0;
  current_pid = getpid ();
#endif
#endif
  CMS_SERVER *cms_server = NULL;


#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semTake (cms_server_list_mutex, WAIT_FOREVER);
    }
  else
    {
      taskLock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"locking mutex . . .\n");
  pthread_mutex_lock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"locked mutex.\n");
#endif
  cms_server = (CMS_SERVER *) cms_server_list->get_head ();
  while (NULL != cms_server)
    {
      if (cms_server->ptinfo->server_pid == current_pid
	  && cms_server->ptinfo->server_tid == current_tid)
	{
	  cms_server->list_id = -1;
	  cms_server->unregister_server ();
	  delete cms_server;
	  cms_server = NULL;
	  cms_server_list->delete_current_node();
	}
      cms_server = (CMS_SERVER *) cms_server_list->get_next ();
    }

#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semGive (cms_server_list_mutex);
    }
  else
    {
      taskUnlock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"unlocking mutex . . .\n");
  pthread_mutex_unlock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"unlocked mutex.\n");
#endif
#ifndef POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"calling  nml_set_kill_servers_on_cleanup(false)\n");
  nml_set_kill_servers_on_cleanup(false);
  rcs_print_debug(PRINT_MISC,"calling nml_cleanup()\n");
  nml_cleanup();
  rcs_print_debug(PRINT_MISC,"calling exit(0)\n");
  exit (0);
#endif
}

void
CMS_SERVER::unregister_server ()
{
  if (server_registered)
    {
      server_registered = 0;
      if (NULL != remote_port)
	{
	  remote_port->unregister_port ();
	}
    }
}

void
CMS_SERVER::delete_from_list ()
{
  if(list_id < 0 || cms_server_list == 0)
    {
      return;
    }

#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semTake (cms_server_list_mutex, WAIT_FOREVER);
    }
  else
    {
      taskLock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"locking mutex . . .\n");
  pthread_mutex_lock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"locked mutex.\n");
#endif

#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  ptinfo->current_pid = GetCurrentProcessId ();
  ptinfo->current_tid = GetCurrentThreadId ();
#else
#ifndef VXWORKS
  ptinfo->current_pid = getpid ();
  ptinfo->current_tid = 0;
#else
  ptinfo->current_pid = taskIdSelf ();
  ptinfo->current_tid = 0;
#endif
#endif

  if (ptinfo->current_pid == ptinfo->server_pid && ptinfo->current_tid == ptinfo->server_tid)
    {
      if (NULL != cms_server_list && list_id > 0)
	{
	  cms_server_list->delete_node (list_id);
	  list_id = -1;
	}
    }

#ifdef VXWORKS
  if (NULL != cms_server_list_mutex)
    {
      semGive (cms_server_list_mutex);
    }
  else
    {
      taskUnlock ();
    }
#elif POSIX_THREADS
  rcs_print_debug(PRINT_MISC,"unlocking mutex . . .\n");
  pthread_mutex_unlock(&cms_server_list_pmutex);
  rcs_print_debug(PRINT_MISC,"unlocked mutex.\n");
#endif
}

const char *
CMS_SERVER::get_buffer_name (long buffer_number)
{
  CMS_SERVER_LOCAL_PORT *local_port;
  local_port = find_local_port (buffer_number);
  if (NULL == local_port)
    {
      return NULL;
    }
  return (const char *) local_port->cms->BufferName;
}

long
CMS_SERVER::get_message_type ()
{
  return -1;
  // I need to be overloaded.
}

int
CMS_SERVER::security_check (REMOTE_CMS_REQUEST *_request, CMS_USER_INFO * user_info, long buffer_number)
{
  CMS_SERVER_LOCAL_PORT *local_port;
  local_port = find_local_port (buffer_number);
  if (!using_passwd_file)
    {
      return 1;
    }
  if (!local_port->security_enabled)
    {
      return 1;
    }
  if (_request->type == REMOTE_CMS_GET_KEYS_REQUEST_TYPE ||
      _request->type == REMOTE_CMS_LOGIN_REQUEST_TYPE)
    {
      return 1;
    }


  if (NULL == user_info)
    {

      if (guest_can_read && (_request->type == REMOTE_CMS_READ_REQUEST_TYPE ||
			     _request->type ==
			     REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE))
	{
	  return 1;
	}

      if (guest_can_write && _request->type == REMOTE_CMS_WRITE_REQUEST_TYPE)
	{
	  return 1;
	}
      rcs_print_error
	("CMS_SERVER: Refusing to process request of unknown user.\n");
      return 0;
    }

  if (user_info->allow_read
      && (_request->type == REMOTE_CMS_READ_REQUEST_TYPE
	  || _request->type == REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE))
    {
      return 1;
    }

  if (user_info->allow_write
      && _request->type == REMOTE_CMS_WRITE_REQUEST_TYPE)
    {
      return 1;
    }

  if (NULL != detailed_security_check)
    {
      return detailed_security_check (user_info->name,
				      get_buffer_name (buffer_number),
				      get_message_type (),
				      _request->type);
    }

  if (!user_info->allow_read && _request->type == REMOTE_CMS_READ_REQUEST_TYPE)
    {
      rcs_print_error ("CMS_SERVER:: %s does not have read permission.",user_info->name);
      return 0;
    }

  if (!user_info->allow_write
      && _request->type == REMOTE_CMS_WRITE_REQUEST_TYPE)
    {
      rcs_print_error ("CMS_SERVER:: %s does not have write permission.",user_info->name);
      return 0;
    }
  return 1;

}

long
CMS_SERVER::get_server_pid_long_int(void)
{
  if(ptinfo)
    {
      return (long) ptinfo->server_pid;
    }
  return -1;
}

void
CMS_SERVER::set_spawner_pid_to_server_pid(void)
{
  if(ptinfo)
    {
      ptinfo->spawner_pid = ptinfo->server_pid;
    }
}

bool
CMS_SERVER::current_pid_equals_spawner_pid(void)
{
#if defined(MS_WINDOWS_API) && defined(HAVE_GET_CURRENT_PROCESS_ID)
  DWORD pid;
  DWORD tid;
  pid = GetCurrentProcessId();
  tid = GetCurrentThreadId();
  bool bret = ( ptinfo->spawner_pid == pid )&&
    (ptinfo->spawner_tid == tid ) && ( pid != ptinfo->server_pid );
  return bret;
#endif
#ifdef VXWORKS
  int pid;
  pid = taskIdSelf ();
  bool bret =  (ptinfo->spawner_pid != pid);
  return bret;
#endif
  return true;
}

CMS_SERVER_LOCAL_WAITING_OBJECT *
CMS_SERVER::get_new_local_waiting_object(long _buffer_number)
{
  CMS_SERVER_LOCAL_PORT *lp = find_local_port(_buffer_number);
  if(lp)
    {
      return lp->get_new_local_waiting_object();
    }
  return 0;
}

CMS_SERVER_LOCAL_WAITING_OBJECT *
CMS_SERVER_LOCAL_PORT::get_new_local_waiting_object(void)
{
  rcs_print_error("This function should never be called.\n");
  return 0;
}  


double 
CMS_SERVER_LOCAL_PORT::get_dvar(const char * /* v */, 
				int & /* id */, 
				long /*type */, 
				bool &got_dvar, 
				bool /* read_new */)
{
  got_dvar=false;
  rcs_print_error("This function should never be called.\n");
  return -77777.7777;
}  
  

int (*detailed_security_check) (const char *user_name,
				const char *buffer_name,
				long msg_type, int access_type) = NULL;

void 
CMS_SERVER_REMOTE_NULL_PORT::run ()
{
  while(true)
    {
      esleep(1.0);
    }
}

CMS_SERVER_REMOTE_NULL_PORT::CMS_SERVER_REMOTE_NULL_PORT(CMS_SERVER *__svr)
  : CMS_SERVER_REMOTE_PORT(__svr)
{
}

CMS_SERVER_REMOTE_NULL_PORT::~CMS_SERVER_REMOTE_NULL_PORT()
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_NULL_PORT::~CMS_SERVER_REMOTE_NULL_PORT() called. (this=%p)\n",
		  (void*)this);
}

void 
CMS_SERVER_REMOTE_NULL_PORT::register_port ()
{
  port_registered=1;
}

void
CMS_SERVER_REMOTE_NULL_PORT::unregister_port ()
{
}

int
CMS_SERVER_REMOTE_NULL_PORT::accept_local_port_cms (CMS * _cms)
{
  rcs_print_debug(PRINT_MISC,
		  "CMS_SERVER_REMOTE_NULL_PORT::accept_local_port_cms(_cms=%p) called. this=%p\n",
		  (void*)_cms,(void*)this);
  return(1);
}


CMS_SERVER_LOCAL_WAITING_OBJECT::CMS_SERVER_LOCAL_WAITING_OBJECT()
{
}
 
CMS_SERVER_LOCAL_WAITING_OBJECT::~CMS_SERVER_LOCAL_WAITING_OBJECT()
{
}

int 
CMS_SERVER_LOCAL_WAITING_OBJECT::wait_for_anything(void)
{
  rcs_print_error("CMS_SERVER_LOCAL_WAITING_OBJECT::wait_for_anything called.\n");
  return -1;
}

int 
CMS_SERVER_LOCAL_WAITING_OBJECT::valid(void)
{
  rcs_print_error("CMS_SERVER_LOCAL_WAITING_OBJECT::valid() called.\n");
  return 0;
}

//  defined(ENABLE_RCS_SERVER)

#else
#include "rcs_empty_source"
#endif





